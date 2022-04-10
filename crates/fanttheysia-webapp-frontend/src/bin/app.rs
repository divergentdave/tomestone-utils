use std::{
    rc::Rc,
    sync::{
        atomic::{AtomicBool, Ordering},
        Mutex,
    },
};

use gloo_events::{EventListener, EventListenerOptions};
use gloo_file::Blob;
use gloo_storage::{LocalStorage, Storage};
use gloo_timers::callback::Timeout;
use gloo_worker::{Bridge, Bridged};
use monaco::{
    api::{CodeEditorOptions, DisposableClosure, TextModel},
    sys::editor::{BuiltinTheme, IDimension, IModelContentChangedEvent},
    yew::{CodeEditor, CodeEditorLink},
};
use wasm_bindgen::{JsCast, JsValue};
use web_sys::{HtmlAnchorElement, HtmlElement, HtmlInputElement, Url};
use yew::prelude::*;

use fanttheysia_webapp_frontend::{ProgressAccumulator, Request, Response, SyntaxChecker};

enum Message {
    /// Text change event from the Monaco editor.
    ModelContentChanged(IModelContentChangedEvent),
    /// Trigger to send the editor's contents to the worker thread, for validation. This is
    /// sent by a debounce timer after the editor's text has changed, in addition to other sources.
    CheckText,
    /// Trigger to save the editor's contents to localStorage. This is done after a timer has
    /// elapsed following a text change, or before the page unloads, if the editor is dirty.
    Autosave,
    /// Signals that the window has been resized, and the editor's size should be recalculated.
    WindowResize,
    /// Incoming responses from the worker's bridge.
    WorkerMessage(Response),
    /// Fired when the user clicks on the "open" button.
    OpenFileClick,
    /// Fired when the user has selected a file to open.
    OpenFileChoice(Event),
    /// Fired when the opened file has been read in its entirety.
    OpenFileDone(String),
    /// Fired when the user clicks on the "save" button.
    SaveFile,
}

fn get_options() -> CodeEditorOptions {
    CodeEditorOptions::default().with_builtin_theme(BuiltinTheme::VsDark)
}

static STORAGE_KEY: &str = "fanttheysia_rules";

fn load_from_storage() -> Result<Option<String>, JsValue> {
    LocalStorage::raw().get(STORAGE_KEY)
}

fn save_to_storage(data: &str) -> Result<(), JsValue> {
    LocalStorage::raw().set(STORAGE_KEY, data)
}

struct Model {
    /// Monaco editor options (just its theme, for now).
    options: Rc<CodeEditorOptions>,
    /// This link will hold hooks to communicate with the Monaco editor. It is created here, and a
    /// copy is passed to the editor component via its properties.
    editor_link: CodeEditorLink,
    /// The data structure holding the editor's contents.
    model: TextModel,
    /// Event listener for "resize" on the window.
    resize_listener: Option<EventListener>,
    /// Event listener for "beforeunload" on the window.
    beforeunload_listener: Option<EventListener>,
    /// Handle for a Monaco event listener.
    _text_listener: DisposableClosure<dyn FnMut(IModelContentChangedEvent)>,
    /// `setTimeout` timer to debounce text change events, for sending it to the worker.
    edit_check_debounce_timer: Option<Timeout>,
    /// `setTimeout` timer to debounce text change events, for autosaving.
    edit_autosave_debounce_timer: Option<Timeout>,
    /// Callback used by `edit_check_debounce_timer`.
    debounced_check_callback: Callback<()>,
    /// Callback used by `edit_autosave_debounce_timer`.
    debounced_autosave_callback: Callback<()>,
    /// Flag to indicate whether the contents of the editor are unsaved. This is set when
    /// the text changes, cleared when an autosave is done, and checked in the "beforeunload"
    /// event listener.
    autosave_is_dirty: Rc<AtomicBool>,
    /// Bridge to the worker, which handles parsing off the main thread.
    worker: Box<dyn Bridge<SyntaxChecker>>,
    /// Flag to indicate there's a request in-flight to the worker.
    worker_check_busy: bool,
    /// Flag to indicate the text has changed again since sending the last request to the worker.
    /// If set, once the response arrives, a new request will be sent with the latest text, and
    /// this flag will be cleared.
    worker_check_queued: bool,
    /// Reference to the invisible `<input type="file">` used to open files.
    file_chooser_ref: NodeRef,
    /// The latest syntax check result from the worker.
    valid: Option<bool>,
    /// The latest error message from the worker (or empty, if none).
    error_message: String,
    /// The latest progress bar update from the worker.
    progress: Option<ProgressAccumulator>,
}

impl Model {
    fn load_from_file(&mut self, ctx: &Context<Self>, files: gloo_file::FileList) {
        let file = if let Some(file) = files.get(0) {
            file
        } else {
            // no files were selected
            return;
        };
        let callback = ctx.link().callback(Message::OpenFileDone);
        let reader_guard = Rc::new(Mutex::new(None));
        let reader_guard_clone = reader_guard.clone();
        let reader = gloo_file::callbacks::read_as_text(file, move |result| {
            match result {
                Ok(data) => callback.emit(data),
                Err(error) => {
                    gloo_console::error!(format!("{}", error));
                }
            }
            drop(reader_guard_clone);
        });
        // Have the closure hold the FileReader guard, so that we don't drop it earlier and cancel
        // the read.
        reader_guard.lock().unwrap().replace(reader);
    }
}

impl Component for Model {
    type Message = Message;
    type Properties = ();

    fn create(ctx: &Context<Self>) -> Self {
        let worker_callback = ctx.link().callback(Message::WorkerMessage);
        let worker = SyntaxChecker::bridge(Rc::new(move |response| worker_callback.emit(response)));

        let load_result = load_from_storage();
        let data = match &load_result {
            Ok(Some(data)) => &*data,
            Ok(None) => "",
            Err(e) => {
                gloo_console::error!(e);
                ""
            }
        };
        let model = TextModel::create(data, Some("yaml"), None).unwrap();

        let text_change_callback = ctx.link().callback(Message::ModelContentChanged);
        let text_change_listener =
            model.on_did_change_content(move |e| text_change_callback.emit(e));

        let debounced_check_callback = ctx.link().callback(|_| Message::CheckText);
        let debounced_autosave_callback = ctx.link().callback(|_| Message::Autosave);

        Self {
            options: Rc::new(get_options()),
            editor_link: CodeEditorLink::default(),
            model,
            resize_listener: None,
            beforeunload_listener: None,
            _text_listener: text_change_listener,
            edit_check_debounce_timer: None,
            edit_autosave_debounce_timer: None,
            debounced_check_callback,
            debounced_autosave_callback,
            autosave_is_dirty: Rc::new(AtomicBool::new(false)),
            worker,
            worker_check_busy: false,
            worker_check_queued: false,
            file_chooser_ref: NodeRef::default(),
            valid: None,
            error_message: String::new(),
            progress: None,
        }
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Message::ModelContentChanged(_event) => {
                self.autosave_is_dirty.store(true, Ordering::SeqCst);

                self.edit_check_debounce_timer.take().map(Timeout::cancel);
                self.edit_check_debounce_timer = Some(Timeout::new(500, {
                    let debounced_check_callback = self.debounced_check_callback.clone();
                    move || debounced_check_callback.emit(())
                }));

                self.edit_autosave_debounce_timer
                    .take()
                    .map(Timeout::cancel);
                self.edit_autosave_debounce_timer = Some(Timeout::new(10000, {
                    let debounced_autosave_callback = self.debounced_autosave_callback.clone();
                    move || debounced_autosave_callback.emit(())
                }));

                false
            }
            Message::CheckText => {
                if self.worker_check_busy {
                    self.worker_check_queued = true;
                } else {
                    self.worker_check_busy = true;
                    self.worker.send(Request::Check(self.model.get_value()));
                }
                false
            }
            Message::Autosave => {
                if let Err(e) = save_to_storage(&self.model.get_value()) {
                    gloo_console::error!(e);
                } else {
                    self.autosave_is_dirty.store(false, Ordering::SeqCst);
                }
                false
            }
            Message::WindowResize => {
                let window = web_sys::window().unwrap();
                let width = window.inner_width().unwrap().as_f64().unwrap();
                let height = window.inner_height().unwrap().as_f64().unwrap() - 120.0;
                self.editor_link
                    .with_editor(|code_editor| {
                        code_editor
                            .as_ref()
                            .layout(Some(&IDimension::new(width, height)));
                        true
                    })
                    .unwrap_or_default()
            }
            Message::WorkerMessage(response) => {
                match response {
                    Response::Valid => {
                        self.valid = Some(true);
                        gloo_console::log!("OK");
                        self.error_message.clear();
                    }
                    Response::Invalid(message) => {
                        self.valid = Some(false);
                        gloo_console::log!(&message);
                        self.error_message = message;
                        self.worker_check_busy = false
                    }
                    Response::Progress(progress) => {
                        self.progress = Some(progress);
                        self.worker_check_busy = false
                    }
                }
                if !self.worker_check_busy && self.worker_check_queued {
                    self.worker_check_queued = false;
                    ctx.link().send_message(Message::CheckText);
                }
                true
            }
            Message::OpenFileClick => {
                match self.file_chooser_ref.cast::<HtmlElement>() {
                    Some(element) => element.click(),
                    None => gloo_console::log!("file chooser reference was bad"),
                };
                false
            }
            Message::OpenFileChoice(event) => {
                if let Some(target) = event.target() {
                    if let Ok(input) = target.dyn_into::<HtmlInputElement>() {
                        if let Some(file_list) = input.files() {
                            self.load_from_file(ctx, file_list.into());
                        } else {
                            gloo_console::log!("file chooser did not have a file list");
                        }
                    } else {
                        gloo_console::log!("file chooser change event target was of wrong type");
                    }
                } else {
                    gloo_console::log!("file chooser change event had no target");
                }
                false
            }
            Message::OpenFileDone(data) => {
                self.model.set_value(&data);
                false
            }
            Message::SaveFile => {
                let blob = Blob::new_with_options(&*self.model.get_value(), Some("text/x-yaml"));
                // playing it fast and loose with error handling
                let url = Url::create_object_url_with_blob(blob.as_ref()).unwrap();
                let window = web_sys::window().unwrap();
                let document = window.document().unwrap();
                let element = document.create_element("a").unwrap();
                let anchor = element.dyn_into::<HtmlAnchorElement>().unwrap();
                anchor.set_attribute("href", &url).unwrap();
                anchor.set_attribute("download", "rules.yaml").unwrap();
                anchor.click();
                false
            }
        }
    }

    fn changed(&mut self, _ctx: &Context<Self>) -> bool {
        false
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let open_click = ctx.link().callback(|_| Message::OpenFileClick);
        let save_click = ctx.link().callback(|_| Message::SaveFile);
        let file_chooser_change = ctx.link().callback(Message::OpenFileChoice);
        let (progress_current, progress_max) = match &self.progress {
            Some(progress) => (format!("{}", progress.current), format!("{}", progress.max)),
            None => ("0".to_string(), "1".to_string()),
        };
        html! {
            <>
            <CodeEditor
                link={ Some(self.editor_link.clone()) }
                model={ Some(self.model.clone()) }
                options={ self.options.clone() }
                />
            <div>
                <button onclick={ open_click }>{"Open File"}</button>
                <input
                    ref={ self.file_chooser_ref.clone() }
                    onchange={ file_chooser_change }
                    type="file"
                    accept=".yaml,.yml,text/x-yaml,text/plain,.txt"
                    style="display: none;"
                    />
                <button onclick={ save_click }>{"Save File"}</button>
                <progress
                    value={ progress_current }
                    max={ progress_max }
                    style="width: 500px;">
                </progress>
                <StatusIndicator valid={ self.valid } />
                <span>{ &self.error_message }</span>
            </div>
            </>
        }
    }

    fn rendered(&mut self, ctx: &Context<Self>, first_render: bool) {
        if !first_render {
            return;
        }

        if let Some(window) = web_sys::window() {
            let callback = ctx.link().callback(|_| Message::WindowResize);
            let callback_copy = callback.clone();
            self.resize_listener = Some(EventListener::new(&window, "resize", move |_event| {
                callback.emit(())
            }));
            callback_copy.emit(());

            let autosave_callback = ctx.link().callback(|_| Message::Autosave);
            let autosave_is_dirty = self.autosave_is_dirty.clone();
            self.beforeunload_listener = Some(EventListener::new_with_options(
                &window,
                "beforeunload",
                EventListenerOptions::enable_prevent_default(),
                move |event| {
                    if autosave_is_dirty.load(Ordering::SeqCst) {
                        event.prevent_default();
                        autosave_callback.emit(());
                    }
                },
            ));

            ctx.link().send_message(Message::CheckText);
        }
    }
}

#[derive(PartialEq, Eq, Properties)]
struct StatusIndicatorProperties {
    valid: Option<bool>,
}

struct StatusIndicator {}

impl Component for StatusIndicator {
    type Message = ();
    type Properties = StatusIndicatorProperties;

    fn create(_ctx: &Context<Self>) -> Self {
        StatusIndicator {}
    }

    fn update(&mut self, _ctx: &Context<Self>, _msg: Self::Message) -> bool {
        false
    }

    fn changed(&mut self, _ctx: &Context<Self>) -> bool {
        true
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let mut class = classes!("status-box");
        let text = match ctx.props().valid {
            Some(true) => {
                class.push("valid");
                "OK"
            }
            Some(false) => {
                class.push("invalid");
                "Error"
            }
            None => "",
        };
        html! {
            <div { class }>{ text }</div>
        }
    }

    fn rendered(&mut self, _ctx: &Context<Self>, _first_render: bool) {}
}

fn main() {
    console_error_panic_hook::set_once();
    yew::start_app::<Model>();
}
