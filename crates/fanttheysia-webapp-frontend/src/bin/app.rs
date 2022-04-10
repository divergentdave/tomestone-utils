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
    ModelContentChanged(IModelContentChangedEvent),
    CheckText,
    Autosave,
    WindowResize,
    WorkerMessage(Response),
    OpenFileClick,
    OpenFileChoice(Event),
    OpenFileDone(String),
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
    options: Rc<CodeEditorOptions>,
    editor_link: CodeEditorLink,
    model: TextModel,
    resize_listener: Option<EventListener>,
    beforeunload_listener: Option<EventListener>,
    _text_listener: DisposableClosure<dyn FnMut(IModelContentChangedEvent)>,
    edit_check_debounce_timer: Option<Timeout>,
    edit_autosave_debounce_timer: Option<Timeout>,
    debounced_check_callback: Callback<()>,
    debounced_autosave_callback: Callback<()>,
    autosave_is_dirty: Rc<AtomicBool>,
    worker: Box<dyn Bridge<SyntaxChecker>>,
    file_chooser_ref: NodeRef,
    valid: Option<bool>,
    error_message: String,
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
                // TODO: queue up a request if there's already a check in-progress
                self.worker.send(Request::Check(self.model.get_value()));
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
            Message::WindowResize => self
                .editor_link
                .with_editor(|code_editor| {
                    // TODO: need to figure out how to make the editor take up 100% of viewport height, I think that would fix resizing
                    //code_editor.as_ref().layout(None);
                    code_editor
                        .as_ref()
                        .layout(Some(&IDimension::new(800, 600)));
                    gloo_console::log!("did layout");
                    true
                })
                .unwrap_or_default(),
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
                    }
                    Response::Progress(progress) => {
                        self.progress = Some(progress);
                    }
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
