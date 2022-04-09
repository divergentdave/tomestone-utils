use std::rc::Rc;

use gloo_events::EventListener;
use gloo_timers::callback::Timeout;
use gloo_worker::{Bridge, Bridged};
use monaco::{
    api::{CodeEditorOptions, DisposableClosure, TextModel},
    sys::editor::{BuiltinTheme, IDimension, IModelContentChangedEvent},
    yew::{CodeEditor, CodeEditorLink},
};
use yew::prelude::*;

use fanttheysia_webapp_frontend::{Request, Response, SyntaxChecker};

enum Message {
    ModelContentChanged(IModelContentChangedEvent),
    TextChangeDebounced,
    WindowResize,
    WorkerMessage(Response),
}

fn get_options() -> CodeEditorOptions {
    CodeEditorOptions::default().with_builtin_theme(BuiltinTheme::VsDark)
}

struct Model {
    options: Rc<CodeEditorOptions>,
    editor_link: CodeEditorLink,
    model: TextModel,
    resize_listener: Option<EventListener>,
    _text_listener: DisposableClosure<dyn FnMut(IModelContentChangedEvent)>,
    edit_debounce_timer: Option<Timeout>,
    debounced_callback: Callback<()>,
    worker: Box<dyn Bridge<SyntaxChecker>>,
}

impl Component for Model {
    type Message = Message;
    type Properties = ();

    fn create(ctx: &Context<Self>) -> Self {
        let worker_callback = ctx.link().callback(Message::WorkerMessage);
        let worker = SyntaxChecker::bridge(Rc::new(move |response| worker_callback.emit(response)));

        // TODO: restore from localStorage
        let model = TextModel::create("---\n---\n---\n---\n---\n", Some("yaml"), None).unwrap();

        let text_change_callback = ctx.link().callback(Message::ModelContentChanged);
        let text_change_listener =
            model.on_did_change_content(move |e| text_change_callback.emit(e));

        let debounced_callback = ctx.link().callback(|_| Message::TextChangeDebounced);

        Self {
            options: Rc::new(get_options()),
            editor_link: CodeEditorLink::default(),
            model,
            resize_listener: None,
            _text_listener: text_change_listener,
            edit_debounce_timer: None,
            debounced_callback,
            worker,
        }
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Message::ModelContentChanged(_event) => {
                self.edit_debounce_timer.take().map(Timeout::cancel);
                self.edit_debounce_timer = Some(Timeout::new(2000, {
                    let debounced_callback = self.debounced_callback.clone();
                    move || debounced_callback.emit(())
                }));
                false
            }
            Message::TextChangeDebounced => {
                self.worker.send(Request::Check(self.model.get_value()));
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
                    Response::Valid => gloo_console::log!("OK"),
                    Response::Invalid(message) => gloo_console::log!(message),
                }
                true
            }
        }
    }

    fn changed(&mut self, _ctx: &Context<Self>) -> bool {
        false
    }

    fn view(&self, _ctx: &Context<Self>) -> Html {
        html! {
            <CodeEditor
                link={ Some(self.editor_link.clone()) }
                model={ Some(self.model.clone()) }
                options={ self.options.clone() }
                />
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
        }
    }
}

fn main() {
    console_error_panic_hook::set_once();
    yew::start_app::<Model>();
}
