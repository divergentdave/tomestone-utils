use std::rc::Rc;

use gloo_events::EventListener;
use monaco::{
    api::{CodeEditorOptions, DisposableClosure, TextModel},
    sys::editor::{BuiltinTheme, IDimension, IModelContentChangedEvent},
    yew::{CodeEditor, CodeEditorLink},
};
use yew::prelude::*;

use fanttheysia_common::TextReplacementRules;

enum Msg {
    ModelContentChanged(IModelContentChangedEvent),
    WindowResize,
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
}

impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(ctx: &Context<Self>) -> Self {
        // TODO: restore from localStorage
        let model = TextModel::create("---\n---\n---\n---\n---\n", Some("yaml"), None).unwrap();

        let callback = ctx.link().callback(Msg::ModelContentChanged);
        let listener = model.on_did_change_content(move |e| callback.emit(e));

        Self {
            options: Rc::new(get_options()),
            editor_link: CodeEditorLink::default(),
            model,
            resize_listener: None,
            _text_listener: listener,
        }
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::ModelContentChanged(_event) => {
                let data = self.model.get_value();
                // TODO 1: move this to a worker
                // TODO 2: debounce text changed events
                match serde_yaml::from_str::<TextReplacementRules>(&data) {
                    Ok(_) => gloo_console::log!("OK"),
                    Err(error) => {
                        gloo_console::log!(format!("{}", error));
                    }
                }
                true
            }
            Msg::WindowResize => self
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
            let callback = ctx.link().callback(|_| Msg::WindowResize);
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
