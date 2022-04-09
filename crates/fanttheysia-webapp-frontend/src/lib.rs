use gloo_worker::{HandlerId, Public, Worker, WorkerLink};
use serde::{Deserialize, Serialize};

use fanttheysia_common::TextReplacementRules;

pub struct SyntaxChecker {
    link: WorkerLink<Self>,
}

#[derive(Serialize, Deserialize)]
pub enum Request {
    Check(String),
}

#[derive(Serialize, Deserialize)]
pub enum Response {
    Valid,
    Invalid(String),
}

impl Worker for SyntaxChecker {
    type Reach = Public<Self>;
    type Message = ();
    type Input = Request;
    type Output = Response;

    fn create(link: WorkerLink<Self>) -> Self {
        Self { link }
    }

    fn update(&mut self, _msg: Self::Message) {}

    fn handle_input(&mut self, input: Self::Input, id: HandlerId) {
        match input {
            Request::Check(data) => {
                let response = match serde_yaml::from_str::<TextReplacementRules>(&data) {
                    Ok(_) => Response::Valid,
                    Err(error) => Response::Invalid(format!("{}", error)),
                };
                self.link.respond(id, response);
            }
        }
    }

    fn name_of_resource() -> &'static str {
        "worker.js"
    }
}
