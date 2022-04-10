use std::ops::{Add, AddAssign};

use gloo_worker::{HandlerId, Public, Worker, WorkerLink};
use serde::{Deserialize, Serialize};

use fanttheysia_common::{
    AchievementTitleRule, GrandCompanyRankRule, PvpRankRule, StructuredTextRule,
    TextReplacementRules,
};
use tomestone_string_interp::{Segment, Text};

#[derive(Serialize, Deserialize)]
pub struct ProgressAccumulator {
    pub current: u32,
    pub max: u32,
}

impl ProgressAccumulator {
    fn new() -> ProgressAccumulator {
        ProgressAccumulator { current: 0, max: 0 }
    }
}

impl Default for ProgressAccumulator {
    fn default() -> ProgressAccumulator {
        ProgressAccumulator::new()
    }
}

impl Add for ProgressAccumulator {
    type Output = ProgressAccumulator;

    fn add(mut self, rhs: Self) -> Self::Output {
        self.current += rhs.current;
        self.max += rhs.max;
        self
    }
}

impl AddAssign for ProgressAccumulator {
    fn add_assign(&mut self, rhs: Self) {
        self.current += rhs.current;
        self.max += rhs.max;
    }
}

trait CalculateProgress {
    fn progress(&self) -> ProgressAccumulator;
}

impl CalculateProgress for TextReplacementRules {
    fn progress(&self) -> ProgressAccumulator {
        let mut accum = ProgressAccumulator::new();
        accum += self.structured_text_rules.progress();
        accum += self.achievement_title_rules.progress();
        accum += self.grand_company_rank_rules.progress();
        accum += self.pvp_rank_rules.progress();
        accum
    }
}

impl<C: CalculateProgress> CalculateProgress for Vec<C> {
    fn progress(&self) -> ProgressAccumulator {
        let mut accum = ProgressAccumulator::new();
        for item in self.iter() {
            accum += item.progress();
        }
        accum
    }
}

impl CalculateProgress for StructuredTextRule {
    fn progress(&self) -> ProgressAccumulator {
        if self.find == self.replace {
            ProgressAccumulator { current: 0, max: 1 }
        } else {
            ProgressAccumulator { current: 1, max: 1 }
        }
    }
}

lazy_static::lazy_static! {
    static ref EMPTY_TEXT: Text = Text::new(vec![Segment::Literal("".to_string())]);
}

impl CalculateProgress for AchievementTitleRule {
    fn progress(&self) -> ProgressAccumulator {
        if self.after == *EMPTY_TEXT {
            ProgressAccumulator { current: 0, max: 1 }
        } else {
            ProgressAccumulator { current: 1, max: 1 }
        }
    }
}

impl CalculateProgress for GrandCompanyRankRule {
    fn progress(&self) -> ProgressAccumulator {
        if self.after == *EMPTY_TEXT {
            ProgressAccumulator { current: 0, max: 1 }
        } else {
            ProgressAccumulator { current: 1, max: 1 }
        }
    }
}

impl CalculateProgress for PvpRankRule {
    fn progress(&self) -> ProgressAccumulator {
        if self.after == *EMPTY_TEXT {
            ProgressAccumulator { current: 0, max: 1 }
        } else {
            ProgressAccumulator { current: 1, max: 1 }
        }
    }
}

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
    Progress(ProgressAccumulator),
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
                match serde_yaml::from_str::<TextReplacementRules>(&data) {
                    Ok(rules) => {
                        self.link.respond(id, Response::Valid);
                        let progress = rules.progress();
                        self.link.respond(id, Response::Progress(progress));
                    }
                    Err(error) => self
                        .link
                        .respond(id, Response::Invalid(format!("{}", error))),
                };
            }
        }
    }

    fn name_of_resource() -> &'static str {
        "worker.js"
    }

    fn resource_path_is_relative() -> bool {
        true
    }
}
