use std::{collections::HashSet, process};

use tomestone_exdf::{Dataset, Language, RootList, Value};
use tomestone_sqpack::GameData;
use tomestone_string_interp::{Expression, Segment, Text, TreeNode, Visitor};

struct GenderExpressionVisitor {
    flag: bool,
}

impl GenderExpressionVisitor {
    fn new() -> GenderExpressionVisitor {
        GenderExpressionVisitor { flag: false }
    }
}

impl Visitor for GenderExpressionVisitor {
    fn visit_expression(&mut self, expr: &Expression) {
        if let Expression::PlayerParameter(child_expr) = expr {
            if &**child_expr == &Expression::Integer(4) {
                self.flag = true;
                return;
            }
        }
        self.recurse_expression(expr);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct IfSegment {
    condition: Expression,
    true_value: Expression,
    false_value: Expression,
}

struct GenderConditionalTextVisitor {
    ifs: Vec<IfSegment>,
}

impl GenderConditionalTextVisitor {
    fn new() -> GenderConditionalTextVisitor {
        GenderConditionalTextVisitor { ifs: vec![] }
    }
}

impl Visitor for GenderConditionalTextVisitor {
    fn visit_tag(&mut self, tag: &Segment) {
        if let Segment::If {
            condition,
            true_value,
            false_value,
        } = tag
        {
            let mut expression_visitor = GenderExpressionVisitor::new();
            condition.accept(&mut expression_visitor);
            if expression_visitor.flag {
                self.ifs.push(IfSegment {
                    condition: condition.clone(),
                    true_value: true_value.clone(),
                    false_value: false_value.clone(),
                });
            } else {
                self.recurse_tag(tag);
            }
        } else {
            self.recurse_tag(tag);
        }
    }

    fn visit_expression(&mut self, expr: &Expression) {
        self.recurse_expression(expr);
    }
}

fn main() {
    // just CLI-based diagnostics and exploration at this point

    dotenv::dotenv().ok();
    let root = if let Ok(root) = std::env::var("FFXIV_INSTALL_DIR") {
        root
    } else {
        eprintln!("error: set the environment variable FFXIV_INSTALL_DIR to the game's installation directory");
        process::exit(1);
    };
    let game_data = if let Ok(game_data) = GameData::new(&root) {
        game_data
    } else {
        eprintln!(
            "error: couldn't read the directory {:?} (from environment variable FFXIV_INSTALL_DIR)",
            root
        );
        process::exit(1);
    };

    let root_list = if let Ok(root_list) = RootList::open(&game_data) {
        root_list
    } else {
        eprintln!("error: couldn't read root list of data files");
        process::exit(1);
    };

    let mut if_tag_set = HashSet::new();
    for name in root_list.iter() {
        let dataset = if let Ok(dataset) = Dataset::load(&game_data, name, Language::English) {
            dataset
        } else {
            eprintln!("error: couldn't load data file {}", name);
            process::exit(1);
        };

        for page in dataset.page_iter() {
            for res in page {
                let (i, row) = res.unwrap();
                for value in row {
                    if let Value::String(data) = value {
                        match Text::parse(data) {
                            Ok(text) => {
                                let mut visitor = GenderConditionalTextVisitor::new();
                                text.accept(&mut visitor);
                                if visitor.ifs.len() > 0 {
                                    println!("{} row {}: {:?}", name, i, text);
                                }
                                for if_tag in visitor.ifs.into_iter() {
                                    if_tag_set.insert(if_tag);
                                }
                            }
                            Err(e) => {
                                eprintln!("error: failed to parse {} row {}: {}", name, i, e);
                                process::exit(1);
                            }
                        }
                    }
                }
            }
        }
    }

    println!();

    for if_tag in if_tag_set.iter() {
        println!("{:?}", if_tag);
    }

    println!();

    println!(
        "Total of {} unique conditional text fragments",
        if_tag_set.len()
    );
}
