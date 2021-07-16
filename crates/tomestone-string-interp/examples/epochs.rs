use std::process;

use chrono::{Local, TimeZone};

use tomestone_exdf::{Dataset, Language, RootList, Value};
use tomestone_sqpack::GameData;
use tomestone_string_interp::{Expression, Segment, Text, TreeNode, Visitor};

struct DateVisitor<'a> {
    name: &'a str,
    row_number: u32,
    text: &'a Text,
}

impl<'a> DateVisitor<'a> {
    fn new(name: &'a str, row_number: u32, text: &'a Text) -> DateVisitor<'a> {
        DateVisitor {
            name,
            row_number,
            text,
        }
    }
}

impl<'a> Visitor for DateVisitor<'a> {
    fn visit_tag(&mut self, tag: &Segment) {
        if let Segment::Time(Expression::Integer(timestamp)) = tag {
            let datetime = Local.timestamp((*timestamp).into(), 0);
            println!("{} row {}: {}", self.name, self.row_number, datetime);
            println!("{:?}", self.text);
            println!();
        }
        self.recurse_tag(tag);
    }

    fn visit_expression(&mut self, expr: &Expression) {
        self.recurse_expression(expr);
    }
}

fn main() {
    dotenv::dotenv().ok();
    let root = if let Ok(root) = std::env::var("FFXIV_INSTALL_DIR") {
        root
    } else {
        eprintln!("error: set the environment variable FFXIV_INSTALL_DIR to the game's installation directory");
        process::exit(1);
    };
    let game_data = match GameData::new(&root) {
        Ok(game_data) => game_data,
        Err(e) => {
            eprintln!(
            "error: couldn't read the directory {:?} (from environment variable FFXIV_INSTALL_DIR), {}",
            root, e
        );
            process::exit(1);
        }
    };

    let root_list = if let Ok(root_list) = RootList::open(&game_data) {
        root_list
    } else {
        eprintln!("error: couldn't read root list of data files");
        process::exit(1);
    };

    for name in root_list.iter() {
        let dataset = if let Ok(dataset) = Dataset::load(&game_data, name, Language::English) {
            dataset
        } else {
            eprintln!("error: couldn't load data file {}", name);
            process::exit(1);
        };

        for page in dataset.page_iter() {
            for res in page {
                let row = res.unwrap();
                for sub_row in row.sub_rows.iter() {
                    for value in sub_row.cells.iter() {
                        if let Value::String(data) = value {
                            match Text::parse(data) {
                                Ok(text) => {
                                    let mut visitor = DateVisitor::new(&name, row.number, &text);
                                    text.accept(&mut visitor);
                                }
                                Err(e) => {
                                    eprintln!(
                                        "error: failed to parse {} row {}: {}",
                                        name, row.number, e
                                    );
                                    process::exit(1);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
