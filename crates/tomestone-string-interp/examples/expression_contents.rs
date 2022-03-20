use std::{collections::BTreeMap, process};
use tomestone_exdf::{Dataset, Language, RootList, Value};
use tomestone_sqpack::GameData;
use tomestone_string_interp::{Expression, Segment, Text, TreeNode, Visitor};

#[derive(Default, Debug)]
struct ExpressionContentsVisitor {
    top_level_param_counters: BTreeMap<u8, u64>,
    input_param_counters: BTreeMap<u32, u64>,
    player_param_counters: BTreeMap<u32, u64>,
    string_param_counters: BTreeMap<u32, u64>,
    object_param_counters: BTreeMap<u32, u64>,
}

impl ExpressionContentsVisitor {
    fn new() -> ExpressionContentsVisitor {
        ExpressionContentsVisitor::default()
    }
}

impl Visitor for ExpressionContentsVisitor {
    fn visit_tag(&mut self, tag: &Segment) {
        self.recurse_tag(tag);
    }

    fn visit_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::TopLevelParameter(number) => {
                *self.top_level_param_counters.entry(*number).or_default() += 1
            }
            Expression::InputParameter(parameter_index) => {
                *self
                    .input_param_counters
                    .entry(*parameter_index)
                    .or_default() += 1
            }
            Expression::PlayerParameter(parameter_index) => {
                *self
                    .player_param_counters
                    .entry(*parameter_index)
                    .or_default() += 1
            }
            Expression::StringParameter(parameter_index) => {
                *self
                    .string_param_counters
                    .entry(*parameter_index)
                    .or_default() += 1
            }
            Expression::ObjectParameter(parameter_index) => {
                *self
                    .object_param_counters
                    .entry(*parameter_index)
                    .or_default() += 1
            }
            _ => {}
        }
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
    let mut data_file_set = game_data.data_files();

    let root_list = if let Ok(root_list) = RootList::open(&game_data, &mut data_file_set) {
        root_list
    } else {
        eprintln!("error: couldn't read root list of data files");
        process::exit(1);
    };

    let mut visitor = ExpressionContentsVisitor::new();
    for name in root_list.iter() {
        for language in [
            Language::Japanese,
            Language::English,
            Language::German,
            Language::French,
        ]
        .iter()
        {
            let dataset = if let Ok(dataset) =
                Dataset::load(&game_data, &mut data_file_set, name, *language)
            {
                dataset
            } else {
                eprintln!("error: couldn't load data file {}", name);
                process::exit(1);
            };

            for page in dataset.page_iter() {
                for res in page {
                    let row = if let Ok(row) = res {
                        row
                    } else {
                        eprintln!("error: couldn't read row");
                        process::exit(1);
                    };
                    for sub_row in row.sub_rows.iter() {
                        for value in sub_row.cells.iter() {
                            if let Value::String(data) = value {
                                match Text::parse(data) {
                                    Ok(text) => {
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
    println!("{:#?}", visitor);
}
