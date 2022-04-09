use std::{collections::BTreeMap, process};
use tomestone_exdf::{Dataset, Language, RootList, Value};
use tomestone_sqpack::GameData;
use tomestone_string_interp::{Expression, Segment, Text, TreeNode, Visitor};

#[derive(Default, Debug)]
struct TagContentsVisitor {
    string_value_counters: BTreeMap<Expression, u64>,
    string_value_sentence_case_counters: BTreeMap<Expression, u64>,
    string_value_title_case_counters: BTreeMap<Expression, u64>,
    string_value_lower_case_counters: BTreeMap<Expression, u64>,
}

impl TagContentsVisitor {
    fn new() -> TagContentsVisitor {
        TagContentsVisitor::default()
    }
}

impl Visitor for TagContentsVisitor {
    fn visit_tag(&mut self, tag: &Segment) {
        match tag {
            Segment::StringValue(expr) => {
                *self.string_value_counters.entry(expr.clone()).or_default() += 1
            }
            Segment::StringValueSentenceCase(expr) => {
                *self
                    .string_value_sentence_case_counters
                    .entry(expr.clone())
                    .or_default() += 1;
            }
            Segment::StringValueTitleCase(expr) => {
                *self
                    .string_value_title_case_counters
                    .entry(expr.clone())
                    .or_default() += 1
            }
            Segment::StringValueLowerCase(expr) => {
                *self
                    .string_value_lower_case_counters
                    .entry(expr.clone())
                    .or_default() += 1
            }
            _ => {}
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
    let mut data_file_set = game_data.data_files();

    let root_list = if let Ok(root_list) = RootList::open(&game_data, &mut data_file_set) {
        root_list
    } else {
        eprintln!("error: couldn't read root list of data files");
        process::exit(1);
    };

    let mut visitor = TagContentsVisitor::new();
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
