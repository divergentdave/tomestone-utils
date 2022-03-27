use std::{collections::BTreeMap, process};
use tomestone_exdf::{Dataset, Language, RootList, Value};
use tomestone_sqpack::GameData;
use tomestone_string_interp::{Expression, Segment, Text, TreeNode, Visitor};

#[derive(Default, Debug)]
struct TagContentsVisitor {
    sheet_arg_count_counters: BTreeMap<usize, u64>,
    sheet_name_counters: BTreeMap<Expression, u64>,
    sheet_row_index_counters: BTreeMap<Expression, u64>,
    sheet_nesting: u8,
    nested_sheet_counter: u64,
    sheet_arg_count_by_name_counters: BTreeMap<Expression, BTreeMap<usize, u64>>,
}

impl TagContentsVisitor {
    fn new() -> TagContentsVisitor {
        TagContentsVisitor::default()
    }
}

impl Visitor for TagContentsVisitor {
    fn visit_tag(&mut self, tag: &Segment) {
        match tag {
            Segment::Sheet {
                name,
                row_index,
                column_index,
                parameters,
            } => {
                let arg_count = column_index.is_some() as usize + parameters.len();
                *self.sheet_arg_count_counters.entry(arg_count).or_default() += 1;
                *self.sheet_name_counters.entry(name.clone()).or_default() += 1;
                *self
                    .sheet_row_index_counters
                    .entry(row_index.clone())
                    .or_default() += 1;
                self.sheet_nesting += 1;
                if self.sheet_nesting >= 2 {
                    self.nested_sheet_counter += 1;
                }
                *self
                    .sheet_arg_count_by_name_counters
                    .entry(name.clone())
                    .or_default()
                    .entry(arg_count)
                    .or_default() += 1;
            }
            _ => {}
        }
        self.recurse_tag(tag);
        if let Segment::Sheet { .. } = tag {
            self.sheet_nesting -= 1;
        }
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

            let gc = Expression::Text(Text::new(vec![Segment::Literal(
                "GrandCompany".to_string(),
            )]));
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
                                        let before = *visitor
                                            .sheet_name_counters
                                            .entry(gc.clone())
                                            .or_default();
                                        text.accept(&mut visitor);
                                        if *visitor.sheet_name_counters.get(&gc).unwrap() != before
                                        {
                                            println!("{:?}", text);
                                        }
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
