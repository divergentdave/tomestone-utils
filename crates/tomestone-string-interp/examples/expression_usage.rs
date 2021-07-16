use std::process;

use tomestone_exdf::{Dataset, Language, RootList, Value};
use tomestone_sqpack::GameData;
use tomestone_string_interp::{Text, TreeNode, Visitor};

#[derive(Default, Debug)]
struct ExpressionUsageCounterVisitor {
    greater_than_or_equal: usize,
    comparison_1: usize,
    less_than_or_equal: usize,
    comparison_2: usize,
    equal: usize,
    comparison_3: usize,
    top_level_parameter: usize,
    integer_parameter: usize,
    player_parameter: usize,
    string_parameter: usize,
    object_parameter: usize,
    expr_ec: usize,
    integer: usize,
    text: usize,
}

impl ExpressionUsageCounterVisitor {
    fn new() -> ExpressionUsageCounterVisitor {
        ExpressionUsageCounterVisitor::default()
    }
}

impl Visitor for ExpressionUsageCounterVisitor {
    fn visit_tag(&mut self, tag: &tomestone_string_interp::Segment) {
        self.recurse_tag(tag);
    }

    fn visit_expression(&mut self, expr: &tomestone_string_interp::Expression) {
        match expr {
            tomestone_string_interp::Expression::GreaterThanOrEqual(_) => {
                self.greater_than_or_equal += 1
            }
            tomestone_string_interp::Expression::TodoComparison1(_) => self.comparison_1 += 1,
            tomestone_string_interp::Expression::LessThanOrEqual(_) => self.less_than_or_equal += 1,
            tomestone_string_interp::Expression::TodoComparison2(_) => self.comparison_2 += 1,
            tomestone_string_interp::Expression::Equal(_) => self.equal += 1,
            tomestone_string_interp::Expression::TodoComparison3(_) => self.comparison_3 += 1,
            tomestone_string_interp::Expression::TopLevelParameter(_) => {
                self.top_level_parameter += 1
            }
            tomestone_string_interp::Expression::IntegerParameter(_) => self.integer_parameter += 1,
            tomestone_string_interp::Expression::PlayerParameter(_) => self.player_parameter += 1,
            tomestone_string_interp::Expression::StringParameter(_) => self.string_parameter += 1,
            tomestone_string_interp::Expression::ObjectParameter(_) => self.object_parameter += 1,
            tomestone_string_interp::Expression::TodoEC => self.expr_ec += 1,
            tomestone_string_interp::Expression::Integer(_) => self.integer += 1,
            tomestone_string_interp::Expression::Text(_) => self.text += 1,
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

    let root_list = if let Ok(root_list) = RootList::open(&game_data) {
        root_list
    } else {
        eprintln!("error: couldn't read root list of data files");
        process::exit(1);
    };

    let mut visitor = ExpressionUsageCounterVisitor::new();
    for name in root_list.iter() {
        for language in [
            Language::Japanese,
            Language::English,
            Language::German,
            Language::French,
        ]
        .iter()
        {
            let dataset = if let Ok(dataset) = Dataset::load(&game_data, name, *language) {
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
