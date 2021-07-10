use std::{process, usize};

use tomestone_exdf::{Dataset, Language, RootList, Value};
use tomestone_sqpack::GameData;
use tomestone_string_interp::{Expression, Segment, Text, TreeNode, Visitor};

#[derive(Default, Debug)]
struct TagUsageCounterVisitor {
    literal: usize,
    reset_time: usize,
    time: usize,
    r#if: usize,
    switch: usize,
    tag_0a: usize,
    if_equals: usize,
    tag_0f: usize,
    new_line: usize,
    gui_icon: usize,
    color_change: usize,
    tag_14: usize,
    soft_hyphen: usize,
    tag_17: usize,
    emphasis_2: usize,
    emphasis: usize,
    tag_1b: usize,
    tag_1c: usize,
    indent: usize,
    command_icon: usize,
    dash: usize,
    value: usize,
    format: usize,
    two_digit_value: usize,
    tag_26: usize,
    sheet: usize,
    highlight: usize,
    link: usize,
    split: usize,
    tag_2d: usize,
    auto_translate: usize,
    tag_2f: usize,
    sheet_ja: usize,
    sheet_en: usize,
    sheet_de: usize,
    sheet_fr: usize,
    tag_40: usize,
    foreground: usize,
    glow: usize,
    zero_padded_value: usize,
    tag_51: usize,
    tag_60: usize,
    tag_61: usize,
}

impl TagUsageCounterVisitor {
    fn new() -> TagUsageCounterVisitor {
        TagUsageCounterVisitor::default()
    }
}

impl Visitor for TagUsageCounterVisitor {
    fn visit_tag(&mut self, tag: &Segment) {
        match tag {
            Segment::Literal(_) => self.literal += 1,
            Segment::TodoResetTime(_) => self.reset_time += 1,
            Segment::Time(_) => self.time += 1,
            Segment::If { .. } => self.r#if += 1,
            Segment::Switch { .. } => self.switch += 1,
            Segment::Todo0A(_) => self.tag_0a += 1,
            Segment::IfEquals { .. } => self.if_equals += 1,
            Segment::Todo0F { .. } => self.tag_0f += 1,
            Segment::NewLine => self.new_line += 1,
            Segment::GuiIcon(_) => self.gui_icon += 1,
            Segment::ColorChange(_) => self.color_change += 1,
            Segment::Todo14(_) => self.tag_14 += 1,
            Segment::SoftHyphen => self.soft_hyphen += 1,
            Segment::Todo17 => self.tag_17 += 1,
            Segment::Emphasis2(_) => self.emphasis_2 += 1,
            Segment::Emphasis(_) => self.emphasis += 1,
            Segment::Todo1B(_) => self.tag_1b += 1,
            Segment::Todo1C(_) => self.tag_1c += 1,
            Segment::Indent => self.indent += 1,
            Segment::CommandIcon(_) => self.command_icon += 1,
            Segment::Dash => self.dash += 1,
            Segment::Value(_) => self.value += 1,
            Segment::TodoFormat(_, _) => self.format += 1,
            Segment::TwoDigitValue(_) => self.two_digit_value += 1,
            Segment::Todo26(_, _, _) => self.tag_26 += 1,
            Segment::Sheet(_) => self.sheet += 1,
            Segment::TodoHighlight(_) => self.highlight += 1,
            Segment::Link(_) => self.link += 1,
            Segment::Split { .. } => self.split += 1,
            Segment::Todo2D(_) => self.tag_2d += 1,
            Segment::AutoTranslate(_, _) => self.auto_translate += 1,
            Segment::Todo2F(_) => self.tag_2f += 1,
            Segment::SheetJa(_) => self.sheet_ja += 1,
            Segment::SheetEn(_) => self.sheet_en += 1,
            Segment::SheetDe(_) => self.sheet_de += 1,
            Segment::SheetFr(_) => self.sheet_fr += 1,
            Segment::Todo40(_) => self.tag_40 += 1,
            Segment::Foreground(_) => self.foreground += 1,
            Segment::Glow(_) => self.glow += 1,
            Segment::ZeroPaddedValue { .. } => self.zero_padded_value += 1,
            Segment::Todo51(_) => self.tag_51 += 1,
            Segment::Todo60(_) => self.tag_60 += 1,
            Segment::Todo61(_) => self.tag_61 += 1,
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

    let mut visitor = TagUsageCounterVisitor::new();
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
                    let (i, row) = if let Ok(tuple) = res {
                        tuple
                    } else {
                        eprintln!("error: couldn't read row");
                        process::exit(1);
                    };
                    for sub_row in row.iter() {
                        for value in sub_row.iter() {
                            if let Value::String(data) = value {
                                match Text::parse(data) {
                                    Ok(text) => {
                                        text.accept(&mut visitor);
                                    }
                                    Err(e) => {
                                        eprintln!(
                                            "error: failed to parse {} row {}: {}",
                                            name, i, e
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
