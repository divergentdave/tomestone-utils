use std::{
    collections::{BTreeSet, HashSet},
    io::Write,
    path::PathBuf,
    process,
};

use clap::{
    builder::{EnumValueParser, ValueParser},
    crate_authors, crate_description, crate_name, crate_version, Arg, ArgMatches, Command,
};

use fanttheysia_common::{
    AchievementTitleRule, GenderConditionalTextVisitor, GrandCompanyRankRule, PvpRankRule,
    StructuredTextRule, TextReplacementRules,
};
use tomestone_exdf::{Dataset, Language, RootList, Value};
use tomestone_sqpack::{DataFileSet, GameData};
use tomestone_string_interp::{Segment, Text, TreeNode};

fn app() -> Command {
    Command::new(crate_name!())
        .version(crate_version!())
        .author(crate_authors!())
        .about(crate_description!())
        .arg(
            Arg::new("ffxiv-install-dir")
                .long("ffxiv-install-dir")
                .required(true)
                .value_parser(ValueParser::path_buf())
                .env("FFXIV_INSTALL_DIR"),
        )
        .subcommand(
            Command::new("report")
                .about("Print a report of all gender conditional text expressions")
                .arg(
                    Arg::new("language")
                        .long("language")
                        .short('l')
                        .required(false)
                        .value_parser(EnumValueParser::<Language>::new()),
                ),
        )
        .subcommand(
            Command::new("template")
                .about("Print a YAML text replacement rules file template")
                .arg(
                    Arg::new("language")
                        .long("language")
                        .short('l')
                        .required(false)
                        .value_parser(EnumValueParser::<Language>::new()),
                ),
        )
        .subcommand(
            Command::new("update")
            .about("Update a YAML text replacement rules file, adding template rules for any novel conditionals")
            .arg(
                Arg::new("language")
                    .long("language")
                    .short('l')
                    .required(false)
                    .value_parser(EnumValueParser::<Language>::new()),
            ),
        )
}

fn foreach_exd_text_value<F: FnMut(&str, u32, &Text)>(
    game_data: &GameData,
    data_file_set: &mut DataFileSet,
    root_list: &RootList,
    language: Language,
    mut f: F,
) {
    for name in root_list.iter() {
        let dataset = if let Ok(dataset) = Dataset::load(game_data, data_file_set, name, language) {
            dataset
        } else {
            eprintln!("error: couldn't load data file {}", name);
            process::exit(1);
        };

        for page in dataset.page_iter() {
            for res in page {
                let row = res.unwrap();
                for sub_row in row.sub_rows {
                    for value in sub_row.cells {
                        let text_data_opt = match &value {
                            Value::String(text_data) => Some(*text_data),
                            Value::StringOwned(text_data) => Some(&**text_data),
                            _ => None,
                        };
                        if let Some(data) = text_data_opt {
                            match Text::parse(data) {
                                Ok(text) => {
                                    f(name, row.number, &text);
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

fn foreach_achievement_title_diff<F: FnMut(u32, Text, Text)>(
    game_data: &GameData,
    data_file_set: &mut DataFileSet,
    language: Language,
    mut f: F,
) {
    let title_dataset =
        if let Ok(dataset) = Dataset::load(game_data, data_file_set, "Title", language) {
            dataset
        } else {
            eprintln!("error: couldn't load data file Title");
            process::exit(1);
        };

    for page in title_dataset.page_iter() {
        for res in page {
            let row = res.unwrap();
            for sub_row in row.sub_rows {
                if let (Value::String(male_title), Value::String(female_title)) =
                    (&sub_row.cells[0], &sub_row.cells[1])
                {
                    match (Text::parse(male_title), Text::parse(female_title)) {
                        (Ok(male_title), Ok(female_title)) => {
                            if male_title != female_title {
                                f(row.number, male_title, female_title);
                            }
                        }
                        (Err(e), _) | (_, Err(e)) => {
                            eprintln!("error: failed to parse Title row {}: {}", row.number, e);
                            process::exit(1);
                        }
                    }
                }
            }
        }
    }
}

struct GrandCompanyRankRecord<'a> {
    female_dataset_name: &'a str,
    male_dataset_name: &'a str,
    female_row_number: u32,
    male_row_number: u32,
    female_singular_rank: Text,
    male_singular_rank: Text,
    female_plural_rank: Text,
    male_plural_rank: Text,
}

fn foreach_grand_company_rank_diff<F: FnMut(GrandCompanyRankRecord)>(
    game_data: &GameData,
    data_file_set: &mut DataFileSet,
    language: Language,
    mut f: F,
) {
    for (female_dataset_name, male_dataset_name) in [
        ("GCRankGridaniaFemaleText", "GCRankGridaniaMaleText"),
        ("GCRankLimsaFemaleText", "GCRankLimsaMaleText"),
        ("GCRankUldahFemaleText", "GCRankUldahMaleText"),
    ] {
        let female_dataset = if let Ok(dataset) =
            Dataset::load(game_data, data_file_set, female_dataset_name, language)
        {
            dataset
        } else {
            eprintln!("error: couldn't load data file {}", female_dataset_name);
            process::exit(1);
        };
        let male_dataset = if let Ok(dataset) =
            Dataset::load(game_data, data_file_set, male_dataset_name, language)
        {
            dataset
        } else {
            eprintln!("error: couldn't load data file {}", male_dataset_name);
            process::exit(1);
        };

        for (female_page, male_page) in female_dataset.page_iter().zip(male_dataset.page_iter()) {
            for (female_res, male_res) in female_page.zip(male_page) {
                let female_row = female_res.unwrap();
                let male_row = male_res.unwrap();
                for (female_sub_row, male_sub_row) in female_row
                    .sub_rows
                    .into_iter()
                    .zip(male_row.sub_rows.into_iter())
                {
                    if let (
                        Value::String(female_singular_rank),
                        Value::String(male_singular_rank),
                        Value::String(female_plural_rank),
                        Value::String(male_plural_rank),
                    ) = (
                        &female_sub_row.cells[0],
                        &male_sub_row.cells[0],
                        &female_sub_row.cells[2],
                        &male_sub_row.cells[2],
                    ) {
                        match (
                            Text::parse(female_singular_rank),
                            Text::parse(male_singular_rank),
                            Text::parse(female_plural_rank),
                            Text::parse(male_plural_rank),
                        ) {
                            (
                                Ok(female_singular_rank),
                                Ok(male_singular_rank),
                                Ok(female_plural_rank),
                                Ok(male_plural_rank),
                            ) => {
                                if female_singular_rank != male_singular_rank
                                    || female_plural_rank != male_plural_rank
                                {
                                    f(GrandCompanyRankRecord {
                                        female_dataset_name,
                                        male_dataset_name,
                                        female_row_number: female_row.number,
                                        male_row_number: male_row.number,
                                        female_singular_rank,
                                        male_singular_rank,
                                        female_plural_rank,
                                        male_plural_rank,
                                    })
                                }
                            }
                            (Err(e), _, _, _) | (_, _, Err(e), _) => {
                                eprintln!(
                                    "error: failed to parse {} row {}: {}",
                                    female_dataset_name, female_row.number, e
                                );
                                process::exit(1);
                            }
                            (_, Err(e), _, _) | (_, _, _, Err(e)) => {
                                eprintln!(
                                    "error: failed to parse {} row {}: {}",
                                    male_dataset_name, male_row.number, e
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

struct PvpRankRecord {
    dataset_name: &'static str,
    row_number: u32,
    female_rank: Text,
    male_rank: Text,
}

fn foreach_pvp_rank_diff<F: FnMut(PvpRankRecord)>(
    game_data: &GameData,
    data_file_set: &mut DataFileSet,
    language: Language,
    mut f: F,
) {
    static DATASET_NAME: &str = "PvPRankTransient";
    let dataset =
        if let Ok(dataset) = Dataset::load(game_data, data_file_set, DATASET_NAME, language) {
            dataset
        } else {
            eprintln!("error: couldn't load data file {}", DATASET_NAME);
            process::exit(1);
        };
    for page in dataset.page_iter() {
        for res in page {
            let row = res.unwrap();
            for sub_row in row.sub_rows {
                if let [Value::String(male_maelstrom_rank), Value::String(male_twin_adder_rank), Value::String(male_immortal_flames_rank), Value::String(female_maelstrom_rank), Value::String(female_twin_adder_rank), Value::String(female_immortal_flames_rank)] =
                    &*sub_row.cells
                {
                    match (
                        Text::parse(male_maelstrom_rank),
                        Text::parse(male_twin_adder_rank),
                        Text::parse(male_immortal_flames_rank),
                        Text::parse(female_maelstrom_rank),
                        Text::parse(female_twin_adder_rank),
                        Text::parse(female_immortal_flames_rank),
                    ) {
                        (
                            Ok(male_maelstrom_rank),
                            Ok(male_twin_adder_rank),
                            Ok(male_immortal_flames_rank),
                            Ok(female_maelstrom_rank),
                            Ok(female_twin_adder_rank),
                            Ok(female_immortal_flames_rank),
                        ) => {
                            if male_maelstrom_rank != female_maelstrom_rank {
                                f(PvpRankRecord {
                                    dataset_name: DATASET_NAME,
                                    row_number: row.number,
                                    male_rank: male_maelstrom_rank,
                                    female_rank: female_maelstrom_rank,
                                });
                            }
                            if male_twin_adder_rank != female_twin_adder_rank {
                                f(PvpRankRecord {
                                    dataset_name: DATASET_NAME,
                                    row_number: row.number,
                                    male_rank: male_twin_adder_rank,
                                    female_rank: female_twin_adder_rank,
                                });
                            }
                            if male_immortal_flames_rank != female_immortal_flames_rank {
                                f(PvpRankRecord {
                                    dataset_name: DATASET_NAME,
                                    row_number: row.number,
                                    male_rank: male_immortal_flames_rank,
                                    female_rank: female_immortal_flames_rank,
                                });
                            }
                        }
                        (Err(e), _, _, _, _, _)
                        | (_, Err(e), _, _, _, _)
                        | (_, _, Err(e), _, _, _)
                        | (_, _, _, Err(e), _, _)
                        | (_, _, _, _, Err(e), _)
                        | (_, _, _, _, _, Err(e)) => {
                            eprintln!(
                                "error: failed to parse {} row {}: {}",
                                DATASET_NAME, row.number, e
                            );
                            process::exit(1);
                        }
                    }
                }
            }
        }
    }
}

fn print_report(
    game_data: &GameData,
    data_file_set: &mut DataFileSet,
    root_list: &RootList,
    language: Language,
) {
    let mut if_tag_set = HashSet::new();
    foreach_exd_text_value(
        game_data,
        data_file_set,
        root_list,
        language,
        |name, row_number, text| {
            let mut visitor = GenderConditionalTextVisitor::new();
            text.accept(&mut visitor);
            if !visitor.ifs.is_empty() {
                println!("{} row {}: {:?}", name, row_number, text);
            }
            for if_tag in visitor.ifs.into_iter() {
                if_tag_set.insert(if_tag);
            }
        },
    );

    println!();

    for if_tag in if_tag_set.iter() {
        println!("{:?}", if_tag);
    }

    println!();

    println!(
        "Total of {} unique conditional text fragments",
        if_tag_set.len()
    );
    println!();

    let mut achievement_title_diffs = 0;
    foreach_achievement_title_diff(
        game_data,
        data_file_set,
        language,
        |number, male_title, female_title| {
            println!("Title row {}: {:?} {:?}", number, male_title, female_title);
            achievement_title_diffs += 1;
        },
    );

    println!();

    println!(
        "Total of {} differing achievement titles",
        achievement_title_diffs
    );
    println!();

    let mut gc_rank_diffs = 0;
    foreach_grand_company_rank_diff(game_data, data_file_set, language, |record| {
        println!(
            "{}/{} row {}/{}: {:?} {:?} {:?} {:?}",
            record.female_dataset_name,
            record.male_dataset_name,
            record.female_row_number,
            record.male_row_number,
            record.female_singular_rank,
            record.male_singular_rank,
            record.female_plural_rank,
            record.male_plural_rank,
        );
        gc_rank_diffs += 1;
    });

    println!(
        "Total of {} differing Grand Company rank names",
        gc_rank_diffs
    );
    println!();

    let mut pvp_rank_diffs = 0;
    foreach_pvp_rank_diff(game_data, data_file_set, language, |record| {
        println!(
            "{} row {}: {:?} {:?}",
            record.dataset_name, record.row_number, record.female_rank, record.male_rank,
        );
        pvp_rank_diffs += 1;
    });

    println!();

    println!("Total of {} differing PvP rank names", pvp_rank_diffs);
}

struct NoOpWriter;

impl Write for NoOpWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

fn build_template(
    game_data: &GameData,
    data_file_set: &mut DataFileSet,
    root_list: &RootList,
    language: Language,
) -> TextReplacementRules {
    let mut rules = TextReplacementRules::new();

    let mut if_tag_set = HashSet::new();
    foreach_exd_text_value(
        game_data,
        data_file_set,
        root_list,
        language,
        |_name, _row_number, text| {
            let mut visitor = GenderConditionalTextVisitor::new();
            text.accept(&mut visitor);
            for if_tag in visitor.ifs.into_iter() {
                if_tag_set.insert(if_tag);
            }
        },
    );
    rules.structured_text_rules.reserve_exact(if_tag_set.len());
    let mut serialization_error_count = 0;
    for if_tag in if_tag_set.into_iter() {
        // TODO: clean this out
        if let Err(e) = serde_yaml::to_writer(NoOpWriter, &if_tag) {
            eprintln!("{}", e);
            eprintln!("{:?}", &if_tag);
            serialization_error_count += 1;
        } else {
            rules.structured_text_rules.push(StructuredTextRule {
                find: vec![Segment::If {
                    condition: if_tag.condition.clone(),
                    true_value: if_tag.true_value.clone(),
                    false_value: if_tag.false_value.clone(),
                }],
                replace: vec![Segment::If {
                    condition: if_tag.condition,
                    true_value: if_tag.true_value,
                    false_value: if_tag.false_value,
                }],
            });
        }
    }

    foreach_achievement_title_diff(
        game_data,
        data_file_set,
        language,
        |_number, male_title, female_title| {
            rules.achievement_title_rules.push(AchievementTitleRule {
                before_female: female_title,
                before_male: male_title,
                after: Text::new(vec![Segment::Literal("".to_string())]),
            });
        },
    );

    foreach_grand_company_rank_diff(game_data, data_file_set, language, |record| {
        rules.grand_company_rank_rules.push(GrandCompanyRankRule {
            before_female: record.female_singular_rank,
            before_male: record.male_singular_rank,
            after: Text::new(vec![Segment::Literal("".to_string())]),
        });
        rules.grand_company_rank_rules.push(GrandCompanyRankRule {
            before_female: record.female_plural_rank,
            before_male: record.male_plural_rank,
            after: Text::new(vec![Segment::Literal("".to_string())]),
        });
    });

    let mut pvp_rank_set = BTreeSet::new();
    foreach_pvp_rank_diff(game_data, data_file_set, language, |record| {
        pvp_rank_set.insert(PvpRankRule {
            before_female: record.female_rank,
            before_male: record.male_rank,
            after: Text::new(vec![Segment::Literal("".to_string())]),
        });
    });
    rules.pvp_rank_rules.extend(pvp_rank_set.into_iter());

    if serialization_error_count > 0 {
        eprintln!(
            "warning: could not serialize {} structured text replacement rules",
            serialization_error_count
        );
    }

    rules
}

fn print_template(
    game_data: &GameData,
    data_file_set: &mut DataFileSet,
    root_list: &RootList,
    language: Language,
) {
    let rules = build_template(game_data, data_file_set, root_list, language);

    let stdout = std::io::stdout();
    let locked = stdout.lock();
    if let Err(e) = serde_yaml::to_writer(locked, &rules) {
        eprintln!("error: failed to serialize rules file: {}", e);
        process::exit(1);
    }
}

fn update_rules(
    game_data: &GameData,
    data_file_set: &mut DataFileSet,
    root_list: &RootList,
    language: Language,
) {
    let stdin = std::io::stdin();
    let locked_stdin = stdin.lock();
    let mut rules: TextReplacementRules = match serde_yaml::from_reader(locked_stdin) {
        Ok(rules) => rules,
        Err(e) => {
            eprintln!("error: failed to parse rules file: {}", e);
            process::exit(1);
        }
    };

    let template_rules = build_template(game_data, data_file_set, root_list, language);

    rules.merge(&template_rules);

    let stdout = std::io::stdout();
    let locked = stdout.lock();
    if let Err(e) = serde_yaml::to_writer(locked, &rules) {
        eprintln!("error: failed to serialize rules file: {}", e);
        process::exit(1);
    }
}

fn parse_language_flag(matches: &ArgMatches) -> Language {
    matches
        .get_one("language")
        .copied()
        .unwrap_or(Language::English)
}

fn main() {
    dotenvy::dotenv().ok();

    let app_matches = app().get_matches();

    let root = app_matches.get_one::<PathBuf>("ffxiv-install-dir").unwrap();
    let game_data = match GameData::new(root) {
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

    match app_matches.subcommand() {
        Some(("report", matches)) => print_report(
            &game_data,
            &mut data_file_set,
            &root_list,
            parse_language_flag(matches),
        ),
        Some(("template", matches)) => print_template(
            &game_data,
            &mut data_file_set,
            &root_list,
            parse_language_flag(matches),
        ),
        Some(("update", matches)) => update_rules(
            &game_data,
            &mut data_file_set,
            &root_list,
            parse_language_flag(matches),
        ),
        _ => {
            eprintln!("{}", app().render_usage());
            process::exit(1);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::app;

    #[test]
    fn verify_app() {
        app().debug_assert();
    }
}
