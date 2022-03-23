use std::{collections::HashSet, io::Write, process};

use clap::{crate_authors, crate_description, crate_name, crate_version, App, Arg};

use fanttheysia_common::GenderConditionalTextVisitor;
use tomestone_exdf::{Dataset, Language, RootList, Value};
use tomestone_sqpack::{DataFileSet, GameData};
use tomestone_string_interp::{Text, TreeNode};

fn app() -> App<'static> {
    App::new(crate_name!())
        .version(crate_version!())
        .author(crate_authors!())
        .about(crate_description!())
        .arg(
            Arg::new("ffxiv-install-dir")
                .long("ffxiv-install-dir")
                .required(true)
                .takes_value(true)
                .env("FFXIV_INSTALL_DIR"),
        )
        .subcommand(
            App::new("report").about("Print a report of all gender conditional text expressions"),
        )
        .subcommand(App::new("template").about("Print a YAML text replacement rules file template"))
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
                        if let Value::String(data) = value {
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

fn print_report(game_data: &GameData, data_file_set: &mut DataFileSet, root_list: &RootList) {
    let mut if_tag_set = HashSet::new();
    foreach_exd_text_value(
        game_data,
        data_file_set,
        root_list,
        Language::English,
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

    let title_dataset =
        if let Ok(dataset) = Dataset::load(game_data, data_file_set, "Title", Language::English) {
            dataset
        } else {
            eprintln!("error: couldn't load data file Title");
            process::exit(1);
        };

    let mut achievement_title_diffs = 0;
    for page in title_dataset.page_iter() {
        for res in page {
            let row = res.unwrap();
            for sub_row in row.sub_rows {
                if sub_row.cells[0] != sub_row.cells[1] {
                    println!("Title row {}: {:?}", row.number, &sub_row.cells[0..2]);
                    achievement_title_diffs += 1;
                }
            }
        }
    }

    println!();

    println!(
        "Total of {} differing achievement titles",
        achievement_title_diffs
    );
    println!();

    let mut gc_rank_diffs = 0;
    for (female_dataset_name, male_dataset_name) in [
        ("GCRankGridaniaFemaleText", "GCRankGridaniaMaleText"),
        ("GCRankLimsaFemaleText", "GCRankLimsaMaleText"),
        ("GCRankUldahFemaleText", "GCRankUldahMaleText"),
    ] {
        let female_dataset = if let Ok(dataset) = Dataset::load(
            game_data,
            data_file_set,
            female_dataset_name,
            Language::English,
        ) {
            dataset
        } else {
            eprintln!("error: couldn't load data file {}", female_dataset_name);
            process::exit(1);
        };
        let male_dataset = if let Ok(dataset) = Dataset::load(
            game_data,
            data_file_set,
            male_dataset_name,
            Language::English,
        ) {
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
                    if female_sub_row.cells[0] != male_sub_row.cells[0]
                        || female_sub_row.cells[2] != male_sub_row.cells[2]
                    {
                        println!(
                            "{}/{} row {}: {:?} {:?} {:?} {:?}",
                            female_dataset_name,
                            male_dataset_name,
                            female_row.number,
                            female_sub_row.cells[0],
                            male_sub_row.cells[0],
                            female_sub_row.cells[2],
                            male_sub_row.cells[2]
                        );
                        gc_rank_diffs += 1;
                    }
                }
            }
        }
    }

    println!(
        "Total of {} differing Grand Company rank names",
        gc_rank_diffs
    );
}

fn print_template(game_data: &GameData, data_file_set: &mut DataFileSet, root_list: &RootList) {
    let mut if_tag_set = HashSet::new();
    foreach_exd_text_value(
        game_data,
        data_file_set,
        root_list,
        Language::English,
        |_name, _row_number, text| {
            let mut visitor = GenderConditionalTextVisitor::new();
            text.accept(&mut visitor);
            for if_tag in visitor.ifs.into_iter() {
                if_tag_set.insert(if_tag);
            }
        },
    );
    let stdout = std::io::stdout();
    let mut locked = stdout.lock();
    for if_tag in if_tag_set.iter() {
        if let Err(e) = serde_yaml::to_writer(&mut locked, &if_tag) {
            writeln!(&mut locked, "{}", e).unwrap()
        }
    }
}

fn main() {
    dotenv::dotenv().ok();

    let app_matches = app().get_matches();

    let root = app_matches.value_of_os("ffxiv-install-dir").unwrap();
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
        Some(("report", _)) => print_report(&game_data, &mut data_file_set, &root_list),
        Some(("template", _)) => print_template(&game_data, &mut data_file_set, &root_list),
        _ => {
            eprintln!("{}", app().render_usage());
            process::exit(1);
        }
    }
}
