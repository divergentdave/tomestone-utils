use std::process;

use tomestone_exdf::{Dataset, Language, RootList};
use tomestone_sqpack::GameData;

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

            let page_iterators = dataset.page_iter().collect::<Vec<_>>();
            let page_count = page_iterators.len();
            let mut total_row_count = 0;
            for page in page_iterators {
                let mut row_count = 0;
                let mut last_row_number = None;
                for res in page {
                    let row_number = if let Ok((row_number, _)) = res {
                        row_number
                    } else {
                        eprintln!("error: couldn't read data set row");
                        process::exit(1);
                    };
                    if let Some(last_row_number) = last_row_number {
                        if row_number != last_row_number + 1 {
                            println!(
                                "Row numbers for {} skipped from {} to {}",
                                name, last_row_number, row_number
                            );
                        }
                    }
                    row_count += 1;
                    last_row_number = Some(row_number);
                }
                if page_count != 1 {
                    println!(
                        "{} ({:?}) page has {} total rows",
                        name, language, row_count
                    );
                }
                total_row_count += row_count;
            }
            assert_eq!(
                total_row_count, dataset.exhf.num_rows,
                "Number of rows didn't match for {}",
                name
            );
        }
    }
}
