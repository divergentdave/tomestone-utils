use std::process;

use tomestone_exdf::{Dataset, Language, RootList};
use tomestone_sqpack::GameData;

fn main() {
    dotenvy::dotenv().ok();
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

    for name in root_list.iter() {
        let dataset = if let Ok(dataset) =
            Dataset::load(&game_data, &mut data_file_set, name, Language::English)
        {
            dataset
        } else {
            eprintln!("error: couldn't load data file {}", name);
            process::exit(1);
        };

        println!("{} {:?}", name, &dataset.exhf);
    }
}
