use std::process;

use tomestone_exdf::{Dataset, Language, RootList, Value};
use tomestone_sqpack::GameData;
use tomestone_string_interp::Text;

/// This program counts how many times certain NPCs say their own names.
/// https://mspsocial.net/@divergentdave/105846202831450009
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

    const NAMES: [(&[u8], &[u8]); 10] = [
        (b"_YSHTOLA_", b"Y'shtola"),
        (b"_ALPHINAUD_", b"Alphinaud"),
        (b"_ALISAIE_", b"Alisaie"),
        (b"_ESTINIEN_", b"Estinien"),
        (b"_MINFILIA_", b"Minfilia"),
        (b"_YDA_", b"Yda"),
        (b"_PAPALYMO_", b"Papalymo"),
        (b"_CID_", b"Cid"),
        (b"_THANCRED_", b"Thancred"),
        (b"_URIANGER_", b"Urianger"),
    ];
    let mut counters = vec![0; NAMES.len()];

    'dataset: for name in root_list.iter() {
        let dataset = if let Ok(dataset) = Dataset::load(&game_data, name, Language::English) {
            dataset
        } else {
            eprintln!("error: couldn't load data file {}", name);
            process::exit(1);
        };

        for page in dataset.page_iter() {
            for res in page {
                let (_, row) = res.unwrap();
                if let &[Value::String(col1), Value::String(col2)] = &*row {
                    let col2_cleaned = if col2.len() > 4 && &col2[..2] == b"(-" {
                        // Skip name in "(-...-)" at the beginning of a line
                        if let Some(pos) = col2.windows(2).position(|window| window == b"-)") {
                            &col2[pos + 2..]
                        } else {
                            col2
                        }
                    } else {
                        col2
                    };
                    for (i, (caps_name, prose_name)) in NAMES.iter().enumerate() {
                        if col1
                            .windows(caps_name.len())
                            .any(|window| window == *caps_name)
                            && col2_cleaned
                                .windows(prose_name.len())
                                .any(|window| window == *prose_name)
                        {
                            println!(
                                "{:?} {:?}",
                                String::from_utf8_lossy(col1),
                                Text::parse(col2)
                            );
                            counters[i] += 1;
                        }
                    }
                } else {
                    continue 'dataset;
                }
            }
        }
    }

    println!();
    for (counter, (_, name)) in counters.iter().zip(NAMES.iter()) {
        println!("{}: {}", String::from_utf8_lossy(name), counter);
    }
}
