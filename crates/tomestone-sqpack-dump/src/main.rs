use std::{
    io::{stdout, Write},
    process,
};

use clap::{
    crate_authors, crate_description, crate_name, crate_version, App, Arg, SubCommand, Values,
};
use once_cell::sync::Lazy;
use regex::Regex;

use tomestone_sqpack::{Category, Error, Expansion, GameData, IndexEntry, IndexHash1, IndexHash2};

fn lookup(game_data: &GameData, mut path_or_crc: Values<'_>) -> Result<Option<Vec<u8>>, Error> {
    static CRC_RE: Lazy<Regex> = Lazy::new(|| Regex::new("^[0-9A-Fa-f]{8}$").unwrap());

    let first_arg = path_or_crc.next().unwrap();
    let second_arg = path_or_crc.next();
    if let Some(second_arg) = second_arg {
        // two CRC-32s
        if CRC_RE.is_match(first_arg) && CRC_RE.is_match(second_arg) {
            let folder_crc = u32::from_str_radix(first_arg, 16).unwrap();
            let file_crc = u32::from_str_radix(second_arg, 16).unwrap();
            let hash = IndexHash1::new(folder_crc, file_crc);
            game_data.lookup_hash_1(&hash)
        } else {
            eprintln!("error: invalid CRC-32 hashes or multiple paths provided");
            process::exit(1);
        }
    } else {
        if CRC_RE.is_match(first_arg) {
            // one CRC-32
            let crc = u32::from_str_radix(first_arg, 16).unwrap();
            let hash = IndexHash2::new(crc);
            game_data.lookup_hash_2(&hash)
        } else {
            // path
            game_data.lookup_path(first_arg)
        }
    }
}

fn list_files(game_data: &GameData, category: Category, expansion: Expansion) -> Result<(), Error> {
    for id in game_data.iter_packs_category_expansion(category, expansion) {
        let index = game_data.get_index_1(&id).unwrap()?;
        for entry in index.iter() {
            let hash = entry.hash();
            println!("{:08x} {:08x}", hash.folder_crc, hash.filename_crc);
        }
    }
    Ok(())
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

    let app = App::new(crate_name!())
        .version(crate_version!())
        .author(crate_authors!())
        .about(crate_description!())
        .subcommand(
            SubCommand::with_name("raw").arg(
                Arg::with_name("path_or_crc")
                    .required(true)
                    .index(1)
                    .min_values(1)
                    .max_values(2),
            ),
        )
        .subcommand(
            SubCommand::with_name("hex").arg(
                Arg::with_name("path_or_crc")
                    .required(true)
                    .index(1)
                    .min_values(1)
                    .max_values(2),
            ),
        )
        .subcommand(
            SubCommand::with_name("list").arg(Arg::with_name("path").required(false).index(1)),
        );
    let app_matches = app.get_matches();
    match app_matches.subcommand() {
        ("raw", Some(matches)) => {
            match lookup(&game_data, matches.values_of("path_or_crc").unwrap()) {
                Ok(Some(data)) => {
                    stdout().write(&data).unwrap();
                }
                Ok(None) => {
                    eprintln!("error: file not found");
                    process::exit(1);
                }
                Err(e) => {
                    eprintln!("error: {}", e);
                    process::exit(1);
                }
            }
        }
        ("hex", Some(matches)) => {
            match lookup(&game_data, matches.values_of("path_or_crc").unwrap()) {
                Ok(Some(data)) => {
                    let mut buf = [0u8; 32];
                    for chunk in data.chunks(16) {
                        hex::decode_to_slice(chunk, &mut buf).unwrap();
                        println!("{}", std::str::from_utf8(&buf[chunk.len() * 2..]).unwrap());
                    }
                }
                Ok(None) => {
                    eprintln!("error: file not found");
                    process::exit(1);
                }
                Err(e) => {
                    eprintln!("error: {}", e);
                    process::exit(1);
                }
            }
        }
        ("list", Some(matches)) => {
            if let Some(path) = matches.value_of("path") {
                let segments: Vec<_> = path.split("/").collect();
                if segments.len() > 2 {
                    eprintln!("error: list only supports up to two path segments");
                    process::exit(1);
                } else if segments.len() == 2 {
                    if let Ok(category) = Category::parse_name(segments[0]) {
                        if let Ok(expansion) = Expansion::parse_name(segments[1]) {
                            match list_files(&game_data, category, expansion) {
                                Ok(_) => {}
                                Err(e) => {
                                    eprintln!("error: couldn't read indices, {}", e);
                                    process::exit(1);
                                }
                            }
                        } else {
                            eprintln!("error: invalid expansion {:?}", segments[1]);
                            process::exit(1);
                        }
                    } else {
                        eprintln!("error: invalid category {:?}", segments[0]);
                        process::exit(1);
                    }
                } else {
                    // one segment, category only, assume it's from the base game
                    if let Ok(category) = Category::parse_name(segments[0]) {
                        match list_files(&game_data, category, Expansion::Base) {
                            Ok(_) => {}
                            Err(e) => {
                                eprintln!("error: couldn't read indices, {}", e);
                                process::exit(1);
                            }
                        }
                    } else {
                        eprintln!("error: invalid category {:?}", segments[0]);
                        process::exit(1);
                    }
                }
            } else {
                for expansion in Expansion::iter_all() {
                    for category in Category::iter_all() {
                        match list_files(&game_data, *category, *expansion) {
                            Ok(_) => {}
                            Err(e) => {
                                eprintln!("error: couldn't read indices, {}", e);
                                process::exit(1);
                            }
                        }
                    }
                }
            }
        }
        _ => {
            eprintln!("{}", app_matches.usage());
            process::exit(1);
        }
    }
}
