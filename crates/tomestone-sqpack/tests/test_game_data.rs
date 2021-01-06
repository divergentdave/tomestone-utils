use std::{fs::File, path::PathBuf};

use nom::error::Error;
use tomestone_sqpack::parser::{drive_streaming_parser, sqpack_header_outer, GrowableBufReader};

#[test]
fn parse_game_data() {
    dotenv::dotenv().unwrap();
    // Don't test anything if the game directory isn't provided
    if let Ok(root) = std::env::var("FFXIV_INSTALL_DIR") {
        let sqpack_dir = PathBuf::from(root).join("game").join("sqpack");
        for res in std::fs::read_dir(sqpack_dir).unwrap() {
            let dir_entry = res.unwrap();
            if dir_entry.file_type().unwrap().is_dir() {
                for res in std::fs::read_dir(dir_entry.path()).unwrap() {
                    let file_entry = res.unwrap();
                    let path = file_entry.path();
                    if file_entry.file_type().unwrap().is_file() {
                        println!("{:?}", path);
                    }
                    match path.extension() {
                        Some(ext)
                            if ext.to_string_lossy().starts_with("dat")
                                || ext == "index"
                                || ext == "index2" =>
                        {
                            let file = File::open(path).unwrap();
                            let mut bufreader = GrowableBufReader::new(file);
                            let parsed = drive_streaming_parser::<_, _, _, Error<&[u8]>>(
                                &mut bufreader,
                                sqpack_header_outer,
                            )
                            .unwrap()
                            .unwrap();
                            println!("{:?}", parsed);
                        }
                        _ => {}
                    }
                }
            }
        }
    }
}
