use std::{
    fs::File,
    io::{Read, Seek, SeekFrom},
    path::PathBuf,
};

use nom::error::Error;
use sha1::{Digest, Sha1};
use tomestone_sqpack::{
    list_repositories,
    parser::{
        drive_streaming_parser, index_segment_headers, sqpack_header_outer, GrowableBufReader,
    },
};

#[test]
fn parse_game_data() {
    dotenv::dotenv().unwrap();
    // Don't test anything if the game directory isn't provided
    if let Ok(root) = std::env::var("FFXIV_INSTALL_DIR") {
        let repositories = list_repositories(&root).unwrap();
        let sqpack_dir = PathBuf::from(root).join("game").join("sqpack");
        for repository in repositories {
            let path = sqpack_dir.join(repository);
            for res in std::fs::read_dir(path).unwrap() {
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
                        let file = File::open(&path).unwrap();
                        let mut bufreader = GrowableBufReader::new(file);
                        let parsed = drive_streaming_parser::<_, _, _, Error<&[u8]>>(
                            &mut bufreader,
                            sqpack_header_outer,
                        )
                        .unwrap()
                        .unwrap();
                        println!("{:?}", parsed);
                        if ext == "index" || ext == "index2" {
                            let size = parsed.1;
                            bufreader.seek(SeekFrom::Start(size.into())).unwrap();
                            let parsed = drive_streaming_parser::<_, _, _, Error<&[u8]>>(
                                &mut bufreader,
                                index_segment_headers,
                            )
                            .unwrap()
                            .unwrap();
                            println!("{:?}", parsed);
                        }
                    }
                    _ => {}
                }
            }
        }
    }
}

#[test]
fn check_index_hashes() {
    dotenv::dotenv().unwrap();
    // Don't test anything if the game directory isn't provided
    if let Ok(root) = std::env::var("FFXIV_INSTALL_DIR") {
        let repositories = list_repositories(&root).unwrap();
        let sqpack_dir = PathBuf::from(root).join("game").join("sqpack");
        for repository in repositories {
            let path = sqpack_dir.join(repository);
            for res in std::fs::read_dir(path).unwrap() {
                let file_entry = res.unwrap();
                let path = file_entry.path();
                if file_entry.file_type().unwrap().is_file() {
                    println!("{:?}", path);
                }
                match path.extension() {
                    Some(ext) if ext == "index" || ext == "index2" => {
                        let file = File::open(&path).unwrap();
                        let mut bufreader = GrowableBufReader::new(file);
                        let parsed = drive_streaming_parser::<_, _, _, Error<&[u8]>>(
                            &mut bufreader,
                            sqpack_header_outer,
                        )
                        .unwrap()
                        .unwrap();
                        let size = parsed.1;
                        bufreader.seek(SeekFrom::Start(size.into())).unwrap();
                        let parsed = drive_streaming_parser::<_, _, _, Error<&[u8]>>(
                            &mut bufreader,
                            index_segment_headers,
                        )
                        .unwrap()
                        .unwrap();
                        for header in &parsed.1 {
                            if header.size == 0 {
                                continue;
                            }
                            bufreader
                                .seek(SeekFrom::Start(header.offset.into()))
                                .unwrap();
                            let mut buf = vec![0; header.size as usize];
                            bufreader.read_exact(&mut buf).unwrap();
                            let mut hash = Sha1::new();
                            hash.update(&buf);
                            assert_eq!(*hash.finalize(), header.hash);
                        }
                    }
                    _ => {}
                }
            }
        }
    }
}
