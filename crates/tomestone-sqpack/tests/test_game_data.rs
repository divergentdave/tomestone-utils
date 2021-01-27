use std::{
    fs::File,
    io::{Read, Seek, SeekFrom},
    panic::{catch_unwind, RefUnwindSafe, UnwindSafe},
    path::PathBuf,
};

use nom::error::Error;
use sha1::{Digest, Sha1};
use tomestone_sqpack::{
    list_repositories,
    parser::{
        data_header, drive_streaming_parser, index_entry_1, index_entry_2, index_segment_headers,
        load_index, sqpack_header_outer, GrowableBufReader,
    },
    Index, IndexEntry,
};

fn forall_sqpack(f: impl Fn(PathBuf, GrowableBufReader<File>) + UnwindSafe + RefUnwindSafe) {
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
                let file = File::open(&path).unwrap();
                let res = catch_unwind(|| f(path, GrowableBufReader::new(file)));
                if let Err(panic) = res {
                    eprintln!("Error while processing {:?}", file_entry.path());
                    panic!(panic);
                }
            }
        }
    }
}

#[test]
fn parse_game_data() {
    forall_sqpack(|path, mut bufreader| match path.extension() {
        Some(ext)
            if ext.to_string_lossy().starts_with("dat") || ext == "index" || ext == "index2" =>
        {
            let parsed = drive_streaming_parser::<_, _, _, Error<&[u8]>>(
                &mut bufreader,
                sqpack_header_outer,
            )
            .unwrap()
            .unwrap();
            println!("{:?}", parsed);
            bufreader.seek(SeekFrom::Start(1024)).unwrap();
            if ext == "index" || ext == "index2" {
                let parsed = drive_streaming_parser::<_, _, _, Error<&[u8]>>(
                    &mut bufreader,
                    index_segment_headers,
                )
                .unwrap()
                .unwrap();
                println!("{:?}", parsed);
            } else {
                // dat file
                let parsed =
                    drive_streaming_parser::<_, _, _, Error<&[u8]>>(&mut bufreader, data_header)
                        .unwrap()
                        .unwrap();
                println!("{:?}", parsed);
            }
        }
        _ => {}
    });
}

#[test]
fn check_index_hashes() {
    forall_sqpack(|path, mut bufreader| match path.extension() {
        Some(ext) if ext == "index" || ext == "index2" => {
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
    });
}

#[test]
fn check_index_order() {
    fn inner<E: IndexEntry>(index: &Index<E>) {
        let mut last_hash: Option<E::Hash> = None;
        for index_entry in index.iter() {
            let hash = index_entry.hash();
            if let Some(last_hash) = &last_hash {
                assert!(last_hash < &hash);
            }
            last_hash = Some(hash);
        }
    }

    forall_sqpack(|path, mut bufreader| match path.extension() {
        Some(ext) if ext == "index" => {
            let index = load_index(&mut bufreader, index_entry_1).unwrap().unwrap();
            inner(&index);
        }
        Some(ext) if ext == "index2" => {
            let index = load_index(&mut bufreader, index_entry_2).unwrap().unwrap();
            inner(&index);
        }
        _ => {}
    });
}
