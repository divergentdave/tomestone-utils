use std::{
    convert::TryInto,
    fs::File,
    io::{Read, Seek, SeekFrom},
    panic::{catch_unwind, RefUnwindSafe, UnwindSafe},
    path::PathBuf,
};

use nom::error::Error;
use sha1::{Digest, Sha1};

use tomestone_sqpack::{
    compression::decompress_sqpack_block,
    parser::{
        block_header, data_entry_headers, data_header, drive_streaming_parser, index_entry_1,
        index_entry_2, index_segment_headers, load_index_reader, sqpack_header_outer,
        GrowableBufReader,
    },
    Expansion, Index, IndexEntry,
};

fn forall_sqpack(f: impl Fn(PathBuf, GrowableBufReader<File>) + UnwindSafe + RefUnwindSafe) {
    dotenv::dotenv().ok();
    // Don't test anything if the game directory isn't provided
    let root = if let Ok(root) = std::env::var("FFXIV_INSTALL_DIR") {
        root
    } else {
        return;
    };
    let sqpack_dir = PathBuf::from(root).join("game").join("sqpack");
    for expansion in Expansion::iter_all() {
        let path = sqpack_dir.join(expansion.name());
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
            .unwrap();
            println!("{:?}", parsed);
            bufreader.seek(SeekFrom::Start(1024)).unwrap();
            if ext == "index" || ext == "index2" {
                let parsed = drive_streaming_parser::<_, _, _, Error<&[u8]>>(
                    &mut bufreader,
                    index_segment_headers,
                )
                .unwrap();
                println!("{:?}", parsed);
            } else {
                // dat file
                let (next_start_position, parsed) =
                    drive_streaming_parser::<_, _, _, Error<&[u8]>>(
                        &mut bufreader,
                        data_header(1024),
                    )
                    .unwrap();
                println!("{:?}", parsed);
                if parsed.data_size > 0 {
                    let parsed = drive_streaming_parser::<_, _, _, Error<&[u8]>>(
                        &mut bufreader,
                        data_entry_headers(next_start_position),
                    )
                    .unwrap();
                    println!("{:?}", parsed);
                }
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
            .unwrap();
            let size = parsed.1;
            bufreader.seek(SeekFrom::Start(size.into())).unwrap();
            let parsed = drive_streaming_parser::<_, _, _, Error<&[u8]>>(
                &mut bufreader,
                index_segment_headers,
            )
            .unwrap();
            for header in &parsed.1 {
                if header.size == 0 {
                    continue;
                }
                bufreader
                    .seek(SeekFrom::Start(header.offset.into()))
                    .unwrap();
                let mut buf = vec![0; header.size.try_into().unwrap()];
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
            let index = load_index_reader(&mut bufreader, index_entry_1).unwrap();
            inner(&index);
        }
        Some(ext) if ext == "index2" => {
            let index = load_index_reader(&mut bufreader, index_entry_2).unwrap();
            inner(&index);
        }
        _ => {}
    });
}

#[test]
fn decompress_all_blocks() {
    forall_sqpack(|path, mut bufreader| {
        if let Some(extension) = path.extension() {
            if extension.to_string_lossy().starts_with("dat") {
                bufreader.seek(SeekFrom::Start(1024)).unwrap();
                let (next_start_position, data_header) =
                    drive_streaming_parser::<_, _, _, Error<&[u8]>>(
                        &mut bufreader,
                        data_header(1024),
                    )
                    .unwrap();
                if data_header.data_size > 0 {
                    let data_blocks = drive_streaming_parser::<_, _, _, Error<&[u8]>>(
                        &mut bufreader,
                        data_entry_headers(next_start_position),
                    )
                    .unwrap();
                    let mut reader = bufreader.into_inner();
                    let mut header_buffer = [0u8; 16];
                    let mut compressed = Vec::new();
                    for block_offset in data_blocks.all_blocks() {
                        reader
                            .seek(SeekFrom::Start(block_offset.try_into().unwrap()))
                            .unwrap();
                        reader.read_exact(&mut header_buffer).unwrap();
                        let (_, (compressed_length, decompressed_length)) =
                            block_header(&header_buffer).unwrap();
                        if compressed_length != 32000 {
                            let mut take = reader.take(compressed_length.try_into().unwrap());
                            take.read_to_end(&mut compressed).unwrap();
                            let decompressed = decompress_sqpack_block(
                                &compressed,
                                decompressed_length.try_into().unwrap(),
                            )
                            .unwrap();
                            println!("{:?}", String::from_utf8_lossy(&decompressed[..4]));
                            compressed.clear();
                            reader = take.into_inner();
                        }
                    }
                }
            }
        }
    });
}
