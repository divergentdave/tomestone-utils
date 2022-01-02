use std::{
    fs::File,
    panic::{catch_unwind, RefUnwindSafe, UnwindSafe},
    path::PathBuf,
};

use super::{index_entry_1, index_entry_2, load_index_reader, GrowableBufReader};
use crate::{
    parser::{collision_entry_1, collision_entry_2},
    Expansion, GameData, Index, IndexEntry, IndexHash, IndexHash1, IndexHash2,
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
        if !path.is_dir() {
            continue;
        }
        for res in std::fs::read_dir(path).unwrap() {
            let file_entry = res.unwrap();
            let path = file_entry.path();
            let file = File::open(&path).unwrap();
            let res = catch_unwind(|| f(path, GrowableBufReader::new(file)));
            if let Err(panic) = res {
                eprintln!("Error while processing {:?}", file_entry.path());
                std::panic::panic_any(panic);
            }
        }
    }
}

#[test]
fn check_index_order() {
    fn inner<E: IndexEntry>(index: &Index<E>) {
        let mut last_hash: Option<E::Hash> = None;
        for (hash, _pointer) in index.iter() {
            if let Some(last_hash) = &last_hash {
                assert!(last_hash <= &hash);
            }
            last_hash = Some(hash);
        }
        let mut last_hash: Option<E::Hash> = None;
        for entry in index.collision_table.iter() {
            if let Some(last_hash) = &last_hash {
                assert!(last_hash <= &entry.hash);
            }
            last_hash = Some(entry.hash);
        }
    }

    forall_sqpack(|path, mut bufreader| match path.extension() {
        Some(ext) if ext == "index" => {
            let index =
                load_index_reader(&mut bufreader, index_entry_1, collision_entry_1).unwrap();
            inner(&index);
        }
        Some(ext) if ext == "index2" => {
            let index =
                load_index_reader(&mut bufreader, index_entry_2, collision_entry_2).unwrap();
            inner(&index);
        }
        _ => {}
    });
}

#[test]
fn game_data() {
    dotenv::dotenv().ok();
    // Don't test anything if the game directory isn't provided
    let root = if let Ok(root) = std::env::var("FFXIV_INSTALL_DIR") {
        root
    } else {
        return;
    };
    let game_data = GameData::new(root).unwrap();
    let mut data_file_set = game_data.data_files();
    const SCD_PATH: &str = "music/ffxiv/BGM_System_Title.scd";

    let scd_data = game_data
        .lookup_path_data(&mut data_file_set, SCD_PATH)
        .unwrap()
        .unwrap();
    assert_eq!(&scd_data[..8], b"SEDBSSCF");

    let scd_data = game_data
        .lookup_hash_1_data(&mut data_file_set, &IndexHash1::hash(SCD_PATH))
        .unwrap();
    assert_eq!(scd_data.len(), 1);
    assert_eq!(&scd_data[0][..8], b"SEDBSSCF");

    let scd_data = game_data
        .lookup_hash_2_data(&mut data_file_set, &IndexHash2::hash(SCD_PATH))
        .unwrap();
    assert_eq!(scd_data.len(), 1);
    assert_eq!(&scd_data[0][..8], b"SEDBSSCF");
}
