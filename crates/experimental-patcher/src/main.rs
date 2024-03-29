use std::{fs::File, path::PathBuf, process};

use clap::{builder::ValueParser, crate_name, crate_version, Arg, Command};

use fanttheysia_common::{StructuralFindAndReplace, TextReplacementRules};
use tomestone_exdf::{
    encoding::encode_exdf_page,
    parser::{exdf::Exdf, parse_row},
    Dataset, Language, Row, Value,
};
use tomestone_sqpack::{
    encoding::{PackSetWriter, RealPackIO},
    sidetables::build_side_tables,
    Category, Expansion, GameData, IndexHash, IndexHash2, PlatformId, SqPackId,
};
use tomestone_string_interp::Text;

fn app() -> Command {
    Command::new(crate_name!())
        .version(crate_version!())
        .arg(
            Arg::new("source directory")
                .long("src")
                .required(true)
                .value_parser(ValueParser::path_buf()),
        )
        .arg(
            Arg::new("destination directory")
                .long("dest")
                .required(true)
                .value_parser(ValueParser::path_buf()),
        )
        .arg(
            Arg::new("rules")
                .long("rules")
                .required(true)
                .value_parser(ValueParser::path_buf()),
        )
}

fn main() {
    let app_matches = app().get_matches();
    let source_path = app_matches.get_one::<PathBuf>("source directory").unwrap();
    let dest_path = app_matches
        .get_one::<PathBuf>("destination directory")
        .unwrap();
    let rules_path = app_matches.get_one::<PathBuf>("rules").unwrap();

    let rules = {
        let file = match File::open(rules_path) {
            Ok(file) => file,
            Err(e) => {
                eprintln!("error: could not open rules file: {}", e);
                process::exit(1);
            }
        };
        match serde_yaml::from_reader::<_, TextReplacementRules>(file) {
            Ok(rules) => rules,
            Err(e) => {
                eprintln!("error: rules file is malformed: {}", e);
                process::exit(1);
            }
        }
    };

    match (source_path.canonicalize(), dest_path.canonicalize()) {
        (Err(e), _) | (_, Err(e)) => {
            eprintln!("error: could not resolve directory: {}", e);
            process::exit(1);
        }
        (Ok(p1), Ok(p2)) if p1 == p2 => {
            eprintln!("error: the source and destination directories are the same");
            process::exit(1);
        }
        _ => {}
    }

    let source_game_data = match GameData::new(source_path) {
        Ok(game_data) => game_data,
        Err(e) => {
            eprintln!("error: couldn't read source directory: {}", e);
            process::exit(1);
        }
    };
    let mut source_data_file_set = source_game_data.data_files();

    let pack_id = SqPackId {
        category: Category::Exd,
        expansion: Expansion::Base,
        number: 0,
    };
    let (source_index, source_index_2) = match (
        source_game_data.get_index_1(&pack_id),
        source_game_data.get_index_2(&pack_id),
    ) {
        (Some(Ok(index)), Some(Ok(index2))) => (index, index2),
        (Some(Err(e)), _) | (_, Some(Err(e))) => {
            eprintln!("error: couldn't load index: {}", e);
            process::exit(1);
        }
        (None, _) | (_, None) => {
            eprintln!("error: couldn't find index in source directory");
            process::exit(1);
        }
    };

    let source_addon_dataset = match Dataset::load(
        &source_game_data,
        &mut source_data_file_set,
        "Addon",
        Language::English,
    ) {
        Ok(dataset) => dataset,
        Err(e) => {
            eprintln!("error: couldn't load dataset: {}", e);
            process::exit(1);
        }
    };
    let addon_exd_hashes = &source_addon_dataset
        .exd_path_iter()
        .map(|path| IndexHash2::hash(&path))
        .collect::<Vec<IndexHash2>>();

    let side_table = build_side_tables(&source_game_data, &mut source_data_file_set, pack_id);
    let dest_io = match RealPackIO::new(
        PathBuf::from(dest_path).join("game").join("sqpack"),
        PlatformId::Win32,
        pack_id,
    ) {
        Ok(io) => io,
        Err(e) => {
            eprintln!("error: couldn't create I/O interface: {}", e);
            process::exit(1);
        }
    };
    let mut writer = match PackSetWriter::new(dest_io, PlatformId::Win32, pack_id) {
        Ok(writer) => writer,
        Err(e) => {
            eprintln!("error: couldn't create writer: {}", e);
            process::exit(1);
        }
    };
    writer.set_side_table(side_table);

    let mut visitor = StructuralFindAndReplace::new(&rules.structured_text_rules);

    let file_iterator =
        source_data_file_set.iter_files_both_hashes(pack_id, source_index, source_index_2);
    for res in file_iterator {
        match res {
            Ok((Some(hash1), Some(hash2), mut data)) => {
                if addon_exd_hashes.contains(&hash2) {
                    let source_exdf = match Exdf::new(data) {
                        Ok(exdf) => exdf,
                        Err(e) => {
                            eprintln!("error: couldn't read exdf header: {:?}", e);
                            process::exit(1);
                        }
                    };
                    let rows_res = source_exdf
                        .iter()
                        .map(|res| {
                            let (row_number, raw_row) = match res {
                                Ok(tuple) => tuple,
                                Err(e) => return Err(e.code),
                            };
                            let mut sub_rows = parse_row(raw_row, &source_addon_dataset.exhf)?;

                            for sub_row in &mut sub_rows {
                                for cell in sub_row.cells.iter_mut() {
                                    let text_data_opt = match cell {
                                        Value::String(text_data) => Some(*text_data),
                                        Value::StringOwned(text_data) => Some(&**text_data),
                                        _ => None,
                                    };
                                    if let Some(text_data) = text_data_opt {
                                        let mut text = match Text::parse(text_data) {
                                            Ok(text) => text,
                                            Err(e) => {
                                                eprintln!(
                                                    "error: couldn't parse tagged text: {}",
                                                    e
                                                );
                                                process::exit(1);
                                            }
                                        };
                                        visitor.visit_text(&mut text);
                                        match tomestone_string_interp::encode(&text) {
                                            Ok(encoded) => *cell = Value::StringOwned(encoded),
                                            Err(e) => {
                                                eprintln!(
                                                    "error: couldn't re-encode modified text: {}",
                                                    e
                                                );
                                                process::exit(1);
                                            }
                                        }
                                    }
                                }
                            }

                            let row = Row {
                                number: row_number,
                                sub_rows,
                            };
                            Ok(row)
                        })
                        .collect::<Result<Vec<_>, nom::error::ErrorKind>>();
                    let rows = match rows_res {
                        Ok(rows) => rows,
                        Err(e) => {
                            eprintln!("error: couldn't parse data row: {:?}", e);
                            process::exit(1);
                        }
                    };
                    data = encode_exdf_page(
                        source_addon_dataset.name(),
                        &source_addon_dataset.exhf,
                        &rows,
                    );
                }

                if let Err(e) = writer.add_file_by_hashes(hash1, hash2, &data) {
                    eprintln!("error: problem while writing files: {}", e);
                    process::exit(1);
                }
            }
            Ok((None, _, _)) | Ok((_, None, _)) => {
                eprintln!("error: a file was present in only one index");
                process::exit(1);
            }
            Err(e) => {
                eprintln!("error: couldn't load original file entries: {}", e);
                process::exit(1);
            }
        }
    }

    if let Err(e) = writer.finalize() {
        eprintln!("error: couldn't finalize written data files: {}", e);
        process::exit(1);
    }
}

#[cfg(test)]
mod tests {
    use crate::app;

    #[test]
    fn verify_app() {
        app().debug_assert();
    }
}
