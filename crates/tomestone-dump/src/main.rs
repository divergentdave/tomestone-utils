use std::{
    collections::BTreeMap,
    fmt::Write as FmtWrite,
    io::{self, stdout, Write},
    process,
};

use clap::{crate_authors, crate_description, crate_name, crate_version, App, Arg, SubCommand};
use once_cell::sync::Lazy;
use regex::{
    bytes::{Regex as BytesRegex, RegexBuilder as BytesRegexBuilder},
    Regex,
};

use tomestone_exdf::{Dataset, Language, RootList, Value};
use tomestone_sqpack::{
    pathdb::{PathDb, PreparedStatements},
    Category, Expansion, GameData, IndexEntry, IndexHash1, IndexHash2,
};
use tomestone_string_interp::Text;

fn lookup<'a>(
    game_data: &GameData,
    statements: Option<&mut PreparedStatements<'_>>,
    mut path_or_crc: impl Iterator<Item = &'a str>,
) -> Result<Option<Vec<u8>>, tomestone_sqpack::Error> {
    static CRC_RE: Lazy<Regex> = Lazy::new(|| Regex::new("^[0-9A-Fa-f]{8}$").unwrap());
    static PATH_LIKE_RE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(
            "^([0-9A-Fa-f]{8})/([0-9A-Fa-f]{8})$|\
            ^<([0-9A-Fa-f]{8})>/<([0-9A-Fa-f]{8})>$|\
            ^(.*)/([0-9A-Fa-f]{8})$|\
            ^(.*)/<([0-9A-Fa-f]{8})>$|\
            ^([0-9A-Fa-f]{8})/(.*)$|\
            ^<([0-9A-Fa-f]{8})>/(.*)$",
        )
        .unwrap()
    });

    let first_arg = path_or_crc.next().unwrap();
    let second_arg = path_or_crc.next();
    if let Some(second_arg) = second_arg {
        // two CRC-32s
        if CRC_RE.is_match(first_arg) && CRC_RE.is_match(second_arg) {
            let folder_crc = u32::from_str_radix(first_arg, 16).unwrap();
            let file_crc = u32::from_str_radix(second_arg, 16).unwrap();
            let hash = IndexHash1::new(folder_crc, file_crc);
            game_data.lookup_hash_1_data(&hash)
        } else {
            eprintln!("error: invalid CRC-32 hashes or multiple paths provided");
            process::exit(1);
        }
    } else if CRC_RE.is_match(first_arg) {
        // one CRC-32
        let crc = u32::from_str_radix(first_arg, 16).unwrap();
        let hash = IndexHash2::new(crc);
        game_data.lookup_hash_2_data(&hash)
    } else {
        let opt = game_data.lookup_path_data(first_arg)?;
        if opt.is_some() {
            // path
            if let Some(statements) = statements {
                statements.add_path(first_arg)?;
            }
            Ok(opt)
        } else if let Some(caps) = PATH_LIKE_RE.captures(first_arg) {
            // combinations of CRC-32 and partial paths, joined with a slash
            let left = caps
                .get(1)
                .or_else(|| caps.get(3))
                .or_else(|| caps.get(5))
                .or_else(|| caps.get(7))
                .or_else(|| caps.get(9))
                .or_else(|| caps.get(11))
                .unwrap()
                .as_str();
            let right = caps
                .get(2)
                .or_else(|| caps.get(4))
                .or_else(|| caps.get(6))
                .or_else(|| caps.get(8))
                .or_else(|| caps.get(10))
                .or_else(|| caps.get(12))
                .unwrap()
                .as_str();
            let mut folder_crcs = vec![tomestone_sqpack::crc32(left.to_lowercase().as_bytes())];
            if let Ok(crc) = u32::from_str_radix(left, 16) {
                folder_crcs.push(crc);
            }
            let mut filename_crcs = vec![tomestone_sqpack::crc32(right.to_lowercase().as_bytes())];
            if let Ok(crc) = u32::from_str_radix(right, 16) {
                filename_crcs.push(crc);
            }
            for folder_crc in folder_crcs.iter().rev() {
                for filename_crc in filename_crcs.iter().rev() {
                    let hash = IndexHash1::new(*folder_crc, *filename_crc);
                    let opt = game_data.lookup_hash_1_data(&hash)?;
                    if opt.is_some() {
                        return Ok(opt);
                    }
                }
            }
            Ok(None)
        } else {
            Ok(None)
        }
    }
}

fn write_file_name_1<W: Write>(
    writer: &mut W,
    statements: &mut PreparedStatements<'_>,
    hash: IndexHash1,
) -> Result<(), tomestone_sqpack::Error> {
    let (folder_matches, filename_matches) = statements.index_1_lookup(hash)?;
    match (folder_matches.len(), filename_matches.len()) {
        (0, 0) => write!(
            writer,
            "<{:08x}>/<{:08x}>",
            hash.folder_crc, hash.filename_crc
        ),
        (1, 1) => write!(writer, "{}/{}", folder_matches[0], filename_matches[0]),
        (0, 1) => write!(writer, "<{:08x}>/{}", hash.folder_crc, filename_matches[0]),
        (1, 0) => write!(writer, "{}/<{:08x}>", folder_matches[0], hash.filename_crc),
        (1, _) => write!(writer, "{}/{:?}", folder_matches[0], filename_matches),
        (_, 1) => write!(writer, "{:?}/{}", folder_matches, filename_matches[0]),
        (_, _) => write!(writer, "{:?}/{:?}", folder_matches, filename_matches),
    }
    .unwrap();
    Ok(())
}

fn write_file_name_2<W: Write>(
    writer: &mut W,
    statements: &mut PreparedStatements<'_>,
    hash: IndexHash2,
) -> Result<(), tomestone_sqpack::Error> {
    let matches = statements.index_2_lookup(hash)?;
    match matches.len() {
        0 => write!(writer, "<{:08x}>", hash.path_crc),
        1 => write!(writer, "{}", matches[0]),
        _ => write!(writer, "{:?}", matches),
    }
    .unwrap();
    Ok(())
}

fn list_files(
    game_data: &GameData,
    category: Category,
    expansion: Expansion,
    statements: &mut PreparedStatements<'_>,
) -> Result<(), tomestone_sqpack::Error> {
    let stdout = stdout();
    let mut locked = stdout.lock();
    for id in game_data.iter_packs_category_expansion(category, expansion) {
        if let Some(Ok(index)) = game_data.get_index_1(&id) {
            for entry in index.iter() {
                let hash = entry.hash();
                write_file_name_1(&mut locked, statements, hash)?;
                locked.write_all(b"\n").unwrap();
            }
        } else {
            let index = game_data.get_index_2(&id).unwrap()?;
            for entry in index.iter() {
                let hash = entry.hash();
                write_file_name_2(&mut locked, statements, hash)?;
                locked.write_all(b"\n").unwrap();
            }
        }
    }
    Ok(())
}

fn write_hex_dump<W: Write>(data: &[u8], mut writer: W) -> io::Result<()> {
    let mut hex_buf = [0u8; 32];
    let mut line_buf = [0u8; 54];
    line_buf[8] = b' ';
    line_buf[17] = b' ';
    line_buf[26] = b' ';
    line_buf[35] = b' ';
    line_buf[36] = b' ';
    line_buf[53] = b'\n';
    for chunk in data.chunks(16) {
        hex::encode_to_slice(chunk, &mut hex_buf[..chunk.len() * 2]).unwrap();

        line_buf[..std::cmp::min(chunk.len() * 2, 8)]
            .copy_from_slice(&hex_buf[..std::cmp::min(chunk.len() * 2, 8)]);
        if chunk.len() <= 4 {
            line_buf[chunk.len() * 2..8].fill(b' ');
        }

        if chunk.len() > 4 {
            line_buf[9..1 + std::cmp::min(chunk.len() * 2, 16)]
                .copy_from_slice(&hex_buf[8..std::cmp::min(chunk.len() * 2, 16)]);
            if chunk.len() <= 8 {
                line_buf[1 + chunk.len() * 2..17].fill(b' ');
            }
        } else {
            line_buf[9..17].fill(b' ');
        }

        if chunk.len() > 8 {
            line_buf[18..2 + std::cmp::min(chunk.len() * 2, 24)]
                .copy_from_slice(&hex_buf[16..std::cmp::min(chunk.len() * 2, 24)]);
            if chunk.len() <= 12 {
                line_buf[2 + chunk.len() * 2..26].fill(b' ');
            }
        } else {
            line_buf[18..26].fill(b' ');
        }

        if chunk.len() > 12 {
            line_buf[27..3 + chunk.len() * 2].copy_from_slice(&hex_buf[24..chunk.len() * 2]);
            line_buf[3 + chunk.len() * 2..35].fill(b' ');
        } else {
            line_buf[27..35].fill(b' ');
        }

        for (src, dest) in chunk.iter().zip(line_buf[37..37 + chunk.len()].iter_mut()) {
            if *src >= 0x20 && *src < 0x7f {
                *dest = *src;
            } else {
                *dest = b'.';
            }
        }
        for byte in line_buf[37 + chunk.len()..53].iter_mut() {
            *byte = b' ';
        }

        writer.write_all(&line_buf)?;
    }
    Ok(())
}

fn print_hex_dump(data: &[u8]) {
    let stdout = stdout();
    let locked = stdout.lock();
    write_hex_dump(data, locked).unwrap();
}

fn parse_repository_path(path: Option<&str>) -> Option<(Category, Expansion)> {
    if let Some(path) = path {
        let segments: Vec<_> = path.split('/').collect();
        match segments.len() {
            0 => unreachable!(),
            1 => {
                // one segment, category only, assume it's from the base game
                if let Ok(category) = Category::parse_name(segments[0]) {
                    Some((category, Expansion::Base))
                } else {
                    eprintln!("error: invalid category {:?}", segments[0]);
                    process::exit(1);
                }
            }
            2 => {
                if let Ok(category) = Category::parse_name(segments[0]) {
                    if let Ok(expansion) = Expansion::parse_name(segments[1]) {
                        Some((category, expansion))
                    } else {
                        eprintln!("error: invalid expansion {:?}", segments[1]);
                        process::exit(1);
                    }
                } else {
                    eprintln!("error: invalid category {:?}", segments[0]);
                    process::exit(1);
                }
            }
            _ => {
                eprintln!("error: only up to two path segments are supported");
                process::exit(1);
            }
        }
    } else {
        None
    }
}

fn do_grep(
    game_data: &GameData,
    statements: &mut PreparedStatements<'_>,
    category: Category,
    expansion: Expansion,
    re: &BytesRegex,
) -> Result<(), tomestone_sqpack::Error> {
    let stdout = stdout();
    let mut locked = stdout.lock();
    for pack_id in game_data.iter_packs_category_expansion(category, expansion) {
        if let Some(Ok(index)) = game_data.get_index_1(&pack_id) {
            for res in game_data.iter_files(pack_id, &index)? {
                let (hash, file) = res?;
                if re.is_match(&file) {
                    locked.write_all(b"File ").unwrap();
                    write_file_name_1(&mut locked, statements, hash)?;
                    locked.write_all(b" matches\n").unwrap();
                }
            }
        } else {
            let index = game_data.get_index_2(&pack_id).unwrap()?;
            for res in game_data.iter_files(pack_id, &index)? {
                let (hash, file) = res?;
                if re.is_match(&file) {
                    locked.write_all(b"File ").unwrap();
                    write_file_name_2(&mut locked, statements, hash)?;
                    locked.write_all(b" matches\n").unwrap();
                }
            }
        }
    }
    Ok(())
}

static PATH_DISCOVERY_RE: Lazy<BytesRegex> = Lazy::new(|| {
    BytesRegex::new(
        "((?:common|bgcommon|bg|cut|chara|shader|ui|sound|vfx|ui_script|exd|game_script|music|\
        sqpack_test|debug)/[-a-zA-Z0-9_./]+)\\x00",
    )
    .unwrap()
});

fn discover_paths(game_data: &GameData) -> Result<(), tomestone_exdf::Error> {
    let db = PathDb::open().map_err::<tomestone_sqpack::Error, _>(From::from)?;
    let mut statements = db
        .prepare()
        .map_err::<tomestone_sqpack::Error, _>(From::from)?;

    let mut indices = BTreeMap::new();
    for category in &[
        Category::BgCommon,
        Category::Bg,
        Category::Cut,
        Category::Chara,
        Category::Ui,
        Category::Vfx,
        Category::Exd,
    ] {
        for expansion in Expansion::iter_all() {
            for pack_id in game_data.iter_packs_category_expansion(*category, *expansion) {
                let index = game_data.get_index_2(&pack_id).unwrap()?;
                indices.insert(pack_id, index);
            }
        }
    }

    for (pack_id, index) in indices.iter() {
        for res in game_data.iter_files(*pack_id, &index)? {
            let (_hash, file) = res?;
            for caps in PATH_DISCOVERY_RE.captures_iter(&file) {
                let discovered_path = std::str::from_utf8(caps.get(1).unwrap().as_bytes()).unwrap();
                if game_data.lookup_path_locator(discovered_path)?.is_some() {
                    statements
                        .add_path(discovered_path)
                        .map_err::<tomestone_sqpack::Error, _>(From::from)?;
                } else if game_data.contains_folder(discovered_path)? {
                    statements
                        .add_folder(discovered_path)
                        .map_err::<tomestone_sqpack::Error, _>(From::from)?;
                } else {
                    let slash_idx = discovered_path.rfind('/').unwrap();
                    let discovered_folder = &discovered_path[..slash_idx];
                    if game_data.contains_folder(discovered_folder)? {
                        statements
                            .add_folder(discovered_folder)
                            .map_err::<tomestone_sqpack::Error, _>(From::from)?;
                    }
                }
            }
        }
    }

    let root_list = RootList::open(&game_data)?;
    for name in root_list.iter() {
        let exh_path = format!("exd/{}.exh", name);
        if let Some(exh_data) = game_data.lookup_path_data(&exh_path)? {
            statements
                .add_path(&exh_path)
                .map_err::<tomestone_sqpack::Error, _>(From::from)?;

            if let Ok((_, exhf)) = tomestone_exdf::parser::exhf::parse_exhf(&exh_data) {
                for language in exhf.languages() {
                    let short_code = language.as_ref().map(Language::short_code);
                    for (page_start, _) in exhf.pages() {
                        let exd_path = if let Some(short_code) = short_code {
                            format!("exd/{}_{}_{}.exd", name, page_start, short_code)
                        } else {
                            format!("exd/{}_{}.exd", name, page_start)
                        };
                        if game_data.lookup_path_locator(&exd_path)?.is_some() {
                            statements
                                .add_path(&exd_path)
                                .map_err::<tomestone_sqpack::Error, _>(From::from)?;
                        }
                    }
                }
            }
        }

        let luab_path = format!("game_script/{}.luab", name);
        if game_data.lookup_path_locator(&luab_path)?.is_some() {
            statements
                .add_path(&luab_path)
                .map_err::<tomestone_sqpack::Error, _>(From::from)?;
        }

        if name.starts_with("quest/") {
            if let Some(underscore_pos) = name.find('_') {
                for luab_path in [
                    format!(
                        "game_script/{}btl{}.luab",
                        &name[..underscore_pos],
                        &name[underscore_pos..]
                    ),
                    format!(
                        "game_script/{}btl2{}.luab",
                        &name[..underscore_pos],
                        &name[underscore_pos..]
                    ),
                ]
                .iter()
                {
                    if game_data.lookup_path_locator(&luab_path)?.is_some() {
                        statements
                            .add_path(&luab_path)
                            .map_err::<tomestone_sqpack::Error, _>(From::from)?;
                    }
                }
            }
        }
    }

    Ok(())
}

fn open_db() -> PathDb {
    match PathDb::open() {
        Ok(db) => db,
        Err(e) => {
            eprintln!("error: couldn't open path hash database, {}", e);
            process::exit(1);
        }
    }
}

fn main() {
    dotenv::dotenv().ok();
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

    let app = App::new(crate_name!())
        .version(crate_version!())
        .author(crate_authors!())
        .about(crate_description!())
        .subcommand(
            SubCommand::with_name("raw")
                .about("Extract a file and write it to standard output")
                .arg(
                    Arg::with_name("path_or_crc")
                        .required(true)
                        .index(1)
                        .min_values(1)
                        .max_values(2),
                ),
        )
        .subcommand(
            SubCommand::with_name("hex")
                .about("Extract a file and print it as a hex dump")
                .arg(
                    Arg::with_name("path_or_crc")
                        .required(true)
                        .index(1)
                        .min_values(1)
                        .max_values(2),
                ),
        )
        .subcommand(
            SubCommand::with_name("list")
                .about("List files by hash or path (where available)")
                .arg(Arg::with_name("path").required(false).index(1)),
        )
        .subcommand(
            SubCommand::with_name("grep")
                .about("Search file contents for regular expressions")
                .arg(Arg::with_name("pattern").required(true).index(1))
                .arg(Arg::with_name("path").required(false).index(2))
                .arg(
                    Arg::with_name("ignore-case")
                        .short("i")
                        .long("ignore-case")
                        .required(false)
                        .takes_value(false),
                ),
        )
        .subcommand(
            SubCommand::with_name("discover_paths")
                .about("Search all files for paths of other files, and update the path database"),
        )
        .subcommand(
            SubCommand::with_name("exd")
                .about("Extract and dump EXHF/EXDF files")
                .arg(Arg::with_name("path").required(true).index(1))
                .arg(
                    Arg::with_name("language")
                        .long("language")
                        .short("l")
                        .required(false)
                        .takes_value(true),
                ),
        );
    let app_matches = app.get_matches();

    let db = open_db();
    let mut statements = db.prepare().unwrap();

    match app_matches.subcommand() {
        ("raw", Some(matches)) => {
            match lookup(
                &game_data,
                Some(&mut statements),
                matches.values_of("path_or_crc").unwrap(),
            ) {
                Ok(Some(data)) => {
                    stdout().write_all(&data).unwrap();
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
            match lookup(
                &game_data,
                Some(&mut statements),
                matches.values_of("path_or_crc").unwrap(),
            ) {
                Ok(Some(data)) => print_hex_dump(&data),
                Ok(None) => {
                    eprintln!("error: file not found");
                    process::exit(1);
                }
                Err(e) => {
                    eprintln!("{}", e);
                    process::exit(1);
                }
            }
        }
        ("list", Some(matches)) => match parse_repository_path(matches.value_of("path")) {
            Some((category, expansion)) => {
                if let Err(e) = list_files(&game_data, category, expansion, &mut statements) {
                    eprintln!("error: couldn't read indices, {}", e);
                    process::exit(1);
                }
            }
            None => {
                for category in Category::iter_all() {
                    for expansion in Expansion::iter_all() {
                        if let Err(e) =
                            list_files(&game_data, *category, *expansion, &mut statements)
                        {
                            eprintln!("error: couldn't read indices, {}", e);
                            process::exit(1);
                        }
                    }
                }
            }
        },
        ("grep", Some(matches)) => {
            let mut builder = BytesRegexBuilder::new(matches.value_of("pattern").unwrap());
            if matches.is_present("ignore-case") {
                builder.case_insensitive(true);
            }
            let re = match builder.build() {
                Ok(re) => re,
                Err(e) => {
                    eprintln!("error: invalid regular expression, {}", e);
                    process::exit(1);
                }
            };
            match parse_repository_path(matches.value_of("path")) {
                Some((category, expansion)) => {
                    if let Err(e) = do_grep(&game_data, &mut statements, category, expansion, &re) {
                        eprintln!("error: couldn't read files, {}", e);
                        process::exit(1);
                    }
                }
                None => {
                    for category in Category::iter_all() {
                        for expansion in Expansion::iter_all() {
                            if let Err(e) =
                                do_grep(&game_data, &mut statements, *category, *expansion, &re)
                            {
                                eprintln!("error: couldn't read files, {}", e);
                                process::exit(1);
                            }
                        }
                    }
                }
            }
        }
        ("discover_paths", Some(_matches)) => {
            if let Err(e) = discover_paths(&game_data) {
                eprintln!("error: {}", e);
                process::exit(1);
            }
        }
        ("exd", Some(matches)) => {
            let original_path = matches.value_of("path").unwrap();
            let language_code = matches.value_of("language").unwrap_or("en");
            let language = match language_code.parse() {
                Ok(language) => language,
                Err(_) => {
                    eprintln!("error: did not recognize language {}", language_code);
                    process::exit(1);
                }
            };
            let path_base = match (original_path.rfind('.'), original_path.starts_with("exd/")) {
                (Some(dot_position), false) => &original_path[..dot_position],
                (Some(dot_position), true) => &original_path[4..dot_position],
                (None, false) => original_path,
                (None, true) => &original_path[4..],
            };

            let dataset = match Dataset::load(&game_data, path_base, language) {
                Ok(dataset) => dataset,
                Err(e) => {
                    eprintln!("error: loading dataset failed: {}", e);
                    process::exit(1);
                }
            };
            println!("{:?}", &dataset.exhf);
            for page_iter in dataset.page_iter() {
                for res in page_iter {
                    let row = match res {
                        Ok(row) => row,
                        Err(e) => {
                            eprintln!("error: reading dataset failed: {}", e);
                            process::exit(1);
                        }
                    };

                    let mut line = format!("{} [[", row.number);
                    for (sub_row_counter, sub_row) in row.sub_rows.iter().enumerate() {
                        if sub_row_counter != 0 {
                            line.push_str("], [");
                        }
                        for (i, value) in sub_row.cells.iter().enumerate() {
                            if i != 0 {
                                line.push_str(", ");
                            }
                            if let Value::String(data) = value {
                                match Text::parse(data) {
                                    Ok(text) => write!(&mut line, "{:?}", text).unwrap(),
                                    Err(e) => {
                                        eprintln!("error: parsing tagged text failed: {}", e);
                                        process::exit(1);
                                    }
                                }
                            } else {
                                write!(&mut line, "{:?}", value).unwrap();
                            }
                        }
                    }
                    line.push_str("]]");
                    println!("{}", line);
                }
            }
        }
        _ => {
            eprintln!("{}", app_matches.usage());
            process::exit(1);
        }
    }
}

#[cfg(test)]
mod tests {
    use tomestone_sqpack::GameData;

    use crate::{lookup, write_hex_dump, PATH_DISCOVERY_RE};

    #[test]
    fn path_discovery_regex() {
        let mut it = PATH_DISCOVERY_RE.captures_iter(
            b"\x00\x00\x00\x00bg/ffxiv/wil_w1/hou/w1h1/texture/w1h1_w1_art2_n.tex\x00\
            exd/AirshipExplorationLevel_0.exd\x00\x00",
        );
        let captures = it.next().unwrap();
        assert_eq!(
            captures.get(1).unwrap().as_bytes(),
            b"bg/ffxiv/wil_w1/hou/w1h1/texture/w1h1_w1_art2_n.tex"
        );
        let captures = it.next().unwrap();
        assert_eq!(
            captures.get(1).unwrap().as_bytes(),
            b"exd/AirshipExplorationLevel_0.exd"
        );
        assert!(it.next().is_none());
    }

    #[test]
    fn hex_dump() {
        fn reftest(data: &[u8], reference: &[u8]) {
            let mut buf: Vec<u8> = Vec::new();
            write_hex_dump(data, &mut buf).unwrap();
            assert_eq!(&buf, reference);
        }

        reftest(b"", b"");
        reftest(
            b"\x7f\n",
            b"7f0a                                 ..              \n",
        );
        reftest(
            b"ABC",
            b"414243                               ABC             \n",
        );
        reftest(
            b"AAAAAAAAAAAAAAAA0",
            b"41414141 41414141 41414141 41414141  AAAAAAAAAAAAAAAA\n\
            30                                   0               \n",
        );
        reftest(
            b"AAAAAAAAAAAAAAAA01",
            b"41414141 41414141 41414141 41414141  AAAAAAAAAAAAAAAA\n\
            3031                                 01              \n",
        );
        reftest(
            b"AAAAAAAAAAAAAAAA012",
            b"41414141 41414141 41414141 41414141  AAAAAAAAAAAAAAAA\n\
            303132                               012             \n",
        );
        reftest(
            b"AAAAAAAAAAAAAAAA0123",
            b"41414141 41414141 41414141 41414141  AAAAAAAAAAAAAAAA\n\
            30313233                             0123            \n",
        );
        reftest(
            b"AAAAAAAAAAAAAAAA01234",
            b"41414141 41414141 41414141 41414141  AAAAAAAAAAAAAAAA\n\
            30313233 34                          01234           \n",
        );
        reftest(
            b"AAAAAAAAAAAAAAAA012345",
            b"41414141 41414141 41414141 41414141  AAAAAAAAAAAAAAAA\n\
            30313233 3435                        012345          \n",
        );
        reftest(
            b"AAAAAAAAAAAAAAAA0123456",
            b"41414141 41414141 41414141 41414141  AAAAAAAAAAAAAAAA\n\
            30313233 343536                      0123456         \n",
        );
        reftest(
            b"AAAAAAAAAAAAAAAA01234567",
            b"41414141 41414141 41414141 41414141  AAAAAAAAAAAAAAAA\n\
            30313233 34353637                    01234567        \n",
        );
        reftest(
            b"AAAAAAAAAAAAAAAA012345678",
            b"41414141 41414141 41414141 41414141  AAAAAAAAAAAAAAAA\n\
            30313233 34353637 38                 012345678       \n",
        );
        reftest(
            b"AAAAAAAAAAAAAAAA0123456789",
            b"41414141 41414141 41414141 41414141  AAAAAAAAAAAAAAAA\n\
            30313233 34353637 3839               0123456789      \n",
        );
        reftest(
            b"AAAAAAAAAAAAAAAA01234567890",
            b"41414141 41414141 41414141 41414141  AAAAAAAAAAAAAAAA\n\
            30313233 34353637 383930             01234567890     \n",
        );
        reftest(
            b"AAAAAAAAAAAAAAAA012345678901",
            b"41414141 41414141 41414141 41414141  AAAAAAAAAAAAAAAA\n\
            30313233 34353637 38393031           012345678901    \n",
        );
        reftest(
            b"AAAAAAAAAAAAAAAA0123456789012",
            b"41414141 41414141 41414141 41414141  AAAAAAAAAAAAAAAA\n\
            30313233 34353637 38393031 32        0123456789012   \n",
        );
        reftest(
            b"AAAAAAAAAAAAAAAA01234567890123",
            b"41414141 41414141 41414141 41414141  AAAAAAAAAAAAAAAA\n\
            30313233 34353637 38393031 3233      01234567890123  \n",
        );
        reftest(
            b"AAAAAAAAAAAAAAAA012345678901234",
            b"41414141 41414141 41414141 41414141  AAAAAAAAAAAAAAAA\n\
            30313233 34353637 38393031 323334    012345678901234 \n",
        );
        reftest(
            b"AAAAAAAAAAAAAAAA0123456789012345",
            b"41414141 41414141 41414141 41414141  AAAAAAAAAAAAAAAA\n\
            30313233 34353637 38393031 32333435  0123456789012345\n",
        );
    }

    #[test]
    fn cli_lookup() {
        dotenv::dotenv().ok();
        // Don't test anything if the game directory isn't probided
        let root = if let Ok(root) = std::env::var("FFXIV_INSTALL_DIR") {
            root
        } else {
            return;
        };
        let game_data = GameData::new(root).unwrap();

        lookup(&game_data, None, vec!["exd/fcauthority.exh"].into_iter())
            .unwrap()
            .unwrap();
        lookup(&game_data, None, vec!["e39b7999", "e69d80a4"].into_iter())
            .unwrap()
            .unwrap();
        lookup(&game_data, None, vec!["E39B7999", "E69D80A4"].into_iter())
            .unwrap()
            .unwrap();
        lookup(&game_data, None, vec!["e39b7999/e69d80a4"].into_iter())
            .unwrap()
            .unwrap();
        lookup(&game_data, None, vec!["<e39b7999>/<e69d80a4>"].into_iter())
            .unwrap()
            .unwrap();
        lookup(&game_data, None, vec!["exd/<e69d80a4>"].into_iter())
            .unwrap()
            .unwrap();
        lookup(
            &game_data,
            None,
            vec!["<e39b7999>/fcauthority.exh"].into_iter(),
        )
        .unwrap()
        .unwrap();
        lookup(&game_data, None, vec!["exd/e69d80a4"].into_iter())
            .unwrap()
            .unwrap();
        lookup(
            &game_data,
            None,
            vec!["e39b7999/fcauthority.exh"].into_iter(),
        )
        .unwrap()
        .unwrap();

        lookup(
            &game_data,
            None,
            vec!["music/ex1/bgm_ex1_deep01.scd"].into_iter(),
        )
        .unwrap()
        .unwrap();
        lookup(&game_data, None, vec!["7cf6ce88", "7f2056de"].into_iter())
            .unwrap()
            .unwrap();
        lookup(&game_data, None, vec!["7cf6ce88/7f2056de"].into_iter())
            .unwrap()
            .unwrap();
        lookup(&game_data, None, vec!["<7cf6ce88>/<7f2056de>"].into_iter())
            .unwrap()
            .unwrap();
        lookup(&game_data, None, vec!["music/ex1/<7f2056de>"].into_iter())
            .unwrap()
            .unwrap();
        lookup(
            &game_data,
            None,
            vec!["<7cf6ce88>/bgm_ex1_deep01.scd"].into_iter(),
        )
        .unwrap()
        .unwrap();
        lookup(&game_data, None, vec!["music/ex1/7f2056de"].into_iter())
            .unwrap()
            .unwrap();
        lookup(
            &game_data,
            None,
            vec!["7cf6ce88/bgm_ex1_deep01.scd"].into_iter(),
        )
        .unwrap()
        .unwrap();
    }
}
