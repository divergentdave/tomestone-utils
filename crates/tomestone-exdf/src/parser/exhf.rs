use std::convert::TryInto;

use nom::{
    bytes::streaming::tag,
    combinator::{map, map_res},
    multi::count,
    number::complete::{be_u16, be_u32, le_u16},
    sequence::{pair, tuple},
    IResult,
};

use tomestone_common::null_padding;

use crate::{ColumnFormat, Language};

#[derive(Debug, Clone)]
struct ExhfHeader {
    row_size: u16,
    num_columns: u16,
    num_pages: u16,
    num_language_codes: u16,
    num_rows: u32,
}

#[derive(Debug)]
pub struct Exhf {
    row_size: u16,
    column_definitions: Vec<(ColumnFormat, u16)>,
    pages: Vec<(u32, u32)>,
    languages: Vec<Language>,
}

impl Exhf {
    fn new(
        header: ExhfHeader,
        column_definitions: Vec<(ColumnFormat, u16)>,
        pages: Vec<(u32, u32)>,
        languages: Vec<Language>,
    ) -> Exhf {
        Exhf {
            row_size: header.row_size,
            column_definitions,
            pages,
            languages,
        }
    }

    pub fn column_definitions(&self) -> &[(ColumnFormat, u16)] {
        &self.column_definitions
    }

    pub fn pages(&self) -> &[(u32, u32)] {
        &self.pages
    }

    pub fn languages(&self) -> &[Language] {
        &self.languages
    }

    pub fn row_size(&self) -> u16 {
        self.row_size
    }
}

fn exhf_header(input: &[u8]) -> IResult<&[u8], ExhfHeader> {
    map(
        tuple((
            tag("EXHF"),
            tag("\x00\x03"),
            be_u16, // size of dataset chunk
            be_u16, // number of datasets
            be_u16, // number of pages
            be_u16, // number of lang codes
            be_u16, // unknown
            be_u32, // unknown
            be_u32, // number of entries
            null_padding(8),
        )),
        |(_, _, row_size, num_columns, num_pages, num_language_codes, _, _, num_rows, ())| {
            ExhfHeader {
                row_size,
                num_columns,
                num_pages,
                num_language_codes,
                num_rows,
            }
        },
    )(input)
}

fn column_entry(input: &[u8]) -> IResult<&[u8], (ColumnFormat, u16)> {
    pair(map_res(be_u16, ColumnFormat::from_u16), be_u16)(input)
}

fn page_entry(input: &[u8]) -> IResult<&[u8], (u32, u32)> {
    pair(be_u32, be_u32)(input)
}

fn language_code(input: &[u8]) -> IResult<&[u8], Language> {
    map_res(le_u16, Language::from_u16)(input)
}

pub fn parse_exhf(input: &[u8]) -> IResult<&[u8], Exhf> {
    let (input, header) = exhf_header(input)?;
    let num_columns = TryInto::<usize>::try_into(header.num_columns).unwrap();
    let num_pages = TryInto::<usize>::try_into(header.num_pages).unwrap();
    let num_language_codes = TryInto::<usize>::try_into(header.num_language_codes).unwrap();
    map(
        tuple((
            count(column_entry, num_columns),
            count(page_entry, num_pages),
            count(language_code, num_language_codes),
        )),
        move |(column_definitions, pages, languages)| {
            let header = header.clone();
            Exhf::new(header, column_definitions, pages, languages)
        },
    )(input)
}

#[cfg(test)]
mod tests {
    use tomestone_sqpack::GameData;

    use super::{exhf_header, parse_exhf};

    #[test]
    fn exhf_game_data() {
        dotenv::dotenv().ok();
        // Don't test anything if the game directory isn't provided
        let root = if let Ok(root) = std::env::var("FFXIV_INSTALL_DIR") {
            root
        } else {
            return;
        };
        let game_data = GameData::new(root).unwrap();
        const EXH_PATH: &str = "exd/fcauthority.exh";

        let exh_data = game_data.lookup_path_data(EXH_PATH).unwrap().unwrap();
        exhf_header(&exh_data).unwrap();
        parse_exhf(&exh_data).unwrap();
    }
}
