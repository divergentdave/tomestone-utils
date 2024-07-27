use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, map_res},
    multi::count,
    number::complete::{be_u16, be_u32, le_u16},
    sequence::{pair, tuple},
    IResult,
};

use tomestone_common::null_padding;

use crate::{Cardinality, ColumnDefinition, ColumnFormat, Language};

#[derive(Debug, Clone)]
struct ExhfHeader {
    row_size: u16,
    num_columns: u16,
    num_pages: u16,
    num_language_codes: u16,
    total_sub_rows: u32,
    cardinality: Cardinality,
    unknown_flag: bool,
    unknown_number: u16,
}

#[derive(Debug)]
pub struct Exhf {
    row_size: u16,
    columns_table_order: Vec<ColumnDefinition>,
    columns_offset_order: Vec<ColumnDefinition>,
    pages: Vec<(u32, u32)>,
    languages: Vec<Option<Language>>,
    total_sub_rows: u32,
    cardinality: Cardinality,
    unknown_flag: bool,
    unknown_number: u16,
}

impl Exhf {
    fn new(
        header: ExhfHeader,
        column_definitions: Vec<ColumnDefinition>,
        pages: Vec<(u32, u32)>,
        languages: Vec<Option<Language>>,
    ) -> Exhf {
        let mut columns_offset_order = column_definitions.clone();
        columns_offset_order.sort_by_key(|def| def.offset);
        Exhf {
            row_size: header.row_size,
            columns_table_order: column_definitions,
            columns_offset_order,
            pages,
            languages,
            total_sub_rows: header.total_sub_rows,
            cardinality: header.cardinality,
            unknown_flag: header.unknown_flag,
            unknown_number: header.unknown_number,
        }
    }

    pub fn columns_table_order(&self) -> &[ColumnDefinition] {
        &self.columns_table_order
    }

    pub fn columns_offset_order(&self) -> &[ColumnDefinition] {
        &self.columns_offset_order
    }

    pub fn pages(&self) -> &[(u32, u32)] {
        &self.pages
    }

    pub fn languages(&self) -> &[Option<Language>] {
        &self.languages
    }

    pub fn row_size(&self) -> u16 {
        self.row_size
    }

    pub fn total_sub_rows(&self) -> u32 {
        self.total_sub_rows
    }

    pub fn cardinality(&self) -> Cardinality {
        self.cardinality
    }

    pub fn unknown_flag(&self) -> bool {
        self.unknown_flag
    }

    pub fn unknown_number(&self) -> u16 {
        self.unknown_number
    }
}

fn exhf_header(input: &[u8]) -> IResult<&[u8], ExhfHeader> {
    map(
        tuple((
            tag(b"EXHF"),
            tag(b"\x00\x03"),
            be_u16,      // size of dataset chunk
            be_u16,      // number of columns
            be_u16,      // number of pages
            be_u16,      // number of lang codes
            be_u16,      // unknown
            cardinality, // single value or multi-value
            be_u16,      // unknown
            be_u32,      // number of entries
            null_padding(8),
        )),
        |(
            _,
            _,
            row_size,
            num_columns,
            num_pages,
            num_language_codes,
            unknown_field,
            cardinality,
            _,
            total_sub_rows,
            (),
        )| {
            ExhfHeader {
                row_size,
                num_columns,
                num_pages,
                num_language_codes,
                total_sub_rows,
                cardinality,
                unknown_flag: unknown_field & 0x4000 != 0,
                unknown_number: unknown_field & 0x3fff,
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

fn language_code(input: &[u8]) -> IResult<&[u8], Option<Language>> {
    map_res(le_u16, Language::from_u16)(input)
}

fn cardinality(input: &[u8]) -> IResult<&[u8], Cardinality> {
    alt((
        map(tag(b"\x00\x01"), |_| Cardinality::Single),
        map(tag(b"\x00\x02"), |_| Cardinality::Multiple),
    ))(input)
}

pub fn parse_exhf(input: &[u8]) -> IResult<&[u8], Exhf> {
    let (input, header) = exhf_header(input)?;
    let num_columns = usize::from(header.num_columns);
    let num_pages = usize::from(header.num_pages);
    let num_language_codes = usize::from(header.num_language_codes);
    map(
        tuple((
            count(column_entry, num_columns),
            count(page_entry, num_pages),
            count(language_code, num_language_codes),
        )),
        move |(column_definitions, pages, languages)| {
            let header = header.clone();
            let column_definitions = column_definitions
                .into_iter()
                .enumerate()
                .map(|(index, (format, offset))| ColumnDefinition {
                    format,
                    offset: offset.into(),
                    index,
                })
                .collect();
            Exhf::new(header, column_definitions, pages, languages)
        },
    )(input)
}

#[cfg(test)]
mod tests {
    use tomestone_common::test_game_data_or_skip;
    use tomestone_sqpack::GameData;

    use super::{exhf_header, parse_exhf};

    #[test]
    fn exhf_game_data() {
        let (game_data, mut data_file_set) = test_game_data_or_skip!();

        const EXH_PATH: &str = "exd/fcauthority.exh";

        let exh_data = game_data
            .lookup_path_data(&mut data_file_set, EXH_PATH)
            .unwrap()
            .unwrap();
        exhf_header(&exh_data).unwrap();
        parse_exhf(&exh_data).unwrap();
    }
}
