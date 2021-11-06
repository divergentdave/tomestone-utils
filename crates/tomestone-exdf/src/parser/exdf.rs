use std::convert::TryInto;

use nom::{
    bytes::streaming::tag,
    combinator::{map, map_res, rest},
    error::ParseError,
    multi::{count, length_value},
    number::complete::{be_u16, be_u32},
    sequence::{pair, tuple},
    Finish, IResult,
};

use tomestone_common::null_padding;

use crate::RawDataRow;

#[derive(Debug)]
struct ExdfHeader {
    offset_table_size: u32,
    data_section_size: u32,
}

#[derive(Debug)]
struct OffsetEntry {
    row_number: u32,
    data_offset: u32,
}

#[derive(Debug)]
pub struct Exdf {
    data: Vec<u8>,
    header: ExdfHeader,
    offsets: Vec<OffsetEntry>,
}

impl Exdf {
    pub fn new(data: Vec<u8>) -> Result<Exdf, nom::error::ErrorKind> {
        let input = &data;
        let (input, header) = exdf_header(input).finish().map_err(|e| e.code)?;
        let offset_entry_count = TryInto::<usize>::try_into(header.offset_table_size / 8).unwrap();
        let (_input, offsets) = count(offset_entry, offset_entry_count)(input)
            .finish()
            .map_err(|e| e.code)?;
        Ok(Exdf {
            data,
            header,
            offsets,
        })
    }

    pub fn lookup(&self, row_number: u32) -> Option<Result<RawDataRow, nom::error::Error<&[u8]>>> {
        match self
            .offsets
            .binary_search_by_key(&row_number, |entry| entry.row_number)
        {
            Ok(offset_idx) => {
                let offset =
                    TryInto::<usize>::try_into(self.offsets[offset_idx].data_offset).unwrap();
                Some(
                    data_row(&self.data[offset..])
                        .finish()
                        .map(|(_, row_contents)| row_contents),
                )
            }
            Err(_) => None,
        }
    }

    pub fn iter(&self) -> ExdfIterator<'_> {
        ExdfIterator {
            data: &self.data,
            offsets: self.offsets.iter(),
        }
    }
}

pub struct ExdfIterator<'a> {
    data: &'a [u8],
    offsets: std::slice::Iter<'a, OffsetEntry>,
}

impl<'a, 'b> Iterator for ExdfIterator<'a> {
    type Item = Result<(u32, RawDataRow<'a>), nom::error::Error<&'a [u8]>>;

    fn next(&mut self) -> Option<Self::Item> {
        let entry = self.offsets.next()?;
        let offset = TryInto::<usize>::try_into(entry.data_offset).unwrap();
        Some(
            data_row(&self.data[offset..])
                .finish()
                .map(|(_, row_contents)| (entry.row_number, row_contents)),
        )
    }
}

fn exdf_header(input: &[u8]) -> IResult<&[u8], ExdfHeader> {
    map(
        tuple((
            tag("EXDF"),
            tag("\x00\x02"),
            be_u16,
            be_u32,
            be_u32,
            null_padding(16),
        )),
        |(_, _, _, offset_table_size, data_section_size, ())| ExdfHeader {
            offset_table_size,
            data_section_size,
        },
    )(input)
}

fn offset_entry(input: &[u8]) -> IResult<&[u8], OffsetEntry> {
    map(pair(be_u32, be_u32), |(row_number, data_offset)| {
        OffsetEntry {
            row_number,
            data_offset,
        }
    })(input)
}

fn data_row(input: &[u8]) -> IResult<&[u8], RawDataRow<'_>> {
    length_value(
        map_res(be_u32, |length| {
            let (length_plus_two, overflow) = length.overflowing_add(2);
            if overflow {
                return Err(nom::error::Error::from_error_kind(
                    b"",
                    nom::error::ErrorKind::TooLarge,
                ));
            }
            Ok(length_plus_two)
        }),
        map(pair(be_u16, rest), |(sub_row_count, data)| RawDataRow {
            data,
            sub_row_count,
        }),
    )(input)
}

#[cfg(test)]
mod tests {
    use std::convert::TryInto;

    use tomestone_sqpack::{Category, Expansion, GameData};

    use super::{exdf_header, Exdf};
    use crate::{parser::exhf::parse_exhf, Dataset, Language, RootList};

    #[test]
    fn exdf_game_data() {
        dotenv::dotenv().ok();
        // Don't test anything if the game directory isn't provided
        let root = if let Ok(root) = std::env::var("FFXIV_INSTALL_DIR") {
            root
        } else {
            return;
        };
        let game_data = GameData::new(root).unwrap();
        const EXH_PATH: &str = "exd/fcauthority.exh";
        const EXD_PATH: &str = "exd/fcauthority_0_en.exd";

        let exh_data = game_data.lookup_path_data(&EXH_PATH).unwrap().unwrap();
        let _exhf = parse_exhf(&exh_data).unwrap().1;

        let exd_data = game_data.lookup_path_data(&EXD_PATH).unwrap().unwrap();
        exdf_header(&exd_data).unwrap();
    }

    #[test]
    #[ignore = "slow test"]
    fn check_exdf_offset_table_order() {
        dotenv::dotenv().ok();
        // Don't test anything if the game directory isn't provided
        let root = if let Ok(root) = std::env::var("FFXIV_INSTALL_DIR") {
            root
        } else {
            return;
        };
        let game_data = GameData::new(root).unwrap();
        for expansion in Expansion::iter_all() {
            for pack_id in game_data.iter_packs_category_expansion(Category::Exd, *expansion) {
                let index = game_data.get_index_2(&pack_id).unwrap().unwrap();
                for res in game_data.iter_files(pack_id, &index).unwrap() {
                    let file = res.unwrap().1;
                    if file.len() > 32 && &file[..4] == b"EXDF" {
                        let exdf = Exdf::new(file).unwrap();
                        let mut last_row_number = None;
                        for entry in exdf.offsets.iter() {
                            if let Some(last) = last_row_number {
                                assert!(entry.row_number > last);
                            }
                            last_row_number = Some(entry.row_number)
                        }
                    }
                }
            }
        }
    }

    #[test]
    #[ignore = "slow test"]
    fn check_exdf_file_size() {
        dotenv::dotenv().ok();
        // Don't test anything if the game directory isn't provided
        let root = if let Ok(root) = std::env::var("FFXIV_INSTALL_DIR") {
            root
        } else {
            return;
        };
        let game_data = GameData::new(root).unwrap();
        for expansion in Expansion::iter_all() {
            for pack_id in game_data.iter_packs_category_expansion(Category::Exd, *expansion) {
                let index = game_data.get_index_2(&pack_id).unwrap().unwrap();
                for res in game_data.iter_files(pack_id, &index).unwrap() {
                    let file = res.unwrap().1;
                    if file.len() > 32 && &file[..4] == b"EXDF" {
                        let header = exdf_header(&file).unwrap().1;
                        let expected_len =
                            (32 + header.offset_table_size + header.data_section_size)
                                .try_into()
                                .unwrap();
                        assert_eq!(file.len(), expected_len);
                    }
                }
            }
        }
    }

    #[test]
    #[ignore = "slow test"]
    fn exd_round_trip_rows() {
        dotenv::dotenv().ok();
        // Don't test anything if the game directory isn't provided
        let root = if let Ok(root) = std::env::var("FFXIV_INSTALL_DIR") {
            root
        } else {
            return;
        };
        let game_data = GameData::new(root).unwrap();

        let root_list = RootList::open(&game_data).unwrap();
        for name in root_list.iter() {
            let padding_offset = match name {
                "Attributive"
                | "GCRankGridaniaFemaleText"
                | "GCRankGridaniaMaleText"
                | "GCRankLimsaFemaleText"
                | "GCRankLimsaMaleText"
                | "GCRankUldahFemaleText"
                | "GCRankUldahMaleText"
                | "HWDDevLevelWebText"
                | "PartyContentTransient" => 2,
                _ => 0,
            };
            let dataset = Dataset::load(&game_data, name, Language::English).unwrap();

            for (page_1, page_2) in dataset.page_iter().zip(dataset.page_iter()) {
                for (res_parsed, res_raw) in page_1.zip(page_2.exdf_iter) {
                    let row = res_parsed.unwrap();
                    let (number_from_raw, row_data) = res_raw.unwrap();
                    assert_eq!(row.number, number_from_raw);
                    let encoded =
                        crate::encoding::encode_row(&row.sub_rows, &dataset.exhf, padding_offset);
                    let encoded_inner = &encoded[6..];
                    assert_eq!(
                        row_data.data,
                        encoded_inner,
                        "data set {}, {:?}, {} sub-rows, {:?}",
                        name,
                        row.sub_rows,
                        row.sub_rows.len(),
                        dataset.exhf
                    );
                }
            }
        }
    }

    #[test]
    #[ignore = "slow test"]
    fn exd_round_trip_pages() {
        dotenv::dotenv().ok();
        // Don't test anything if the game directory isn't provided
        let root = if let Ok(root) = std::env::var("FFXIV_INSTALL_DIR") {
            root
        } else {
            return;
        };
        let game_data = GameData::new(root).unwrap();

        let root_list = RootList::open(&game_data).unwrap();
        for name in root_list.iter() {
            let dataset = Dataset::load(&game_data, name, Language::English).unwrap();

            for (page_1, page_2) in dataset.page_iter().zip(dataset.page_iter()) {
                let rows = page_1.map(|res| res.unwrap()).collect::<Vec<_>>();
                let encoded_data = crate::encoding::encode_exdf_page(name, &dataset.exhf, &rows);
                let original_data = page_2.exdf_iter.data;
                assert_eq!(original_data, encoded_data, "{} {:#?}", name, rows);
            }
        }
    }
}
