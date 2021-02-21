use std::convert::TryInto;

use nom::{
    bytes::streaming::tag,
    combinator::{complete, map},
    multi::{count, length_data},
    number::streaming::{be_u16, be_u32},
    sequence::{pair, tuple},
    IResult,
};

use super::null_padding;

#[derive(Debug)]
struct ExdfHeader {
    offset_table_size: u32,
    data_section_size: u32,
}

#[derive(Debug)]
pub struct Exdf<'a> {
    data: &'a [u8],
    header: ExdfHeader,
    offsets: Vec<(u32, u32)>,
}

impl<'a> Exdf<'a> {
    pub fn new(data: &[u8]) -> Result<Exdf, nom::error::Error<&[u8]>> {
        let input = data;
        let (input, header) = match complete(exdf_header)(input) {
            Ok((input, header)) => (input, header),
            Err(nom::Err::Incomplete(_)) => unreachable!(),
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => return Err(e),
        };
        let offset_entry_count = TryInto::<usize>::try_into(header.offset_table_size / 8).unwrap();
        let (_input, offsets) = match complete(count(offset_entry, offset_entry_count))(input) {
            Ok((input, offsets)) => (input, offsets),
            Err(nom::Err::Incomplete(_)) => unreachable!(),
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => return Err(e),
        };
        Ok(Exdf {
            data,
            header,
            offsets,
        })
    }

    pub fn lookup(&self, row_number: u32) -> Option<Result<&'a [u8], nom::error::Error<&'a [u8]>>> {
        match self
            .offsets
            .binary_search_by_key(&row_number, |(row, _)| *row)
        {
            Ok(offset_idx) => {
                let offset = TryInto::<usize>::try_into(self.offsets[offset_idx].1).unwrap();
                match complete(data_row)(&self.data[offset..]) {
                    Ok((_, row_contents)) => Some(Ok(row_contents)),
                    Err(nom::Err::Incomplete(_)) => unreachable!(),
                    Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => Some(Err(e)),
                }
            }
            Err(_) => None,
        }
    }

    pub fn iter<'b>(&'b self) -> ExdfIterator<'a, 'b> {
        ExdfIterator {
            data: self.data,
            offsets: self.offsets.iter(),
        }
    }
}

pub struct ExdfIterator<'a, 'b> {
    data: &'a [u8],
    offsets: std::slice::Iter<'b, (u32, u32)>,
}

impl<'a, 'b> Iterator for ExdfIterator<'a, 'b> {
    type Item = Result<(u32, &'a [u8]), nom::error::Error<&'a [u8]>>;

    fn next(&mut self) -> Option<Result<(u32, &'a [u8]), nom::error::Error<&'a [u8]>>> {
        let (row_number, offset) = self.offsets.next()?;
        let offset = TryInto::<usize>::try_into(*offset).unwrap();
        match complete(data_row)(&self.data[offset..]) {
            Ok((_, row_contents)) => Some(Ok((*row_number, row_contents))),
            Err(nom::Err::Incomplete(_)) => unreachable!(),
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => Some(Err(e)),
        }
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

fn offset_entry(input: &[u8]) -> IResult<&[u8], (u32, u32)> {
    pair(be_u32, be_u32)(input)
}

fn data_row(input: &[u8]) -> IResult<&[u8], &[u8]> {
    length_data(map(pair(be_u32, be_u16), |(length, _)| length))(input)
}

#[cfg(test)]
mod tests {
    use std::convert::TryInto;

    use nom::combinator::complete;

    use tomestone_sqpack::{Category, Expansion, GameData};

    use super::{exdf_header, Exdf};
    use crate::parser::exhf::parse_exhf;

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
        let _exhf = complete(parse_exhf)(&exh_data).unwrap().1;

        let exd_data = game_data.lookup_path_data(&EXD_PATH).unwrap().unwrap();
        complete(exdf_header)(&exd_data).unwrap();
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
                        let exdf = Exdf::new(&file).unwrap();
                        let mut last_row_number = None;
                        for (row, _) in exdf.offsets.iter() {
                            if let Some(last) = last_row_number {
                                assert!(row > last);
                            }
                            last_row_number = Some(row)
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
                        let header = complete(exdf_header)(&file).unwrap().1;
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
}
