use std::convert::TryInto;

use nom::{
    combinator::map,
    number::complete::{be_f32, be_i16, be_i32, be_i8, be_u16, be_u32, be_u8},
    sequence::tuple,
};

use crate::{Cardinality, ColumnFormat, RawDataRow, Value};

use self::exhf::Exhf;

pub mod exdf;
pub mod exhf;

pub fn parse_row<'a>(
    row_data: RawDataRow<'a>,
    exhf: &Exhf,
) -> Result<Vec<(u16, Vec<Value<'a>>)>, nom::error::ErrorKind> {
    let row_size: usize = exhf.row_size().try_into().unwrap();
    let (is_multiple, wrapped_sub_row_length, values_offset) = match exhf.cardinality() {
        Cardinality::Single => {
            if row_data.sub_row_count != 1 {
                return Err(nom::error::ErrorKind::Verify);
            }
            (false, row_size, 0)
        }
        Cardinality::Multiple => (true, row_size + 2, 2),
    };
    (0..row_data.sub_row_count)
        .map(|sub_row_counter| {
            let sub_row_start_precursor =
                TryInto::<usize>::try_into(sub_row_counter).unwrap() * wrapped_sub_row_length;
            let sub_row_index = if is_multiple {
                be_u16(&row_data.data[sub_row_start_precursor..])
                    .map_err(|_: nom::Err<nom::error::Error<&'a [u8]>>| nom::error::ErrorKind::Eof)?
                    .1
            } else {
                0
            };
            let sub_row_start = sub_row_start_precursor + values_offset;
            let sub_row = exhf
                .columns_table_order()
                .iter()
                .map(
                    |column_def| -> Result<Value<'a>, nom::Err<nom::error::Error<&'a [u8]>>> {
                        let offset = sub_row_start + column_def.offset;
                        let input = &row_data.data[offset..];
                        Ok(match column_def.format {
                            ColumnFormat::String => {
                                let value: usize = be_u32(input)?.1.try_into().unwrap();
                                let row_end = sub_row_start + row_size;
                                let start_offset = row_end + value;
                                let end_offset = if let Some(null_byte) = row_data.data
                                    [start_offset..]
                                    .iter()
                                    .position(|byte| *byte == 0)
                                {
                                    start_offset + null_byte
                                } else {
                                    row_data.data.len()
                                };
                                Value::String(&row_data.data[start_offset..end_offset])
                            }
                            ColumnFormat::Bool => {
                                Value::Bool(map(be_u8, |value| value != 0)(input)?.1)
                            }
                            ColumnFormat::I8 => Value::I8(be_i8(input)?.1),
                            ColumnFormat::U8 => Value::U8(be_u8(input)?.1),
                            ColumnFormat::I16 => Value::I16(be_i16(input)?.1),
                            ColumnFormat::U16 => Value::U16(be_u16(input)?.1),
                            ColumnFormat::I32 => Value::I32(be_i32(input)?.1),
                            ColumnFormat::U32 => Value::U32(be_u32(input)?.1),
                            ColumnFormat::Float => Value::Float(be_f32(input)?.1),
                            ColumnFormat::I16x4 => Value::I16x4(
                                map(tuple((be_i16, be_i16, be_i16, be_i16)), |(a, b, c, d)| {
                                    [a, b, c, d]
                                })(input)?
                                .1,
                            ),
                            ColumnFormat::Bitflag(bit) => {
                                Value::Bitflag((be_u8(input)?.1 >> bit) & 1 != 0)
                            }
                        })
                    },
                )
                .collect::<Result<Vec<Value<'a>>, nom::Err<nom::error::Error<&'a [u8]>>>>()
                .map_err(|e| match e {
                    nom::Err::Incomplete(_) => unreachable!(),
                    nom::Err::Error(e) | nom::Err::Failure(e) => e.code,
                })?;
            Ok((sub_row_index, sub_row))
        })
        .collect::<Result<Vec<(u16, Vec<Value<'a>>)>, nom::error::ErrorKind>>()
}

#[cfg(test)]
mod tests {
    use crate::{Language, RootList};
    use tomestone_sqpack::GameData;

    #[test]
    #[ignore = "slow test"]
    fn exdf_game_data() {
        dotenv::dotenv().ok();
        // Don't test anything if the game directory isn't probided
        let root = if let Ok(root) = std::env::var("FFXIV_INSTALL_DIR") {
            root
        } else {
            return;
        };
        let game_data = GameData::new(root).unwrap();

        let root_list = RootList::open(&game_data).unwrap();
        for name in root_list.iter() {
            let exh_path = format!("exd/{}.exh", name);
            let exh_data = game_data.lookup_path_data(&exh_path).unwrap().unwrap();
            let (_, exhf) = super::exhf::parse_exhf(&exh_data).unwrap();
            for language in exhf.languages() {
                let short_code = language.as_ref().map(Language::short_code);
                for (page_start, _other) in exhf.pages() {
                    let exd_path = if let Some(short_code) = short_code {
                        format!("exd/{}_{}_{}.exd", name, page_start, short_code)
                    } else {
                        format!("exd/{}_{}.exd", name, page_start)
                    };
                    if let Some(exd_data) = game_data.lookup_path_data(&exd_path).unwrap() {
                        let exdf = super::exdf::Exdf::new(exd_data).unwrap();
                        for row_res in exdf.iter() {
                            let (_, row_data) = row_res.unwrap();
                            super::parse_row(row_data, &exhf).unwrap();
                        }
                    }
                }
            }
        }
    }
}
