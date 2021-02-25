use std::convert::TryInto;

use nom::{
    combinator::map,
    number::complete::{be_f32, be_i16, be_i32, be_i8, be_u16, be_u32, be_u8},
    sequence::tuple,
};

use crate::{ColumnFormat, Value};

use self::exhf::Exhf;

pub mod exdf;
pub mod exhf;

pub fn parse_row<'a>(
    data: &'a [u8],
    exhf: &Exhf,
) -> Result<Vec<Value<'a>>, nom::Err<nom::error::Error<&'a [u8]>>> {
    exhf.column_definitions()
        .iter()
        .map(
            |(format, offset)| -> Result<Value<'a>, nom::Err<nom::error::Error<&'a [u8]>>> {
                let offset: usize = (*offset).try_into().unwrap();
                let input = &data[offset..];
                Ok(match format {
                    ColumnFormat::String => {
                        let value: usize = be_u32(input)?.1.try_into().unwrap();
                        let row_end: usize = exhf.row_size().try_into().unwrap();
                        let start_offset = row_end + value;
                        let end_offset = if let Some(null_byte) =
                            data[start_offset..].iter().position(|byte| *byte == 0)
                        {
                            start_offset + null_byte
                        } else {
                            data.len()
                        };
                        Value::String(&data[start_offset..end_offset])
                    }
                    ColumnFormat::Bool => Value::Bool(map(be_u8, |value| value != 0)(input)?.1),
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
                    ColumnFormat::Bitflag(bit) => Value::Bitflag((be_u8(input)?.1 >> bit) & 1 != 0),
                })
            },
        )
        .collect()
}
