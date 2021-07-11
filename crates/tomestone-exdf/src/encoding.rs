use std::convert::TryInto;

use crate::{parser::exhf::Exhf, Value};

// TODO, future work: write new code to pre-compute file sizes, and then encode in-place with one allocation.

pub fn encode_row(row: &Vec<Vec<Value<'_>>>, header: &Exhf) -> Vec<u8> {
    let mut inner_length: usize = TryInto::<usize>::try_into(header.row_size()).unwrap();
    for sub_row in row.iter() {
        for value in sub_row.iter() {
            if let Value::String(data) = value {
                inner_length += data.len() + 1;
            }
        }
    }
    let outer_length = ((inner_length + 6) + 3) / 4 * 4;
    let inner_length: u32 = inner_length.try_into().unwrap();

    let mut data = vec![0; outer_length];
    data[..4].copy_from_slice(&inner_length.to_be_bytes());
    data[4..6].copy_from_slice(&TryInto::<u16>::try_into(row.len()).unwrap().to_be_bytes());
    let row_size = TryInto::<usize>::try_into(header.row_size()).unwrap();
    let mut fixed_data_offset = 6;
    let mut string_data_offset_relative: u32 = 0;
    let mut string_data_offset_vec = 6 + row_size * row.len();
    for sub_row in row.iter() {
        for (value, (format, column_offset)) in sub_row.iter().zip(header.column_definitions()) {
            let column_offset: usize = (*column_offset).try_into().unwrap();
            let off = fixed_data_offset + column_offset;
            match (value, format) {
                (Value::String(val), crate::ColumnFormat::String) => {
                    data[off..off + 4].copy_from_slice(
                        &TryInto::<u32>::try_into(string_data_offset_relative)
                            .unwrap()
                            .to_be_bytes(),
                    );
                    data[string_data_offset_vec..string_data_offset_vec + val.len()]
                        .copy_from_slice(val);
                    data[string_data_offset_vec + val.len()] = 0;
                    string_data_offset_relative += TryInto::<u32>::try_into(val.len()).unwrap() + 1;
                    string_data_offset_vec += val.len() + 1;
                }
                (Value::Bool(val), crate::ColumnFormat::Bool) => data[off] = *val as u8,
                (Value::I8(val), crate::ColumnFormat::I8) => data[off] = *val as u8,
                (Value::U8(val), crate::ColumnFormat::U8) => data[off] = *val,
                (Value::I16(val), crate::ColumnFormat::I16) => {
                    data[off..off + 2].copy_from_slice(&val.to_be_bytes())
                }
                (Value::U16(val), crate::ColumnFormat::U16) => {
                    data[off..off + 2].copy_from_slice(&val.to_be_bytes())
                }
                (Value::I32(val), crate::ColumnFormat::I32) => {
                    data[off..off + 4].copy_from_slice(&val.to_be_bytes())
                }
                (Value::U32(val), crate::ColumnFormat::U32) => {
                    data[off..off + 4].copy_from_slice(&val.to_be_bytes())
                }
                (Value::Float(val), crate::ColumnFormat::Float) => {
                    data[off..off + 4].copy_from_slice(&val.to_be_bytes())
                }
                (Value::I16x4(val), crate::ColumnFormat::I16x4) => {
                    data[off..off + 2].copy_from_slice(&val[0].to_be_bytes());
                    data[off + 2..off + 4].copy_from_slice(&val[1].to_be_bytes());
                    data[off + 4..off + 6].copy_from_slice(&val[2].to_be_bytes());
                    data[off + 6..off + 8].copy_from_slice(&val[3].to_be_bytes());
                }
                (Value::Bitflag(val), crate::ColumnFormat::Bitflag(bit_position)) => {
                    if *val {
                        data[off] |= 1 << bit_position;
                    }
                }
                _ => panic!("Mismatched column type"),
            }
        }
        fixed_data_offset += row_size;
    }

    data
}
