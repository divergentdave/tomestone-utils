use std::convert::TryInto;

use crate::{parser::exhf::Exhf, Cardinality, Value};

// TODO, future work: write new code to pre-compute file sizes, and then encode in-place with one allocation.

pub fn encode_row(row: &Vec<(u16, Vec<Value<'_>>)>, header: &Exhf, padding_offset: u32) -> Vec<u8> {
    let row_size: usize = header.row_size().into();
    let inner_length_fixed: usize = match header.cardinality() {
        Cardinality::Single => row_size * row.len(),
        Cardinality::Multiple => (row_size + 2) * row.len(),
    };
    let mut inner_length_variable: usize = 0;
    for (_sub_row_index, sub_row) in row.iter() {
        for value in sub_row.iter() {
            if let Value::String(data) = value {
                inner_length_variable += data.len() + 1;
            }
        }
    }
    let inner_length_unpadded: u32 = (inner_length_fixed + inner_length_variable)
        .try_into()
        .unwrap();
    let inner_length = match header.cardinality() {
        Cardinality::Single => {
            ((inner_length_unpadded + padding_offset + 2) + 3) / 4 * 4 - padding_offset - 2
        }
        Cardinality::Multiple => ((inner_length_unpadded + 3) / 4 * 4),
    };
    let outer_length = TryInto::<usize>::try_into(inner_length).unwrap() + 6;

    let mut data = vec![0; outer_length];
    data[..4].copy_from_slice(&inner_length.to_be_bytes());
    data[4..6].copy_from_slice(&TryInto::<u16>::try_into(row.len()).unwrap().to_be_bytes());
    let row_size = TryInto::<usize>::try_into(header.row_size()).unwrap();
    let mut fixed_data_offset = 6;
    let mut string_data_offset_relative: u32 = 0;
    let mut string_data_offset_vec = 6 + row_size * row.len();
    for (sub_row_index, sub_row) in row.iter() {
        if let Cardinality::Multiple = header.cardinality() {
            data[fixed_data_offset..fixed_data_offset + 2]
                .copy_from_slice(&sub_row_index.to_be_bytes());
            fixed_data_offset += 2;
        }
        for column_def in header.columns_offset_order() {
            let off = fixed_data_offset + column_def.offset;
            let value = &sub_row[column_def.index];
            match (value, column_def.format) {
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

pub fn encode_exdf_page(
    name: &str,
    header: &Exhf,
    rows: &[(u32, Vec<(u16, Vec<Value<'_>>)>)],
) -> Vec<u8> {
    const HEADER_LENGTH: u32 = 32;

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

    let offsets_len: u32 = TryInto::<u32>::try_into(rows.len()).unwrap() * 8;

    let mut offsets_section = Vec::with_capacity(offsets_len.try_into().unwrap());
    let mut data_section = Vec::new();
    for (number, row) in rows {
        let mut encoded_row = encode_row(row, header, padding_offset);
        let row_offset: u32 = data_section.len().try_into().unwrap();
        data_section.append(&mut encoded_row);
        offsets_section.extend_from_slice(&number.to_be_bytes());
        offsets_section
            .extend_from_slice(&(HEADER_LENGTH + offsets_len + row_offset).to_be_bytes());
    }

    let data_len: u32 = data_section.len().try_into().unwrap();
    let mut data = vec![0; HEADER_LENGTH as usize];
    data[..6].copy_from_slice(b"EXDF\x00\x02");
    data[8..12].copy_from_slice(&offsets_len.to_be_bytes());
    data[12..16].copy_from_slice(&data_len.to_be_bytes());
    data.append(&mut offsets_section);
    data.append(&mut data_section);

    data
}

pub fn encode_exhf(header: &Exhf) -> Vec<u8> {
    const HEADER_LENGTH: u32 = 32;
    let mut data = Vec::with_capacity(
        HEADER_LENGTH as usize
            + header.columns_table_order().len() * 4
            + header.pages().len() * 8
            + header.languages().len() * 2,
    );
    data.resize(HEADER_LENGTH as usize, 0);
    data[..6].copy_from_slice(b"EXHF\x00\x03");
    data[6..8].copy_from_slice(&header.row_size().to_be_bytes());
    data[8..10].copy_from_slice(
        &TryInto::<u16>::try_into(header.columns_table_order().len())
            .unwrap()
            .to_be_bytes(),
    );
    data[10..12].copy_from_slice(
        &TryInto::<u16>::try_into(header.pages().len())
            .unwrap()
            .to_be_bytes(),
    );
    data[12..14].copy_from_slice(
        &TryInto::<u16>::try_into(header.languages().len())
            .unwrap()
            .to_be_bytes(),
    );
    data[14..16].copy_from_slice(
        &(((header.unknown_flag() as u16) << 14) | header.unknown_number()).to_be_bytes(),
    );
    data[17] = match header.cardinality() {
        Cardinality::Single => 1,
        Cardinality::Multiple => 2,
    };
    data[20..24].copy_from_slice(&header.total_sub_rows().to_be_bytes());
    for column in header.columns_table_order() {
        data.extend_from_slice(&column.format.to_u16().to_be_bytes());
        data.extend_from_slice(
            &TryInto::<u16>::try_into(column.offset)
                .unwrap()
                .to_be_bytes(),
        );
    }
    for (page_start, page_size) in header.pages() {
        data.extend_from_slice(&page_start.to_be_bytes());
        data.extend_from_slice(&page_size.to_be_bytes());
    }
    for opt in header.languages() {
        let value = match opt {
            Some(language) => *language as u16,
            None => 0,
        };
        data.extend_from_slice(&value.to_le_bytes());
    }
    data
}

#[cfg(test)]
mod tests {
    use tomestone_sqpack::GameData;

    use crate::{parser::exhf::parse_exhf, RootList};

    #[test]
    #[ignore = "slow test"]
    fn exh_round_trip() {
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
            let exh_path = format!("exd/{}.exh", name);
            let exh_data = game_data.lookup_path_data(&exh_path).unwrap().unwrap();
            let exhf = parse_exhf(&exh_data).unwrap().1;
            let encoded = crate::encoding::encode_exhf(&exhf);
            assert_eq!(exh_data, encoded);
        }
    }
}
