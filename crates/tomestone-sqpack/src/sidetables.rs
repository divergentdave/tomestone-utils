//! These side table data structures are used to record certain information about the internal
//! layout of SQPack files, so that I can reuse it in round-trip construction of similar files.
//! The eventual intent is to replace these side tables with better heuristics or exact
//! calculations, once I have a better understanding of the file format.

use std::{
    collections::BTreeMap,
    fs::File,
    io::{Read, Seek, SeekFrom},
};

use nom::{
    combinator::map_opt,
    number::streaming::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};

use crate::{
    parser::{drive_streaming_parser_smaller, type_2_block_table, DataContentType},
    DataFileSet, FilePointer, GameData, IndexHash2, SqPackId,
};

#[derive(Clone)]
pub struct SideTableEntry {
    pub unknown_entry_field: u32,
    pub padded_entry_size: u32,
    pub content_type: DataContentType,
    pub block_compression: Vec<bool>,
}

fn data_entry_header_raw(
    input: &[u8],
) -> IResult<&[u8], (u32, DataContentType, u32, u32, u32, u16, u16)> {
    // header length, type, uncompressed size, unknown, block buffer size, number of blocks, unknown
    tuple((
        le_u32,
        map_opt(le_u32, DataContentType::parse),
        le_u32,
        le_u32,
        le_u32,
        le_u16,
        le_u16,
    ))(input)
}

fn count_zeros(file: &mut File) -> Result<usize, std::io::Error> {
    let mut buf = [0; 4096];
    let mut count = 0;
    loop {
        let read_count = file.read(&mut buf)?;
        if read_count == 0 {
            break;
        }
        if let Some(nonzero_pos) = buf[..read_count].iter().position(|byte| *byte != 0) {
            return Ok(count + nonzero_pos);
        }
        count += read_count;
    }
    Ok(count)
}

#[allow(unused)]
pub fn build_side_tables(
    game_data: &GameData,
    data_file_set: &mut DataFileSet,
    pack_id: SqPackId,
) -> BTreeMap<IndexHash2, SideTableEntry> {
    let mut reversed_index: BTreeMap<FilePointer, IndexHash2> = BTreeMap::new();
    let index_2 = game_data.get_index_2(&pack_id).unwrap().unwrap();
    for (hash, pointer) in index_2.iter() {
        assert!(reversed_index.insert(pointer, hash).is_none());
    }

    let mut table = BTreeMap::new();
    for (locator, hash) in reversed_index.iter() {
        let mut file = data_file_set.open(pack_id, locator.data_file_id()).unwrap();
        file.seek(SeekFrom::Start(locator.offset().into())).unwrap();
        let entry_header_fields =
            drive_streaming_parser_smaller(&mut file, data_entry_header_raw).unwrap();
        if let DataContentType::Binary = entry_header_fields.1 {
            let mut block_table_buf = vec![0; entry_header_fields.5 as usize * 8];
            file.read_exact(&mut block_table_buf).unwrap();
            let block_table = type_2_block_table(entry_header_fields.5)(&block_table_buf)
                .unwrap()
                .1;
            let block_compression = block_table
                .iter()
                .map(|(block_offset, _, _)| {
                    file.seek(SeekFrom::Start(
                        Into::<u64>::into(locator.offset())
                            + Into::<u64>::into(entry_header_fields.0)
                            + Into::<u64>::into(*block_offset),
                    ))
                    .unwrap();
                    let mut block_header_buf = [0; 16];
                    file.read_exact(&mut block_header_buf).unwrap();
                    let compressed_length =
                        u32::from_le_bytes(block_header_buf[8..12].try_into().unwrap());
                    compressed_length != 32000
                })
                .collect();

            // find the first non-null byte after the last block, and round to 128
            let last_block_offset = block_table.last().unwrap().0;
            file.seek(SeekFrom::Start(
                Into::<u64>::into(locator.offset())
                    + Into::<u64>::into(entry_header_fields.0)
                    + Into::<u64>::into(last_block_offset),
            ))
            .unwrap();
            let mut block_header_buf = [0; 16];
            file.read_exact(&mut block_header_buf).unwrap();
            let block_header_length =
                u32::from_le_bytes(block_header_buf[0..4].try_into().unwrap());
            let compressed_length = u32::from_le_bytes(block_header_buf[8..12].try_into().unwrap());
            let decompressed_length =
                u32::from_le_bytes(block_header_buf[12..16].try_into().unwrap());
            let last_block_end = if compressed_length == 32000 {
                locator.offset()
                    + entry_header_fields.0
                    + last_block_offset
                    + block_header_length
                    + decompressed_length
            } else {
                locator.offset()
                    + entry_header_fields.0
                    + last_block_offset
                    + block_header_length
                    + compressed_length
            };
            file.seek(SeekFrom::Start(last_block_end.into())).unwrap();
            let zeros = count_zeros(&mut file).unwrap();
            let padded_entry_size = (last_block_end - locator.offset()
                + TryInto::<u32>::try_into(zeros).unwrap())
                / 128
                * 128;

            let entry = SideTableEntry {
                unknown_entry_field: entry_header_fields.3,
                padded_entry_size,
                content_type: entry_header_fields.1,
                block_compression,
            };
            table.insert(*hash, entry);
        }
    }

    table
}
