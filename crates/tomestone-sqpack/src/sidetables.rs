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
    combinator::{map, map_opt},
    number::streaming::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};

use crate::{
    parser::{drive_streaming_parser_smaller, type_2_block_table, DataContentType},
    DataFileSet, FilePointer, GameData, IndexHash2, SqPackId,
};

/// This structure provides extra information, beyond the list of compressed files in each SqPack
/// set, that is needed to perform a byte-exact round-trip of index and data files. Certain
/// choices, such as whether compression is used, the positions of entries in the data files, etc.
/// are captured in this data structure, to enable round-trip reference tests, and in turn learn
/// more about the file format. The data stored here applies to one pack identifier, i.e., one
/// combination of pack type, expansion, and pack number. Thus, the information will be applicable
/// to one `.index` file, one `.index2` file, and one `.dat0` file, along with any other additional
/// data files.
#[derive(Default)]
pub struct SideTables {
    /// Extra information about individual data file entries.
    pub file_entries: BTreeMap<IndexHash2, FileEntryAux>,
    /// When encoding a SqPack data file using side tables, the first part of each file is reserved
    /// for entries to be re-encoded in their original positions. If any files to be encoded do not
    /// appear in the list of existing positions, or the space they were supposed to occupy has
    /// been taken up by a longer entry, then those entries must be stored at an offset greater
    /// `reserved_file_space`, in the second part of the file, where they can be stored
    /// sequentially, without impacting any other entries' original positions.
    pub reserved_file_space: Vec<u32>,
    /// Modification datetimes from .dat* files. The packed date fields and packed time fields are
    /// stored here, if they aren't all null bytes. (Index files appear to have all null bytes in
    /// these fields.)
    pub sqpack_data_datetimes: BTreeMap<u8, (u32, u32)>,
}

#[derive(Clone)]
pub struct FileEntryAux {
    pub unknown_entry_field: u32,
    pub content_type: DataContentType,
    pub block_compression: Vec<bool>,
    pub entry_pointer: FilePointer,
}

struct DataEntryHeaderRaw {
    header_length: u32,
    data_content_type: DataContentType,
    _uncompressed_size: u32,
    unknown_1: u32,
    _block_buffer_size: u32,
    number_of_blocks: u16,
    _unknown_2: u16,
}

fn data_entry_header_raw(input: &[u8]) -> IResult<&[u8], DataEntryHeaderRaw> {
    map(
        tuple((
            le_u32,
            map_opt(le_u32, DataContentType::parse),
            le_u32,
            le_u32,
            le_u32,
            le_u16,
            le_u16,
        )),
        |(
            header_length,
            data_content_type,
            _uncompressed_size,
            unknown_1,
            _block_buffer_size,
            number_of_blocks,
            _unknown_2,
        )| DataEntryHeaderRaw {
            header_length,
            data_content_type,
            _uncompressed_size,
            unknown_1,
            _block_buffer_size,
            number_of_blocks,
            _unknown_2,
        },
    )(input)
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
) -> SideTables {
    let mut reversed_index: BTreeMap<FilePointer, IndexHash2> = BTreeMap::new();
    let index_2 = game_data.get_index_2(&pack_id).unwrap().unwrap();
    for (hash, pointer) in index_2.iter() {
        assert!(reversed_index.insert(pointer, hash).is_none());
    }

    let mut file_entries = BTreeMap::new();
    for (locator, hash) in reversed_index.iter() {
        let mut file = data_file_set.open(pack_id, locator.data_file_id()).unwrap();
        file.seek(SeekFrom::Start(locator.offset().into())).unwrap();
        let entry_header_fields =
            drive_streaming_parser_smaller(&mut file, data_entry_header_raw).unwrap();
        if let DataContentType::Binary = entry_header_fields.data_content_type {
            let mut block_table_buf = vec![0; entry_header_fields.number_of_blocks as usize * 8];
            file.read_exact(&mut block_table_buf).unwrap();
            let block_table =
                type_2_block_table(entry_header_fields.number_of_blocks)(&block_table_buf)
                    .unwrap()
                    .1;
            let block_compression = block_table
                .iter()
                .map(|(block_offset, _, _)| {
                    file.seek(SeekFrom::Start(
                        Into::<u64>::into(locator.offset())
                            + Into::<u64>::into(entry_header_fields.header_length)
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
                    + Into::<u64>::into(entry_header_fields.header_length)
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
                    + entry_header_fields.header_length
                    + last_block_offset
                    + block_header_length
                    + decompressed_length
            } else {
                locator.offset()
                    + entry_header_fields.header_length
                    + last_block_offset
                    + block_header_length
                    + compressed_length
            };
            file.seek(SeekFrom::Start(last_block_end.into())).unwrap();
            let zeros = count_zeros(file).unwrap();
            let padded_entry_size = (last_block_end - locator.offset()
                + TryInto::<u32>::try_into(zeros).unwrap())
                / 128
                * 128;

            let entry = FileEntryAux {
                unknown_entry_field: entry_header_fields.unknown_1,
                content_type: entry_header_fields.data_content_type,
                block_compression,
                entry_pointer: *locator,
            };
            file_entries.insert(*hash, entry);
        }
    }

    let mut sqpack_data_datetimes = BTreeMap::new();
    let max_dat_number = data_file_set.max_dat_number(pack_id);
    let mut reserved_file_space = vec![0; (max_dat_number + 1).into()];
    for dat_file_number in 0..=max_dat_number {
        let file = data_file_set.open(pack_id, dat_file_number).unwrap();

        let file_len = file.metadata().unwrap().len().try_into().unwrap();
        reserved_file_space[usize::from(dat_file_number)] = file_len;

        file.seek(SeekFrom::Start(24)).unwrap();
        let mut buf = [0u8; 8];
        file.read_exact(&mut buf).unwrap();
        let packed_date = u32::from_le_bytes(buf[..4].try_into().unwrap());
        let packed_time = u32::from_le_bytes(buf[4..].try_into().unwrap());
        if packed_date != 0 || packed_time != 0 {
            let year = packed_date / 10000;
            assert!(year > 1900);
            let month = (packed_date / 100) % 100;
            assert!((1..=12).contains(&month));
            let date = packed_date % 100;
            assert!((1..=31).contains(&date));

            let hour = packed_time / 1000000;
            assert!(hour <= 24);
            let minute = (packed_time / 10000) % 100;
            assert!(minute <= 59);
            let second = (packed_time / 100) % 100;
            assert!(second <= 59);

            sqpack_data_datetimes.insert(dat_file_number, (packed_date, packed_time));
        }
    }

    SideTables {
        file_entries,
        sqpack_data_datetimes,
        reserved_file_space,
    }
}
