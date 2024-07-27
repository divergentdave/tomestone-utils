//! These side table data structures are used to record certain information about the internal
//! layout of SQPack files, so that I can reuse it in round-trip construction of similar files.
//! The eventual intent is to replace these side tables with better heuristics or exact
//! calculations, once I have a better understanding of the file format.

use std::{
    collections::{BTreeMap, BTreeSet},
    fs::File,
    io::{Read, Seek, SeekFrom},
};

use nom::{
    combinator::{map, map_opt},
    number::streaming::{le_u16, le_u32},
    sequence::tuple,
    Finish, IResult,
};

use crate::{
    parser::{
        drive_streaming_parser, drive_streaming_parser_smaller, index_segment_headers,
        sqpack_header_outer, type_2_block_table, DataContentType, GrowableBufReader,
    },
    DataFileSet, Error, FilePointer, GameData, IndexEntry1, IndexEntry2, IndexHash2, SqPackId,
    ZeroEntry,
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
    /// Locations and (shifted) lengths of entries with type 0.
    pub zero_entries: BTreeSet<ZeroEntry>,
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
    /// When an index segment is empty, indicates whether the segment's offset should point to
    /// where the segment would appear (true), or if its offset should be encoded as zero (false).
    /// This has no effect if the index segment is not empty.
    ///
    /// The working hypothesis is that these offset fields are path-dependent across multiple
    /// updates to the files, so if a segment is non-empty in one revision, then the offset will
    /// be filled in to point to the segment, and if the segment is later emptied out, then the
    /// offset field won't be set back to zero.
    pub index_empty_segment_offset_present: [bool; 4],
    /// This functions the same as `index_empty_segment_offset_present`, but applies to `.index2`
    /// files.
    pub index2_empty_segment_offset_present: [bool; 4],
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

/// This function assumes the file handle it is passed has its cursor at the beginning of a data
/// entry. It will read enough of the entry to determine its length, and seek to the end of it,
/// rounded up to the next 128-byte boundary.
fn skip_entry(
    file: &mut File,
    entry_offset: u64,
    entry_header_fields: &DataEntryHeaderRaw,
) -> Result<(), Error> {
    match entry_header_fields.data_content_type {
        DataContentType::Empty => todo!(),
        DataContentType::Binary => {
            // Read the last block table entry.
            file.seek(SeekFrom::Current(
                (entry_header_fields.number_of_blocks as i64 - 1) * 8,
            ))?;
            let mut block_table_entry_buf = vec![0; 8];
            file.read_exact(&mut block_table_entry_buf)?;
            let block_entry = type_2_block_table(1)(&block_table_entry_buf)
                .finish()
                .map_err(|e| e.code)?
                .1[0];
            let last_block_offset = block_entry.0;

            // Read the last block's header.
            file.seek(SeekFrom::Start(
                entry_offset
                    + u64::from(entry_header_fields.header_length)
                    + u64::from(last_block_offset),
            ))?;
            let mut block_header_buf = [0; 16];
            file.read_exact(&mut block_header_buf)?;
            let block_header_length =
                u32::from_le_bytes(block_header_buf[0..4].try_into().unwrap());
            let compressed_length = u32::from_le_bytes(block_header_buf[8..12].try_into().unwrap());
            let decompressed_length =
                u32::from_le_bytes(block_header_buf[12..16].try_into().unwrap());

            let last_block_end = if compressed_length == 32000 {
                entry_offset
                    + u64::from(entry_header_fields.header_length)
                    + u64::from(last_block_offset)
                    + u64::from(block_header_length)
                    + u64::from(decompressed_length)
            } else {
                entry_offset
                    + u64::from(entry_header_fields.header_length)
                    + u64::from(last_block_offset)
                    + u64::from(block_header_length)
                    + u64::from(compressed_length)
            };
            let final_position = (last_block_end + 127) / 128 * 128;
            file.seek(SeekFrom::Start(final_position))?;
        }
        DataContentType::Model => todo!(),
        DataContentType::Texture => todo!(),
        DataContentType::Unsupported => {
            file.seek(SeekFrom::Start(
                entry_offset + u64::from(std::cmp::max(entry_header_fields.unknown_1, 1)) * 128,
            ))?;
        }
    }
    Ok(())
}

#[allow(unused, clippy::needless_borrows_for_generic_args)]
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
    let mut file_pointers = BTreeSet::new();
    for (locator, hash) in reversed_index.iter() {
        file_pointers.insert(*locator);
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

            let entry = FileEntryAux {
                unknown_entry_field: entry_header_fields.unknown_1,
                content_type: entry_header_fields.data_content_type,
                block_compression,
                entry_pointer: *locator,
            };
            file_entries.insert(*hash, entry);
        }
    }

    let index_1 = game_data.get_index_1(&pack_id).unwrap().unwrap();
    let zero_entries = index_1
        .tombstone_table
        .iter()
        .cloned()
        .collect::<BTreeSet<_>>();
    file_pointers.append(&mut zero_entries.iter().map(|e| e.pointer).collect());

    let mut sqpack_data_datetimes = BTreeMap::new();
    let max_dat_number = data_file_set.max_dat_number(pack_id);
    let mut reserved_file_space = vec![0; (max_dat_number + 1).into()];
    for dat_file_number in 0..=max_dat_number {
        let mut file = data_file_set.open(pack_id, dat_file_number).unwrap();

        // Save the original lengths of each data file.
        let file_len = file.metadata().unwrap().len().try_into().unwrap();
        reserved_file_space[usize::from(dat_file_number)] = file_len;

        // Save the date and time fields from each data header.
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

        // Linear scan to read all entry headers, store the tombstones (type zero) and their lengths.
        const SQPACK_HEADER_LENGTH: u32 = 1024;
        file.seek(SeekFrom::Start(SQPACK_HEADER_LENGTH.into()))
            .unwrap();
        let mut data_header_length_buf = [0u8; 4];
        file.read_exact(&mut data_header_length_buf).unwrap();
        let data_header_length = u32::from_le_bytes(data_header_length_buf);
        let entry_offset = SQPACK_HEADER_LENGTH + data_header_length;
        file.seek(SeekFrom::Start(u64::from(entry_offset))).unwrap();

        let mut last_entry_offset = None;
        'outer: loop {
            let entry_offset = file.stream_position().unwrap();

            let pointer = FilePointer::new(dat_file_number, entry_offset.try_into().unwrap());
            if !file_pointers.contains(&pointer) {
                panic!(
                    "entry scan is desynced from index, at 0x{:02x}",
                    entry_offset
                );
            }
            if let Some(last_entry_offset) = last_entry_offset {
                if entry_offset <= last_entry_offset {
                    panic!("not making progress scanning through entries");
                }
            }
            last_entry_offset = Some(entry_offset);

            let entry_header_fields =
                drive_streaming_parser_smaller(&mut file, data_entry_header_raw).unwrap();

            skip_entry(file, entry_offset, &entry_header_fields).unwrap();

            // Scan through null padding until there's a new entry header (aligned to 128).
            let mut chunk_buf = [0; 128];
            loop {
                let res = file.read_exact(&mut chunk_buf);
                if let Err(e) = &res {
                    if e.kind() == std::io::ErrorKind::UnexpectedEof {
                        break 'outer;
                    }
                }
                res.unwrap();
                if chunk_buf[0..4] != [0, 0, 0, 0] {
                    file.seek(SeekFrom::Current(-128)).unwrap();
                    break;
                }
            }
        }
    }

    // Check if index segment offset fields are nonzero.
    let file = File::open(game_data.build_index_path::<IndexEntry1>(pack_id)).unwrap();
    let mut bufreader = GrowableBufReader::new(file);
    let file_header = drive_streaming_parser(&mut bufreader, sqpack_header_outer).unwrap();
    let index_header = drive_streaming_parser(&mut bufreader, index_segment_headers).unwrap();
    let segment_headers = index_header.2;
    let index_empty_segment_offset_present = [
        segment_headers[0].offset != 0,
        segment_headers[1].offset != 0,
        segment_headers[2].offset != 0,
        segment_headers[3].offset != 0,
    ];

    let file = File::open(game_data.build_index_path::<IndexEntry2>(pack_id)).unwrap();
    let mut bufreader = GrowableBufReader::new(file);
    let file_header = drive_streaming_parser(&mut bufreader, sqpack_header_outer).unwrap();
    let index_header = drive_streaming_parser(&mut bufreader, index_segment_headers).unwrap();
    let segment_headers = index_header.2;
    let index2_empty_segment_offset_present = [
        segment_headers[0].offset != 0,
        segment_headers[1].offset != 0,
        segment_headers[2].offset != 0,
        segment_headers[3].offset != 0,
    ];

    SideTables {
        file_entries,
        zero_entries,
        sqpack_data_datetimes,
        reserved_file_space,
        index_empty_segment_offset_present,
        index2_empty_segment_offset_present,
    }
}
