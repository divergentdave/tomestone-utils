use std::{
    collections::{BTreeMap, BTreeSet},
    fs::File,
    io::{self, Read, Seek, SeekFrom, Write},
    ops::Range,
    path::PathBuf,
};

use sha1::{Digest, Sha1};

#[allow(unused)]
use crate::compression::compress_sqpack_block;
use crate::sidetables::SideTables;
use crate::{
    compression, Category, Expansion, FilePointer, IndexHash, IndexHash1, IndexHash2, IndexPointer,
    PlatformId, SqPackId, SqPackType,
};

static ZEROS: [u8; 4096] = [0; 4096];

pub trait SetLen {
    fn set_len(&self, size: u64) -> Result<(), io::Error>;
}

impl SetLen for File {
    fn set_len(&self, size: u64) -> Result<(), io::Error> {
        self.set_len(size)
    }
}

pub trait PackIO {
    type F: Read + Write + Seek + SetLen;

    fn open_index_file(&mut self) -> Result<Self::F, io::Error>;
    fn open_index2_file(&mut self) -> Result<Self::F, io::Error>;
    fn open_dat_file(&mut self, number: u8) -> Result<Self::F, io::Error>;
}

pub struct RealPackIO {
    base: PathBuf,
    platform_id: PlatformId,
    pack_id: SqPackId,
}

impl RealPackIO {
    pub fn new(
        base: PathBuf,
        platform_id: PlatformId,
        pack_id: SqPackId,
    ) -> Result<RealPackIO, io::Error> {
        Ok(RealPackIO {
            base,
            platform_id,
            pack_id,
        })
    }
}

impl PackIO for RealPackIO {
    type F = File;

    fn open_index_file(&mut self) -> Result<File, io::Error> {
        assert_eq!(self.platform_id, PlatformId::Win32);
        File::create(self.base.join(self.pack_id.expansion.name()).join(format!(
            "{:02x}{:02x}{:02x}.win32.index",
            self.pack_id.category as u8, self.pack_id.expansion as u8, self.pack_id.number
        )))
    }

    fn open_index2_file(&mut self) -> Result<File, io::Error> {
        assert_eq!(self.platform_id, PlatformId::Win32);
        File::create(self.base.join(self.pack_id.expansion.name()).join(format!(
            "{:02x}{:02x}{:02x}.win32.index2",
            self.pack_id.category as u8, self.pack_id.expansion as u8, self.pack_id.number
        )))
    }

    fn open_dat_file(&mut self, number: u8) -> Result<File, io::Error> {
        assert_eq!(self.platform_id, PlatformId::Win32);
        File::create(self.base.join(self.pack_id.expansion.name()).join(format!(
            "{:02x}{:02x}{:02x}.win32.dat{}",
            self.pack_id.category as u8, self.pack_id.expansion as u8, self.pack_id.number, number
        )))
    }
}

struct SqPackHeader([u8; 1024]);

impl SqPackHeader {
    fn new(platform_id: PlatformId, pack_type: SqPackType) -> SqPackHeader {
        let mut header = [0u8; 1024];
        header[..6].copy_from_slice(b"SqPack");
        header[8..12].copy_from_slice(&(platform_id as u32).to_le_bytes());
        header[12..16].copy_from_slice(&1024u32.to_le_bytes());
        header[16..20].copy_from_slice(&1u32.to_le_bytes());
        header[20..24].copy_from_slice(&(pack_type as u32).to_le_bytes());
        header[32..36].copy_from_slice(b"\xff\xff\xff\xff");
        SqPackHeader(header)
    }

    fn write_date_time(&mut self, packed_date: u32, packed_time: u32) {
        self.0[24..28].copy_from_slice(&packed_date.to_le_bytes());
        self.0[28..32].copy_from_slice(&packed_time.to_le_bytes());
    }

    fn finalize(&mut self) {
        let mut sha = Sha1::new();
        sha.update(&self.0[..0x3c0]);
        let hash = sha.finalize();
        self.0[0x3c0..0x3d4].copy_from_slice(&hash);
    }
}

#[derive(Default)]
struct SegmentAccumulator {
    offset: Option<u32>,
    hash: Option<Sha1>,
    length: u32,
}

struct IndexHeader {
    buf: [u8; 1024],
    segment_accumulators: [SegmentAccumulator; 4],
}

fn index_segment_headers_skeleton() -> IndexHeader {
    let mut header = [0u8; 1024];
    header[..4].copy_from_slice(&1024u32.to_le_bytes());
    header[4..8].copy_from_slice(&1u32.to_le_bytes());
    header[80..84].copy_from_slice(&1u32.to_le_bytes());
    let segment_accumulators: [SegmentAccumulator; 4] = [
        SegmentAccumulator {
            offset: Some(2048),
            hash: Some(Sha1::new()),
            length: 0,
        },
        SegmentAccumulator {
            offset: None,
            hash: Some(Sha1::new()),
            length: 0,
        },
        SegmentAccumulator {
            offset: None,
            hash: Some(Sha1::new()),
            length: 0,
        },
        SegmentAccumulator {
            offset: None,
            hash: Some(Sha1::new()),
            length: 0,
        },
    ];
    IndexHeader {
        buf: header,
        segment_accumulators,
    }
}

fn index_segment_headers_finalize(
    header: &mut IndexHeader,
    second_segment: &[u8],
    segments_not_present_heuristic: bool,
) {
    let mut next_segment_offset = 2048 + header.segment_accumulators[0].length;

    // unknown
    header.segment_accumulators[1].offset = Some(next_segment_offset);
    header.segment_accumulators[1].length = 256;
    next_segment_offset += header.segment_accumulators[1].length;
    header.segment_accumulators[1]
        .hash
        .as_mut()
        .unwrap()
        .update(second_segment);

    // tombstone entries
    if header.segment_accumulators[2].length == 0 {
        header.segment_accumulators[2].offset = Some(0);
    } else {
        header.segment_accumulators[2].offset = Some(next_segment_offset);
        next_segment_offset += header.segment_accumulators[2].length;
    }

    // folders
    if header.segment_accumulators[3].length > 0 {
        header.segment_accumulators[3].offset = Some(next_segment_offset);
    } else {
        header.segment_accumulators[3].offset = Some(0);
    }

    header.buf[8..12]
        .copy_from_slice(&header.segment_accumulators[0].offset.unwrap().to_le_bytes());
    header.buf[12..16].copy_from_slice(&header.segment_accumulators[0].length.to_le_bytes());
    if !segments_not_present_heuristic {
        header.buf[16..36].copy_from_slice(
            &header.segment_accumulators[0]
                .hash
                .take()
                .unwrap()
                .finalize(),
        );
    }

    header.buf[84..88]
        .copy_from_slice(&header.segment_accumulators[1].offset.unwrap().to_le_bytes());
    header.buf[88..92].copy_from_slice(&header.segment_accumulators[1].length.to_le_bytes());
    header.buf[92..112].copy_from_slice(
        &header.segment_accumulators[1]
            .hash
            .take()
            .unwrap()
            .finalize(),
    );

    header.buf[156..160]
        .copy_from_slice(&header.segment_accumulators[2].offset.unwrap().to_le_bytes());
    header.buf[160..164].copy_from_slice(&header.segment_accumulators[2].length.to_le_bytes());
    if !segments_not_present_heuristic {
        header.buf[164..184].copy_from_slice(
            &header.segment_accumulators[2]
                .hash
                .take()
                .unwrap()
                .finalize(),
        );
    }

    header.buf[228..232]
        .copy_from_slice(&header.segment_accumulators[3].offset.unwrap().to_le_bytes());
    header.buf[232..236].copy_from_slice(&header.segment_accumulators[3].length.to_le_bytes());
    if !segments_not_present_heuristic {
        header.buf[236..256].copy_from_slice(
            &header.segment_accumulators[3]
                .hash
                .take()
                .unwrap()
                .finalize(),
        );
    }
    if header.segment_accumulators[3].length == 0 {
        // what is this? comes after fourth segment header and another 44 null bytes.
        header.buf[300] = 2;
    }

    let mut sha = Sha1::new();
    sha.update(&header.buf[..0x3c0]);
    let hash = sha.finalize();
    header.buf[0x3c0..0x3d4].copy_from_slice(&hash);
}

/// Data header, after the SqPack header.
///
/// ```text
/// 0x000-0x004: Data header length
/// 0x004-0x008: Null bytes
/// 0x008-0x00C: 16
/// 0x00C-0x010: File length, shifted right by 7
/// 0x010-0x014: Data file number plus one (1 through 8)
/// 0x014-0x018: Null bytes
/// 0x018-0x01C: Data file size limit
/// 0x01C-0x020: Null bytes
/// 0x020-0x034: SHA-1 hash of the data section
/// 0x034-0x3c0: Null bytes
/// 0x3c0-0x3d4: SHA-1 hash of the preceding 0x3c0 bytes
/// 0x3d4-0x400: Null bytes
/// ```
struct DataHeader {
    buf: [u8; 1024],
}

fn data_header_skeleton(dat_file_number: u8, file_size_limit: u32) -> DataHeader {
    let mut header = [0u8; 1024];
    header[..4].copy_from_slice(&1024u32.to_le_bytes());
    header[8..12].copy_from_slice(&16u32.to_le_bytes());
    header[16] = dat_file_number + 1;
    header[24..28].copy_from_slice(&file_size_limit.to_le_bytes());
    DataHeader { buf: header }
}

fn data_header_finalize(
    header: &mut DataHeader,
    segments_not_present_heuristic: bool,
    data_length: u64,
    data_section_hash: Sha1,
) {
    header.buf[12..16].copy_from_slice(&u32::to_le_bytes((data_length >> 7).try_into().unwrap()));
    if !segments_not_present_heuristic {
        header.buf[32..52].copy_from_slice(&data_section_hash.finalize());
    }

    let mut sha = Sha1::new();
    sha.update(&header.buf[..0x3c0]);
    let hash = sha.finalize();
    header.buf[0x3c0..0x3d4].copy_from_slice(&hash);
}

struct DatFileRecord<IO: PackIO> {
    dat_file_number: u8,
    file: IO::F,
    sqpack_header: SqPackHeader,
    data_header: DataHeader,
    free_list: FreeList,
}

enum Block {
    Compressed {
        uncompressed_len: usize,
        compressed: Vec<u8>,
        block_size: u16,
    },
    Uncompressed {
        len: u32,
        data: Vec<u8>,
        block_size: u16,
    },
}

impl Block {
    fn block_size(&self) -> u16 {
        match self {
            Block::Compressed { block_size, .. } => *block_size,
            Block::Uncompressed { block_size, .. } => *block_size,
        }
    }
}

struct FolderEntry {
    folder_crc: u32,
    files_offset: u32,
    files_span: u32,
}

pub struct PackSetWriter<IO: PackIO> {
    io: IO,
    platform_id: PlatformId,
    index: IO::F,
    index_sqpack_header: SqPackHeader,
    index_segment_headers: IndexHeader,
    index2: IO::F,
    index2_sqpack_header: SqPackHeader,
    index2_segment_headers: IndexHeader,
    dats: Vec<DatFileRecord<IO>>,
    dat_file_number: u8,
    entries: BTreeMap<IndexHash1, FilePointer>,
    entries2: BTreeMap<IndexHash2, FilePointer>,
    file_size_limit: u32,
    segments_not_present_heuristic: bool,
    side_table: SideTables,
    extra_folder_heuristic: bool,
}

impl<IO: PackIO> PackSetWriter<IO> {
    pub fn new(mut io: IO, platform_id: PlatformId, pack_id: SqPackId) -> Result<Self, io::Error> {
        let segments_not_present_heuristic = matches!(
            pack_id,
            SqPackId {
                category: Category::Debug,
                ..
            } | SqPackId {
                category: Category::SqpackTest,
                expansion: Expansion::Base,
                ..
            }
        );
        let extra_folder_heuristic = matches!(
            pack_id,
            SqPackId {
                category: Category::Bg,
                expansion: Expansion::Ex3,
                ..
            }
        );
        let file_size_limit = match pack_id.category {
            Category::Debug => 200000000,
            _ => 2000000000,
        };
        let mut index = io.open_index_file()?;
        let index_sqpack_header = SqPackHeader::new(platform_id, SqPackType::Index);
        index.write_all(&index_sqpack_header.0)?;
        let index_segment_headers = index_segment_headers_skeleton();
        index.write_all(&index_segment_headers.buf)?;
        let mut index2 = io.open_index2_file()?;
        let index2_sqpack_header = SqPackHeader::new(platform_id, SqPackType::Index);
        index2.write_all(&index2_sqpack_header.0)?;
        let index2_segment_headers = index_segment_headers_skeleton();
        index2.write_all(&index2_segment_headers.buf)?;
        let writer = PackSetWriter {
            io,
            platform_id,
            index,
            index_sqpack_header,
            index_segment_headers,
            index2,
            index2_sqpack_header,
            index2_segment_headers,
            dats: Vec::new(),
            dat_file_number: 0,
            entries: BTreeMap::new(),
            entries2: BTreeMap::new(),
            file_size_limit,
            segments_not_present_heuristic,
            side_table: Default::default(),
            extra_folder_heuristic,
        };
        Ok(writer)
    }

    #[allow(unused)]
    pub fn set_side_table(&mut self, side_table: SideTables) {
        self.side_table = side_table
    }

    fn create_new_dat_file(&mut self) -> Result<(), io::Error> {
        let mut file = self.io.open_dat_file(self.dat_file_number)?;

        if let Some(reserved_length) = self
            .side_table
            .reserved_file_space
            .get(usize::from(self.dat_file_number))
        {
            file.set_len((*reserved_length).into())?
        }

        let sqpack_header = SqPackHeader::new(self.platform_id, SqPackType::Data);
        file.write_all(&sqpack_header.0)?;
        let data_header = data_header_skeleton(self.dat_file_number, self.file_size_limit);
        file.write_all(&data_header.buf)?;

        let mut free_list = FreeList::new(self.file_size_limit);
        // Reserve space for the headers that were just written
        free_list
            .reserve(
                0..(sqpack_header.0.len() + data_header.buf.len())
                    .try_into()
                    .unwrap(),
            )
            .unwrap();

        self.dats.push(DatFileRecord {
            dat_file_number: self.dat_file_number,
            file,
            sqpack_header,
            data_header,
            free_list,
        });
        Ok(())
    }

    pub fn add_file(&mut self, path: &str, data: &[u8]) -> Result<(), io::Error> {
        let hash1 = IndexHash1::hash(path);
        let hash2 = IndexHash2::hash(path);
        self.add_file_by_hashes(hash1, hash2, data)
    }

    fn choose_entry_location(
        &mut self,
        hash2: IndexHash2,
        total_size_padded: u32,
    ) -> Result<FilePointer, io::Error> {
        // Check for a preferred location from the side tables, and check if it's free.
        if let Some(side_table_entry) = self.side_table.file_entries.get(&hash2) {
            let data_file_id = side_table_entry.entry_pointer.data_file_id;
            let desired_range = side_table_entry.entry_pointer.offset
                ..side_table_entry.entry_pointer.offset + total_size_padded;
            if self.dats[usize::from(data_file_id)]
                .free_list
                .reserve(desired_range)
                .is_ok()
            {
                return Ok(side_table_entry.entry_pointer);
            }
        }

        // Check for free and non-reserved space in each data file.
        for (i, dat) in self.dats.iter_mut().enumerate() {
            let search_start = self
                .side_table
                .reserved_file_space
                .get(i)
                .copied()
                .unwrap_or_default();
            if let Ok(offset) = dat.free_list.reserve_next(search_start, total_size_padded) {
                return Ok(FilePointer::new(dat.dat_file_number, offset));
            }
        }

        // Allocate a new data file, and try to allcate space in it. Repeat until a new data file
        // has enough non-reserved space to service the request.
        loop {
            self.dat_file_number += 1;
            self.create_new_dat_file()?;

            let search_start = self
                .side_table
                .reserved_file_space
                .get(usize::from(self.dat_file_number))
                .copied()
                .unwrap_or_default();
            let dat = &mut self.dats[usize::from(self.dat_file_number)];
            if let Ok(offset) = dat.free_list.reserve_next(search_start, total_size_padded) {
                return Ok(FilePointer::new(dat.dat_file_number, offset));
            }
        }
    }

    pub fn add_file_by_hashes(
        &mut self,
        hash1: IndexHash1,
        hash2: IndexHash2,
        data: &[u8],
    ) -> Result<(), io::Error> {
        // deferred initial set-up of the first file, do this during this call instead of the
        // constructor so that the side table can be set up before we create the data file header.
        if self.dats.is_empty() {
            self.create_new_dat_file()?;
        }

        let file_entry_opt = self.side_table.file_entries.get(&hash2);

        // for now, assume the data entry has the binary type
        let blocks: Vec<Block> = data
            .chunks(16000)
            .enumerate()
            .map(|(i, slice)| {
                if file_entry_opt
                    .and_then(|file_entry| file_entry.block_compression.get(i).copied())
                    .unwrap_or(true)
                {
                    let compressed = compression::compress_sqpack_block(slice)?;
                    let block_size =
                        u16::try_from((16 + compressed.len() + 127) / 128 * 128).unwrap();
                    Ok(Block::Compressed {
                        uncompressed_len: slice.len(),
                        compressed,
                        block_size,
                    })
                } else {
                    let len = slice.len().try_into().unwrap();
                    let block_size = u16::try_from((16 + len + 127) / 128 * 128).unwrap();
                    Ok(Block::Uncompressed {
                        len,
                        data: slice.to_vec(),
                        block_size,
                    })
                }
            })
            .collect::<Result<Vec<Block>, io::Error>>()?;
        let entry_header_size: u32 = (((24 + 8 * blocks.len()) + 127) / 128 * 128)
            .try_into()
            .unwrap();
        let total_size_unpadded: u32 = entry_header_size
            + blocks
                .iter()
                .map(|chunk| u32::from(chunk.block_size()))
                .sum::<u32>();
        let total_size_padded_shifted = (total_size_unpadded + 127) / 128;
        let total_size_padded = total_size_padded_shifted * 128;

        let mut unknown = total_size_padded_shifted - 1; // still working on this, needs corrections
        if let Some(entry) = file_entry_opt {
            unknown = entry.unknown_entry_field;
        }

        let pointer = self.choose_entry_location(hash2, total_size_padded)?;

        let block_buffer_size: u32 = blocks
            .iter()
            .map(|block| u32::from(block.block_size() >> 7))
            .sum();
        let mut entry_header_buf = vec![0; entry_header_size as usize];
        entry_header_buf[0..4].copy_from_slice(&u32::to_le_bytes(entry_header_size)); // data entry header length
        entry_header_buf[4] = 2; // content type, binary
        entry_header_buf[8..12].copy_from_slice(&u32::to_le_bytes(data.len().try_into().unwrap()));
        entry_header_buf[12..16].copy_from_slice(&u32::to_le_bytes(unknown)); // unknown
        entry_header_buf[16..20].copy_from_slice(&u32::to_le_bytes(block_buffer_size)); // block buffer size
        entry_header_buf[20..24]
            .copy_from_slice(&u32::to_le_bytes(blocks.len().try_into().unwrap())); // number of blocks

        // next, block table
        let mut block_offset: u32 = 0;
        for (i, block) in blocks.iter().enumerate() {
            entry_header_buf[24 + i * 8..28 + i * 8].copy_from_slice(&block_offset.to_le_bytes()); // offset
            entry_header_buf[28 + i * 8..30 + i * 8]
                .copy_from_slice(&u16::to_le_bytes(block.block_size())); // block size
            entry_header_buf[30 + i * 8..32 + i * 8].copy_from_slice(&u16::to_le_bytes(
                match block {
                    Block::Compressed {
                        uncompressed_len, ..
                    } => (*uncompressed_len).try_into().unwrap(),
                    Block::Uncompressed { len, .. } => (*len).try_into().unwrap(),
                },
            ));
            block_offset += u32::from(block.block_size());
        }

        let dat_file_record = &mut self
            .dats
            .get_mut(usize::try_from(pointer.data_file_id).unwrap())
            .unwrap();
        let dat_file = &mut dat_file_record.file;
        dat_file.seek(SeekFrom::Start(pointer.offset.into()))?;
        dat_file.write_all(&entry_header_buf)?;

        for block in blocks.iter() {
            let (data, original_len) = match block {
                Block::Compressed {
                    compressed,
                    uncompressed_len,
                    ..
                } => (compressed, *uncompressed_len),
                Block::Uncompressed { data, .. } => (data, data.len()),
            };

            let compressed_size: u32 = data.len().try_into().unwrap();
            let mut compressed_size_field = compressed_size;
            if let Block::Uncompressed { .. } = block {
                compressed_size_field = 32000;
            }
            // the block itself is next
            let mut block_header_buf = [0; 16];
            block_header_buf[0] = 16;
            block_header_buf[8..12].copy_from_slice(&compressed_size_field.to_le_bytes());
            block_header_buf[12..16]
                .copy_from_slice(&u32::to_le_bytes(original_len.try_into().unwrap()));

            dat_file.write_all(&block_header_buf)?;

            // compressed data is next
            dat_file.write_all(data)?;
            // pad out before next block
            let padding_length = (16 + compressed_size + 127) / 128 * 128 - (16 + compressed_size);
            dat_file.write_all(&[0; 127][..padding_length as usize])?;
        }

        let position_after = dat_file.stream_position()?;
        assert_eq!(
            u64::from(total_size_unpadded),
            position_after - u64::from(pointer.offset)
        );

        let padding_length =
            TryInto::<usize>::try_into(total_size_padded - total_size_unpadded).unwrap();
        for _ in 0..(padding_length / ZEROS.len()) {
            dat_file.write_all(&ZEROS)?;
        }
        dat_file.write_all(&ZEROS[..padding_length % ZEROS.len()])?;

        // TODO: not handling collisions yet
        assert!(self.entries.insert(hash1, pointer).is_none());
        assert!(self.entries2.insert(hash2, pointer).is_none());
        Ok(())
    }

    pub fn finalize(mut self) -> Result<IO, io::Error> {
        // Write file index entries into the first segment of the body, and
        // save folder hashes and ranges for a later section.
        let mut entry_buffer = [0; 16];
        let mut last_folder_crc = None;
        let mut folder_table: Vec<FolderEntry> = Vec::new();
        let seg_accum = &mut self.index_segment_headers.segment_accumulators[0];
        for (hash, locator) in self.entries {
            if last_folder_crc != Some(hash.folder_crc) {
                let files_offset = seg_accum.offset.unwrap() + seg_accum.length;
                if let Some(last_folder) = folder_table.last_mut() {
                    let span = files_offset - last_folder.files_offset;
                    last_folder.files_span = span;
                }
                folder_table.push(FolderEntry {
                    folder_crc: hash.folder_crc,
                    files_offset,
                    files_span: 0,
                });
            }
            entry_buffer[0..4].copy_from_slice(&hash.filename_crc.to_le_bytes());
            entry_buffer[4..8].copy_from_slice(&hash.folder_crc.to_le_bytes());
            entry_buffer[8..12]
                .copy_from_slice(&IndexPointer::Pointer(locator).to_u32().to_le_bytes());
            self.index.write_all(&entry_buffer)?;
            seg_accum.length += 16;
            seg_accum.hash.as_mut().unwrap().update(&entry_buffer);
            last_folder_crc = Some(hash.folder_crc);
        }
        if let Some(last_folder) = folder_table.last_mut() {
            let span = (seg_accum.offset.unwrap() + seg_accum.length) - last_folder.files_offset;
            last_folder.files_span = span;
        }

        // TODO: need to figure out where this comes from, hardcoded for now
        if self.extra_folder_heuristic {
            folder_table.insert(
                0,
                FolderEntry {
                    folder_crc: 0,
                    files_offset: 0xa00,
                    files_span: 16,
                },
            );
        }

        // unknown data in the second segment.
        let mut index_second_segment = [0; 256];
        index_second_segment[0..8].fill(0xff);
        index_second_segment[12..16].fill(0xff);
        self.index.write_all(&index_second_segment)?;

        // Write out zero-type/tombstone entries, if their file locations have not been taken.
        // Write their positions in the third segment, sorted first by increasing length, then by
        // increasing offset.
        let mut entry_header_buf = [0u8; 16];
        let mut index_entry_buf = [0u8; 16];
        let seg_accum = &mut self.index_segment_headers.segment_accumulators[2];
        for entry in self.side_table.zero_entries.iter() {
            // add one for the entry header.
            let total_length = (entry.shifted_length + 1) * 128;
            let offset = entry.pointer.offset();
            let to_reserve = offset..offset + total_length;
            let data_file_id = entry.pointer.data_file_id();
            let dat_file_record = &mut self.dats[usize::from(data_file_id)];
            if dat_file_record.free_list.reserve(to_reserve).is_ok() {
                // write entry header to data file.
                entry_header_buf[0] = 0x80;
                entry_header_buf[12..16].copy_from_slice(&entry.shifted_length.to_le_bytes());
                dat_file_record.file.seek(SeekFrom::Start(offset.into()))?;
                dat_file_record.file.write_all(&entry_header_buf)?;

                // write index entry to index segment.
                index_entry_buf[4..8].copy_from_slice(&(entry.pointer.offset() >> 7).to_le_bytes()); // TODO: what about file ID?
                index_entry_buf[8..12].copy_from_slice(&(entry.shifted_length + 1).to_le_bytes());
                self.index.write_all(&index_entry_buf)?;
                seg_accum.length += 16;
                seg_accum.hash.as_mut().unwrap().update(&index_entry_buf);
            }
        }

        // folders in the fourth segment.
        let mut entry_buffer = [0; 16];
        let seg_accum = &mut self.index_segment_headers.segment_accumulators[3];
        for folder in folder_table {
            entry_buffer[0..4].copy_from_slice(&folder.folder_crc.to_le_bytes());
            entry_buffer[4..8].copy_from_slice(&folder.files_offset.to_le_bytes());
            entry_buffer[8..12].copy_from_slice(&folder.files_span.to_le_bytes());
            self.index.write_all(&entry_buffer)?;
            seg_accum.length += 16;
            seg_accum.hash.as_mut().unwrap().update(&entry_buffer);
        }

        // write updated sqpack and index headers in first index file
        self.index.seek(SeekFrom::Start(0))?;
        self.index_sqpack_header.finalize();
        self.index.write_all(&self.index_sqpack_header.0)?;
        index_segment_headers_finalize(
            &mut self.index_segment_headers,
            &index_second_segment,
            self.segments_not_present_heuristic,
        );
        self.index.write_all(&self.index_segment_headers.buf)?;

        // write file index entries into the first segment of the body.
        let mut entry_buffer = [0; 8];
        let seg_accum = &mut self.index2_segment_headers.segment_accumulators[0];
        for (hash, pointer) in self.entries2 {
            entry_buffer[0..4].copy_from_slice(&hash.path_crc.to_le_bytes());
            entry_buffer[4..8]
                .copy_from_slice(&IndexPointer::Pointer(pointer).to_u32().to_le_bytes());
            self.index2.write_all(&entry_buffer)?;
            seg_accum.length += 8;
            seg_accum.hash.as_mut().unwrap().update(&entry_buffer);
        }

        // unknown data in the second segment.
        let mut index2_second_segment = [0; 256];
        index2_second_segment[0..4].fill(0xff);
        index2_second_segment[12..16].fill(0xff);
        self.index2.write_all(&index2_second_segment)?;

        self.index2.seek(SeekFrom::Start(0))?;
        self.index2_sqpack_header.finalize();
        self.index2.write_all(&self.index2_sqpack_header.0)?;
        index_segment_headers_finalize(
            &mut self.index2_segment_headers,
            &index2_second_segment,
            self.segments_not_present_heuristic,
        );
        self.index2.write_all(&self.index2_segment_headers.buf)?;

        for DatFileRecord {
            dat_file_number,
            file,
            sqpack_header: header,
            data_header,
            free_list: _,
        } in self.dats.iter_mut()
        {
            if let Some((packed_date, packed_time)) =
                self.side_table.sqpack_data_datetimes.get(dat_file_number)
            {
                header.write_date_time(*packed_date, *packed_time);
            }

            let data_section_start = (header.0.len() + data_header.buf.len()).try_into().unwrap();

            // Compute the data section hash.
            file.seek(SeekFrom::Start(data_section_start))?;
            let mut data_section_hash = Sha1::new();
            let mut temp_buf = [0u8; 4096];
            loop {
                let count = file.read(&mut temp_buf)?;
                if count == 0 {
                    break;
                }
                data_section_hash.update(&temp_buf[..count]);
            }

            // Save the data section length.
            let data_length = file.stream_position()? - data_section_start;

            // Write out the completed headers.
            file.seek(SeekFrom::Start(0))?;
            header.finalize();
            file.write_all(&header.0)?;
            data_header_finalize(
                data_header,
                self.segments_not_present_heuristic,
                data_length,
                data_section_hash,
            );
            file.write_all(&data_header.buf)?;
        }

        Ok(self.io)
    }
}

#[derive(Clone)]
struct FreeListEntry(Range<u32>);

impl Ord for FreeListEntry {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0
            .start
            .cmp(&other.0.start)
            .then_with(|| self.0.end.cmp(&other.0.end))
    }
}

impl PartialOrd for FreeListEntry {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for FreeListEntry {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for FreeListEntry {}

#[derive(Debug, PartialEq, Eq)]
struct FreeListExhaustedError;

/// This records which sections of a data file are occupied or not, internally stored as an ordered
/// list of ranges. When space is reserved in the file, free space ranges will be removed,
/// truncated, or split.
struct FreeList {
    inner: BTreeSet<FreeListEntry>,
}

impl FreeList {
    fn new(length: u32) -> FreeList {
        let mut inner = BTreeSet::new();
        inner.insert(FreeListEntry(0..length));
        FreeList { inner }
    }

    /// Helper method to remove a free list entry, allocate a region from it, and add the
    /// remainders back to the free list.
    fn split_entry(&mut self, entry: &FreeListEntry, allocation: Range<u32>) {
        assert!(self.inner.remove(entry));
        if entry.0.start < allocation.start {
            self.inner
                .insert(FreeListEntry((entry.0.start)..(allocation.start)));
        }
        if entry.0.end > allocation.end {
            self.inner
                .insert(FreeListEntry((allocation.end)..(entry.0.end)));
        }
    }

    /// Reserve a particular range from the free list, return `Ok(())` if successful, or an error
    /// if the requested space is already taken.
    fn reserve(&mut self, to_reserve: Range<u32>) -> Result<(), FreeListExhaustedError> {
        let pivot = FreeListEntry((to_reserve.start)..(to_reserve.start));

        // Use a reverse range iterator to check the free list entry immediately before the pivot.
        // If there is an entry, and it covers the entire requested range, then we can remove it,
        // split it, re-insert the remainders, and return success.
        if let Some(left_free_entry) = self.inner.range(..=pivot.clone()).rev().next() {
            if left_free_entry.0.contains(&to_reserve.start)
                && left_free_entry.0.contains(&(to_reserve.end - 1))
            {
                let left_free_entry = left_free_entry.clone();
                self.split_entry(&left_free_entry, to_reserve);
                return Ok(());
            }
        }

        // If the leftward check didn't succeed, the remaining case is that there may be a free
        // list entry beginning right where our desired start is. Check for that, and check if
        // the free list entry is long enough. If so, remove it, truncate it from the left, and
        // re-insert it, returning success. If not, return failure.
        if let Some(right_free_entry) = self.inner.range(pivot..).next() {
            if right_free_entry.0.contains(&to_reserve.start)
                && right_free_entry.0.contains(&(to_reserve.end - 1))
            {
                let right_free_entry = right_free_entry.clone();
                self.split_entry(&right_free_entry, to_reserve);
                return Ok(());
            }
        }

        Err(FreeListExhaustedError)
    }

    /// Reserve the next available region of a given length, starting the search at the given
    /// offset. Returns an error if there is no contiguous free region of this length available.
    fn reserve_next(
        &mut self,
        search_range_start: u32,
        requested_length: u32,
    ) -> Result<u32, FreeListExhaustedError> {
        let pivot = FreeListEntry(search_range_start..search_range_start);

        // First, use a reverse range iterator to check if the start of the search range is already
        // within a free list entry. If so, and there's enough room from the start of the search
        // range to the end of the free list entry for the requested length, carve it up and
        // return it.
        if let Some(left_free_entry) = self.inner.range(..=pivot.clone()).rev().next() {
            if left_free_entry.0.contains(&search_range_start)
                && left_free_entry
                    .0
                    .contains(&(search_range_start + requested_length - 1))
            {
                let left_free_entry = left_free_entry.clone();
                let new_range = search_range_start..search_range_start + requested_length;
                self.split_entry(&left_free_entry, new_range);
                return Ok(search_range_start);
            }
        }

        // Failing that, check all free list entries at higher offsets for space, and allocate from
        // the beginning of the first one that's large enough.
        for free_entry in self.inner.range(pivot..) {
            if free_entry.0.end - free_entry.0.start >= requested_length {
                let free_entry = free_entry.clone();
                self.split_entry(
                    &free_entry,
                    free_entry.0.start..free_entry.0.start + requested_length,
                );
                return Ok(free_entry.0.start);
            }
        }

        Err(FreeListExhaustedError)
    }
}

#[cfg(test)]
mod tests {
    use super::{FreeList, FreeListExhaustedError};
    use quickcheck::{Arbitrary, QuickCheck, TestResult};
    use std::ops::Range;

    const SIZE_LIMIT: usize = 256;

    /// This is a simplified implementation of FreeList's API, for model-based testing purposes.
    /// It represents free/occupied space with a bit map rather than ranges. The available range
    /// is fixed at SIZE_LIMIT.
    struct FreeListModel {
        occupied: [bool; SIZE_LIMIT],
    }

    impl FreeListModel {
        fn new() -> FreeListModel {
            FreeListModel {
                occupied: [false; SIZE_LIMIT],
            }
        }

        fn reserve(&mut self, to_reserve: Range<u32>) -> Result<(), FreeListExhaustedError> {
            assert!(usize::try_from(to_reserve.start).unwrap() < SIZE_LIMIT);
            assert!(usize::try_from(to_reserve.end).unwrap() <= SIZE_LIMIT);

            for i in to_reserve.clone() {
                let i = usize::try_from(i).unwrap();
                if self.occupied[i] {
                    return Err(FreeListExhaustedError);
                }
            }
            for i in to_reserve {
                let i = usize::try_from(i).unwrap();
                self.occupied[i] = true;
            }

            Ok(())
        }

        fn reserve_next(
            &mut self,
            search_range_start: u32,
            requested_length: u32,
        ) -> Result<u32, FreeListExhaustedError> {
            let mut start: usize = search_range_start.try_into().unwrap();
            let requested_length_usize = usize::try_from(requested_length).unwrap();
            loop {
                if SIZE_LIMIT - start < requested_length_usize {
                    return Err(FreeListExhaustedError);
                }
                if let Some(next_occupied) = self.occupied[start..SIZE_LIMIT]
                    .iter()
                    .take(requested_length_usize)
                    .position(|o| *o)
                {
                    start += next_occupied + 1;
                } else {
                    self.occupied[start..start + requested_length_usize].fill(true);
                    return Ok(start.try_into().unwrap());
                }
            }
        }
    }

    #[derive(Debug, Clone)]
    enum Op {
        Reserve(Range<u32>),
        ReserveNext(u32, u32),
    }

    impl Arbitrary for Op {
        fn arbitrary(g: &mut quickcheck::Gen) -> Op {
            if bool::arbitrary(g) {
                let mut a = 0;
                let mut b = 0;
                while a == b {
                    a = u32::arbitrary(g) % u32::try_from(SIZE_LIMIT).unwrap();
                    b = u32::arbitrary(g) % u32::try_from(SIZE_LIMIT).unwrap();
                }
                Op::Reserve(std::cmp::min(a, b)..std::cmp::max(a, b))
            } else {
                let start = u32::arbitrary(g) % u32::try_from(SIZE_LIMIT).unwrap();
                let mut length = 0;
                while length == 0 {
                    length = u32::arbitrary(g) % u32::try_from(SIZE_LIMIT).unwrap();
                }
                Op::ReserveNext(start, length)
            }
        }
    }

    /// Check that the internal state of the free list is consistent, with none of the free ranges
    /// overlapping, and no empty ranges.
    fn validate_free_list(free_list: &FreeList) -> bool {
        let mut last_end = 0;
        for entry in free_list.inner.iter() {
            let range = &entry.0;
            if range.start >= range.end {
                return false;
            }
            if range.start < last_end {
                return false;
            }
            last_end = range.end;
        }
        true
    }

    fn property_model_equivalent(operations: Vec<Op>) -> TestResult {
        let mut free_list = FreeList::new(SIZE_LIMIT.try_into().unwrap());
        let mut free_list_model = FreeListModel::new();
        for op in operations.iter() {
            match op {
                Op::Reserve(to_reserve) => {
                    let actual_result = free_list.reserve(to_reserve.clone());
                    let model_result = free_list_model.reserve(to_reserve.clone());
                    if actual_result != model_result {
                        return TestResult::failed();
                    }
                }
                Op::ReserveNext(search_range_start, requested_length) => {
                    let actual_result =
                        free_list.reserve_next(*search_range_start, *requested_length);
                    let model_result =
                        free_list_model.reserve_next(*search_range_start, *requested_length);
                    if actual_result != model_result {
                        return TestResult::failed();
                    }
                }
            }
            if !validate_free_list(&free_list) {
                return TestResult::failed();
            }
        }
        return TestResult::passed();
    }

    #[test]
    fn free_list_model_quickcheck() {
        let mut qc = QuickCheck::new();
        qc.quickcheck(property_model_equivalent as fn(Vec<Op>) -> TestResult);
    }

    #[test]
    fn free_list_model_regression_1() {
        assert!(!property_model_equivalent(vec![Op::ReserveNext(255, 1)]).is_failure());
    }
}
