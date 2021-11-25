use std::{
    collections::BTreeMap,
    fs::File,
    io::{self, Seek, SeekFrom, Write},
    path::PathBuf,
};

use sha1::{Digest, Sha1};

#[allow(unused)]
use crate::compression::compress_sqpack_block;
use crate::{
    compression, Category, DataLocator, Expansion, IndexHash, IndexHash1, IndexHash2, PlatformId,
    SqPackId, SqPackType,
};

pub trait PackIO {
    type F: Write + Seek;

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

fn sqpack_header_skeleton(platform_id: PlatformId, pack_type: SqPackType) -> SqPackHeader {
    let mut header = [0u8; 1024];
    header[..6].copy_from_slice(b"SqPack");
    header[8..12].copy_from_slice(&(platform_id as u32).to_le_bytes());
    header[12..16].copy_from_slice(&1024u32.to_le_bytes());
    header[16..20].copy_from_slice(&1u32.to_le_bytes());
    header[20..24].copy_from_slice(&(pack_type as u32).to_le_bytes());
    // date?
    // unknown?
    header[32..36].copy_from_slice(b"\xff\xff\xff\xff");
    SqPackHeader(header)
}

fn sqpack_header_finalize(header: &mut SqPackHeader) {
    let mut sha = Sha1::new();
    sha.update(&header.0[..0x3c0]);
    let hash = sha.finalize();
    header.0[0x3c0..0x3d4].copy_from_slice(&hash);
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
    folder_segment_present: bool,
    segments_not_present_heuristic: bool,
) {
    header.segment_accumulators[1].offset = Some(2048 + header.segment_accumulators[0].length);
    header.segment_accumulators[1].length = 256;
    header.segment_accumulators[1]
        .hash
        .as_mut()
        .unwrap()
        .update(second_segment);
    header.segment_accumulators[2].offset = Some(0);
    if folder_segment_present {
        header.segment_accumulators[3].offset = Some(
            2048 + header.segment_accumulators[0].length + header.segment_accumulators[1].length,
        );
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
    if !folder_segment_present {
        // what is this? comes after fourth segment header and another 44 null bytes.
        header.buf[300] = 2;
    }

    let mut sha = Sha1::new();
    sha.update(&header.buf[..0x3c0]);
    let hash = sha.finalize();
    header.buf[0x3c0..0x3d4].copy_from_slice(&hash);
}

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
    data_section_hash: &mut Option<Sha1>,
    segments_not_present_heuristic: bool,
) {
    if !segments_not_present_heuristic {
        header.buf[32..52].copy_from_slice(&data_section_hash.take().unwrap().finalize());
    }

    let mut sha = Sha1::new();
    sha.update(&header.buf[..0x3c0]);
    let hash = sha.finalize();
    header.buf[0x3c0..0x3d4].copy_from_slice(&hash);
}

struct DatFileRecord<IO: PackIO> {
    file: IO::F,
    sqpack_header: SqPackHeader,
    data_header: DataHeader,
    data_section_hash: Option<Sha1>,
}

const ENTRY_ALIGNMENT: u32 = 128;

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
    dat_file_position: u32,
    entries: BTreeMap<IndexHash1, DataLocator>,
    entries2: BTreeMap<IndexHash2, DataLocator>,
    file_size_limit: u32,
    segments_not_present_heuristic: bool,
}

impl<IO: PackIO> PackSetWriter<IO> {
    pub fn new(mut io: IO, platform_id: PlatformId, pack_id: SqPackId) -> Result<Self, io::Error> {
        let segments_not_present_heuristic = match pack_id {
            SqPackId {
                category: Category::Debug,
                ..
            }
            | SqPackId {
                category: Category::SqpackTest,
                expansion: Expansion::Base,
                ..
            } => true,
            _ => false,
        };
        let file_size_limit = match pack_id.category {
            Category::Debug => 200000000,
            _ => 2000000000,
        };
        let mut index = io.open_index_file()?;
        let index_sqpack_header = sqpack_header_skeleton(platform_id, SqPackType::Index);
        index.write_all(&index_sqpack_header.0)?;
        let index_segment_headers = index_segment_headers_skeleton();
        index.write_all(&index_segment_headers.buf)?;
        let mut index2 = io.open_index2_file()?;
        let index2_sqpack_header = sqpack_header_skeleton(platform_id, SqPackType::Index);
        index2.write_all(&index2_sqpack_header.0)?;
        let index2_segment_headers = index_segment_headers_skeleton();
        index2.write_all(&index2_segment_headers.buf)?;
        let mut writer = PackSetWriter {
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
            dat_file_position: 2048,
            entries: BTreeMap::new(),
            entries2: BTreeMap::new(),
            file_size_limit,
            segments_not_present_heuristic,
        };
        writer.create_new_dat_file()?;
        Ok(writer)
    }

    fn create_new_dat_file(&mut self) -> Result<(), io::Error> {
        let mut file = self.io.open_dat_file(self.dat_file_number)?;
        let sqpack_header = sqpack_header_skeleton(self.platform_id, SqPackType::Data);
        file.write_all(&sqpack_header.0)?;
        let data_header = data_header_skeleton(self.dat_file_number, self.file_size_limit);
        file.write_all(&data_header.buf)?;
        self.dats.push(DatFileRecord {
            file,
            sqpack_header,
            data_header,
            data_section_hash: Some(Sha1::new()),
        });
        Ok(())
    }

    pub fn add_file(&mut self, path: &str, data: &[u8]) -> Result<(), io::Error> {
        let hash1 = IndexHash1::hash(path);
        let hash2 = IndexHash2::hash(path);
        self.add_file_by_hashes(hash1, hash2, data)
    }

    pub fn add_file_by_hashes(
        &mut self,
        hash1: IndexHash1,
        hash2: IndexHash2,
        data: &[u8],
    ) -> Result<(), io::Error> {
        // for now, assume the data entry has the binary type, and there is one data block
        let entry_header_size = std::cmp::max(128, 24 + 8 * 1);
        let block_header_size = 16;
        let compressed = compression::compress_sqpack_block(data)?;
        let compressed_size: u32 = compressed.len().try_into().unwrap();
        let total_size = (entry_header_size + block_header_size + compressed_size + 7) / 8 * 8;
        let current_file_size = self.dats.last_mut().unwrap().file.stream_position()?;
        if current_file_size + total_size as u64 > self.file_size_limit as u64 {
            self.dat_file_number += 1;
            self.dat_file_position = 2048;
            self.create_new_dat_file()?;
        }

        let locator = DataLocator::new(self.dat_file_number, self.dat_file_position);

        let mut entry_header_buf = vec![0; entry_header_size as usize];
        entry_header_buf[0] = 0x80; // data entry header length
        entry_header_buf[4] = 2; // content type, binary
        entry_header_buf[8..12].copy_from_slice(&u32::to_le_bytes(data.len().try_into().unwrap()));
        entry_header_buf[12] = 2; // unknown
        entry_header_buf[16] = 1; // block buffer size
        entry_header_buf[20] = 1; // number of blocks

        // next, block table, assume only one entry with offset zero
        entry_header_buf[28..30].copy_from_slice(&u16::to_le_bytes(0x80)); // block size
        entry_header_buf[30..32]
            .copy_from_slice(&u16::to_le_bytes(data.len().try_into().unwrap_or_default())); // uncompressed size

        let dat_file = &mut self.dats.last_mut().unwrap().file;
        let position_before = dat_file.stream_position()?;
        dat_file.write_all(&entry_header_buf)?;

        // the block itself is next, offset of 0 means immediately after data entry header.
        let mut block_header_buf = [0; 16];
        block_header_buf[0] = 16;
        block_header_buf[8..12].copy_from_slice(&compressed_size.to_le_bytes());
        block_header_buf[12..16].copy_from_slice(&u32::to_le_bytes(data.len().try_into().unwrap()));

        dat_file.write_all(&block_header_buf)?;

        // compressed data is next
        dat_file.write_all(&compressed)?;
        let padding_length = (compressed_size + 7) / 8 * 8 - compressed_size;
        dat_file.write_all(&[0; 7][..padding_length as usize])?;
        let position_after = dat_file.stream_position()?;

        assert_eq!(total_size as u64, position_after - position_before);
        self.dat_file_position += total_size;

        // Next entry needs to be aligned by 128, not 8, so seek ahead further.
        self.dat_file_position =
            (self.dat_file_position + ENTRY_ALIGNMENT - 1) / ENTRY_ALIGNMENT * ENTRY_ALIGNMENT;
        dat_file.seek(SeekFrom::Start(self.dat_file_position as u64))?;

        assert!(self.entries.insert(hash1, locator).is_none());
        assert!(self.entries2.insert(hash2, locator).is_none());
        Ok(())
    }

    pub fn finalize(mut self) -> Result<IO, io::Error> {
        // write file index entries into the first segment of the body.
        let mut entry_buffer = [0; 16];
        for (hash, locator) in self.entries {
            entry_buffer[0..4].copy_from_slice(&hash.filename_crc.to_le_bytes());
            entry_buffer[4..8].copy_from_slice(&hash.folder_crc.to_le_bytes());
            entry_buffer[8..12].copy_from_slice(&locator.to_u32().to_le_bytes());
            self.index.write_all(&entry_buffer)?;
            self.index_segment_headers.segment_accumulators[0].length += 16;
        }

        // unknown data in the second segment.
        let mut index_second_segment = [0; 256];
        index_second_segment[0..8].fill(0xff);
        index_second_segment[12..16].fill(0xff);
        self.index.write_all(&index_second_segment)?;

        self.index.seek(SeekFrom::Start(0))?;
        sqpack_header_finalize(&mut self.index_sqpack_header);
        self.index.write_all(&self.index_sqpack_header.0)?;
        index_segment_headers_finalize(
            &mut self.index_segment_headers,
            &index_second_segment,
            true,
            self.segments_not_present_heuristic,
        );
        self.index.write_all(&self.index_segment_headers.buf)?;

        // write file index entries into the first segment of the body.
        let mut entry_buffer = [0; 8];
        for (hash, locator) in self.entries2 {
            entry_buffer[0..4].copy_from_slice(&hash.path_crc.to_le_bytes());
            entry_buffer[4..8].copy_from_slice(&locator.to_u32().to_le_bytes());
            self.index2.write_all(&entry_buffer)?;
            self.index2_segment_headers.segment_accumulators[0].length += 16;
        }

        // unknown data in the second segment.
        let mut index2_second_segment = [0; 256];
        index2_second_segment[0..4].fill(0xff);
        index2_second_segment[12..16].fill(0xff);
        self.index2.write_all(&index2_second_segment)?;

        self.index2.seek(SeekFrom::Start(0))?;
        sqpack_header_finalize(&mut self.index2_sqpack_header);
        self.index2.write_all(&self.index2_sqpack_header.0)?;
        index_segment_headers_finalize(
            &mut self.index2_segment_headers,
            &index2_second_segment,
            false,
            self.segments_not_present_heuristic,
        );
        self.index2.write_all(&self.index2_segment_headers.buf)?;

        for DatFileRecord {
            file,
            sqpack_header: header,
            data_header,
            data_section_hash,
        } in self.dats.iter_mut()
        {
            file.seek(SeekFrom::Start(0))?;
            sqpack_header_finalize(header);
            file.write_all(&header.0)?;
            data_header_finalize(
                data_header,
                data_section_hash,
                self.segments_not_present_heuristic,
            );
            file.write_all(&data_header.buf)?;
        }

        Ok(self.io)
    }
}
