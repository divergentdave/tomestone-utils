use std::{
    collections::BTreeMap,
    fs::File,
    io::{self, Seek, SeekFrom, Write},
    path::PathBuf,
};

use sha1::{Digest, Sha1};

#[allow(unused)]
use crate::compression::compress_sqpack_block;
use crate::{DataLocator, IndexHash, IndexHash1, IndexHash2, PlatformId, SqPackId, SqPackType};

pub trait PackIO {
    type F: Write + Seek;

    fn open_index_file(&mut self) -> Result<Self::F, io::Error>;
    fn open_index2_file(&mut self) -> Result<Self::F, io::Error>;
    fn open_dat_file(&mut self, number: u32) -> Result<Self::F, io::Error>;
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

    fn open_dat_file(&mut self, number: u32) -> Result<File, io::Error> {
        assert_eq!(self.platform_id, PlatformId::Win32);
        File::create(self.base.join(self.pack_id.expansion.name()).join(format!(
            "{:02x}{:02x}{:02x}.win32.dat{}",
            self.pack_id.category as u8, self.pack_id.expansion as u8, self.pack_id.number, number
        )))
    }
}

struct SqPackHeader([u8; 1024]);

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

fn index_segment_headers_finalize(header: &mut IndexHeader) {
    header.segment_accumulators[1].offset = Some(2048 + header.segment_accumulators[0].length);
    header.segment_accumulators[1].length = 256; // what is this? data is ffffffff ffffffff 00000000 ffffffff, then zeros
    header.segment_accumulators[1]
        .hash
        .as_mut()
        .unwrap()
        .update(&[0xff; 8]);
    header.segment_accumulators[1]
        .hash
        .as_mut()
        .unwrap()
        .update(&[0; 4]);
    header.segment_accumulators[1]
        .hash
        .as_mut()
        .unwrap()
        .update(&[0xff; 4]);
    header.segment_accumulators[1]
        .hash
        .as_mut()
        .unwrap()
        .update(&[0; 240]);
    header.segment_accumulators[2].offset = Some(0);
    header.segment_accumulators[3].offset =
        Some(2048 + header.segment_accumulators[0].length + header.segment_accumulators[1].length);

    header.buf[8..12]
        .copy_from_slice(&header.segment_accumulators[0].offset.unwrap().to_le_bytes());
    header.buf[12..16].copy_from_slice(&header.segment_accumulators[0].length.to_le_bytes());
    header.buf[16..36].copy_from_slice(
        &header.segment_accumulators[0]
            .hash
            .take()
            .unwrap()
            .finalize(),
    );

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
    header.buf[164..184].copy_from_slice(
        &header.segment_accumulators[2]
            .hash
            .take()
            .unwrap()
            .finalize(),
    );

    header.buf[228..232]
        .copy_from_slice(&header.segment_accumulators[3].offset.unwrap().to_le_bytes());
    header.buf[232..236].copy_from_slice(&header.segment_accumulators[3].length.to_le_bytes());
    header.buf[236..256].copy_from_slice(
        &header.segment_accumulators[3]
            .hash
            .take()
            .unwrap()
            .finalize(),
    );

    let mut sha = Sha1::new();
    sha.update(&header.buf[..0x3c0]);
    let hash = sha.finalize();
    header.buf[0x3c0..0x3d4].copy_from_slice(&hash);
}

pub struct PackSetWriter<IO: PackIO> {
    io: IO,
    index: IO::F,
    index_sqpack_header: SqPackHeader,
    index_segment_headers: IndexHeader,
    index2: IO::F,
    index2_sqpack_header: SqPackHeader,
    index2_segment_headers: IndexHeader,
    dats: Vec<IO::F>,
    entries: BTreeMap<IndexHash1, DataLocator>,
    entries2: BTreeMap<IndexHash2, DataLocator>,
    file_size_limit: u32,
}

impl<IO: PackIO> PackSetWriter<IO> {
    pub fn new(mut io: IO, platform_id: PlatformId) -> Result<Self, io::Error> {
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
        Ok(PackSetWriter {
            io,
            index,
            index_sqpack_header,
            index_segment_headers,
            index2,
            index2_sqpack_header,
            index2_segment_headers,
            dats: Vec::new(),
            entries: BTreeMap::new(),
            entries2: BTreeMap::new(),
            file_size_limit: 2000000000,
        })
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
        let locator = DataLocator {
            data_file_id: 0,
            offset: 0,
        };
        assert!(self.entries.insert(hash1, locator).is_none());
        assert!(self.entries2.insert(hash2, locator).is_none());
        todo!();
    }

    pub fn finalize(mut self) -> Result<IO, io::Error> {
        self.index.write_all(&[0xff; 8])?;
        self.index.write_all(&[0; 4])?;
        self.index.write_all(&[0xff; 4])?;
        self.index.write_all(&[0; 240])?;

        self.index.seek(SeekFrom::Start(0))?;
        sqpack_header_finalize(&mut self.index_sqpack_header);
        self.index.write_all(&self.index_sqpack_header.0)?;
        index_segment_headers_finalize(&mut self.index_segment_headers);
        self.index.write_all(&self.index_segment_headers.buf)?;

        self.index2.seek(SeekFrom::Start(0))?;
        sqpack_header_finalize(&mut self.index2_sqpack_header);
        self.index2.write_all(&self.index2_sqpack_header.0)?;
        index_segment_headers_finalize(&mut self.index2_segment_headers);
        self.index2.write_all(&self.index2_segment_headers.buf)?;

        Ok(self.io)
    }
}
