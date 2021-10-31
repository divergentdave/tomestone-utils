use std::{
    fs::File,
    io::{self, Seek, SeekFrom, Write},
    path::PathBuf,
};

use sha1::{Digest, Sha1};

#[allow(unused)]
use crate::compression::compress_sqpack_block;
use crate::{IndexHash, IndexHash1, IndexHash2, PlatformId, SqPackId};

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

pub struct PackSetWriter<IO: PackIO> {
    io: IO,
    index: IO::F,
    index_header: [u8; 1024],
    index2: IO::F,
    index2_header: [u8; 1024],
    dats: Vec<IO::F>,
}

fn sqpack_header_skeleton(platform_id: PlatformId) -> [u8; 1024] {
    let mut header = [0u8; 1024];
    header[..6].copy_from_slice(b"SqPack");
    header[8..12].copy_from_slice(&(platform_id as u32).to_le_bytes());
    header[12..16].copy_from_slice(&1024u32.to_le_bytes());
    header
}

fn sqpack_header_finalize(header: &mut [u8; 1024]) {
    let mut sha = Sha1::new();
    sha.update(&header[..0x3c0]);
    let hash = sha.finalize();
    header[0x3c0..0x3d4].copy_from_slice(&hash);
}

impl<IO: PackIO> PackSetWriter<IO> {
    pub fn new(mut io: IO, platform_id: PlatformId) -> Result<Self, io::Error> {
        let mut index = io.open_index_file()?;
        let index_header = sqpack_header_skeleton(platform_id);
        index.write_all(&index_header)?;
        let mut index2 = io.open_index2_file()?;
        let index2_header = sqpack_header_skeleton(platform_id);
        index2.write_all(&index2_header)?;
        Ok(PackSetWriter {
            io,
            index,
            index_header,
            index2,
            index2_header,
            dats: Vec::new(),
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
        todo!();
    }

    pub fn finalize(mut self) -> Result<IO, io::Error> {
        sqpack_header_finalize(&mut self.index_header);
        self.index.seek(SeekFrom::Start(0))?;
        self.index.write_all(&self.index_header)?;

        sqpack_header_finalize(&mut self.index2_header);
        self.index2.seek(SeekFrom::Start(0))?;
        self.index2.write_all(&self.index2_header)?;

        Ok(self.io)
    }
}
