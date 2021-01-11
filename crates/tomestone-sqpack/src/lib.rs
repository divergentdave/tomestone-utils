use std::{
    ffi::OsString,
    io,
    path::{Path, PathBuf},
};

pub mod parser;

pub(crate) const SHA1_OUTPUT_SIZE: usize = 20;

#[derive(Debug)]
pub(crate) struct PlatformIdParseError;

#[derive(Debug, PartialEq, Eq)]
pub enum PlatformId {
    Win32 = 0,
    PS3 = 1,
    PS4 = 2,
}

impl PlatformId {
    pub(crate) fn from_u8(value: u8) -> Result<PlatformId, PlatformIdParseError> {
        match value {
            0 => Ok(PlatformId::Win32),
            1 => Ok(PlatformId::PS3),
            2 => Ok(PlatformId::PS4),
            _ => Err(PlatformIdParseError),
        }
    }
}

#[derive(Debug)]
pub(crate) struct SqPackTypeParseError;

#[derive(Debug, PartialEq, Eq)]
pub enum SqPackType {
    SQDB = 0,
    Data = 1,
    Index = 2,
}

impl SqPackType {
    pub(crate) fn from_u32(value: u32) -> Result<SqPackType, SqPackTypeParseError> {
        match value {
            0 => Ok(SqPackType::SQDB),
            1 => Ok(SqPackType::Data),
            2 => Ok(SqPackType::Index),
            _ => Err(SqPackTypeParseError),
        }
    }
}

pub fn list_repositories<P: AsRef<Path>>(root: P) -> Result<Vec<OsString>, io::Error> {
    let sqpack_dir = root.as_ref().join("game").join("sqpack");
    sqpack_dir
        .read_dir()?
        .filter_map(|res| {
            let entry = match res {
                Ok(entry) => entry,
                Err(e) => return Some(Err(e)),
            };
            let file_type = match entry.file_type() {
                Ok(file_type) => file_type,
                Err(e) => return Some(Err(e)),
            };
            if file_type.is_dir() {
                Some(Ok(entry.file_name()))
            } else {
                None
            }
        })
        .collect()
}

#[derive(Debug)]
pub enum IndexType {
    ZERO = 0,
    FILES = 1,
    TWO = 2,
    THREE = 3,
    FOUR = 4,
    FIVE = 5,
}

impl IndexType {
    pub fn parse(value: u32) -> Option<IndexType> {
        match value {
            0 => Some(IndexType::ZERO),
            1 => Some(IndexType::FILES),
            2 => Some(IndexType::TWO),
            3 => Some(IndexType::THREE),
            4 => Some(IndexType::FOUR),
            5 => Some(IndexType::FIVE),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct IndexSegmentHeader {
    pub index_type: IndexType,
    pub offset: u32,
    pub size: u32,
    pub hash: [u8; SHA1_OUTPUT_SIZE],
}

pub trait IndexEntry {
    type Hash: PartialOrd + Ord + PartialEq + Eq;
    const SIZE: u32;
    fn hash(&self) -> Self::Hash;
    fn data_location(&self) -> (u8, u32);
}

#[derive(Debug)]
pub struct IndexEntry1 {
    filename_crc: u32,
    folder_crc: u32,
    data_file_id: u8,
    offset: u32,
}

impl IndexEntry for IndexEntry1 {
    type Hash = (u32, u32);
    const SIZE: u32 = 16;

    fn hash(&self) -> Self::Hash {
        (self.folder_crc, self.filename_crc)
    }

    fn data_location(&self) -> (u8, u32) {
        (self.data_file_id, self.offset)
    }
}

#[derive(Debug)]
pub struct IndexEntry2 {
    path_crc: u32,
    data_file_id: u8,
    offset: u32,
}

impl IndexEntry for IndexEntry2 {
    type Hash = u32;
    const SIZE: u32 = 8;

    fn hash(&self) -> Self::Hash {
        self.path_crc
    }

    fn data_location(&self) -> (u8, u32) {
        (self.data_file_id, self.offset)
    }
}

#[derive(Debug)]
pub struct Index<E: IndexEntry> {
    repository: String,
    path: PathBuf,
    table: Vec<E>,
}
