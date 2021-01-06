use std::{ffi::OsString, io, path::Path};

pub mod parser;

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
