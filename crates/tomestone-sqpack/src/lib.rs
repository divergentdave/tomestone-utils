use once_cell::sync::OnceCell;
use std::{
    collections::hash_map::HashMap,
    convert::TryInto,
    ffi::OsString,
    io,
    path::{Path, PathBuf},
};

pub mod compression;
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

#[derive(Debug)]
pub struct DataHeader {
    pub data_size: u64,
    pub spanned_dat: u32,
    pub max_file_size: u32,
}

pub trait IndexHash {
    fn hash(path: &str) -> Self;
}

fn crc32(data: &[u8]) -> u32 {
    let mut hasher = crc32fast::Hasher::new();
    hasher.update(data);
    hasher.finalize()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct IndexHash1 {
    folder_crc: u32,
    filename_crc: u32,
}

impl IndexHash1 {
    pub fn new(folder_crc: u32, filename_crc: u32) -> IndexHash1 {
        IndexHash1 {
            folder_crc,
            filename_crc,
        }
    }
}

impl IndexHash for IndexHash1 {
    fn hash(path: &str) -> Self {
        let (folder, filename) = if let Some(last_separator_pos) = path.rfind('/') {
            let (folder_slice, filename_slice) = path.split_at(last_separator_pos + 1);
            (folder_slice.to_lowercase(), filename_slice.to_lowercase())
        } else {
            ("".to_string(), path.to_lowercase())
        };
        IndexHash1 {
            folder_crc: crc32(folder.as_bytes()),
            filename_crc: crc32(filename.as_bytes()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct IndexHash2 {
    path_crc: u32,
}

impl IndexHash2 {
    pub fn new(path_crc: u32) -> IndexHash2 {
        IndexHash2 { path_crc }
    }
}

impl IndexHash for IndexHash2 {
    fn hash(path: &str) -> Self {
        IndexHash2 {
            path_crc: crc32(path.to_lowercase().as_bytes()),
        }
    }
}

pub trait IndexEntry {
    type Hash: PartialEq + Eq + PartialOrd + Ord;
    const SIZE: u32;
    fn hash(&self) -> Self::Hash;
    fn data_location(&self) -> (u8, u32);
}

#[derive(Debug)]
pub struct IndexEntry1 {
    hash: IndexHash1,
    data_file_id: u8,
    offset: u32,
}

impl IndexEntry for IndexEntry1 {
    type Hash = IndexHash1;
    const SIZE: u32 = 16;

    fn hash(&self) -> Self::Hash {
        self.hash
    }

    fn data_location(&self) -> (u8, u32) {
        (self.data_file_id, self.offset)
    }
}

#[derive(Debug)]
pub struct IndexEntry2 {
    hash: IndexHash2,
    data_file_id: u8,
    offset: u32,
}

impl IndexEntry for IndexEntry2 {
    type Hash = IndexHash2;
    const SIZE: u32 = 8;

    fn hash(&self) -> Self::Hash {
        self.hash
    }

    fn data_location(&self) -> (u8, u32) {
        (self.data_file_id, self.offset)
    }
}

#[derive(Debug)]
pub struct Index<E: IndexEntry> {
    table: Vec<E>,
}

impl<E: IndexEntry> Index<E> {
    pub(crate) fn new(table: Vec<E>) -> Index<E> {
        Index { table }
    }

    pub fn iter(&self) -> impl Iterator<Item = &E> {
        self.table.iter()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Category {
    Common = 0,
    BgCommon = 1,
    Bg = 2,
    Cut = 3,
    Chara = 4,
    Shader = 5,
    Ui = 6,
    Sound = 7,
    Vfx = 8,
    UiScript = 9,
    Exd = 0xA,
    GameScript = 0xB,
    Music = 0xC,
    SqpackTest = 0x12,
    Debug = 0x13,
}

impl Category {
    pub fn parse_name(name: &str) -> Result<Category, ()> {
        match name {
            "common" => Ok(Category::Common),
            "bgcommon" => Ok(Category::BgCommon),
            "bg" => Ok(Category::Bg),
            "cut" => Ok(Category::Cut),
            "chara" => Ok(Category::Chara),
            "shader" => Ok(Category::Shader),
            "ui" => Ok(Category::Ui),
            "sound" => Ok(Category::Sound),
            "vfx" => Ok(Category::Vfx),
            "ui_script" => Ok(Category::UiScript),
            "exd" => Ok(Category::Exd),
            "game_script" => Ok(Category::GameScript),
            "music" => Ok(Category::Music),
            "sqpack_test" => Ok(Category::SqpackTest),
            "debug" => Ok(Category::Debug),
            _ => Err(()),
        }
    }

    pub fn iter_all() -> impl Iterator<Item = &'static Category> {
        const LIST: [Category; 15] = [
            Category::Common,
            Category::BgCommon,
            Category::Bg,
            Category::Cut,
            Category::Chara,
            Category::Shader,
            Category::Ui,
            Category::Sound,
            Category::Vfx,
            Category::UiScript,
            Category::Exd,
            Category::GameScript,
            Category::Music,
            Category::SqpackTest,
            Category::Debug,
        ];
        LIST.iter()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expansion {
    Base = 0,
    Ex1 = 1,
    Ex2 = 2,
    Ex3 = 3,
}

impl Expansion {
    pub fn parse_name(name: &str) -> Result<Expansion, ()> {
        match name {
            "ffxiv" => Ok(Expansion::Base),
            "ex1" => Ok(Expansion::Ex1),
            "ex2" => Ok(Expansion::Ex2),
            "ex3" => Ok(Expansion::Ex3),
            _ => Err(()),
        }
    }

    pub fn iter_all() -> impl Iterator<Item = &'static Expansion> {
        const LIST: [Expansion; 4] = [
            Expansion::Base,
            Expansion::Ex1,
            Expansion::Ex2,
            Expansion::Ex3,
        ];
        LIST.iter()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SqPackId {
    category: Category,
    expansion: Expansion,
    number: u8,
}

#[derive(Debug)]
pub enum DataBlocks {
    Empty,
    Unsupported,
    Binary {
        base_position: usize,
        blocks: Vec<(u32, u16, u16)>,
    },
    Model(),
    Texture(),
}

impl DataBlocks {
    pub fn all_blocks<'a>(&'a self) -> Box<dyn Iterator<Item = usize> + 'a> {
        match self {
            DataBlocks::Binary {
                base_position,
                blocks,
            } => {
                let base_position = *base_position;
                Box::new(blocks.iter().map(
                    move |(offset, _block_size, _decompressed_data_size)| {
                        base_position + TryInto::<usize>::try_into(*offset).unwrap()
                    },
                ))
            }
            _ => Box::new(vec![].into_iter()),
        }
    }
}

pub struct GameData {
    root_path: PathBuf,
    repositories: Vec<OsString>,
    index_map: HashMap<SqPackId, OnceCell<Index<IndexEntry2>>>,
    decompressed_map: HashMap<SqPackId, OnceCell<()>>,
}

impl GameData {
    pub fn new<P: AsRef<Path>>(path: P) -> std::io::Result<GameData> {
        let root_path = path.as_ref().to_owned();
        let repositories = list_repositories(&root_path)?;
        Ok(GameData {
            root_path,
            repositories,
            index_map: HashMap::new(),
            decompressed_map: HashMap::new(),
        })
    }

    pub fn lookup(&self, path: &str) {
        todo!()
    }
}
