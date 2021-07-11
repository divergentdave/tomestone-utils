use std::{
    collections::{BTreeMap, BTreeSet},
    convert::TryInto,
    fmt,
    fs::File,
    io,
    path::{Path, PathBuf},
};

use once_cell::sync::{Lazy, OnceCell};
use parser::{decompress_file, load_index_1, load_index_2};
use pathdb::DbError;
use regex::Regex;

mod compression;
mod parser;
pub mod pathdb;

pub(crate) const SHA1_OUTPUT_SIZE: usize = 20;

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Nom(nom::error::ErrorKind),
    Inflate(miniz_oxide::inflate::TINFLStatus),
    Db(DbError),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Io(e) => e.fmt(f),
            Error::Nom(e) => write!(f, "error: {:?}", e),
            Error::Inflate(e) => write!(f, "error: {:?}", e),
            Error::Db(e) => e.fmt(f),
        }
    }
}

impl std::error::Error for Error {}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Error {
        Error::Io(e)
    }
}

impl From<nom::error::ErrorKind> for Error {
    fn from(e: nom::error::ErrorKind) -> Error {
        Error::Nom(e)
    }
}

impl From<miniz_oxide::inflate::TINFLStatus> for Error {
    fn from(e: miniz_oxide::inflate::TINFLStatus) -> Error {
        Error::Inflate(e)
    }
}

impl From<DbError> for Error {
    fn from(e: DbError) -> Error {
        Error::Db(e)
    }
}

#[derive(Debug)]
pub struct EnumParseError;

#[derive(Debug, PartialEq, Eq)]
pub enum PlatformId {
    Win32 = 0,
    Ps3 = 1,
    Ps4 = 2,
}

impl PlatformId {
    pub(crate) fn from_u8(value: u8) -> Result<PlatformId, EnumParseError> {
        match value {
            0 => Ok(PlatformId::Win32),
            1 => Ok(PlatformId::Ps3),
            2 => Ok(PlatformId::Ps4),
            _ => Err(EnumParseError),
        }
    }
}

#[derive(Debug)]
pub(crate) struct SqPackTypeParseError;

#[derive(Debug, PartialEq, Eq)]
pub enum SqPackType {
    Sqdb = 0,
    Data = 1,
    Index = 2,
}

impl SqPackType {
    pub(crate) fn from_u32(value: u32) -> Result<SqPackType, SqPackTypeParseError> {
        match value {
            0 => Ok(SqPackType::Sqdb),
            1 => Ok(SqPackType::Data),
            2 => Ok(SqPackType::Index),
            _ => Err(SqPackTypeParseError),
        }
    }
}

#[derive(Debug)]
pub enum IndexType {
    Zero = 0,
    Files = 1,
    Two = 2,
    Three = 3,
    Four = 4,
    Five = 5,
    Six = 6,
}

impl IndexType {
    pub fn parse(value: u32) -> Option<IndexType> {
        match value {
            0 => Some(IndexType::Zero),
            1 => Some(IndexType::Files),
            2 => Some(IndexType::Two),
            3 => Some(IndexType::Three),
            4 => Some(IndexType::Four),
            5 => Some(IndexType::Five),
            6 => Some(IndexType::Six),
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

pub fn crc32(data: &[u8]) -> u32 {
    let mut hasher = crc32fast::Hasher::new();
    hasher.update(data);
    !hasher.finalize()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct IndexHash1 {
    pub folder_crc: u32,
    pub filename_crc: u32,
}

impl IndexHash1 {
    pub fn new(folder_crc: u32, filename_crc: u32) -> IndexHash1 {
        IndexHash1 {
            folder_crc,
            filename_crc,
        }
    }

    fn split_path(path: &str) -> (String, String) {
        if let Some(last_separator_pos) = path.rfind('/') {
            let folder_slice = &path[..last_separator_pos];
            let filename_slice = &path[last_separator_pos + 1..];
            (folder_slice.to_lowercase(), filename_slice.to_lowercase())
        } else {
            ("".to_string(), path.to_lowercase())
        }
    }
}

impl IndexHash for IndexHash1 {
    fn hash(path: &str) -> Self {
        let (folder, filename) = IndexHash1::split_path(path);
        IndexHash1 {
            folder_crc: crc32(folder.as_bytes()),
            filename_crc: crc32(filename.as_bytes()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct IndexHash2 {
    pub path_crc: u32,
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

pub trait IndexEntry: fmt::Debug + Clone {
    type Hash: PartialEq + Eq + PartialOrd + Ord;
    const SIZE: u32;
    const FILE_EXTENSION: &'static str;
    fn hash(&self) -> Self::Hash;
    fn data_location(&self) -> DataLocator;
}

#[derive(Debug, Clone)]
pub struct IndexEntry1 {
    hash: IndexHash1,
    data_locator: DataLocator,
}

impl IndexEntry for IndexEntry1 {
    type Hash = IndexHash1;
    const SIZE: u32 = 16;
    const FILE_EXTENSION: &'static str = "index";

    fn hash(&self) -> Self::Hash {
        self.hash
    }

    fn data_location(&self) -> DataLocator {
        self.data_locator
    }
}

#[derive(Debug, Clone)]
pub struct IndexEntry2 {
    hash: IndexHash2,
    data_locator: DataLocator,
}

impl IndexEntry for IndexEntry2 {
    type Hash = IndexHash2;
    const SIZE: u32 = 8;
    const FILE_EXTENSION: &'static str = "index2";

    fn hash(&self) -> Self::Hash {
        self.hash
    }

    fn data_location(&self) -> DataLocator {
        self.data_locator
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

    pub fn get(&self, hash: &E::Hash) -> Option<&E> {
        if let Ok(index) = self.table.binary_search_by_key(hash, IndexEntry::hash) {
            Some(&self.table[index])
        } else {
            None
        }
    }
}

impl Index<IndexEntry1> {
    pub fn contains_folder(&self, crc: &u32) -> bool {
        self.table
            .binary_search_by_key(crc, |e| e.hash().folder_crc)
            .is_ok()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
    pub fn parse_name(name: &str) -> Result<Category, EnumParseError> {
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
            _ => Err(EnumParseError),
        }
    }

    pub fn from_u8(value: u8) -> Result<Category, EnumParseError> {
        match value {
            0 => Ok(Category::Common),
            1 => Ok(Category::BgCommon),
            2 => Ok(Category::Bg),
            3 => Ok(Category::Cut),
            4 => Ok(Category::Chara),
            5 => Ok(Category::Shader),
            6 => Ok(Category::Ui),
            7 => Ok(Category::Sound),
            8 => Ok(Category::Vfx),
            9 => Ok(Category::UiScript),
            0xA => Ok(Category::Exd),
            0xB => Ok(Category::GameScript),
            0xC => Ok(Category::Music),
            0x12 => Ok(Category::SqpackTest),
            0x13 => Ok(Category::Debug),
            _ => Err(EnumParseError),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expansion {
    Base = 0,
    Ex1 = 1,
    Ex2 = 2,
    Ex3 = 3,
    Ex4 = 4,
}

impl Expansion {
    pub fn parse_name(name: &str) -> Result<Expansion, EnumParseError> {
        match name {
            "ffxiv" => Ok(Expansion::Base),
            "ex1" => Ok(Expansion::Ex1),
            "ex2" => Ok(Expansion::Ex2),
            "ex3" => Ok(Expansion::Ex3),
            "ex4" => Ok(Expansion::Ex4),
            _ => Err(EnumParseError),
        }
    }

    pub fn from_u8(value: u8) -> Result<Expansion, EnumParseError> {
        match value {
            0 => Ok(Expansion::Base),
            1 => Ok(Expansion::Ex1),
            2 => Ok(Expansion::Ex2),
            3 => Ok(Expansion::Ex3),
            4 => Ok(Expansion::Ex4),
            _ => Err(EnumParseError),
        }
    }

    pub fn iter_all() -> impl Iterator<Item = &'static Expansion> {
        const LIST: [Expansion; 5] = [
            Expansion::Base,
            Expansion::Ex1,
            Expansion::Ex2,
            Expansion::Ex3,
            Expansion::Ex4,
        ];
        LIST.iter()
    }

    pub fn name(&self) -> &'static str {
        match self {
            Expansion::Base => "ffxiv",
            Expansion::Ex1 => "ex1",
            Expansion::Ex2 => "ex2",
            Expansion::Ex3 => "ex3",
            Expansion::Ex4 => "ex4",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SqPackId {
    pub category: Category,
    pub expansion: Expansion,
    pub number: u8,
}

#[derive(Debug)]
pub enum DataBlocks {
    Empty,
    Unsupported,
    Binary {
        base_position: u32,
        blocks: Vec<(u32, u16, u16)>,
    },
    Model(),
    Texture(),
}

impl DataBlocks {
    pub fn all_blocks<'a>(&'a self) -> Box<dyn Iterator<Item = u32> + 'a> {
        match self {
            DataBlocks::Binary {
                base_position,
                blocks,
            } => {
                let base_position = *base_position;
                Box::new(blocks.iter().map(
                    move |(offset, _block_size, _decompressed_data_size)| base_position + *offset,
                ))
            }
            _ => Box::new(vec![].into_iter()), // TODO!
        }
    }
}

fn list_packs(root_path: &Path) -> io::Result<BTreeSet<SqPackId>> {
    static RE: Lazy<Regex> = Lazy::new(|| {
        Regex::new("^([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})\\.win32\\.index2?$").unwrap()
    });

    let sqpack_dir = root_path.join("game").join("sqpack");
    let mut ids = BTreeSet::new();
    for expansion in Expansion::iter_all() {
        let expansion_dir = sqpack_dir.join(expansion.name());
        if !expansion_dir.is_dir() {
            continue;
        }
        for entry in expansion_dir.read_dir()? {
            if let Ok(name) = entry?.file_name().into_string() {
                if let Some(caps) = RE.captures(&name) {
                    if let (Ok(category_num), Ok(expansion_num), Ok(number)) = (
                        u8::from_str_radix(caps.get(1).unwrap().as_str(), 16),
                        u8::from_str_radix(caps.get(2).unwrap().as_str(), 16),
                        u8::from_str_radix(caps.get(3).unwrap().as_str(), 16),
                    ) {
                        if let (Ok(category), Ok(expansion)) = (
                            Category::from_u8(category_num),
                            Expansion::from_u8(expansion_num),
                        ) {
                            let id = SqPackId {
                                category,
                                expansion,
                                number,
                            };
                            ids.insert(id);
                        }
                    }
                }
            }
        }
    }
    Ok(ids)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct DataLocator {
    pub data_file_id: u8,
    pub offset: u32,
}

impl DataLocator {
    fn from_u32(packed: u32) -> DataLocator {
        DataLocator {
            data_file_id: ((packed & 0xf) >> 1).try_into().unwrap(),
            offset: (packed & !0xf) << 3,
        }
    }
}

pub struct GameData {
    root_path: PathBuf,
    index_map_1: BTreeMap<SqPackId, OnceCell<Index<IndexEntry1>>>,
    index_map_2: BTreeMap<SqPackId, OnceCell<Index<IndexEntry2>>>,
}

impl GameData {
    pub fn new<P: AsRef<Path>>(path: P) -> io::Result<GameData> {
        let root_path = path.as_ref().to_owned();
        let ids = list_packs(&root_path)?;
        let mut index_map_1 = BTreeMap::new();
        let mut index_map_2 = BTreeMap::new();
        for id in ids {
            index_map_1.insert(id, OnceCell::new());
            index_map_2.insert(id, OnceCell::new());
        }
        Ok(GameData {
            root_path,
            index_map_1,
            index_map_2,
        })
    }

    fn build_index_path<I: IndexEntry>(&self, id: SqPackId) -> PathBuf {
        self.root_path
            .join("game")
            .join("sqpack")
            .join(id.expansion.name())
            .join(format!(
                "{:02x}{:02x}{:02x}.win32.{}",
                id.category as u8,
                id.expansion as u8,
                id.number,
                I::FILE_EXTENSION
            ))
    }

    fn build_data_path(&self, id: SqPackId, dat_number: u8) -> PathBuf {
        self.root_path
            .join("game")
            .join("sqpack")
            .join(id.expansion.name())
            .join(format!(
                "{:02x}{:02x}{:02x}.win32.dat{}",
                id.category as u8, id.expansion as u8, id.number, dat_number,
            ))
    }

    fn fetch_data(&self, pack_id: SqPackId, data_locator: DataLocator) -> Result<Vec<u8>, Error> {
        // TODO: reuse of opened files
        let path = self.build_data_path(pack_id, data_locator.data_file_id);
        let mut file = File::open(path)?;
        decompress_file(&mut file, data_locator.offset)
    }

    pub fn iter_files<'a, I: IndexEntry>(
        &'a self,
        pack_id: SqPackId,
        index: &'a Index<I>,
    ) -> Result<impl Iterator<Item = Result<(I::Hash, Vec<u8>), Error>> + 'a, Error> {
        let mut files = Vec::new();
        for i in 0.. {
            let path = self.build_data_path(pack_id, i);
            if path.is_file() {
                files.push(File::open(path)?);
            } else {
                break;
            }
        }
        let mut entries: Vec<I> = index.iter().cloned().collect();
        entries.sort_unstable_by_key(IndexEntry::data_location);
        Ok(entries.into_iter().map(move |entry| {
            let locator = entry.data_location();
            Ok((
                entry.hash(),
                decompress_file(
                    &mut files[TryInto::<usize>::try_into(locator.data_file_id).unwrap()],
                    locator.offset,
                )?,
            ))
        }))
    }

    pub fn lookup_path_locator(
        &self,
        path: &str,
    ) -> Result<Option<(SqPackId, DataLocator)>, Error> {
        let segments: Vec<_> = path.splitn(3, '/').collect();
        let category = if let Ok(category) = Category::parse_name(segments[0]) {
            category
        } else {
            return Ok(None);
        };
        let expansion = if let Some(segment) = segments.get(1) {
            if let Ok(expansion) = Expansion::parse_name(segment) {
                expansion
            } else {
                Expansion::Base
            }
        } else {
            Expansion::Base
        };

        let hash = IndexHash2::hash(path);

        for id in self.iter_packs_category_expansion(category, expansion) {
            let index = self.get_index_2(&id).unwrap()?;
            if let Some(entry) = index.get(&hash) {
                return Ok(Some((id, entry.data_location())));
            }
        }
        Ok(None)
    }

    pub fn lookup_path_data(&self, path: &str) -> Result<Option<Vec<u8>>, Error> {
        if let Some((pack_id, data_locator)) = self.lookup_path_locator(path)? {
            Ok(Some(self.fetch_data(pack_id, data_locator)?))
        } else {
            Ok(None)
        }
    }

    pub fn contains_folder(&self, path: &str) -> Result<bool, Error> {
        let segments: Vec<_> = path.splitn(3, '/').collect();
        let category = if let Ok(category) = Category::parse_name(segments[0]) {
            category
        } else {
            return Ok(false);
        };
        let expansion = if let Some(segment) = segments.get(1) {
            if let Ok(expansion) = Expansion::parse_name(segment) {
                expansion
            } else {
                Expansion::Base
            }
        } else {
            Expansion::Base
        };

        let crc = crc32(path.to_lowercase().as_bytes());

        for id in self.iter_packs_category_expansion(category, expansion) {
            if let Some(Ok(index)) = self.get_index_1(&id) {
                if index.contains_folder(&crc) {
                    return Ok(true);
                }
            }
        }
        Ok(false)
    }

    pub fn lookup_hash_1_data(&self, hash: &IndexHash1) -> Result<Option<Vec<u8>>, Error> {
        for id in self.iter_packs() {
            let index = self.get_index_1(&id).unwrap()?;
            if let Some(entry) = index.get(hash) {
                return Ok(Some(self.fetch_data(id, entry.data_location())?));
            }
        }
        Ok(None)
    }

    pub fn lookup_hash_2_data(&self, hash: &IndexHash2) -> Result<Option<Vec<u8>>, Error> {
        for id in self.iter_packs() {
            let index = self.get_index_2(&id).unwrap()?;
            if let Some(entry) = index.get(hash) {
                return Ok(Some(self.fetch_data(id, entry.data_location())?));
            }
        }
        Ok(None)
    }

    pub fn iter_packs(&self) -> impl Iterator<Item = SqPackId> + '_ {
        self.index_map_2.keys().copied()
    }

    pub fn iter_packs_category_expansion(
        &self,
        category: Category,
        expansion: Expansion,
    ) -> impl Iterator<Item = SqPackId> + '_ {
        self.index_map_2
            .range(
                SqPackId {
                    category,
                    expansion,
                    number: 0,
                }..=SqPackId {
                    category,
                    expansion,
                    number: 0xFF,
                },
            )
            .map(|(id, _)| *id)
    }

    pub fn get_index_1(&self, id: &SqPackId) -> Option<Result<&Index<IndexEntry1>, Error>> {
        self.index_map_1.get(id).map(|cell| {
            cell.get_or_try_init(|| -> Result<Index<IndexEntry1>, Error> {
                let path = self.build_index_path::<IndexEntry1>(*id);
                load_index_1(path)
            })
        })
    }

    pub fn get_index_2(&self, id: &SqPackId) -> Option<Result<&Index<IndexEntry2>, Error>> {
        self.index_map_2.get(id).map(|cell| {
            cell.get_or_try_init(|| {
                let path = self.build_index_path::<IndexEntry2>(*id);
                load_index_2(path)
            })
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::Expansion;

    #[test]
    fn expansion_round_trip() {
        assert_eq!(Expansion::parse_name("ffxiv").unwrap().name(), "ffxiv");
        assert_eq!(Expansion::parse_name("ex1").unwrap().name(), "ex1");
        assert_eq!(Expansion::parse_name("ex2").unwrap().name(), "ex2");
        assert_eq!(Expansion::parse_name("ex3").unwrap().name(), "ex3");

        assert_eq!(
            Expansion::parse_name(Expansion::Base.name()).unwrap(),
            Expansion::Base
        );
        assert_eq!(
            Expansion::parse_name(Expansion::Ex1.name()).unwrap(),
            Expansion::Ex1
        );
        assert_eq!(
            Expansion::parse_name(Expansion::Ex2.name()).unwrap(),
            Expansion::Ex2
        );
        assert_eq!(
            Expansion::parse_name(Expansion::Ex3.name()).unwrap(),
            Expansion::Ex3
        );
        assert_eq!(
            Expansion::parse_name(Expansion::Ex4.name()).unwrap(),
            Expansion::Ex4
        );

        assert_eq!(Expansion::from_u8(0).unwrap() as u8, 0);
        assert_eq!(Expansion::from_u8(1).unwrap() as u8, 1);
        assert_eq!(Expansion::from_u8(2).unwrap() as u8, 2);
        assert_eq!(Expansion::from_u8(3).unwrap() as u8, 3);

        assert_eq!(
            Expansion::from_u8(Expansion::Base as u8).unwrap(),
            Expansion::Base
        );
        assert_eq!(
            Expansion::from_u8(Expansion::Ex1 as u8).unwrap(),
            Expansion::Ex1
        );
        assert_eq!(
            Expansion::from_u8(Expansion::Ex2 as u8).unwrap(),
            Expansion::Ex2
        );
        assert_eq!(
            Expansion::from_u8(Expansion::Ex3 as u8).unwrap(),
            Expansion::Ex3
        );
        assert_eq!(
            Expansion::from_u8(Expansion::Ex4 as u8).unwrap(),
            Expansion::Ex4
        );
    }
}
