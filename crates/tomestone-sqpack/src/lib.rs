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

use crate::encoding::{PackSetWriter, RealPackIO};

mod compression;
mod encoding;
pub(crate) mod parser;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

pub fn write_packs<I: Iterator<Item = (SqPackId, Vec<u8>)>>(
    base: PathBuf,
    platform_id: PlatformId,
    packs: I,
) -> Result<(), Error> {
    for (pack_id, blob) in packs {
        let io = RealPackIO::new(base.clone(), platform_id, pack_id)?;
        let mut writer = PackSetWriter::new(io, PlatformId::Win32)?;
        writer.add_file("todo", &blob)?;
        writer.finalize()?;
    }
    todo!()
}

#[cfg(test)]
mod tests {
    use std::{
        collections::BTreeMap,
        convert::TryInto,
        fs::File,
        io::{Cursor, Read, Seek, Write},
        sync::{Arc, RwLock},
    };

    use nom::{
        bytes::streaming::{tag, take},
        combinator::{map, peek, verify},
        multi::length_value,
        number::streaming::le_u32,
        sequence::{pair, tuple},
        IResult,
    };
    use sha1::{Digest, Sha1};
    use tomestone_common::null_padding;

    use crate::{
        encoding::{PackIO, PackSetWriter},
        parser::{
            drive_streaming_parser, integrity_checked_header, sqpack_header_outer,
            GrowableBufReader,
        },
        DataLocator, Expansion, GameData, IndexEntry, IndexEntry1, IndexEntry2, IndexHash1,
        IndexHash2,
    };

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

    fn data_header_inner(input: &[u8]) -> IResult<&[u8], (u32, u64, u32, u32, [u8; 20])> {
        map(
            tuple((
                le_u32,
                null_padding(4),
                tag(b"\x10\x00\x00\x00"),
                le_u32,
                le_u32,
                null_padding(4),
                le_u32,
                null_padding(4),
                take(20usize),
            )),
            |(size, _, _, data_size, dat_number, _, max_file_size, _, data_hash)| {
                (
                    size,
                    (data_size as u64) * 128,
                    dat_number,
                    max_file_size,
                    data_hash.try_into().unwrap(),
                )
            },
        )(input)
    }

    fn data_header_outer(input: &[u8]) -> IResult<&[u8], (u32, u64, u32, u32, [u8; 20])> {
        integrity_checked_header(
            input,
            map(le_u32, |size| size.try_into().unwrap()),
            data_header_inner,
        )
    }

    /// A parser function that performs a strict validation of a .dat0/.dat1/etc. file. In normal
    /// operation, dat files are only read one data entry at a time, making use of the indexes, and
    /// the headers are entirely ignored. For testing purposes, this reads and checks all headers
    /// in the file, and returns nothing.
    fn validate_data_file(input: &[u8]) -> IResult<&[u8], ()> {
        map(
            verify(
                pair(
                    sqpack_header_outer,
                    length_value(
                        peek(map(data_header_outer, |data_header| {
                            data_header.0 as u64 + data_header.1
                        })),
                        pair(data_header_outer, |input| Ok((&b""[..], input))),
                    ),
                ),
                |(_sqpack_header, (data_header, data))| {
                    let hash_from_header = data_header.4;
                    let mut hash = Sha1::new();
                    hash.update(data);
                    let hash_value = &*hash.finalize();

                    if hash_from_header == [0; 20] {
                        // special case for 130000.win32.dat0
                        true
                    } else if hash_value == hash_from_header {
                        true
                    } else {
                        false
                    }
                },
            ),
            |_| (),
        )(input)
    }

    #[test]
    #[ignore = "slow test"]
    fn sqpack_dat_validation() {
        dotenv::dotenv().ok();
        // Don't test anything if the game directory isn't provided
        let root = if let Ok(root) = std::env::var("FFXIV_INSTALL_DIR") {
            root
        } else {
            return;
        };
        let game_data = GameData::new(root).unwrap();

        for pack_id in game_data.iter_packs() {
            for i in 0.. {
                let path = game_data.build_data_path(pack_id, i);
                if path.is_file() {
                    let file = File::open(path).unwrap();
                    let mut reader = GrowableBufReader::new(file);
                    drive_streaming_parser(&mut reader, validate_data_file).unwrap();
                } else {
                    break;
                }
            }
        }
    }

    #[derive(Clone)]
    struct MockFile {
        inner: Arc<RwLock<Cursor<Vec<u8>>>>,
    }

    impl MockFile {
        fn new() -> MockFile {
            MockFile {
                inner: Arc::new(RwLock::new(Cursor::new(Vec::new()))),
            }
        }
    }

    impl<'a> Write for MockFile {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.inner.write().unwrap().write(buf)
        }

        fn flush(&mut self) -> std::io::Result<()> {
            self.inner.write().unwrap().flush()
        }
    }

    impl<'a> Seek for MockFile {
        fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
            self.inner.write().unwrap().seek(pos)
        }
    }

    struct MockedPackIO {
        index: MockFile,
        index2: MockFile,
        files: Vec<MockFile>,
    }

    impl MockedPackIO {
        fn new() -> MockedPackIO {
            MockedPackIO {
                index: MockFile::new(),
                index2: MockFile::new(),
                files: Vec::new(),
            }
        }
    }

    impl PackIO for MockedPackIO {
        type F = MockFile;

        fn open_index_file(&mut self) -> Result<Self::F, std::io::Error> {
            Ok(self.index.clone())
        }

        fn open_index2_file(&mut self) -> Result<Self::F, std::io::Error> {
            Ok(self.index2.clone())
        }

        fn open_dat_file(&mut self, number: u32) -> Result<Self::F, std::io::Error> {
            if number <= self.files.len().try_into().unwrap() {
                self.files
                    .resize_with((number + 1).try_into().unwrap(), || MockFile {
                        inner: Arc::new(RwLock::new(Cursor::new(Vec::new()))),
                    })
            }
            Ok(self.files[TryInto::<usize>::try_into(number).unwrap()].clone())
        }
    }

    fn print_hexdump_diff(label: &str, left: &[u8], right: &[u8]) {
        const ROW_SIZE: usize = 8;
        const DUMP_LENGTH: usize = 256;

        assert_ne!(left, right);

        let first_difference_pos = left.iter().zip(right.iter()).position(|(l, r)| *l != *r);
        if left.len() != right.len() {
            if let Some(first_difference_pos) = first_difference_pos {
                println!(
                    "{0} differs in length, and the first other difference is at {1} (0x{1:0x})",
                    label, first_difference_pos
                );
            } else {
                println!(
                    "{} differs in length, one file is truncated ({} vs. {})",
                    label,
                    left.len(),
                    right.len()
                );
            }
        } else {
            println!(
                "{0} differs, and the first difference is at {1} (0x{1:0x})",
                label,
                first_difference_pos.unwrap()
            );
        }

        /*
          0xAAAAAAAA: XXXX XXXX XXXX XXXX ........ | XXXX XXXX XXXX XXXX ........n
          012345678901234567890123456789012345678901234567890123456789012345678901
        */
        let stdout = std::io::stdout();
        let mut locked = stdout.lock();
        let start_pos = first_difference_pos.unwrap() / ROW_SIZE * ROW_SIZE;
        let mut hex_buf = [0u8; 16];
        let mut address_buf = [0u8; 8];
        let mut line_buf = [b' '; 72];
        line_buf[0] = b'0';
        line_buf[1] = b'x';
        line_buf[10] = b':';
        line_buf[71] = b'\n';
        let mut address = start_pos;
        for (left_chunk, right_chunk) in left[start_pos..]
            .chunks(ROW_SIZE)
            .zip(right[start_pos..].chunks(ROW_SIZE))
            .take(DUMP_LENGTH / ROW_SIZE)
        {
            hex::encode_to_slice(
                TryInto::<u32>::try_into(address).unwrap().to_be_bytes(),
                &mut address_buf,
            )
            .unwrap();
            line_buf[2..10].copy_from_slice(&address_buf);
            address += ROW_SIZE;

            hex::encode_to_slice(left_chunk, &mut hex_buf[..left_chunk.len() * 2]).unwrap();
            if left_chunk.len() * 2 < hex_buf.len() {
                hex_buf[left_chunk.len() * 2..].fill(b' ');
            }
            line_buf[12..16].copy_from_slice(&hex_buf[..4]);
            line_buf[17..21].copy_from_slice(&hex_buf[4..8]);
            line_buf[22..26].copy_from_slice(&hex_buf[8..12]);
            line_buf[27..31].copy_from_slice(&hex_buf[12..]);
            for (src, dest) in left_chunk.iter().zip(line_buf[32..40].iter_mut()) {
                if *src >= 0x20 && *src < 0x7f {
                    *dest = *src;
                } else {
                    *dest = b'.';
                }
            }
            for byte in line_buf[32 + left_chunk.len()..40].iter_mut() {
                *byte = b' ';
            }

            hex::encode_to_slice(right_chunk, &mut hex_buf[..right_chunk.len() * 2]).unwrap();
            if right_chunk.len() * 2 < hex_buf.len() {
                hex_buf[right_chunk.len() * 2..].fill(b' ');
            }
            line_buf[43..47].copy_from_slice(&hex_buf[..4]);
            line_buf[48..52].copy_from_slice(&hex_buf[4..8]);
            line_buf[53..57].copy_from_slice(&hex_buf[8..12]);
            line_buf[58..62].copy_from_slice(&hex_buf[12..]);
            for (src, dest) in right_chunk.iter().zip(line_buf[63..71].iter_mut()) {
                if *src >= 0x20 && *src < 0x7f {
                    *dest = *src;
                } else {
                    *dest = b'.';
                }
            }
            for byte in line_buf[63 + right_chunk.len()..71].iter_mut() {
                *byte = b' ';
            }

            locked.write_all(&line_buf).unwrap();
        }
    }

    #[test]
    #[ignore = "slow test"]
    fn sqpack_data_round_trip() {
        dotenv::dotenv().ok();
        // Don't test anything if the game directory isn't provided
        let root = if let Ok(root) = std::env::var("FFXIV_INSTALL_DIR") {
            root
        } else {
            return;
        };
        let game_data = GameData::new(root).unwrap();

        for pack_id in game_data.iter_packs() {
            let mut original_index_file = Vec::new();
            let path = game_data.build_index_path::<IndexEntry1>(pack_id);
            File::open(path)
                .unwrap()
                .read_to_end(&mut original_index_file)
                .unwrap();
            let mut original_index2_file = Vec::new();
            let path = game_data.build_index_path::<IndexEntry2>(pack_id);
            File::open(path)
                .unwrap()
                .read_to_end(&mut original_index2_file)
                .unwrap();
            let mut original_dat_files = Vec::new();
            for i in 0.. {
                let path = game_data.build_data_path(pack_id, i);
                if path.is_file() {
                    let mut file_data = Vec::new();
                    File::open(path)
                        .unwrap()
                        .read_to_end(&mut file_data)
                        .unwrap();
                    original_dat_files.push(file_data);
                } else {
                    break;
                }
            }

            // TODOs
            if original_dat_files.len() > 1 {
                continue;
            }
            if original_dat_files.len() > 0 && original_dat_files[0].len() != 2048 {
                continue;
            }
            // next shortest is 2083456!
            dbg!(pack_id);

            let mut original_entries: BTreeMap<
                DataLocator,
                (Option<IndexHash1>, Option<IndexHash2>),
            > = BTreeMap::new();
            let index_1 = game_data.get_index_1(&pack_id).unwrap().unwrap();
            for index_entry in index_1.iter() {
                let locator = index_entry.data_location();
                assert!(original_entries
                    .entry(locator)
                    .or_default()
                    .0
                    .replace(index_entry.hash())
                    .is_none());
            }
            let index_2 = game_data.get_index_2(&pack_id).unwrap().unwrap();
            for index_entry in index_2.iter() {
                let locator = index_entry.data_location();
                assert!(original_entries
                    .entry(locator)
                    .or_default()
                    .1
                    .replace(index_entry.hash())
                    .is_none());
            }

            // iter_files returns entries in the order of their location within the data file.
            let all_files = game_data.iter_files(pack_id, index_2).unwrap().map(|res| {
                let (hash, data) = res.unwrap();
                (index_2.get(&hash).unwrap().data_location(), data)
            });
            let mocked_io = MockedPackIO::new();
            let mut writer = PackSetWriter::new(mocked_io, crate::PlatformId::Win32).unwrap();
            for (locator, data) in all_files {
                let hashes = original_entries.get(&locator).unwrap();
                let hash1 = hashes.0.unwrap();
                let hash2 = hashes.1.unwrap();
                writer.add_file_by_hashes(hash1, hash2, &data).unwrap();
            }
            let mocked_io = writer.finalize().unwrap();

            let index_matches =
                mocked_io.index.inner.read().unwrap().get_ref() == &original_index_file;
            let index2_matches =
                mocked_io.index2.inner.read().unwrap().get_ref() == &original_index2_file;
            if !index_matches {
                print_hexdump_diff(
                    ".index",
                    mocked_io.index.inner.read().unwrap().get_ref(),
                    &original_index_file,
                );
            } else if !index2_matches {
                print_hexdump_diff(
                    ".index2",
                    mocked_io.index2.inner.read().unwrap().get_ref(),
                    &original_index2_file,
                );
            }
            let mut dat_matches = mocked_io.files.len() == original_dat_files.len();
            for (i, (new, original)) in mocked_io
                .files
                .iter()
                .zip(original_dat_files.iter())
                .enumerate()
            {
                let guard = new.inner.read().unwrap();
                dat_matches &= guard.get_ref() == original;
                if guard.get_ref() != original {
                    print_hexdump_diff(
                        &format!(".dat{}", i),
                        new.inner.read().unwrap().get_ref(),
                        original,
                    );
                    break;
                }
            }
            assert!(index_matches && index2_matches && dat_matches)
        }
    }
}
