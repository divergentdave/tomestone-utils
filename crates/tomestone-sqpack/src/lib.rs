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
use sidetables::SideTables;

use crate::encoding::{PackSetWriter, RealPackIO};

mod compression;
pub mod encoding;
pub(crate) mod parser;
pub mod pathdb;
pub mod sidetables;

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
pub struct IndexSegmentHeader {
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

pub trait IndexHash: Clone + Copy + PartialEq + Eq + PartialOrd + Ord {
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
    type Hash: IndexHash;
    const SIZE: u32;
    const FILE_EXTENSION: &'static str;
    fn hash(&self) -> Self::Hash;
    fn pointer(&self) -> IndexPointer;
}

#[derive(Debug, Clone)]
pub struct IndexEntry1 {
    hash: IndexHash1,
    pointer: IndexPointer,
}

impl IndexEntry for IndexEntry1 {
    type Hash = IndexHash1;
    const SIZE: u32 = 16;
    const FILE_EXTENSION: &'static str = "index";

    fn hash(&self) -> Self::Hash {
        self.hash
    }

    fn pointer(&self) -> IndexPointer {
        self.pointer
    }
}

#[derive(Debug, Clone)]
pub struct IndexEntry2 {
    hash: IndexHash2,
    pointer: IndexPointer,
}

impl IndexEntry for IndexEntry2 {
    type Hash = IndexHash2;
    const SIZE: u32 = 8;
    const FILE_EXTENSION: &'static str = "index2";

    fn hash(&self) -> Self::Hash {
        self.hash
    }

    fn pointer(&self) -> IndexPointer {
        self.pointer
    }
}

/// Identifies the location and size of an entry in a data file with type zero. These are likely
/// placeholders for files that were previously deleted. Hypothetically, their purpose is to
/// reduce the size of binary patches when files are added and removed from SqPack files, and the
/// list of these entries in `.index` files, sorted by their size, provides an efficient way to
/// look up and reuse these spaces.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ZeroEntry {
    pub shifted_length: u32,
    pub pointer: FilePointer,
}

impl ZeroEntry {
    pub fn new(pointer: FilePointer, shifted_length: u32) -> ZeroEntry {
        ZeroEntry {
            pointer,
            shifted_length,
        }
    }
}

#[derive(Debug)]
pub struct Index<E: IndexEntry> {
    index_table: Vec<E>,
    collision_table: Vec<CollisionEntry<E::Hash>>,
    /// Note: it is expected this will be populated for `.index` files, and empty for `.index2`
    /// files.
    tombstone_table: Vec<ZeroEntry>,
}

impl<E: IndexEntry> Index<E> {
    pub(crate) fn new(
        index_table: Vec<E>,
        collision_table: Vec<CollisionEntry<E::Hash>>,
        tombstone_table: Vec<ZeroEntry>,
    ) -> Index<E> {
        Index {
            index_table,
            collision_table,
            tombstone_table,
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (E::Hash, FilePointer)> + '_ {
        IndexIter {
            iter: self.index_table.iter(),
            collision_table: &self.collision_table,
            collision_extra: None,
        }
    }

    pub fn get(&self, hash: &E::Hash) -> Option<&E> {
        if let Ok(index) = self
            .index_table
            .binary_search_by_key(hash, IndexEntry::hash)
        {
            Some(&self.index_table[index])
        } else {
            None
        }
    }

    pub fn lookup(&self, path: &str) -> Option<FilePointer> {
        let hash = E::Hash::hash(path);
        let pointer = if let Some(entry) = self.get(&hash) {
            entry.pointer()
        } else {
            return None;
        };
        match pointer {
            IndexPointer::Pointer(pointer) => Some(pointer),
            IndexPointer::Collision => {
                let path_lower = path.to_lowercase();
                let start_index = self
                    .collision_table
                    .partition_point(|entry| entry.hash < hash);
                for collision_entry in self.collision_table[start_index..].iter() {
                    if collision_entry.hash != hash {
                        break;
                    }
                    if collision_entry.path == path_lower {
                        return Some(collision_entry.pointer);
                    }
                }
                None
            }
        }
    }
}

impl Index<IndexEntry1> {
    pub fn contains_folder(&self, crc: &u32) -> bool {
        self.index_table
            .binary_search_by_key(crc, |e| e.hash().folder_crc)
            .is_ok()
    }
}

struct CollisionIterExtraData<'a, E: IndexEntry> {
    collision_table_iter: std::slice::Iter<'a, CollisionEntry<E::Hash>>,
    hash_to_match: E::Hash,
}

struct IndexIter<'a, E: IndexEntry> {
    iter: std::slice::Iter<'a, E>,
    collision_table: &'a [CollisionEntry<E::Hash>],
    collision_extra: Option<CollisionIterExtraData<'a, E>>,
}

impl<'a, E: IndexEntry> Iterator for IndexIter<'a, E> {
    type Item = (E::Hash, FilePointer);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(extra_data) = self.collision_extra.as_mut() {
                for collision_entry in &mut extra_data.collision_table_iter {
                    if collision_entry.hash == extra_data.hash_to_match {
                        return Some((collision_entry.hash, collision_entry.pointer));
                    }
                }
                self.collision_extra = None;
            }
            if let Some(entry) = self.iter.next() {
                match entry.pointer() {
                    IndexPointer::Pointer(pointer) => return Some((entry.hash(), pointer)),
                    IndexPointer::Collision => {
                        let hash = entry.hash();
                        self.collision_extra = Some(CollisionIterExtraData {
                            collision_table_iter: self.collision_table.iter(),
                            hash_to_match: hash,
                        });
                    }
                }
            } else {
                return None;
            }
        }
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
    Ex5 = 5,
}

impl Expansion {
    pub fn parse_name(name: &str) -> Result<Expansion, EnumParseError> {
        match name {
            "ffxiv" => Ok(Expansion::Base),
            "ex1" => Ok(Expansion::Ex1),
            "ex2" => Ok(Expansion::Ex2),
            "ex3" => Ok(Expansion::Ex3),
            "ex4" => Ok(Expansion::Ex4),
            "ex5" => Ok(Expansion::Ex5),
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
            5 => Ok(Expansion::Ex5),
            _ => Err(EnumParseError),
        }
    }

    pub fn iter_all() -> impl Iterator<Item = &'static Expansion> {
        const LIST: [Expansion; 6] = [
            Expansion::Base,
            Expansion::Ex1,
            Expansion::Ex2,
            Expansion::Ex3,
            Expansion::Ex4,
            Expansion::Ex5,
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
            Expansion::Ex5 => "ex5",
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

/// This represents a pointer to an entry in one of the data files. It consists of a number
/// identifying a data file in a given pack set, and an offset within that file. The file number
/// must be between 0 and 7, and the offset must be aligned to 128.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct FilePointer {
    data_file_id: u8,
    offset: u32,
}

impl FilePointer {
    pub fn new(data_file_id: u8, offset: u32) -> FilePointer {
        assert_eq!(offset & 0x7f, 0);
        assert!(data_file_id < 8);
        FilePointer {
            data_file_id,
            offset,
        }
    }

    pub fn data_file_id(&self) -> u8 {
        self.data_file_id
    }

    pub fn offset(&self) -> u32 {
        self.offset
    }

    pub fn from_u32(packed: u32) -> FilePointer {
        match IndexPointer::from_u32(packed) {
            IndexPointer::Pointer(pointer) => pointer,
            IndexPointer::Collision => panic!(
                "called FilePointer::from_u32 on a number with the collision tombstone bit set"
            ),
        }
    }
}

/// This represents a value stored in the file table of an index. It is either a pointer to a
/// file's entry in a .dat file, or a placeholder for multiple colliding files, in which case the
/// collision table must be scanned instead to find the file entry's location.
#[derive(Debug, Clone, Copy)]
pub enum IndexPointer {
    Pointer(FilePointer),
    Collision,
}

impl IndexPointer {
    pub fn from_u32(packed: u32) -> IndexPointer {
        if packed & 1 == 0 {
            IndexPointer::Pointer(FilePointer {
                data_file_id: ((packed & 0xf) >> 1).try_into().unwrap(),
                offset: (packed & !0xf) << 3,
            })
        } else {
            debug_assert!(packed == 1);
            IndexPointer::Collision
        }
    }

    pub fn to_u32(self) -> u32 {
        match self {
            IndexPointer::Pointer(ptr) => {
                ((ptr.data_file_id as u32 & 7) << 1) | ((ptr.offset >> 3) & !0xf)
            }
            IndexPointer::Collision => 1,
        }
    }
}

/// This holds an entry from an index file's collision table. The entire path is stored, for
/// disambiguation.
#[derive(Debug)]
pub struct CollisionEntry<H: IndexHash> {
    hash: H,
    pointer: FilePointer,
    _maybe_collision_index: u32,
    path: String,
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

    pub fn lookup_path_locator(
        &self,
        path: &str,
    ) -> Result<Option<(SqPackId, FilePointer)>, Error> {
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

        for id in self.iter_packs_category_expansion(category, expansion) {
            let index = self.get_index_2(&id).unwrap()?;
            if let Some(pointer) = index.lookup(path) {
                return Ok(Some((id, pointer)));
            }
        }
        Ok(None)
    }

    pub fn lookup_path_data(
        &self,
        data_file_set: &mut DataFileSet,
        path: &str,
    ) -> Result<Option<Vec<u8>>, Error> {
        if let Some((pack_id, file_pointer)) = self.lookup_path_locator(path)? {
            Ok(Some(data_file_set.fetch_data(pack_id, file_pointer)?))
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

    pub fn lookup_hash_1_locator(
        &self,
        hash: &IndexHash1,
    ) -> Result<Vec<(SqPackId, FilePointer)>, Error> {
        let mut pointers = Vec::new();
        for id in self.iter_packs() {
            let index = self.get_index_1(&id).unwrap()?;
            if let Some(entry) = index.get(hash) {
                match entry.pointer() {
                    IndexPointer::Pointer(pointer) => pointers.push((id, pointer)),
                    IndexPointer::Collision => todo!(),
                }
            }
        }
        Ok(pointers)
    }

    pub fn lookup_hash_2_locator(
        &self,
        hash: &IndexHash2,
    ) -> Result<Vec<(SqPackId, FilePointer)>, Error> {
        let mut pointers = Vec::new();
        for id in self.iter_packs() {
            let index = self.get_index_2(&id).unwrap()?;
            if let Some(entry) = index.get(hash) {
                match entry.pointer() {
                    IndexPointer::Pointer(pointer) => pointers.push((id, pointer)),
                    IndexPointer::Collision => todo!(),
                }
            }
        }
        Ok(pointers)
    }

    pub fn lookup_hash_1_data(
        &self,
        data_file_set: &mut DataFileSet,
        hash: &IndexHash1,
    ) -> Result<Vec<Vec<u8>>, Error> {
        self.lookup_hash_1_locator(hash)?
            .into_iter()
            .map(|(id, pointer)| data_file_set.fetch_data(id, pointer))
            .collect()
    }

    pub fn lookup_hash_2_data(
        &self,
        data_file_set: &mut DataFileSet,
        hash: &IndexHash2,
    ) -> Result<Vec<Vec<u8>>, Error> {
        self.lookup_hash_2_locator(hash)?
            .into_iter()
            .map(|(id, pointer)| data_file_set.fetch_data(id, pointer))
            .collect()
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

    pub fn data_files(&self) -> DataFileSet {
        DataFileSet::new(self.root_path.clone())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct DataFileKey {
    pack_id: SqPackId,
    dat_number: u8,
}

/// This provides access to `.dat?` files, and lazily caches open file handles, so they can be
/// reused. It is intended that each unit of parallelism should have its own `DataFileSet`.
pub struct DataFileSet {
    root_path: PathBuf,
    files: BTreeMap<DataFileKey, File>,
}

impl DataFileSet {
    fn new(root_path: PathBuf) -> DataFileSet {
        DataFileSet {
            root_path,
            files: BTreeMap::new(),
        }
    }

    fn build_data_path(root_path: &Path, id: SqPackId, dat_number: u8) -> PathBuf {
        root_path
            .join("game")
            .join("sqpack")
            .join(id.expansion.name())
            .join(format!(
                "{:02x}{:02x}{:02x}.win32.dat{}",
                id.category as u8, id.expansion as u8, id.number, dat_number,
            ))
    }

    pub fn open(&mut self, pack_id: SqPackId, dat_number: u8) -> Result<&mut File, io::Error> {
        let key = DataFileKey {
            pack_id,
            dat_number,
        };
        Ok(match self.files.entry(key) {
            std::collections::btree_map::Entry::Vacant(e) => e.insert(File::open(
                Self::build_data_path(&self.root_path, pack_id, dat_number),
            )?),
            std::collections::btree_map::Entry::Occupied(e) => e.into_mut(),
        })
    }

    pub fn fetch_data(
        &mut self,
        pack_id: SqPackId,
        file_pointer: FilePointer,
    ) -> Result<Vec<u8>, Error> {
        decompress_file(
            self.open(pack_id, file_pointer.data_file_id())?,
            file_pointer.offset(),
        )
    }

    pub fn iter_files<'a, I: IndexEntry>(
        &'a mut self,
        pack_id: SqPackId,
        index: &'a Index<I>,
    ) -> impl Iterator<Item = Result<(I::Hash, Vec<u8>), Error>> + 'a {
        let mut entries: Vec<_> = index.iter().collect();
        // Sort by file pointer to improve disk locality.
        entries.sort_unstable_by_key(|(_, pointer)| *pointer);
        entries.into_iter().map(move |(hash, pointer)| {
            Ok((
                hash,
                decompress_file(
                    self.open(pack_id, pointer.data_file_id())?,
                    pointer.offset(),
                )?,
            ))
        })
    }

    pub fn iter_files_both_hashes<'a>(
        &'a mut self,
        pack_id: SqPackId,
        index: &'a Index<IndexEntry1>,
        index2: &'a Index<IndexEntry2>,
    ) -> impl Iterator<Item = Result<(Option<IndexHash1>, Option<IndexHash2>, Vec<u8>), Error>> + 'a
    {
        let mut entries = BTreeMap::<FilePointer, (Option<IndexHash1>, Option<IndexHash2>)>::new();
        for (hash, pointer) in index.iter() {
            assert!(entries
                .entry(pointer)
                .or_default()
                .0
                .replace(hash)
                .is_none());
        }
        for (hash, pointer) in index2.iter() {
            assert!(entries
                .entry(pointer)
                .or_default()
                .1
                .replace(hash)
                .is_none());
        }
        entries.into_iter().map(move |(pointer, (hash1, hash2))| {
            Ok((
                hash1,
                hash2,
                decompress_file(
                    self.open(pack_id, pointer.data_file_id())?,
                    pointer.offset(),
                )?,
            ))
        })
    }

    pub fn max_dat_number(&self, pack_id: SqPackId) -> u8 {
        let mut number = 0;
        for i in 0u8.. {
            if Self::build_data_path(&self.root_path, pack_id, i).is_file() {
                number = i;
            } else {
                break;
            }
        }
        number
    }
}

pub enum PathOrHashes {
    Path(String),
    Hashes(IndexHash1, IndexHash2),
}

pub fn write_packs<
    PackIt: Iterator<Item = (SqPackId, FileIt)>,
    FileIt: Iterator<Item = (PathOrHashes, Vec<u8>)>,
>(
    base: PathBuf,
    platform_id: PlatformId,
    packs: PackIt,
    mut side_tables: BTreeMap<SqPackId, SideTables>,
) -> Result<(), Error> {
    for (pack_id, files) in packs {
        let io = RealPackIO::new(base.clone(), platform_id, pack_id)?;
        let mut writer = PackSetWriter::new(io, PlatformId::Win32, pack_id)?;
        if let Some(side_table) = side_tables.remove(&pack_id) {
            writer.set_side_table(side_table);
        }
        for (path_or_hashes, blob) in files {
            match path_or_hashes {
                PathOrHashes::Path(path) => writer.add_file(&path, &blob)?,
                PathOrHashes::Hashes(hash1, hash2) => {
                    writer.add_file_by_hashes(hash1, hash2, &blob)?
                }
            }
        }
        writer.finalize()?;
    }
    todo!()
}

#[cfg(test)]
mod tests {
    use std::{
        convert::TryInto,
        fs::File,
        io::{Cursor, Read, Seek, SeekFrom, Write},
        sync::{Arc, RwLock},
    };

    use tomestone_common::test_game_data_or_skip;

    use crate::{
        encoding::{PackIO, PackSetWriter, SetLen},
        sidetables::build_side_tables,
        Category, Expansion, GameData, IndexEntry1, IndexEntry2,
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
        assert_eq!(
            Expansion::parse_name(Expansion::Ex5.name()).unwrap(),
            Expansion::Ex5
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
        assert_eq!(
            Expansion::from_u8(Expansion::Ex5 as u8).unwrap(),
            Expansion::Ex5
        );
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

    impl Read for MockFile {
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
            self.inner.write().unwrap().read(buf)
        }
    }

    impl Write for MockFile {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.inner.write().unwrap().write(buf)
        }

        fn flush(&mut self) -> std::io::Result<()> {
            self.inner.write().unwrap().flush()
        }
    }

    impl Seek for MockFile {
        fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
            self.inner.write().unwrap().seek(pos)
        }
    }

    impl SetLen for MockFile {
        fn set_len(&self, size: u64) -> std::io::Result<()> {
            self.inner
                .write()
                .unwrap()
                .get_mut()
                .resize(size.try_into().unwrap(), 0);
            Ok(())
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

        fn open_dat_file(&mut self, number: u8) -> Result<Self::F, std::io::Error> {
            if number <= self.files.len().try_into().unwrap() {
                self.files
                    .resize_with((number + 1).try_into().unwrap(), || MockFile {
                        inner: Arc::new(RwLock::new(Cursor::new(Vec::new()))),
                    })
            }
            Ok(self.files[TryInto::<usize>::try_into(number).unwrap()].clone())
        }
    }

    const ROW_SIZE: usize = 8;
    const DUMP_LENGTH: usize = 256;

    fn print_hexdump_diff_inner(start_pos: usize, left: &[u8], right: &[u8]) {
        /*
          0xAAAAAAAA: XXXX XXXX XXXX XXXX ........ | XXXX XXXX XXXX XXXX ........n
          012345678901234567890123456789012345678901234567890123456789012345678901
        */
        let stdout = std::io::stdout();
        let mut locked = stdout.lock();
        let start_pos = start_pos / ROW_SIZE * ROW_SIZE;
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

    fn print_hexdump_diff(label: &str, left: &[u8], right: &[u8]) {
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
                return;
            }
        } else {
            println!(
                "{0} differs, and the first difference is at {1} (0x{1:0x})",
                label,
                first_difference_pos.unwrap()
            );
        }

        print_hexdump_diff_inner(first_difference_pos.unwrap(), left, right);

        // try to reconstruct header sizes, and possibly print a second hexdump diff after them.
        if left.len() < 16 {
            return;
        }
        let sqpack_header_size = u32::from_le_bytes(left[12..16].try_into().unwrap()) as usize;
        if left.len() < sqpack_header_size + 4 {
            return;
        }
        let dat_or_index_header_size = u32::from_le_bytes(
            left[sqpack_header_size..sqpack_header_size + 4]
                .try_into()
                .unwrap(),
        ) as usize;
        let headers_guess = sqpack_header_size + dat_or_index_header_size;
        if first_difference_pos.unwrap() < headers_guess {
            let next_difference_pos = left
                .iter()
                .zip(right.iter())
                .skip(headers_guess)
                .position(|(l, r)| *l != *r)
                .map(|pos| pos + headers_guess);
            if let Some(next_difference_pos) = next_difference_pos {
                println!(
                    "Next difference after headers (?) is at {0} (0x{0:0x})",
                    next_difference_pos
                );
                print_hexdump_diff_inner(next_difference_pos, left, right);
            }
        }
    }

    #[test]
    #[ignore = "slow test"]
    fn sqpack_data_round_trip() {
        let (game_data, _data_file_set) = test_game_data_or_skip!();

        for pack_id in game_data.iter_packs() {
            // TODOs
            if matches!(
                pack_id.category,
                Category::Common
                    | Category::BgCommon
                    | Category::Bg
                    | Category::Cut
                    | Category::Chara
                    | Category::Ui
                    | Category::Vfx
            ) {
                // Model and texture data entries aren't supported yet.
                continue;
            }
            if matches!(pack_id.category, Category::SqpackTest | Category::Debug) {
                // weird files, get an EOF error
                continue;
            }

            dbg!(pack_id);

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

            let index_1 = game_data.get_index_1(&pack_id).unwrap().unwrap();
            let index_2 = game_data.get_index_2(&pack_id).unwrap().unwrap();

            let mut data_file_set = game_data.data_files();

            let side_table = build_side_tables(&game_data, &mut data_file_set, pack_id);

            let mut original_max_dat_file_id = 0;
            for (_, pointer) in index_2.iter() {
                if pointer.data_file_id() > original_max_dat_file_id {
                    original_max_dat_file_id = pointer.data_file_id();
                }
            }

            let mocked_io = MockedPackIO::new();
            let mut writer =
                PackSetWriter::new(mocked_io, crate::PlatformId::Win32, pack_id).unwrap();
            writer.set_side_table(side_table);
            for res in data_file_set.iter_files_both_hashes(pack_id, index_1, index_2) {
                let (hash1, hash2, data) = res.unwrap();
                writer
                    .add_file_by_hashes(hash1.unwrap(), hash2.unwrap(), &data)
                    .unwrap();
            }
            let original_dat_file_count = original_max_dat_file_id + 1;
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
            let mut dat_matches = mocked_io.files.len() == original_dat_file_count.into();
            if !dat_matches {
                println!(
                    "Wrong number of .dat files, expected {}, got {}",
                    original_dat_file_count,
                    mocked_io.files.len()
                );
            }
            for (i, new) in mocked_io.files.iter().enumerate() {
                let guard = new.inner.read().unwrap();

                let original_file = data_file_set.open(pack_id, i.try_into().unwrap()).unwrap();
                original_file.seek(SeekFrom::Start(0)).unwrap();
                let mut original = Vec::with_capacity(guard.get_ref().len());
                original_file.read_to_end(&mut original).unwrap();

                dat_matches &= guard.get_ref() == &original;
                if guard.get_ref() != &original {
                    print_hexdump_diff(&format!(".dat{}", i), guard.get_ref(), &original);
                    break;
                }
            }
            assert!(index_matches && index2_matches && dat_matches)
        }
    }
}
