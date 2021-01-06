pub mod parser;

#[derive(Debug, PartialEq, Eq)]
pub enum PlatformId {
    Win32 = 0,
    PS3 = 1,
    PS4 = 2,
}

#[derive(Debug, PartialEq, Eq)]
pub enum SqPackType {
    SQDB = 0,
    Data = 1,
    Index = 2,
}
