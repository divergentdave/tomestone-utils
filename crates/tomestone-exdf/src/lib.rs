use std::fmt;

pub mod parser;

#[derive(Debug)]
pub struct EnumParseError;

#[derive(Debug, PartialEq, Eq)]
pub enum Language {
    NoLanguage = 0,
    Japanese = 1,
    English = 2,
    German = 3,
    French = 4,
    ChineseSingapore = 5,
    ChineseTraditional = 6,
    Korean = 7,
}

impl Language {
    fn from_u16(value: u16) -> Result<Language, EnumParseError> {
        match value {
            0 => Ok(Language::NoLanguage),
            1 => Ok(Language::Japanese),
            2 => Ok(Language::English),
            3 => Ok(Language::German),
            4 => Ok(Language::French),
            5 => Ok(Language::ChineseSingapore),
            6 => Ok(Language::ChineseTraditional),
            7 => Ok(Language::Korean),
            _ => Err(EnumParseError),
        }
    }

    pub fn short_code(&self) -> Option<&'static str> {
        match self {
            Language::NoLanguage => None,
            Language::Japanese => Some("ja"),
            Language::English => Some("en"),
            Language::German => Some("de"),
            Language::French => Some("fr"),
            Language::ChineseSingapore => Some("cns"),
            Language::ChineseTraditional => Some("cnt"),
            Language::Korean => Some("ko"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ColumnFormat {
    String,
    Bool,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    Float,
    I16x4,
    Bitflag(u8),
}

impl ColumnFormat {
    pub fn from_u16(value: u16) -> Result<ColumnFormat, EnumParseError> {
        match value {
            0 => Ok(ColumnFormat::String),
            1 => Ok(ColumnFormat::Bool),
            2 => Ok(ColumnFormat::I8),
            3 => Ok(ColumnFormat::U8),
            4 => Ok(ColumnFormat::I16),
            5 => Ok(ColumnFormat::U16),
            6 => Ok(ColumnFormat::I32),
            7 => Ok(ColumnFormat::U32),
            9 => Ok(ColumnFormat::Float),
            0xb => Ok(ColumnFormat::I16x4),
            value if value >= 0x19 && value <= 0x20 => {
                Ok(ColumnFormat::Bitflag((value - 0x19) as u8))
            }
            _ => Err(EnumParseError),
        }
    }
}

pub enum Value<'a> {
    String(&'a [u8]),
    Bool(bool),
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    Float(f32),
    I16x4([i16; 4]),
    Bitflag(bool),
}

impl<'a> fmt::Debug for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::String(value) => String::from_utf8_lossy(value).fmt(f),
            Value::Bool(value) => value.fmt(f),
            Value::I8(value) => value.fmt(f),
            Value::U8(value) => value.fmt(f),
            Value::I16(value) => value.fmt(f),
            Value::U16(value) => value.fmt(f),
            Value::I32(value) => value.fmt(f),
            Value::U32(value) => value.fmt(f),
            Value::Float(value) => value.fmt(f),
            Value::I16x4(value) => value.fmt(f),
            Value::Bitflag(value) => value.fmt(f),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{EnumParseError, Language};

    #[test]
    fn language_round_trip() {
        assert!(matches!(
            Language::from_u16(Language::NoLanguage as u16),
            Ok(Language::NoLanguage)
        ));
        assert!(matches!(
            Language::from_u16(Language::Japanese as u16),
            Ok(Language::Japanese)
        ));
        assert!(matches!(
            Language::from_u16(Language::English as u16),
            Ok(Language::English)
        ));
        assert!(matches!(
            Language::from_u16(Language::German as u16),
            Ok(Language::German)
        ));
        assert!(matches!(
            Language::from_u16(Language::French as u16),
            Ok(Language::French)
        ));
        assert!(matches!(
            Language::from_u16(Language::ChineseSingapore as u16),
            Ok(Language::ChineseSingapore)
        ));
        assert!(matches!(
            Language::from_u16(Language::ChineseTraditional as u16),
            Ok(Language::ChineseTraditional)
        ));
        assert!(matches!(
            Language::from_u16(Language::Korean as u16),
            Ok(Language::Korean)
        ));
        assert!(matches!(Language::from_u16(65535), Err(EnumParseError)));
    }
}
