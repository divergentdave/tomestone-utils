use std::{fmt, str::FromStr, string::FromUtf8Error};

use parser::{
    exdf::{Exdf, ExdfIterator},
    exhf::{parse_exhf, Exhf},
    parse_row,
};
use tomestone_sqpack::GameData;

pub mod parser;

#[derive(Debug)]
pub struct EnumParseError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Language {
    Japanese = 1,
    English = 2,
    German = 3,
    French = 4,
    ChineseSimplified = 5,
    ChineseTraditional = 6,
    Korean = 7,
}

impl Language {
    fn from_u16(value: u16) -> Result<Option<Language>, EnumParseError> {
        match value {
            0 => Ok(None),
            1 => Ok(Some(Language::Japanese)),
            2 => Ok(Some(Language::English)),
            3 => Ok(Some(Language::German)),
            4 => Ok(Some(Language::French)),
            5 => Ok(Some(Language::ChineseSimplified)),
            6 => Ok(Some(Language::ChineseTraditional)),
            7 => Ok(Some(Language::Korean)),
            _ => Err(EnumParseError),
        }
    }

    pub fn short_code(&self) -> &'static str {
        match self {
            Language::Japanese => "ja",
            Language::English => "en",
            Language::German => "de",
            Language::French => "fr",
            Language::ChineseSimplified => "cns",
            Language::ChineseTraditional => "cnt",
            Language::Korean => "ko",
        }
    }
}

impl FromStr for Language {
    type Err = EnumParseError;

    fn from_str(s: &str) -> Result<Self, EnumParseError> {
        match s {
            "ja" => Ok(Language::Japanese),
            "en" => Ok(Language::English),
            "de" => Ok(Language::German),
            "fr" => Ok(Language::French),
            "cns" => Ok(Language::ChineseSimplified),
            "cnt" => Ok(Language::ChineseTraditional),
            "ko" => Ok(Language::Korean),
            _ => Err(EnumParseError),
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
            0x19..=0x20 => Ok(ColumnFormat::Bitflag((value - 0x19) as u8)),
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

#[derive(Debug)]
pub enum Error {
    Sqpack(tomestone_sqpack::Error),
    Nom(nom::error::ErrorKind),
    NoSuchFile,
    LanguageUnavailable,
    Utf8(FromUtf8Error),
}

impl From<tomestone_sqpack::Error> for Error {
    fn from(e: tomestone_sqpack::Error) -> Error {
        Error::Sqpack(e)
    }
}

impl From<nom::error::ErrorKind> for Error {
    fn from(e: nom::error::ErrorKind) -> Error {
        Error::Nom(e)
    }
}

impl<'a> From<nom::error::Error<&'a [u8]>> for Error {
    fn from(e: nom::error::Error<&'a [u8]>) -> Error {
        Error::Nom(e.code)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Sqpack(e) => e.fmt(f),
            Error::Nom(e) => write!(f, "parsing error: {:?}", e),
            Error::NoSuchFile => write!(f, "file not found"),
            Error::LanguageUnavailable => write!(f, "language data not available"),
            Error::Utf8(e) => e.fmt(f),
        }
    }
}

struct DatasetPage {
    _row_start: u32,
    exdf: Exdf,
}

pub struct DatasetPageIter<'a> {
    exdf_iter: ExdfIterator<'a>,
    exhf: &'a Exhf,
}

impl<'a> Iterator for DatasetPageIter<'a> {
    type Item = Result<(u32, Vec<Value<'a>>), Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let (row_number, data) = match self.exdf_iter.next()? {
            Ok((row_number, data)) => (row_number, data),
            Err(e) => return Some(Err(e.into())),
        };
        match parse_row(data, &self.exhf) {
            Ok(row) => Some(Ok((row_number, row))),
            Err(e) => Some(Err(e.into())),
        }
    }
}

pub struct Dataset {
    pub exhf: Exhf,
    pages: Vec<DatasetPage>,
}

impl Dataset {
    pub fn load(game_data: &GameData, base: &str, language: Language) -> Result<Dataset, Error> {
        let exh_path = format!("exd/{}.exh", base);
        let exh_data = match game_data.lookup_path_data(&exh_path) {
            Ok(Some(exh_data)) => exh_data,
            Ok(None) => return Err(Error::NoSuchFile),
            Err(e) => return Err(Error::Sqpack(e)),
        };
        let exhf = match parse_exhf(&exh_data) {
            Ok((_, exhf)) => exhf,
            Err(nom::Err::Incomplete(_)) => unreachable!(),
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => return Err(Error::Nom(e.code)),
        };

        let short_code = if exhf.languages().contains(&Some(language)) {
            Some(language.short_code())
        } else if exhf.languages().contains(&None) {
            None
        } else {
            return Err(Error::LanguageUnavailable);
        };

        let pages = exhf
            .pages()
            .iter()
            .map(|(page_start, _)| {
                let exd_path = if let Some(short_code) = short_code {
                    format!("exd/{}_{}_{}.exd", base, page_start, short_code)
                } else {
                    format!("exd/{}_{}.exd", base, page_start)
                };
                let exdf_data = match game_data.lookup_path_data(&exd_path) {
                    Ok(Some(exd_data)) => exd_data,
                    Ok(None) => return Err(Error::NoSuchFile),
                    Err(e) => return Err(Error::Sqpack(e)),
                };
                let exdf = Exdf::new(exdf_data)?;
                Ok(DatasetPage {
                    _row_start: *page_start,
                    exdf,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Dataset { exhf, pages })
    }

    pub fn page_iter(&self) -> impl Iterator<Item = DatasetPageIter<'_>> {
        let exhf = &self.exhf;
        self.pages.iter().map(move |p| DatasetPageIter {
            exdf_iter: p.exdf.iter(),
            exhf,
        })
    }
}

pub struct RootList {
    text: String,
}

impl RootList {
    pub fn open(game_data: &GameData) -> Result<RootList, Error> {
        let data = match game_data.lookup_path_data("exd/root.exl") {
            Ok(Some(toc_data)) => toc_data,
            Ok(None) => return Err(Error::NoSuchFile),
            Err(e) => return Err(Error::Sqpack(e)),
        };
        match String::from_utf8(data) {
            Ok(text) => Ok(RootList { text }),
            Err(e) => Err(Error::Utf8(e)),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &str> {
        self.text.split_ascii_whitespace().filter_map(|line| {
            if let Some(comma_pos) = line.find(',') {
                let name = &line[..comma_pos];
                if name == "EXLT" {
                    return None;
                }
                Some(name)
            } else {
                // skip empty line
                None
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{EnumParseError, Language};

    #[test]
    fn language_round_trip() {
        assert!(matches!(Language::from_u16(0), Ok(None)));
        assert!(matches!(
            Language::from_u16(Language::Japanese as u16),
            Ok(Some(Language::Japanese))
        ));
        assert!(matches!(
            Language::from_u16(Language::English as u16),
            Ok(Some(Language::English))
        ));
        assert!(matches!(
            Language::from_u16(Language::German as u16),
            Ok(Some(Language::German))
        ));
        assert!(matches!(
            Language::from_u16(Language::French as u16),
            Ok(Some(Language::French))
        ));
        assert!(matches!(
            Language::from_u16(Language::ChineseSimplified as u16),
            Ok(Some(Language::ChineseSimplified))
        ));
        assert!(matches!(
            Language::from_u16(Language::ChineseTraditional as u16),
            Ok(Some(Language::ChineseTraditional))
        ));
        assert!(matches!(
            Language::from_u16(Language::Korean as u16),
            Ok(Some(Language::Korean))
        ));
        assert!(matches!(Language::from_u16(65535), Err(EnumParseError)));
    }
}
