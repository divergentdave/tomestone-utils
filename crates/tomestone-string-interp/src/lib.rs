use std::{fmt, string::FromUtf8Error};

#[derive(Debug)]
pub enum Error {
    Utf8(FromUtf8Error),
    Eof,
    UnsupportedType(u8),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Utf8(e) => e.fmt(f),
            Error::Eof => write!(f, "unexpected end of string"),
            Error::UnsupportedType(type_code) => write!(f, "unsupported type {}", type_code),
        }
    }
}

impl From<FromUtf8Error> for Error {
    fn from(e: FromUtf8Error) -> Error {
        Error::Utf8(e)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Segment {
    Literal(String),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Text {
    segments: Vec<Segment>,
}

impl Text {
    pub fn parse(mut input: &[u8]) -> Result<Text, Error> {
        let mut segments = vec![];
        while !input.is_empty() {
            if let Some(start_pos) = input.iter().position(|&byte| byte == 2u8) {
                if start_pos > 0 {
                    segments.push(Segment::Literal(String::from_utf8(
                        input[..start_pos].to_owned(),
                    )?));
                }
                if input.len() <= start_pos + 1 {
                    return Err(Error::Eof);
                }
                let type_byte = input[start_pos + 1];
                match type_byte {
                    _ => return Err(Error::UnsupportedType(type_byte)),
                }
            } else {
                segments.push(Segment::Literal(String::from_utf8(input.to_owned())?));
                input = &input[input.len()..input.len()];
            }
        }
        Ok(Text { segments })
    }
}

#[cfg(test)]
mod tests {
    use super::{Segment, Text};

    #[test]
    fn simple() {
        assert_eq!(
            Text::parse(b"Hello world").unwrap(),
            Text {
                segments: vec![Segment::Literal("Hello world".to_string())]
            }
        );
    }
}
