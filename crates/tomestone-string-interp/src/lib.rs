use std::{fmt, string::FromUtf8Error};

mod parser;

#[derive(Debug)]
pub enum Error {
    Nom(nom::error::ErrorKind),
    Utf8(FromUtf8Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Nom(e) => write!(f, "parsing error: {:?}", e),
            Error::Utf8(e) => e.fmt(f),
        }
    }
}

impl nom::error::ParseError<&[u8]> for Error {
    fn from_error_kind(_: &[u8], kind: nom::error::ErrorKind) -> Error {
        Error::Nom(kind)
    }

    fn append(_: &[u8], _: nom::error::ErrorKind, other: Error) -> Error {
        other
    }
}

impl<I, E> nom::error::FromExternalError<I, E> for Error {
    fn from_external_error(_: I, kind: nom::error::ErrorKind, _: E) -> Error {
        Error::Nom(kind)
    }
}

impl From<FromUtf8Error> for Error {
    fn from(e: FromUtf8Error) -> Error {
        Error::Utf8(e)
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

pub trait TreeNode {
    fn accept<V: Visitor>(&self, visitor: &mut V);
}

pub trait Visitor {
    fn visit_tag(&mut self, _tag: &Segment) {}
    fn visit_expression(&mut self, _expr: &Expression) {}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    GreaterThanOrEqual(Box<(Expression, Expression)>),
    TodoComparison1(Box<(Expression, Expression)>),
    LessThanOrEqual(Box<(Expression, Expression)>),
    TodoComparison2(Box<(Expression, Expression)>),
    Equal(Box<(Expression, Expression)>),
    TodoComparison3(Box<(Expression, Expression)>),
    TopLevelParameter(u8),
    IntegerParameter(Box<Expression>),
    PlayerParameter(Box<Expression>),
    StringParameter(Box<Expression>),
    ObjectParameter(Box<Expression>),
    TodoEC,
    Integer(u32),
    Text(Box<Text>),
}

impl TreeNode for Expression {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_expression(self);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Segment {
    Literal(String),
    TodoResetTime(Vec<u8>),
    Time(Expression),
    If {
        condition: Expression,
        true_value: Expression,
        false_value: Expression,
    },
    Switch {
        discriminant: Expression,
        cases: Vec<Expression>,
    },
    Todo0A(Expression),
    IfEquals {
        left: Expression,
        right: Expression,
        true_value: Expression,
        false_value: Expression,
    },
    Todo0F {
        player: Expression,
        self_value: Expression,
        other_value: Expression,
    },
    NewLine,
    GuiIcon(Expression),
    ColorChange(Expression),
    Todo14(Expression),
    Emphasis2(u32),
    Emphasis(u32),
    Todo1B(Vec<u8>),
    Todo1C(Vec<u8>),
    Indent,
    CommandIcon(Expression),
    Dash,
    Value(Expression),
    TodoFormat(Expression, Vec<u8>),
    TwoDigitValue(Expression),
    Todo26(Expression, Expression, Expression),
    Sheet(Vec<Expression>),
    TodoHighlight(Expression),
    Link(Vec<Expression>),
    Split {
        input: Expression,
        separator: Expression,
        index: Expression,
    },
    Todo2D(Expression),
    AutoTranslate(Expression, Expression),
    Todo2F(Expression),
    SheetJa(Vec<Expression>),
    SheetEn(Vec<Expression>),
    SheetDe(Vec<Expression>),
    SheetFr(Vec<Expression>),
    Todo40(Expression),
    Foreground(Expression),
    Glow(Expression),
    ZeroPaddedValue {
        value: Expression,
        digits: Expression,
    },
    Todo51(Expression),
    Todo60(Vec<u8>),
    Todo61(Expression),
}

impl TreeNode for Segment {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_tag(self);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Text {
    segments: Vec<Segment>,
}

impl Text {
    pub fn parse(input: &[u8]) -> Result<Text, Error> {
        match parser::tagged_text(input) {
            Ok((_, text)) => Ok(text),
            Err(nom::Err::Incomplete(_)) => unreachable!(),
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => Err(e),
        }
    }
}

impl TreeNode for Text {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        for segment in self.segments.iter() {
            segment.accept(visitor);
        }
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
