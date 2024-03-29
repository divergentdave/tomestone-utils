use std::{fmt, num::NonZeroU8, string::FromUtf8Error, vec::IntoIter};

use nom::Finish;

mod encoding;
mod parser;
mod serialization;
mod types;

pub use encoding::{encode, EncodeError};

#[derive(Debug)]
pub enum Error {
    Nom(nom::error::ErrorKind),
    Utf8(FromUtf8Error),
    NullByte,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Nom(e) => write!(f, "parsing error: {:?}", e),
            Error::Utf8(e) => e.fmt(f),
            Error::NullByte => write!(f, "null byte encountered"),
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
    fn accept_mut<V: MutVisitor>(&mut self, visitor: &mut V);
}

pub trait Visitor {
    fn visit_tag(&mut self, _tag: &Segment) {}

    fn visit_expression(&mut self, _expr: &Expression) {}

    fn recurse_tag(&mut self, tag: &Segment)
    where
        Self: Sized,
    {
        match tag {
            Segment::Literal(_) => {}
            Segment::TodoResetTime(_) => {}
            Segment::Time(expr) => expr.accept(self),
            Segment::If {
                condition,
                true_value,
                false_value,
            } => {
                condition.accept(self);
                true_value.accept(self);
                false_value.accept(self);
            }
            Segment::Switch {
                discriminant,
                cases,
            } => {
                discriminant.accept(self);
                for case in cases {
                    case.accept(self);
                }
            }
            Segment::Todo0A(expr) => expr.accept(self),
            Segment::IfEquals {
                left,
                right,
                true_value,
                false_value,
            } => {
                left.accept(self);
                right.accept(self);
                true_value.accept(self);
                false_value.accept(self);
            }
            Segment::Todo0F {
                player,
                self_value,
                other_value,
            } => {
                player.accept(self);
                self_value.accept(self);
                other_value.accept(self);
            }
            Segment::NewLine => {}
            Segment::GuiIcon(expr) => expr.accept(self),
            Segment::ColorChange(expr) => expr.accept(self),
            Segment::Todo14(expr) => expr.accept(self),
            Segment::SoftHyphen => {}
            Segment::Todo17 => {}
            Segment::Todo19(_) => {}
            Segment::Emphasis(_) => {}
            Segment::Todo1B(_) => {}
            Segment::Todo1C(_) => {}
            Segment::NonBreakingSpace => {}
            Segment::CommandIcon(expr) => expr.accept(self),
            Segment::Dash => {}
            Segment::IntegerValue(expr) => expr.accept(self),
            Segment::TodoFormat(expr, _) => expr.accept(self),
            Segment::TwoDigitValue(expr) => expr.accept(self),
            Segment::Todo26(arg1, arg2, arg3) => {
                arg1.accept(self);
                arg2.accept(self);
                arg3.accept(self);
            }
            Segment::Sheet {
                name,
                row_index,
                column_index,
                parameters,
            } => {
                name.accept(self);
                row_index.accept(self);
                if let Some(column_index) = column_index {
                    column_index.accept(self);
                }
                for param in parameters.iter() {
                    param.accept(self);
                }
            }
            Segment::StringValue(expr) => expr.accept(self),
            Segment::StringValueSentenceCase(expr) => expr.accept(self),
            Segment::Split {
                input,
                separator,
                index,
            } => {
                input.accept(self);
                separator.accept(self);
                index.accept(self);
            }
            Segment::StringValueTitleCase(expr) => expr.accept(self),
            Segment::AutoTranslate(arg1, arg2) => {
                arg1.accept(self);
                arg2.accept(self);
            }
            Segment::StringValueLowerCase(expr) => expr.accept(self),
            Segment::SheetJa(args) => {
                for arg in args.iter() {
                    arg.accept(self);
                }
            }
            Segment::SheetEn(args) => {
                for arg in args.iter() {
                    arg.accept(self);
                }
            }
            Segment::SheetDe(args) => {
                for arg in args.iter() {
                    arg.accept(self);
                }
            }
            Segment::SheetFr(args) => {
                for arg in args.iter() {
                    arg.accept(self);
                }
            }
            Segment::Todo40(expr) => expr.accept(self),
            Segment::Foreground(expr) => expr.accept(self),
            Segment::Glow(expr) => expr.accept(self),
            Segment::Ruby {
                annotated,
                annotation,
            } => {
                annotated.accept(self);
                annotation.accept(self);
            }
            Segment::ZeroPaddedValue { value, digits } => {
                value.accept(self);
                digits.accept(self);
            }
            Segment::Todo51(expr) => expr.accept(self),
            Segment::Todo60(_) => {}
            Segment::Todo61(expr) => expr.accept(self),
        }
    }

    fn recurse_expression(&mut self, expr: &Expression)
    where
        Self: Sized,
    {
        match expr {
            Expression::GreaterThanOrEqual(boite)
            | Expression::GreaterThan(boite)
            | Expression::LessThanOrEqual(boite)
            | Expression::LessThan(boite)
            | Expression::Equal(boite)
            | Expression::NotEqual(boite) => {
                boite.0.accept(self);
                boite.1.accept(self);
            }
            Expression::TopLevelParameter(_)
            | Expression::InputParameter(_)
            | Expression::PlayerParameter(_)
            | Expression::StringParameter(_)
            | Expression::ObjectParameter(_)
            | Expression::TodoEC
            | Expression::Integer(_) => {}
            Expression::Text(boite) => boite.accept(self),
        }
    }
}

pub trait MutVisitor {
    fn visit_tag(&mut self, _tag: &mut Segment) {}

    fn visit_expression(&mut self, _expr: &mut Expression) {}

    fn recurse_tag(&mut self, tag: &mut Segment)
    where
        Self: Sized,
    {
        match tag {
            Segment::Literal(_) => {}
            Segment::TodoResetTime(_) => {}
            Segment::Time(expr) => expr.accept_mut(self),
            Segment::If {
                condition,
                true_value,
                false_value,
            } => {
                condition.accept_mut(self);
                true_value.accept_mut(self);
                false_value.accept_mut(self);
            }
            Segment::Switch {
                discriminant,
                cases,
            } => {
                discriminant.accept_mut(self);
                for case in cases {
                    case.accept_mut(self);
                }
            }
            Segment::Todo0A(expr) => expr.accept_mut(self),
            Segment::IfEquals {
                left,
                right,
                true_value,
                false_value,
            } => {
                left.accept_mut(self);
                right.accept_mut(self);
                true_value.accept_mut(self);
                false_value.accept_mut(self);
            }
            Segment::Todo0F {
                player,
                self_value,
                other_value,
            } => {
                player.accept_mut(self);
                self_value.accept_mut(self);
                other_value.accept_mut(self);
            }
            Segment::NewLine => {}
            Segment::GuiIcon(expr) => expr.accept_mut(self),
            Segment::ColorChange(expr) => expr.accept_mut(self),
            Segment::Todo14(expr) => expr.accept_mut(self),
            Segment::SoftHyphen => {}
            Segment::Todo17 => {}
            Segment::Todo19(_) => {}
            Segment::Emphasis(_) => {}
            Segment::Todo1B(_) => {}
            Segment::Todo1C(_) => {}
            Segment::NonBreakingSpace => {}
            Segment::CommandIcon(expr) => expr.accept_mut(self),
            Segment::Dash => {}
            Segment::IntegerValue(expr) => expr.accept_mut(self),
            Segment::TodoFormat(expr, _) => expr.accept_mut(self),
            Segment::TwoDigitValue(expr) => expr.accept_mut(self),
            Segment::Todo26(arg1, arg2, arg3) => {
                arg1.accept_mut(self);
                arg2.accept_mut(self);
                arg3.accept_mut(self);
            }
            Segment::Sheet {
                name,
                row_index,
                column_index,
                parameters,
            } => {
                name.accept_mut(self);
                row_index.accept_mut(self);
                if let Some(column_index) = column_index {
                    column_index.accept_mut(self);
                }
                for param in parameters.iter_mut() {
                    param.accept_mut(self);
                }
            }
            Segment::StringValue(expr) => expr.accept_mut(self),
            Segment::StringValueSentenceCase(expr) => expr.accept_mut(self),
            Segment::Split {
                input,
                separator,
                index,
            } => {
                input.accept_mut(self);
                separator.accept_mut(self);
                index.accept_mut(self);
            }
            Segment::StringValueTitleCase(expr) => expr.accept_mut(self),
            Segment::AutoTranslate(arg1, arg2) => {
                arg1.accept_mut(self);
                arg2.accept_mut(self);
            }
            Segment::StringValueLowerCase(expr) => expr.accept_mut(self),
            Segment::SheetJa(args) => {
                for arg in args.iter_mut() {
                    arg.accept_mut(self);
                }
            }
            Segment::SheetEn(args) => {
                for arg in args.iter_mut() {
                    arg.accept_mut(self);
                }
            }
            Segment::SheetDe(args) => {
                for arg in args.iter_mut() {
                    arg.accept_mut(self);
                }
            }
            Segment::SheetFr(args) => {
                for arg in args.iter_mut() {
                    arg.accept_mut(self);
                }
            }
            Segment::Todo40(expr) => expr.accept_mut(self),
            Segment::Foreground(expr) => expr.accept_mut(self),
            Segment::Glow(expr) => expr.accept_mut(self),
            Segment::Ruby {
                annotated,
                annotation,
            } => {
                annotated.accept_mut(self);
                annotation.accept_mut(self);
            }
            Segment::ZeroPaddedValue { value, digits } => {
                value.accept_mut(self);
                digits.accept_mut(self);
            }
            Segment::Todo51(expr) => expr.accept_mut(self),
            Segment::Todo60(_) => {}
            Segment::Todo61(expr) => expr.accept_mut(self),
        }
    }

    fn recurse_expression(&mut self, expr: &mut Expression)
    where
        Self: Sized,
    {
        match expr {
            Expression::GreaterThanOrEqual(boite)
            | Expression::GreaterThan(boite)
            | Expression::LessThanOrEqual(boite)
            | Expression::LessThan(boite)
            | Expression::Equal(boite)
            | Expression::NotEqual(boite) => {
                boite.0.accept_mut(self);
                boite.1.accept_mut(self);
            }
            Expression::TopLevelParameter(_)
            | Expression::InputParameter(_)
            | Expression::PlayerParameter(_)
            | Expression::StringParameter(_)
            | Expression::ObjectParameter(_)
            | Expression::TodoEC
            | Expression::Integer(_) => {}
            Expression::Text(boite) => boite.accept_mut(self),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expression {
    GreaterThanOrEqual(Box<(Expression, Expression)>),
    GreaterThan(Box<(Expression, Expression)>),
    LessThanOrEqual(Box<(Expression, Expression)>),
    LessThan(Box<(Expression, Expression)>),
    Equal(Box<(Expression, Expression)>),
    NotEqual(Box<(Expression, Expression)>),
    TopLevelParameter(u8),
    InputParameter(u32),
    PlayerParameter(u32),
    StringParameter(u32),
    ObjectParameter(u32),
    TodoEC, // This is possibly a "last color" argument for color altering tags.
    Integer(u32),
    Text(Text),
}

impl TreeNode for Expression {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_expression(self);
    }

    fn accept_mut<V: MutVisitor>(&mut self, visitor: &mut V) {
        visitor.visit_expression(self);
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Segment {
    /// Plain Unicode text.
    Literal(String),
    TodoResetTime(Vec<NonZeroU8>),
    Time(Expression),
    /// A basic conditional expression. If the condition expression (first argument) evaluates to
    /// 1, then it will return the value of the true branch (second argument), and if it evaluates
    /// to 0, then it will return the value of the false branch (third argument).
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
    // Adds a line break.
    NewLine,
    GuiIcon(Expression),
    ColorChange(Expression),
    Todo14(Expression),
    SoftHyphen,
    Todo17,
    /// This tag is rarely used, and it seems to have no visible effect.
    Todo19(bool),
    /// Delimits italic text. When the argument is 1, the following text will be italic, until the
    /// next `Emphasis` tag with argument 0.
    Emphasis(bool),
    Todo1B(Vec<NonZeroU8>),
    Todo1C(Vec<NonZeroU8>),
    NonBreakingSpace,
    CommandIcon(Expression),
    /// Adds a dash/hyphen.
    Dash,
    /// Takes an integer from the result of an expression, and applies default formatting to it.
    IntegerValue(Expression),
    TodoFormat(Expression, Vec<NonZeroU8>),
    TwoDigitValue(Expression),
    Todo26(Expression, Expression, Expression),
    /// Looks up a value from one of the tables. The resulting value may be either a number or a
    /// string. The first argument is the name of the table, the second argument is the index of a
    /// row, and the third argument is the index of a column. Any remaining arguments are passed as
    /// input parameters to interpolate tagged text, assuming a string value was selected.
    Sheet {
        name: Expression,
        row_index: Expression,
        column_index: Option<Expression>,
        parameters: Vec<Expression>,
    },
    /// Takes a string-valued expression, and emits it unchanged.
    StringValue(Expression),
    /// Takes a string-valued expression, and capitalizes the first character.
    StringValueSentenceCase(Expression),
    /// Split a string at each occurrence of a separator, and return one of the resulting
    /// substrings.
    Split {
        input: Expression,
        separator: Expression,
        index: Expression,
    },
    /// Takes a string-valued expression, and capitalizes the first letter of each word.
    StringValueTitleCase(Expression),
    AutoTranslate(Expression, Expression),
    /// Takes a string-valued expression, and makes every character lower case.
    StringValueLowerCase(Expression),
    SheetJa(Vec<Expression>),
    SheetEn(Vec<Expression>),
    SheetDe(Vec<Expression>),
    SheetFr(Vec<Expression>),
    Todo40(Expression),
    Foreground(Expression),
    Glow(Expression),
    Ruby {
        annotated: Expression,
        annotation: Expression,
    },
    ZeroPaddedValue {
        value: Expression,
        digits: Expression,
    },
    Todo51(Expression),
    Todo60(Vec<NonZeroU8>),
    Todo61(Expression),
}

impl TreeNode for Segment {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_tag(self);
    }

    fn accept_mut<V: MutVisitor>(&mut self, visitor: &mut V) {
        visitor.visit_tag(self);
    }
}

impl fmt::Debug for Segment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Segment::Literal(string) => string.fmt(f),
            Segment::TodoResetTime(data) => f.debug_tuple("TodoResetTime").field(data).finish(),
            Segment::Time(arg) => f.debug_tuple("Time").field(arg).finish(),
            Segment::If {
                condition,
                true_value,
                false_value,
            } => f
                .debug_struct("If")
                .field("condition", condition)
                .field("true_value", true_value)
                .field("false_value", false_value)
                .finish(),
            Segment::Switch {
                discriminant,
                cases,
            } => f
                .debug_struct("Switch")
                .field("discriminant", discriminant)
                .field("cases", cases)
                .finish(),
            Segment::Todo0A(arg) => f.debug_tuple("Todo0A").field(arg).finish(),
            Segment::IfEquals {
                left,
                right,
                true_value,
                false_value,
            } => f
                .debug_struct("IfEquals")
                .field("left", left)
                .field("right", right)
                .field("true_value", true_value)
                .field("false_value", false_value)
                .finish(),
            Segment::Todo0F {
                player,
                self_value,
                other_value,
            } => f
                .debug_struct("Todo0F")
                .field("player", player)
                .field("self_value", self_value)
                .field("other_value", other_value)
                .finish(),
            Segment::NewLine => write!(f, "NewLine"),
            Segment::GuiIcon(arg) => f.debug_tuple("GuiIcon").field(arg).finish(),
            Segment::ColorChange(arg) => f.debug_tuple("ColorChange").field(arg).finish(),
            Segment::Todo14(arg) => f.debug_tuple("Todo14").field(arg).finish(),
            Segment::SoftHyphen => f.debug_tuple("SoftHyphen").finish(),
            Segment::Todo17 => f.debug_tuple("Todo17").finish(),
            Segment::Todo19(arg) => f.debug_tuple("Todo19").field(arg).finish(),
            Segment::Emphasis(arg) => f.debug_tuple("Emphasis").field(arg).finish(),
            Segment::Todo1B(arg) => f.debug_tuple("Todo1B").field(arg).finish(),
            Segment::Todo1C(arg) => f.debug_tuple("Todo1C").field(arg).finish(),
            Segment::NonBreakingSpace => write!(f, "NonBreakingSpace"),
            Segment::CommandIcon(arg) => f.debug_tuple("CommandIcon").field(arg).finish(),
            Segment::Dash => write!(f, "Dash"),
            Segment::IntegerValue(arg) => f.debug_tuple("IntegerValue").field(arg).finish(),
            Segment::TodoFormat(arg1, arg2) => {
                f.debug_tuple("TodoFormat").field(arg1).field(arg2).finish()
            }
            Segment::TwoDigitValue(arg) => f.debug_tuple("TwoDigitValue").field(arg).finish(),
            Segment::Todo26(arg1, arg2, arg3) => f
                .debug_tuple("Todo26")
                .field(arg1)
                .field(arg2)
                .field(arg3)
                .finish(),
            Segment::Sheet {
                name,
                row_index,
                column_index,
                parameters,
            } => f
                .debug_struct("Sheet")
                .field("name", name)
                .field("row_index", row_index)
                .field("column_index", column_index)
                .field("parameters", parameters)
                .finish(),
            Segment::StringValue(arg) => f.debug_tuple("StringValue").field(arg).finish(),
            Segment::StringValueSentenceCase(arg) => {
                f.debug_tuple("StringValueSentenceCase").field(arg).finish()
            }
            Segment::Split {
                input,
                separator,
                index,
            } => f
                .debug_struct("Split")
                .field("input", input)
                .field("separator", separator)
                .field("index", index)
                .finish(),
            Segment::StringValueTitleCase(arg) => {
                f.debug_tuple("StringValueTitleCase").field(arg).finish()
            }
            Segment::AutoTranslate(arg1, arg2) => f
                .debug_tuple("AutoTranslate")
                .field(arg1)
                .field(arg2)
                .finish(),
            Segment::StringValueLowerCase(arg) => {
                f.debug_tuple("StringValueLowerCase").field(arg).finish()
            }
            Segment::SheetJa(args) => f.debug_tuple("SheetJa").field(args).finish(),
            Segment::SheetEn(args) => f.debug_tuple("SheetEn").field(args).finish(),
            Segment::SheetDe(args) => f.debug_tuple("SheetDe").field(args).finish(),
            Segment::SheetFr(args) => f.debug_tuple("SheetFr").field(args).finish(),
            Segment::Todo40(arg) => f.debug_tuple("Todo40").field(arg).finish(),
            Segment::Foreground(arg) => f.debug_tuple("Foreground").field(arg).finish(),
            Segment::Glow(arg) => f.debug_tuple("Glow").field(arg).finish(),
            Segment::Ruby {
                annotated,
                annotation,
            } => f
                .debug_struct("Ruby")
                .field("annotated", annotated)
                .field("annotation", annotation)
                .finish(),
            Segment::ZeroPaddedValue { value, digits } => f
                .debug_struct("ZeroPaddedValue")
                .field("value", value)
                .field("digits", digits)
                .finish(),
            Segment::Todo51(arg) => f.debug_tuple("Todo51").field(arg).finish(),
            Segment::Todo60(data) => f.debug_tuple("Todo60").field(data).finish(),
            Segment::Todo61(arg) => f.debug_tuple("Todo61").field(arg).finish(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Text {
    segments: Vec<Segment>,
}

impl Text {
    pub fn new(segments: Vec<Segment>) -> Text {
        #[cfg(debug_assertions)]
        if segments.len() > 1 {
            debug_assert!(segments
                .iter()
                .zip(segments[1..].iter())
                .all(|pair| !(matches!(pair, (Segment::Literal(_), Segment::Literal(_))))));
        }
        Text { segments }
    }

    pub fn parse(input: &[u8]) -> Result<Text, Error> {
        nom::combinator::complete(parser::tagged_text)(input)
            .finish()
            .map(|(_, text)| text)
    }

    pub fn encode(&self) -> Result<Vec<u8>, encoding::EncodeError> {
        encoding::encode(self)
    }

    pub fn into_vec(self) -> Vec<Segment> {
        self.segments
    }
}

impl TreeNode for Text {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        for segment in self.segments.iter() {
            segment.accept(visitor);
        }
    }

    fn accept_mut<V: MutVisitor>(&mut self, visitor: &mut V) {
        for segment in self.segments.iter_mut() {
            segment.accept_mut(visitor);
        }
    }
}

impl fmt::Debug for Text {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.segments.fmt(f)
    }
}

impl IntoIterator for Text {
    type Item = Segment;

    type IntoIter = IntoIter<Segment>;

    fn into_iter(self) -> Self::IntoIter {
        self.segments.into_iter()
    }
}

#[derive(Default)]
pub struct TextAccumulator {
    next_literal: Option<String>,
    segments: Vec<Segment>,
}

impl TextAccumulator {
    pub fn new() -> TextAccumulator {
        TextAccumulator::default()
    }

    pub fn with_capacity(cap: usize) -> TextAccumulator {
        TextAccumulator {
            next_literal: None,
            segments: Vec::with_capacity(cap),
        }
    }

    pub fn push(&mut self, next: Segment) {
        match (next, &mut self.next_literal) {
            (Segment::Literal(value), None) => self.next_literal = Some(value),
            (Segment::Literal(value), Some(next_literal)) => next_literal.push_str(&value),
            (next, None) => self.segments.push(next),
            (next, next_literal @ Some(_)) => {
                self.segments.reserve(2);
                self.segments
                    .push(Segment::Literal(next_literal.take().unwrap()));
                self.segments.push(next);
            }
        }
    }
}

impl From<TextAccumulator> for Text {
    fn from(mut acc: TextAccumulator) -> Text {
        if let Some(last_string) = acc.next_literal.take() {
            acc.segments.push(Segment::Literal(last_string))
        }
        Text::new(acc.segments)
    }
}

impl Extend<Segment> for TextAccumulator {
    fn extend<T: IntoIterator<Item = Segment>>(&mut self, iter: T) {
        let iter = iter.into_iter();
        match iter.size_hint() {
            (lower_bound, None) => self.segments.reserve(lower_bound),
            (_, Some(upper_bound)) => self.segments.reserve(upper_bound),
        }
        for next in iter {
            self.push(next);
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

#[cfg(test)]
mod proptests {
    use std::{iter::empty, num::NonZeroU8};

    use quickcheck::{Arbitrary, Gen, QuickCheck, TestResult};

    use super::{Expression, Segment, Text};

    fn arbitrary_expr(g: &mut Gen, depth: usize) -> Expression {
        let choices = if depth >= 1 {
            &[6, 7, 8, 9, 10, 11, 12][..]
        } else {
            &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13][..]
        };
        match g.choose(choices).unwrap() {
            0 => Expression::GreaterThanOrEqual(Box::new((
                arbitrary_expr(g, depth + 1),
                arbitrary_expr(g, depth + 1),
            ))),
            1 => Expression::GreaterThan(Box::new((
                arbitrary_expr(g, depth + 1),
                arbitrary_expr(g, depth + 1),
            ))),
            2 => Expression::LessThanOrEqual(Box::new((
                arbitrary_expr(g, depth + 1),
                arbitrary_expr(g, depth + 1),
            ))),
            3 => Expression::LessThan(Box::new((
                arbitrary_expr(g, depth + 1),
                arbitrary_expr(g, depth + 1),
            ))),
            4 => Expression::Equal(Box::new((
                arbitrary_expr(g, depth + 1),
                arbitrary_expr(g, depth + 1),
            ))),
            5 => Expression::NotEqual(Box::new((
                arbitrary_expr(g, depth + 1),
                arbitrary_expr(g, depth + 1),
            ))),
            6 => Expression::TopLevelParameter(u8::arbitrary(g) & 0xf),
            7 => Expression::InputParameter(u32::arbitrary(g)),
            8 => Expression::PlayerParameter(u32::arbitrary(g)),
            9 => Expression::StringParameter(u32::arbitrary(g)),
            10 => Expression::ObjectParameter(u32::arbitrary(g)),
            11 => Expression::TodoEC,
            12 => Expression::Integer(u32::arbitrary(g)),
            13 => Expression::Text(arbitrary_text(g, depth + 1)),
            _ => unreachable!(),
        }
    }

    fn shrink_expr(expr: &Expression) -> Box<dyn Iterator<Item = Expression>> {
        match expr {
            Expression::GreaterThanOrEqual(boite) => Box::new(
                shrink_expr(&boite.0)
                    .map({
                        let right = boite.1.clone();
                        move |left| Expression::GreaterThanOrEqual(Box::new((left, right.clone())))
                    })
                    .chain(shrink_expr(&boite.1).map({
                        let left = boite.0.clone();
                        move |right| Expression::GreaterThanOrEqual(Box::new((left.clone(), right)))
                    })),
            ),
            Expression::GreaterThan(boite) => Box::new(
                shrink_expr(&boite.0)
                    .map({
                        let right = boite.1.clone();
                        move |left| Expression::GreaterThan(Box::new((left, right.clone())))
                    })
                    .chain(shrink_expr(&boite.1).map({
                        let left = boite.0.clone();
                        move |right| Expression::GreaterThan(Box::new((left.clone(), right)))
                    })),
            ),
            Expression::LessThanOrEqual(boite) => Box::new(
                shrink_expr(&boite.0)
                    .map({
                        let right = boite.1.clone();
                        move |left| Expression::LessThanOrEqual(Box::new((left, right.clone())))
                    })
                    .chain(shrink_expr(&boite.1).map({
                        let left = boite.0.clone();
                        move |right| Expression::LessThanOrEqual(Box::new((left.clone(), right)))
                    })),
            ),
            Expression::LessThan(boite) => Box::new(
                shrink_expr(&boite.0)
                    .map({
                        let right = boite.1.clone();
                        move |left| Expression::LessThan(Box::new((left, right.clone())))
                    })
                    .chain(shrink_expr(&boite.1).map({
                        let left = boite.0.clone();
                        move |right| Expression::LessThan(Box::new((left.clone(), right)))
                    })),
            ),
            Expression::Equal(boite) => Box::new(
                shrink_expr(&boite.0)
                    .map({
                        let right = boite.1.clone();
                        move |left| Expression::Equal(Box::new((left, right.clone())))
                    })
                    .chain(shrink_expr(&boite.1).map({
                        let left = boite.0.clone();
                        move |right| Expression::Equal(Box::new((left.clone(), right)))
                    })),
            ),
            Expression::NotEqual(boite) => Box::new(
                shrink_expr(&boite.0)
                    .map({
                        let right = boite.1.clone();
                        move |left| Expression::NotEqual(Box::new((left, right.clone())))
                    })
                    .chain(shrink_expr(&boite.1).map({
                        let left = boite.0.clone();
                        move |right| Expression::NotEqual(Box::new((left.clone(), right)))
                    })),
            ),
            Expression::TopLevelParameter(parameter_index) => {
                Box::new(parameter_index.shrink().map(Expression::TopLevelParameter))
            }
            Expression::InputParameter(parameter_index) => {
                Box::new(parameter_index.shrink().map(Expression::InputParameter))
            }
            Expression::PlayerParameter(parameter_index) => {
                Box::new(parameter_index.shrink().map(Expression::PlayerParameter))
            }
            Expression::StringParameter(parameter_index) => {
                Box::new(parameter_index.shrink().map(Expression::StringParameter))
            }
            Expression::ObjectParameter(parameter_index) => {
                Box::new(parameter_index.shrink().map(Expression::ObjectParameter))
            }
            Expression::TodoEC => Box::new(empty()),
            Expression::Integer(value) => Box::new(value.shrink().map(Expression::Integer)),
            Expression::Text(boite) => Box::new(boite.shrink().map(Expression::Text)),
        }
    }

    fn shrink_expr_list(expressions: &[Expression]) -> Box<dyn Iterator<Item = Vec<Expression>>> {
        #[derive(Clone)]
        struct ExpressionShrinkWrapper(Expression);

        impl Arbitrary for ExpressionShrinkWrapper {
            fn arbitrary(g: &mut Gen) -> Self {
                ExpressionShrinkWrapper(arbitrary_expr(g, 0))
            }

            fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
                Box::new(shrink_expr(&self.0).map(ExpressionShrinkWrapper))
            }
        }

        let wrapped = expressions
            .iter()
            .cloned()
            .map(ExpressionShrinkWrapper)
            .collect::<Vec<_>>();
        Box::new(
            wrapped
                .shrink()
                .map(|vec| vec.into_iter().map(|wrapped| wrapped.0).collect()),
        )
    }

    fn shrink_expr_list_preserve_length(
        expressions: &[Expression],
    ) -> Box<dyn Iterator<Item = Vec<Expression>>> {
        let expressions = expressions.to_owned();
        Box::new((0..expressions.len()).flat_map(move |i| {
            let expressions = expressions.clone();
            shrink_expr(&expressions[i]).map(move |shrunk| {
                let mut expressions = expressions.clone();
                expressions[i] = shrunk;
                expressions
            })
        }))
    }

    fn arbitrary_segment(g: &mut Gen, depth: usize) -> Segment {
        match g
            .choose(&[
                0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
                23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43,
            ])
            .unwrap()
        {
            0 => {
                let string = String::arbitrary(g);
                let mut string =
                    string.replace(|char| char == '\x00' || char == '\x02', "\u{fffd}");
                if string.is_empty() {
                    string.push('\u{fffd}');
                }
                Segment::Literal(string)
            }
            1 => Segment::TodoResetTime(Vec::<NonZeroU8>::arbitrary(g)),
            2 => Segment::Time(arbitrary_expr(g, depth)),
            3 => Segment::If {
                condition: arbitrary_expr(g, depth),
                true_value: arbitrary_expr(g, depth),
                false_value: arbitrary_expr(g, depth),
            },
            4 => {
                let mut cases: Vec<Expression> = Vec::<()>::arbitrary(g)
                    .into_iter()
                    .map(|()| arbitrary_expr(g, depth))
                    .collect();
                if cases.is_empty() {
                    cases.push(arbitrary_expr(g, depth));
                }
                Segment::Switch {
                    discriminant: arbitrary_expr(g, depth),
                    cases,
                }
            }
            5 => Segment::Todo0A(arbitrary_expr(g, depth)),
            6 => Segment::IfEquals {
                left: arbitrary_expr(g, depth),
                right: arbitrary_expr(g, depth),
                true_value: arbitrary_expr(g, depth),
                false_value: arbitrary_expr(g, depth),
            },
            7 => Segment::Todo0F {
                player: arbitrary_expr(g, depth),
                self_value: arbitrary_expr(g, depth),
                other_value: arbitrary_expr(g, depth),
            },
            8 => Segment::NewLine,
            9 => Segment::GuiIcon(arbitrary_expr(g, depth)),
            10 => Segment::ColorChange(arbitrary_expr(g, depth)),
            11 => Segment::Todo14(arbitrary_expr(g, depth)),
            12 => Segment::SoftHyphen,
            13 => Segment::Todo17,
            14 => Segment::Todo19(bool::arbitrary(g)),
            15 => Segment::Emphasis(bool::arbitrary(g)),
            16 => Segment::Todo1B(Vec::<NonZeroU8>::arbitrary(g)),
            17 => Segment::Todo1C(Vec::<NonZeroU8>::arbitrary(g)),
            18 => Segment::NonBreakingSpace,
            19 => Segment::CommandIcon(arbitrary_expr(g, depth)),
            20 => Segment::Dash,
            21 => Segment::IntegerValue(arbitrary_expr(g, depth)),
            22 => Segment::TodoFormat(arbitrary_expr(g, depth), Vec::<NonZeroU8>::arbitrary(g)),
            23 => Segment::TwoDigitValue(arbitrary_expr(g, depth)),
            24 => Segment::Todo26(
                arbitrary_expr(g, depth),
                arbitrary_expr(g, depth),
                arbitrary_expr(g, depth),
            ),
            25 => {
                let mut args: Vec<Expression> = Vec::<()>::arbitrary(g)
                    .into_iter()
                    .map(|()| arbitrary_expr(g, depth))
                    .collect();
                let column_index = args.pop();
                Segment::Sheet {
                    name: arbitrary_expr(g, depth),
                    row_index: arbitrary_expr(g, depth),
                    column_index,
                    parameters: args,
                }
            }
            26 => Segment::StringValue(arbitrary_expr(g, depth)),
            27 => Segment::StringValueSentenceCase(arbitrary_expr(g, depth)),
            28 => Segment::Split {
                input: arbitrary_expr(g, depth),
                separator: arbitrary_expr(g, depth),
                index: arbitrary_expr(g, depth),
            },
            29 => Segment::StringValueTitleCase(arbitrary_expr(g, depth)),
            30 => Segment::AutoTranslate(arbitrary_expr(g, depth), arbitrary_expr(g, depth)),
            31 => Segment::StringValueLowerCase(arbitrary_expr(g, depth)),
            32 => {
                let mut args: Vec<Expression> = Vec::<()>::arbitrary(g)
                    .into_iter()
                    .map(|()| arbitrary_expr(g, depth))
                    .collect();
                while args.len() < 3 {
                    args.push(arbitrary_expr(g, depth));
                }
                Segment::SheetJa(args)
            }
            33 => {
                let mut args: Vec<Expression> = Vec::<()>::arbitrary(g)
                    .into_iter()
                    .map(|()| arbitrary_expr(g, depth))
                    .collect();
                while args.len() < 3 {
                    args.push(arbitrary_expr(g, depth));
                }
                Segment::SheetEn(args)
            }
            34 => {
                let mut args: Vec<Expression> = Vec::<()>::arbitrary(g)
                    .into_iter()
                    .map(|()| arbitrary_expr(g, depth))
                    .collect();
                while args.len() < 3 {
                    args.push(arbitrary_expr(g, depth));
                }
                Segment::SheetDe(args)
            }
            35 => {
                let mut args: Vec<Expression> = Vec::<()>::arbitrary(g)
                    .into_iter()
                    .map(|()| arbitrary_expr(g, depth))
                    .collect();
                while args.len() < 3 {
                    args.push(arbitrary_expr(g, depth));
                }
                Segment::SheetFr(args)
            }
            36 => Segment::Todo40(arbitrary_expr(g, depth)),
            37 => Segment::Foreground(arbitrary_expr(g, depth)),
            38 => Segment::Glow(arbitrary_expr(g, depth)),
            39 => Segment::Ruby {
                annotated: arbitrary_expr(g, depth),
                annotation: arbitrary_expr(g, depth),
            },
            40 => Segment::ZeroPaddedValue {
                value: arbitrary_expr(g, depth),
                digits: arbitrary_expr(g, depth),
            },
            41 => Segment::Todo51(arbitrary_expr(g, depth)),
            42 => Segment::Todo60(Arbitrary::arbitrary(g)),
            43 => Segment::Todo61(arbitrary_expr(g, depth)),
            _ => unreachable!(),
        }
    }

    impl Arbitrary for Segment {
        fn arbitrary(g: &mut Gen) -> Segment {
            arbitrary_segment(g, 0)
        }

        fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
            match self {
                Segment::Literal(string) => Box::new(
                    string
                        .shrink()
                        .filter(|string| {
                            !string.is_empty() && !string.bytes().any(|byte| byte == 2)
                        })
                        .map(Segment::Literal),
                ),
                Segment::TodoResetTime(_) => Box::new(empty()),
                Segment::Time(arg) => Box::new(shrink_expr(arg).map(Segment::Time)),
                Segment::If {
                    condition,
                    true_value,
                    false_value,
                } => Box::new(
                    [
                        {
                            let true_value = true_value.clone();
                            let false_value = false_value.clone();
                            Box::new(shrink_expr(condition).map(move |condition| Segment::If {
                                condition,
                                true_value: true_value.clone(),
                                false_value: false_value.clone(),
                            })) as Box<dyn Iterator<Item = Self>>
                        },
                        {
                            let condition = condition.clone();
                            let false_value = false_value.clone();
                            Box::new(shrink_expr(true_value).map(move |true_value| Segment::If {
                                condition: condition.clone(),
                                true_value,
                                false_value: false_value.clone(),
                            })) as Box<dyn Iterator<Item = Self>>
                        },
                        {
                            let condition = condition.clone();
                            let true_value = true_value.clone();
                            Box::new(
                                shrink_expr(false_value).map(move |false_value| Segment::If {
                                    condition: condition.clone(),
                                    true_value: true_value.clone(),
                                    false_value,
                                }),
                            ) as Box<dyn Iterator<Item = Self>>
                        },
                    ]
                    .into_iter()
                    .flatten(),
                ),
                Segment::Switch {
                    discriminant,
                    cases,
                } => Box::new(
                    shrink_expr(discriminant)
                        .map({
                            let cases = cases.clone();
                            move |discriminant| Segment::Switch {
                                discriminant,
                                cases: cases.clone(),
                            }
                        })
                        .chain(shrink_expr_list(cases).filter(|vec| !vec.is_empty()).map({
                            let discriminant = discriminant.clone();
                            move |cases| Segment::Switch {
                                discriminant: discriminant.clone(),
                                cases,
                            }
                        })),
                ),
                Segment::Todo0A(arg) => Box::new(shrink_expr(arg).map(Segment::Todo0A)),
                Segment::IfEquals {
                    left,
                    right,
                    true_value,
                    false_value,
                } => Box::new(
                    [
                        {
                            let right = right.clone();
                            let true_value = true_value.clone();
                            let false_value = false_value.clone();
                            Box::new(shrink_expr(left).map(move |left| Segment::IfEquals {
                                left,
                                right: right.clone(),
                                true_value: true_value.clone(),
                                false_value: false_value.clone(),
                            })) as Box<dyn Iterator<Item = Self>>
                        },
                        {
                            let left = left.clone();
                            let true_value = true_value.clone();
                            let false_value = false_value.clone();
                            Box::new(shrink_expr(right).map(move |right| Segment::IfEquals {
                                left: left.clone(),
                                right,
                                true_value: true_value.clone(),
                                false_value: false_value.clone(),
                            })) as Box<dyn Iterator<Item = Self>>
                        },
                        {
                            let left = left.clone();
                            let right = right.clone();
                            let false_value = false_value.clone();
                            Box::new(shrink_expr(true_value).map(move |true_value| {
                                Segment::IfEquals {
                                    left: left.clone(),
                                    right: right.clone(),
                                    true_value,
                                    false_value: false_value.clone(),
                                }
                            }))
                        },
                        {
                            let left = left.clone();
                            let right = right.clone();
                            let true_value = true_value.clone();
                            Box::new(shrink_expr(false_value).map(move |false_value| {
                                Segment::IfEquals {
                                    left: left.clone(),
                                    right: right.clone(),
                                    true_value: true_value.clone(),
                                    false_value,
                                }
                            }))
                        },
                    ]
                    .into_iter()
                    .flatten(),
                ),
                Segment::Todo0F {
                    player,
                    self_value,
                    other_value,
                } => Box::new(
                    [
                        {
                            let self_value = self_value.clone();
                            let other_value = other_value.clone();
                            Box::new(shrink_expr(player).map(move |player| Segment::Todo0F {
                                player,
                                self_value: self_value.clone(),
                                other_value: other_value.clone(),
                            })) as Box<dyn Iterator<Item = Self>>
                        },
                        {
                            let player = player.clone();
                            let other_value = other_value.clone();
                            Box::new(shrink_expr(self_value).map(move |self_value| {
                                Segment::Todo0F {
                                    player: player.clone(),
                                    self_value,
                                    other_value: other_value.clone(),
                                }
                            })) as Box<dyn Iterator<Item = Self>>
                        },
                        {
                            let player = player.clone();
                            let self_value = self_value.clone();
                            Box::new(shrink_expr(other_value).map(move |other_value| {
                                Segment::Todo0F {
                                    player: player.clone(),
                                    self_value: self_value.clone(),
                                    other_value,
                                }
                            })) as Box<dyn Iterator<Item = Self>>
                        },
                    ]
                    .into_iter()
                    .flatten(),
                ),
                Segment::NewLine => Box::new(empty()),
                Segment::GuiIcon(arg) => Box::new(shrink_expr(arg).map(Segment::GuiIcon)),
                Segment::ColorChange(arg) => Box::new(shrink_expr(arg).map(Segment::ColorChange)),
                Segment::Todo14(arg) => Box::new(shrink_expr(arg).map(Segment::Todo14)),
                Segment::SoftHyphen => Box::new(empty()),
                Segment::Todo17 => Box::new(empty()),
                Segment::Todo19(value) => Box::new(value.shrink().map(Segment::Todo19)),
                Segment::Emphasis(value) => Box::new(value.shrink().map(Segment::Emphasis)),
                Segment::Todo1B(_) => Box::new(empty()),
                Segment::Todo1C(_) => Box::new(empty()),
                Segment::NonBreakingSpace => Box::new(empty()),
                Segment::CommandIcon(arg) => Box::new(shrink_expr(arg).map(Segment::CommandIcon)),
                Segment::Dash => Box::new(empty()),
                Segment::IntegerValue(arg) => Box::new(shrink_expr(arg).map(Segment::IntegerValue)),
                Segment::TodoFormat(arg, bytes) => Box::new(shrink_expr(arg).map({
                    let bytes = bytes.clone();
                    move |arg| Segment::TodoFormat(arg, bytes.clone())
                })),
                Segment::TwoDigitValue(arg) => {
                    Box::new(shrink_expr(arg).map(Segment::TwoDigitValue))
                }
                Segment::Todo26(arg1, arg2, arg3) => {
                    Box::new(
                        [
                            {
                                let arg2 = arg2.clone();
                                let arg3 = arg3.clone();
                                Box::new(shrink_expr(arg1).map(move |arg1| {
                                    Segment::Todo26(arg1, arg2.clone(), arg3.clone())
                                }))
                                    as Box<dyn Iterator<Item = Self>>
                            },
                            {
                                let arg1 = arg1.clone();
                                let arg3 = arg3.clone();
                                Box::new(shrink_expr(arg2).map(move |arg2| {
                                    Segment::Todo26(arg1.clone(), arg2, arg3.clone())
                                }))
                                    as Box<dyn Iterator<Item = Self>>
                            },
                            {
                                let arg1 = arg1.clone();
                                let arg2 = arg2.clone();
                                Box::new(shrink_expr(arg3).map(move |arg3| {
                                    Segment::Todo26(arg1.clone(), arg2.clone(), arg3)
                                }))
                                    as Box<dyn Iterator<Item = Self>>
                            },
                        ]
                        .into_iter()
                        .flatten(),
                    )
                }
                Segment::Sheet {
                    name,
                    row_index,
                    column_index,
                    parameters,
                } => Box::new(
                    [
                        {
                            let row_index = row_index.clone();
                            let column_index = column_index.clone();
                            let parameters = parameters.clone();
                            Box::new(shrink_expr(name).map(move |name| Segment::Sheet {
                                name,
                                row_index: row_index.clone(),
                                column_index: column_index.clone(),
                                parameters: parameters.clone(),
                            })) as Box<dyn Iterator<Item = Self>>
                        },
                        {
                            let name = name.clone();
                            let column_index = column_index.clone();
                            let parameters = parameters.clone();
                            Box::new(shrink_expr(row_index).map(move |row_index| Segment::Sheet {
                                name: name.clone(),
                                row_index,
                                column_index: column_index.clone(),
                                parameters: parameters.clone(),
                            }))
                        },
                        {
                            let name = name.clone();
                            let row_index = row_index.clone();
                            let column_index = column_index.clone();
                            let mut rest = Vec::with_capacity(parameters.len());
                            rest.extend(parameters.iter().cloned());
                            if let Some(column_index) = column_index {
                                rest.push(column_index);
                            }
                            Box::new(shrink_expr_list(&rest).map(move |mut rest| {
                                let column_index = rest.pop();
                                Segment::Sheet {
                                    name: name.clone(),
                                    row_index: row_index.clone(),
                                    column_index,
                                    parameters: rest,
                                }
                            }))
                        },
                    ]
                    .into_iter()
                    .flatten(),
                ),
                Segment::StringValue(arg) => Box::new(shrink_expr(arg).map(Segment::StringValue)),
                Segment::StringValueSentenceCase(arg) => {
                    Box::new(shrink_expr(arg).map(Segment::StringValueSentenceCase))
                }
                Segment::Split {
                    input,
                    separator,
                    index,
                } => Box::new(
                    [
                        {
                            let separator = separator.clone();
                            let index = index.clone();
                            Box::new(shrink_expr(input).map(move |input| Segment::Split {
                                input,
                                separator: separator.clone(),
                                index: index.clone(),
                            })) as Box<dyn Iterator<Item = Self>>
                        },
                        {
                            let input = input.clone();
                            let index = index.clone();
                            Box::new(shrink_expr(separator).map(move |separator| Segment::Split {
                                input: input.clone(),
                                separator,
                                index: index.clone(),
                            })) as Box<dyn Iterator<Item = Self>>
                        },
                        {
                            let input = input.clone();
                            let separator = separator.clone();
                            Box::new(shrink_expr(index).map(move |index| Segment::Split {
                                input: input.clone(),
                                separator: separator.clone(),
                                index,
                            })) as Box<dyn Iterator<Item = Self>>
                        },
                    ]
                    .into_iter()
                    .flatten(),
                ),
                Segment::StringValueTitleCase(arg) => {
                    Box::new(shrink_expr(arg).map(Segment::StringValueTitleCase))
                }
                Segment::AutoTranslate(arg1, arg2) => Box::new(
                    shrink_expr(arg1)
                        .map({
                            let arg2 = arg2.clone();
                            move |arg1| Segment::AutoTranslate(arg1, arg2.clone())
                        })
                        .chain(shrink_expr(arg2).map({
                            let arg1 = arg1.clone();
                            move |arg2| Segment::AutoTranslate(arg1.clone(), arg2)
                        })),
                ),
                Segment::StringValueLowerCase(arg) => {
                    Box::new(shrink_expr(arg).map(Segment::StringValueLowerCase))
                }
                Segment::SheetJa(args) => {
                    Box::new(shrink_expr_list_preserve_length(args).map(Segment::SheetJa))
                }
                Segment::SheetEn(args) => {
                    Box::new(shrink_expr_list_preserve_length(args).map(Segment::SheetEn))
                }
                Segment::SheetDe(args) => {
                    Box::new(shrink_expr_list_preserve_length(args).map(Segment::SheetDe))
                }
                Segment::SheetFr(args) => {
                    Box::new(shrink_expr_list_preserve_length(args).map(Segment::SheetFr))
                }
                Segment::Todo40(arg) => Box::new(shrink_expr(arg).map(Segment::Todo40)),
                Segment::Foreground(arg) => Box::new(shrink_expr(arg).map(Segment::Foreground)),
                Segment::Glow(arg) => Box::new(shrink_expr(arg).map(Segment::Glow)),
                Segment::Ruby {
                    annotated,
                    annotation,
                } => Box::new(
                    shrink_expr(annotated)
                        .map({
                            let annotation = annotation.clone();
                            move |annotated| Segment::Ruby {
                                annotated,
                                annotation: annotation.clone(),
                            }
                        })
                        .chain(shrink_expr(annotation).map({
                            let annotated = annotated.clone();
                            move |annotation| Segment::Ruby {
                                annotated: annotated.clone(),
                                annotation,
                            }
                        })),
                ),
                Segment::ZeroPaddedValue { value, digits } => Box::new(
                    [
                        {
                            let digits = digits.clone();
                            Box::new(shrink_expr(value).map(move |value| {
                                Segment::ZeroPaddedValue {
                                    value,
                                    digits: digits.clone(),
                                }
                            })) as Box<dyn Iterator<Item = Self>>
                        },
                        {
                            let value = value.clone();
                            Box::new(shrink_expr(digits).map(move |digits| {
                                Segment::ZeroPaddedValue {
                                    value: value.clone(),
                                    digits,
                                }
                            })) as Box<dyn Iterator<Item = Self>>
                        },
                    ]
                    .into_iter()
                    .flatten(),
                ),
                Segment::Todo51(arg) => Box::new(shrink_expr(arg).map(Segment::Todo51)),
                Segment::Todo60(_) => Box::new(empty()),
                Segment::Todo61(arg) => Box::new(shrink_expr(arg).map(Segment::Todo61)),
            }
        }
    }

    fn arbitrary_text(g: &mut Gen, depth: usize) -> Text {
        Text {
            segments: vec![arbitrary_segment(g, depth)],
        }
    }

    impl Arbitrary for Text {
        fn arbitrary(g: &mut Gen) -> Text {
            Text {
                segments: Vec::<Segment>::arbitrary(g),
            }
        }

        fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
            Box::new(self.segments.shrink().map(|segments| Text { segments }))
        }
    }

    fn property_encode_nul_free(tag: Segment) -> bool {
        let text = Text {
            segments: vec![tag],
        };
        if let Ok(data) = crate::encoding::encode(&text) {
            !data.iter().any(|byte| *byte == 0)
        } else {
            true
        }
    }

    #[test]
    fn encode_nul_free() {
        QuickCheck::new().quickcheck(property_encode_nul_free as fn(Segment) -> bool);
    }

    fn property_encode_round_trip(tag: Segment) -> TestResult {
        let text = Text {
            segments: vec![tag],
        };
        if let Ok(data) = crate::encoding::encode(&text) {
            match Text::parse(&data) {
                Ok(parsed) => {
                    if parsed == text {
                        TestResult::passed()
                    } else {
                        eprintln!(
                            "round trip failed, {:?} => {:02x?} => {:?}",
                            text, data, parsed
                        );
                        TestResult::failed()
                    }
                }
                Err(e) => {
                    eprintln!(
                        "round trip failed, {:?} => {:02x?} => parse error {:?}",
                        text, data, e
                    );
                    TestResult::failed()
                }
            }
        } else {
            TestResult::discard()
        }
    }

    #[test]
    fn encode_round_trip() {
        QuickCheck::new().quickcheck(property_encode_round_trip as fn(Segment) -> TestResult);
    }

    #[test]
    fn regression_01() {
        assert!(!property_encode_round_trip(Segment::Dash).is_failure());
        assert!(!property_encode_round_trip(Segment::NonBreakingSpace).is_failure());
        assert!(!property_encode_round_trip(Segment::NewLine).is_failure());
    }

    #[test]
    fn regression_02() {
        let _ = Text::parse(b"\x02\x24\x24");
    }

    #[test]
    fn regression_03() {
        assert!(
            !property_encode_round_trip(Segment::IntegerValue(Expression::Integer(0x544600)))
                .is_failure()
        );
    }

    #[test]
    fn regression_04() {
        assert!(
            !property_encode_round_trip(Segment::IntegerValue(Expression::PlayerParameter(65793)))
                .is_failure()
        );
    }
}
