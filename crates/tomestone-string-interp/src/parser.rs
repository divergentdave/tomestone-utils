use std::convert::TryInto;

use nom::{
    bytes::complete::{tag, take_while},
    combinator::{all_consuming, map, map_res},
    error::{ErrorKind, ParseError},
    multi::{length_value, many1, many_m_n},
    number::complete::{be_u16, be_u24, be_u32, be_u8},
    sequence::{pair, tuple},
    IResult,
};

use super::{Error, Expression, Segment, Text};

mod types {
    pub mod tag {
        pub const TODO_RESET_TIME: u8 = 0x06;
        pub const TIME: u8 = 0x07;
        pub const IF: u8 = 0x08;
        pub const SWITCH: u8 = 0x09;
        pub const TODO_0A: u8 = 0x0a;
        pub const IF_EQUALS: u8 = 0x0c;
        pub const TODO_0F: u8 = 0x0f;
        pub const NEW_LINE: u8 = 0x10;
        pub const GUI_ICON: u8 = 0x12;
        pub const COLOR_CHANGE: u8 = 0x13;
        pub const TODO_14: u8 = 0x14;
        pub const EMPHASIS_2: u8 = 0x19;
        pub const EMPHASIS: u8 = 0x1a;
        pub const TODO_1B: u8 = 0x1b;
        pub const TODO_1C: u8 = 0x1c;
        pub const INDENT: u8 = 0x1d;
        pub const COMMAND_ICON: u8 = 0x1e;
        pub const DASH: u8 = 0x1f;
        pub const VALUE: u8 = 0x20;
        pub const TODO_FORMAT: u8 = 0x22;
        pub const TWO_DIGIT_VALUE: u8 = 0x24;
        pub const TODO_26: u8 = 0x26;
        pub const SHEET: u8 = 0x28;
        pub const TODO_HIGHLIGHT: u8 = 0x29;
        pub const LINK: u8 = 0x2b;
        pub const SPLIT: u8 = 0x2c;
        pub const TODO_2D: u8 = 0x2d;
        pub const AUTO_TRANSLATE: u8 = 0x2e;
        pub const TODO_2F: u8 = 0x2f;
        pub const SHEET_JA: u8 = 0x30;
        pub const SHEET_EN: u8 = 0x31;
        pub const SHEET_DE: u8 = 0x32;
        pub const SHEET_FR: u8 = 0x33;
        pub const TODO_40: u8 = 0x40;
        pub const FOREGROUND: u8 = 0x48;
        pub const GLOW: u8 = 0x49;
        pub const ZERO_PADDED_VALUE: u8 = 0x50;
        pub const TODO_51: u8 = 0x51;
        pub const TODO_60: u8 = 0x60;
        pub const TODO_61: u8 = 0x61;
    }
    pub mod expr {
        pub const GEQ: u8 = 0xe0;
        pub const TODO_COMPARISON_1: u8 = 0xe1;
        pub const LEQ: u8 = 0xe2;
        pub const TODO_COMPARISON_2: u8 = 0xe3;
        pub const EQ: u8 = 0xe4;
        pub const TODO_COMPARISON_3: u8 = 0xe5;

        pub const INTEGER_PARAM: u8 = 0xe8;
        pub const PLAYER_PARAM: u8 = 0xe9;
        pub const STRING_PARAM: u8 = 0xea;
        pub const OBJECT_PARAM: u8 = 0xeb;
        pub const TODO_EC: u8 = 0xec;

        pub const BYTE: u8 = 0xf0;
        pub const INT16_MINUS_ONE: u8 = 0xf1;
        pub const INT16: u8 = 0xf2;
        pub const TODO_INT16: u8 = 0xf4;
        pub const INT24: u8 = 0xf6;
        pub const INT24_SHIFTED_8: u8 = 0xfd;
        pub const INT32: u8 = 0xfe;

        pub const TAGGED_TEXT: u8 = 0xff;
    }
}

fn integer(input: &[u8]) -> IResult<&[u8], u32, Error> {
    let (input, first_byte) = be_u8(input)?;
    match first_byte {
        1..=0xEF => Ok((input, first_byte as u32 - 1)),
        0xF0 => map(be_u8, |byte| byte as u32)(input),
        0xF1 => map(be_u8, |byte| (byte as u32) << 8)(input),
        0xF2 => map(be_u16, |short| short as u32)(input),
        0xFA => be_u24(input),
        0xFE => be_u32(input),
        _ => Err(nom::Err::Failure(Error::from_error_kind(
            input,
            ErrorKind::Alt,
        ))),
    }
}

fn integer_usize(input: &[u8]) -> IResult<&[u8], usize, Error> {
    map_res(integer, TryInto::try_into)(input)
}

fn expression_tagged_text(input: &[u8]) -> IResult<&[u8], Expression, Error> {
    length_value(
        integer_usize,
        all_consuming(map(tagged_text, |text| Expression::Text(Box::new(text)))),
    )(input)
}

fn expression(input: &[u8]) -> IResult<&[u8], Expression, Error> {
    let (input, type_byte) = be_u8(input)?;
    match type_byte {
        1..=0xcf => Ok((input, Expression::Integer(type_byte as u32 - 1))),
        0xd0..=0xdf => Ok((input, Expression::TopLevelParameter(type_byte - 0xd0))),
        types::expr::GEQ => map(pair(expression, expression), |(left, right)| {
            Expression::GreaterThanOrEqual(Box::new((left, right)))
        })(input),
        types::expr::TODO_COMPARISON_1 => map(pair(expression, expression), |(left, right)| {
            Expression::TodoComparison1(Box::new((left, right)))
        })(input),
        types::expr::LEQ => map(pair(expression, expression), |(left, right)| {
            Expression::LessThanOrEqual(Box::new((left, right)))
        })(input),
        types::expr::TODO_COMPARISON_2 => map(pair(expression, expression), |(left, right)| {
            Expression::TodoComparison2(Box::new((left, right)))
        })(input),
        types::expr::EQ => map(pair(expression, expression), |(left, right)| {
            Expression::Equal(Box::new((left, right)))
        })(input),
        types::expr::TODO_COMPARISON_3 => map(pair(expression, expression), |(left, right)| {
            Expression::TodoComparison3(Box::new((left, right)))
        })(input),
        types::expr::INTEGER_PARAM => map(expression, |expr| {
            Expression::IntegerParameter(Box::new(expr))
        })(input),
        types::expr::PLAYER_PARAM => map(expression, |expr| {
            Expression::PlayerParameter(Box::new(expr))
        })(input),
        types::expr::STRING_PARAM => map(expression, |expr| {
            Expression::StringParameter(Box::new(expr))
        })(input),
        types::expr::OBJECT_PARAM => map(expression, |expr| {
            Expression::ObjectParameter(Box::new(expr))
        })(input),
        types::expr::TODO_EC => Ok((input, Expression::TodoEC)),
        types::expr::BYTE => map(be_u8, |byte| Expression::Integer(byte as u32))(input),
        types::expr::INT16_MINUS_ONE => {
            map(be_u16, |short| Expression::Integer(short as u32 - 1))(input)
        }
        types::expr::INT16 => map(be_u16, |short| Expression::Integer(short as u32))(input),
        types::expr::TODO_INT16 => map(be_u16, |short| Expression::Integer(short as u32))(input),
        types::expr::INT24 => map(be_u24, Expression::Integer)(input),
        types::expr::INT24_SHIFTED_8 => map(be_u24, |value| Expression::Integer(value << 8))(input),
        types::expr::INT32 => map(be_u32, Expression::Integer)(input),
        types::expr::TAGGED_TEXT => expression_tagged_text(input),
        _ => Err(nom::Err::Error(Error::from_error_kind(
            input,
            ErrorKind::Alt,
        ))),
    }
}

fn segment_if(input: &[u8]) -> IResult<&[u8], Segment, Error> {
    map(
        tuple((expression, expression, expression)),
        |(condition, true_value, false_value)| Segment::If {
            condition,
            true_value,
            false_value,
        },
    )(input)
}

fn segment_switch(input: &[u8]) -> IResult<&[u8], Segment, Error> {
    map(
        pair(expression, many1(expression)),
        |(discriminant, cases)| Segment::Switch {
            discriminant,
            cases,
        },
    )(input)
}

fn segment_if_equals(input: &[u8]) -> IResult<&[u8], Segment, Error> {
    map(
        tuple((expression, expression, expression, expression)),
        |(left, right, true_value, false_value)| Segment::IfEquals {
            left,
            right,
            true_value,
            false_value,
        },
    )(input)
}

fn segment_no_data(return_value: Segment) -> impl Fn(&[u8]) -> IResult<&[u8], Segment, Error> {
    move |input| Ok((input, return_value.clone()))
}

fn segment_split(input: &[u8]) -> IResult<&[u8], Segment, Error> {
    map(
        tuple((expression, expression, expression)),
        |(input, separator, index)| Segment::Split {
            input,
            separator,
            index,
        },
    )(input)
}

fn segment_todo_format(input: &[u8]) -> IResult<&[u8], Segment, Error> {
    map(pair(expression, take_while(|_| true)), |(arg1, arg2)| {
        Segment::TodoFormat(arg1, arg2.to_owned())
    })(input)
}

fn segment(input: &[u8]) -> IResult<&[u8], Segment, Error> {
    fn contents<'a>(
        mut f: impl FnMut(&'a [u8]) -> IResult<&'a [u8], Segment, Error>,
    ) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], Segment, Error> {
        move |input| length_value(integer_usize, all_consuming(&mut f))(input)
    }

    let (input, type_byte) = be_u8(input)?;
    match type_byte {
        types::tag::TODO_RESET_TIME => contents(map(take_while(|_| true), |data: &[u8]| {
            Segment::TodoResetTime(data.to_owned())
        }))(input),
        types::tag::TIME => contents(map(expression, Segment::Time))(input),
        types::tag::IF => contents(segment_if)(input),
        types::tag::SWITCH => contents(segment_switch)(input),
        types::tag::TODO_0A => contents(map(expression, Segment::Todo0A))(input),
        types::tag::IF_EQUALS => contents(segment_if_equals)(input),
        types::tag::TODO_0F => contents(map(
            tuple((expression, expression, expression)),
            |(player, self_value, other_value)| Segment::Todo0F {
                player,
                self_value,
                other_value,
            },
        ))(input),
        types::tag::NEW_LINE => contents(segment_no_data(Segment::NewLine))(input),
        types::tag::GUI_ICON => contents(map(expression, Segment::GuiIcon))(input),
        types::tag::COLOR_CHANGE => contents(map(expression, Segment::ColorChange))(input),
        types::tag::TODO_14 => contents(map(expression, Segment::Todo14))(input),
        types::tag::EMPHASIS_2 => contents(map(integer, Segment::Emphasis2))(input),
        types::tag::EMPHASIS => contents(map(integer, Segment::Emphasis))(input),
        types::tag::TODO_1B => contents(map(take_while(|_| true), |data: &[u8]| {
            Segment::Todo1B(data.to_owned())
        }))(input),
        types::tag::TODO_1C => contents(map(take_while(|_| true), |data: &[u8]| {
            Segment::Todo1C(data.to_owned())
        }))(input),
        types::tag::INDENT => contents(segment_no_data(Segment::Indent))(input),
        types::tag::COMMAND_ICON => contents(map(expression, Segment::CommandIcon))(input),
        types::tag::DASH => contents(segment_no_data(Segment::Dash))(input),
        types::tag::VALUE => contents(map(expression, Segment::Value))(input),
        types::tag::TODO_FORMAT => contents(segment_todo_format)(input),
        types::tag::TWO_DIGIT_VALUE => contents(map(expression, Segment::TwoDigitValue))(input),
        types::tag::TODO_26 => contents(map(
            tuple((expression, expression, expression)),
            |(arg1, arg2, arg3)| Segment::Todo26(arg1, arg2, arg3),
        ))(input),
        types::tag::SHEET => {
            contents(map(many_m_n(2, usize::MAX, expression), Segment::Sheet))(input)
        }
        types::tag::TODO_HIGHLIGHT => contents(map(expression, Segment::TodoHighlight))(input),
        types::tag::LINK => {
            contents(map(many_m_n(1, usize::MAX, expression), Segment::Link))(input)
        }
        types::tag::SPLIT => contents(segment_split)(input),
        types::tag::TODO_2D => contents(map(expression, Segment::Todo2D))(input),
        types::tag::AUTO_TRANSLATE => {
            contents(map(pair(expression, expression), |(arg1, arg2)| {
                Segment::AutoTranslate(arg1, arg2)
            }))(input)
        }
        types::tag::TODO_2F => contents(map(expression, Segment::Todo2F))(input),
        types::tag::SHEET_JA => {
            contents(map(many_m_n(3, usize::MAX, expression), Segment::SheetJa))(input)
        }
        types::tag::SHEET_EN => {
            contents(map(many_m_n(3, usize::MAX, expression), Segment::SheetEn))(input)
        }
        types::tag::SHEET_DE => {
            contents(map(many_m_n(3, usize::MAX, expression), Segment::SheetDe))(input)
        }
        types::tag::SHEET_FR => {
            contents(map(many_m_n(3, usize::MAX, expression), Segment::SheetFr))(input)
        }
        types::tag::TODO_40 => contents(map(expression, Segment::Todo40))(input),
        types::tag::FOREGROUND => contents(map(expression, Segment::Foreground))(input),
        types::tag::GLOW => contents(map(expression, Segment::Glow))(input),
        types::tag::ZERO_PADDED_VALUE => {
            contents(map(pair(expression, expression), |(value, digits)| {
                Segment::ZeroPaddedValue { value, digits }
            }))(input)
        }
        types::tag::TODO_51 => contents(map(expression, Segment::Todo51))(input),
        types::tag::TODO_60 => contents(map(take_while(|_| true), |data: &[u8]| {
            Segment::Todo60(data.to_owned())
        }))(input),
        types::tag::TODO_61 => contents(map(expression, Segment::Todo61))(input),
        _ => Err(nom::Err::Failure(Error::from_error_kind(
            input,
            ErrorKind::Alt,
        ))),
    }
}

pub fn tagged_text(mut input: &[u8]) -> IResult<&[u8], Text, Error> {
    let mut segments = vec![];
    while !input.is_empty() {
        if let Some(start_pos) = input.iter().position(|&byte| byte == 2) {
            if start_pos > 0 {
                segments.push(Segment::Literal(
                    String::from_utf8(input[..start_pos].to_owned())
                        .map_err(|e| nom::Err::Failure(e.into()))?,
                ));
            }
            input = &input[start_pos + 1..];
            {
                let (new_input, segment) = segment(input).map_err(|e| e.map(Into::into))?;
                segments.push(segment);
                input = new_input;
            }
            {
                let (new_input, _) = tag(b"\x03")(input)?;
                input = new_input;
            }
        } else {
            segments.push(Segment::Literal(
                String::from_utf8(input.to_owned()).map_err(|e| nom::Err::Failure(e.into()))?,
            ));
            input = &input[input.len()..input.len()];
        }
    }
    Ok((input, Text { segments }))
}

#[cfg(test)]
mod tests {
    use tomestone_exdf::{Dataset, Language, RootList, Value};
    use tomestone_sqpack::GameData;

    use super::tagged_text;

    #[test]
    #[ignore = "slow test"]
    fn string_game_data() {
        dotenv::dotenv().ok();
        // Don't test anything if the game directory isn't probided
        let root = if let Ok(root) = std::env::var("FFXIV_INSTALL_DIR") {
            root
        } else {
            return;
        };
        let game_data = GameData::new(root).unwrap();

        let root_list = RootList::open(&game_data).unwrap();
        for name in root_list.iter() {
            // TODO: test other language files too
            let dataset = Dataset::load(&game_data, name, Language::English).unwrap();
            for page in dataset.page_iter() {
                for res in page {
                    let (i, row) = res.unwrap();
                    for value in row {
                        if let Value::String(data) = value {
                            let res = tagged_text(data);
                            if res.is_err() {
                                eprintln!("Error while parsing {} row {}", name, i);
                                eprintln!("{:02x?}", data);
                            }
                            res.unwrap();
                        }
                    }
                }
            }
        }
    }
}
