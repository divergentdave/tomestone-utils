use std::{convert::TryInto, num::NonZeroU8};

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

fn integer(input: &[u8]) -> IResult<&[u8], u32, Error> {
    use crate::types::expr::*;

    let (input, first_byte) = be_u8(input)?;
    match first_byte {
        1..=0xEF => Ok((input, first_byte as u32 - 1)),
        BYTE => map(be_u8, |byte| (byte as u32))(input),
        BYTE_SHIFTED_8 => map(be_u8, |byte| ((byte as u32) << 8))(input),
        BYTE_SHIFTED_16 => map(be_u8, |byte| ((byte as u32) << 16))(input),
        BYTE_SHIFTED_24 => map(be_u8, |byte| ((byte as u32) << 24))(input),
        INT16 => map(be_u16, |short| (short as u32))(input),
        INT16_SHIFTED_8 => map(be_u16, |short| ((short as u32) << 8))(input),
        INT16_SHIFTED_16 => map(be_u16, |short| ((short as u32) << 16))(input),
        INT16_FIRST_AND_THIRD_BYTES => map(pair(be_u8, be_u8), |(hi, lo)| {
            ((hi as u32) << 16) | (lo as u32)
        })(input),
        INT16_SECOND_AND_FOURTH_BYTES => map(pair(be_u8, be_u8), |(hi, lo)| {
            ((hi as u32) << 24) | ((lo as u32) << 8)
        })(input),
        INT16_FIRST_AND_LAST_BYTES => map(pair(be_u8, be_u8), |(hi, lo)| {
            ((hi as u32) << 24) | (lo as u32)
        })(input),
        INT24 => be_u24(input),
        INT24_SHIFTED_8 => map(be_u24, |value| (value << 8))(input),
        INT24_FIRST_SECOND_AND_FOURTH_BYTES => map(pair(be_u8, be_u16), |(hi, lo)| {
            ((hi as u32) << 24) | (lo as u32)
        })(input),
        INT24_FIRST_THIRD_AND_FOURTH_BYTES => map(pair(be_u16, be_u8), |(hi, lo)| {
            ((hi as u32) << 16) | (lo as u32)
        })(input),
        INT32 => be_u32(input),
        _ => Err(nom::Err::Failure(Error::from_error_kind(
            input,
            ErrorKind::Alt,
        ))),
    }
}

fn integer_usize(input: &[u8]) -> IResult<&[u8], usize, Error> {
    map_res(integer, TryInto::try_into)(input)
}

fn boolean(input: &[u8]) -> IResult<&[u8], bool, Error> {
    map_res(integer, |n| match n {
        0 => Ok(false),
        1 => Ok(true),
        _ => Err(()),
    })(input)
}

fn expression_tagged_text(input: &[u8]) -> IResult<&[u8], Expression, Error> {
    length_value(
        integer_usize,
        all_consuming(map(tagged_text, |text| Expression::Text(Box::new(text)))),
    )(input)
}

fn expression(input: &[u8]) -> IResult<&[u8], Expression, Error> {
    use crate::types::expr::*;

    let (input, type_byte) = be_u8(input)?;
    match type_byte {
        1..=0xcf => Ok((input, Expression::Integer(type_byte as u32 - 1))),
        0xd0..=0xdf => Ok((input, Expression::TopLevelParameter(type_byte - 0xd0))),
        GEQ => map(pair(expression, expression), |(left, right)| {
            Expression::GreaterThanOrEqual(Box::new((left, right)))
        })(input),
        GT => map(pair(expression, expression), |(left, right)| {
            Expression::GreaterThan(Box::new((left, right)))
        })(input),
        LEQ => map(pair(expression, expression), |(left, right)| {
            Expression::LessThanOrEqual(Box::new((left, right)))
        })(input),
        LT => map(pair(expression, expression), |(left, right)| {
            Expression::LessThan(Box::new((left, right)))
        })(input),
        EQ => map(pair(expression, expression), |(left, right)| {
            Expression::Equal(Box::new((left, right)))
        })(input),
        NEQ => map(pair(expression, expression), |(left, right)| {
            Expression::NotEqual(Box::new((left, right)))
        })(input),
        INPUT_PARAM => map(integer, Expression::InputParameter)(input),
        PLAYER_PARAM => map(integer, Expression::PlayerParameter)(input),
        STRING_PARAM => map(integer, Expression::StringParameter)(input),
        OBJECT_PARAM => map(integer, Expression::ObjectParameter)(input),
        TODO_EC => Ok((input, Expression::TodoEC)),
        BYTE => map(be_u8, |byte| Expression::Integer(byte as u32))(input),
        BYTE_SHIFTED_8 => map(be_u8, |byte| Expression::Integer((byte as u32) << 8))(input),
        BYTE_SHIFTED_16 => map(be_u8, |byte| Expression::Integer((byte as u32) << 16))(input),
        BYTE_SHIFTED_24 => map(be_u8, |byte| Expression::Integer((byte as u32) << 24))(input),
        INT16 => map(be_u16, |short| Expression::Integer(short as u32))(input),
        INT16_SHIFTED_8 => map(be_u16, |short| Expression::Integer((short as u32) << 8))(input),
        INT16_SHIFTED_16 => map(be_u16, |short| Expression::Integer((short as u32) << 16))(input),
        INT16_FIRST_AND_THIRD_BYTES => map(pair(be_u8, be_u8), |(hi, lo)| {
            Expression::Integer(((hi as u32) << 16) | (lo as u32))
        })(input),
        INT16_SECOND_AND_FOURTH_BYTES => map(pair(be_u8, be_u8), |(hi, lo)| {
            Expression::Integer(((hi as u32) << 24) | ((lo as u32) << 8))
        })(input),
        INT16_FIRST_AND_LAST_BYTES => map(pair(be_u8, be_u8), |(hi, lo)| {
            Expression::Integer(((hi as u32) << 24) | (lo as u32))
        })(input),
        INT24 => map(be_u24, Expression::Integer)(input),
        INT24_SHIFTED_8 => map(be_u24, |value| Expression::Integer(value << 8))(input),
        INT24_FIRST_SECOND_AND_FOURTH_BYTES => map(pair(be_u8, be_u16), |(hi, lo)| {
            Expression::Integer(((hi as u32) << 24) | (lo as u32))
        })(input),
        INT24_FIRST_THIRD_AND_FOURTH_BYTES => map(pair(be_u16, be_u8), |(hi, lo)| {
            Expression::Integer(((hi as u32) << 16) | (lo as u32))
        })(input),
        INT32 => map(be_u32, Expression::Integer)(input),
        TAGGED_TEXT => expression_tagged_text(input),
        _ => Err(nom::Err::Error(Error::from_error_kind(
            input,
            ErrorKind::Alt,
        ))),
    }
}

fn copy_c_string_data(data: &[u8]) -> Result<Vec<NonZeroU8>, Error> {
    data.iter()
        .map(|byte| NonZeroU8::new(*byte).ok_or(Error::NullByte))
        .collect()
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
    map_res(
        pair(expression, take_while(|_| true)),
        |(arg1, arg2)| -> Result<Segment, Error> {
            Ok(Segment::TodoFormat(arg1, copy_c_string_data(arg2)?))
        },
    )(input)
}

fn segment(input: &[u8]) -> IResult<&[u8], Segment, Error> {
    use crate::types::tag::*;

    fn contents<'a>(
        mut f: impl FnMut(&'a [u8]) -> IResult<&'a [u8], Segment, Error>,
    ) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], Segment, Error> {
        move |input| length_value(integer_usize, all_consuming(&mut f))(input)
    }

    let (input, type_byte) = be_u8(input)?;
    match type_byte {
        TODO_RESET_TIME => contents(map_res(
            take_while(|_| true),
            |data: &[u8]| -> Result<Segment, Error> {
                Ok(Segment::TodoResetTime(copy_c_string_data(data)?))
            },
        ))(input),
        TIME => contents(map(expression, Segment::Time))(input),
        IF => contents(segment_if)(input),
        SWITCH => contents(segment_switch)(input),
        TODO_0A => contents(map(expression, Segment::Todo0A))(input),
        IF_EQUALS => contents(segment_if_equals)(input),
        TODO_0F => contents(map(
            tuple((expression, expression, expression)),
            |(player, self_value, other_value)| Segment::Todo0F {
                player,
                self_value,
                other_value,
            },
        ))(input),
        NEW_LINE => contents(segment_no_data(Segment::NewLine))(input),
        GUI_ICON => contents(map(expression, Segment::GuiIcon))(input),
        COLOR_CHANGE => contents(map(expression, Segment::ColorChange))(input),
        TODO_14 => contents(map(expression, Segment::Todo14))(input),
        SOFT_HYPHEN => contents(segment_no_data(Segment::SoftHyphen))(input),
        TODO_17 => contents(segment_no_data(Segment::Todo17))(input),
        TODO_19 => contents(map(boolean, Segment::Todo19))(input),
        EMPHASIS => contents(map(boolean, Segment::Emphasis))(input),
        TODO_1B => contents(map_res(
            take_while(|_| true),
            |data: &[u8]| -> Result<Segment, Error> {
                Ok(Segment::Todo1B(copy_c_string_data(data)?))
            },
        ))(input),
        TODO_1C => contents(map_res(
            take_while(|_| true),
            |data: &[u8]| -> Result<Segment, Error> {
                Ok(Segment::Todo1C(copy_c_string_data(data)?))
            },
        ))(input),
        NON_BREAKING_SPACE => contents(segment_no_data(Segment::NonBreakingSpace))(input),
        COMMAND_ICON => contents(map(expression, Segment::CommandIcon))(input),
        DASH => contents(segment_no_data(Segment::Dash))(input),
        VALUE => contents(map(expression, Segment::Value))(input),
        TODO_FORMAT => contents(segment_todo_format)(input),
        TWO_DIGIT_VALUE => contents(map(expression, Segment::TwoDigitValue))(input),
        TODO_26 => contents(map(
            tuple((expression, expression, expression)),
            |(arg1, arg2, arg3)| Segment::Todo26(arg1, arg2, arg3),
        ))(input),
        SHEET => contents(map(many_m_n(2, usize::MAX, expression), Segment::Sheet))(input),
        TODO_HIGHLIGHT => contents(map(expression, Segment::TodoHighlight))(input),
        LINK => contents(map(many_m_n(1, usize::MAX, expression), Segment::Link))(input),
        SPLIT => contents(segment_split)(input),
        TODO_2D => contents(map(expression, Segment::Todo2D))(input),
        AUTO_TRANSLATE => contents(map(pair(expression, expression), |(arg1, arg2)| {
            Segment::AutoTranslate(arg1, arg2)
        }))(input),
        TODO_2F => contents(map(expression, Segment::Todo2F))(input),
        SHEET_JA => contents(map(many_m_n(3, usize::MAX, expression), Segment::SheetJa))(input),
        SHEET_EN => contents(map(many_m_n(3, usize::MAX, expression), Segment::SheetEn))(input),
        SHEET_DE => contents(map(many_m_n(3, usize::MAX, expression), Segment::SheetDe))(input),
        SHEET_FR => contents(map(many_m_n(3, usize::MAX, expression), Segment::SheetFr))(input),
        TODO_40 => contents(map(expression, Segment::Todo40))(input),
        FOREGROUND => contents(map(expression, Segment::Foreground))(input),
        GLOW => contents(map(expression, Segment::Glow))(input),
        RUBY => contents(map(
            pair(expression, expression),
            |(annotated, annotation)| Segment::Ruby {
                annotated,
                annotation,
            },
        ))(input),
        ZERO_PADDED_VALUE => contents(map(pair(expression, expression), |(value, digits)| {
            Segment::ZeroPaddedValue { value, digits }
        }))(input),
        TODO_51 => contents(map(expression, Segment::Todo51))(input),
        TODO_60 => contents(map_res(
            take_while(|_| true),
            |data: &[u8]| -> Result<Segment, Error> {
                Ok(Segment::Todo60(copy_c_string_data(data)?))
            },
        ))(input),
        TODO_61 => contents(map(expression, Segment::Todo61))(input),
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
    Ok((input, Text::new(segments)))
}

#[cfg(test)]
mod tests {
    use tomestone_common::test_game_data_or_skip;
    use tomestone_exdf::{Dataset, Language, RootList, Value};
    use tomestone_sqpack::GameData;

    use super::tagged_text;

    #[test]
    #[ignore = "slow test"]
    fn string_game_data() {
        let (game_data, mut data_file_set) = test_game_data_or_skip!();

        let root_list = RootList::open(&game_data, &mut data_file_set).unwrap();
        for name in root_list.iter() {
            for language in [
                Language::Japanese,
                Language::English,
                Language::German,
                Language::French,
            ]
            .iter()
            {
                let dataset =
                    Dataset::load(&game_data, &mut data_file_set, name, *language).unwrap();
                for page in dataset.page_iter() {
                    for res in page {
                        let row = res.unwrap();
                        for sub_row in row.sub_rows {
                            for value in sub_row.cells {
                                if let Value::String(data) = value {
                                    let res = tagged_text(data);
                                    if res.is_err() {
                                        eprintln!(
                                            "Error while parsing {} row {}",
                                            name, row.number
                                        );
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
    }
}
