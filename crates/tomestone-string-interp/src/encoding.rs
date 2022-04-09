use std::convert::TryInto;

use crate::{Expression, Segment, Text};

pub enum EncodeError {
    UnrepresentableInteger,
    NullByte,
}

impl std::fmt::Display for EncodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            EncodeError::UnrepresentableInteger => write!(f, "integer value isn't representable"),
            EncodeError::NullByte => write!(f, "null byte in literal text string"),
        }
    }
}

fn encode_integer(buf: &mut Vec<u8>, value: u32) -> Result<(), EncodeError> {
    use crate::types::expr::*;

    match value.to_be_bytes() {
        [0, 0, 0, b] if b <= 0xEE => buf.push(b + 1),
        [0, 0, 0, b] if b != 0 => buf.extend_from_slice(&[BYTE, b]),
        [0, 0, b, 0] if b != 0 => buf.extend_from_slice(&[BYTE_SHIFTED_8, b]),
        [0, b, 0, 0] if b != 0 => buf.extend_from_slice(&[BYTE_SHIFTED_16, b]),
        [b, 0, 0, 0] if b != 0 => buf.extend_from_slice(&[BYTE_SHIFTED_24, b]),
        [0, 0, b1, b0] if b1 != 0 && b0 != 0 => buf.extend_from_slice(&[INT16, b1, b0]),
        [0, b2, b1, 0] if b2 != 0 && b1 != 0 => buf.extend_from_slice(&[INT16_SHIFTED_8, b2, b1]),
        [b3, b2, 0, 0] if b3 != 0 && b2 != 0 => buf.extend_from_slice(&[INT16_SHIFTED_16, b3, b2]),
        [0, b2, 0, b0] if b2 != 0 && b0 != 0 => {
            buf.extend_from_slice(&[INT16_FIRST_AND_THIRD_BYTES, b2, b0])
        }
        [b3, 0, 0, b0] if b3 != 0 && b0 != 0 => {
            buf.extend_from_slice(&[INT16_FIRST_AND_LAST_BYTES, b3, b0])
        }
        [b3, 0, b1, 0] if b3 != 0 && b1 != 0 => {
            buf.extend_from_slice(&[INT16_SECOND_AND_FOURTH_BYTES, b3, b1])
        }
        [0, b2, b1, b0] if b2 != 0 && b1 != 0 && b0 != 0 => {
            buf.extend_from_slice(&[INT24, b2, b1, b0])
        }
        [b3, 0, b1, b0] if b3 != 0 && b1 != 0 && b0 != 0 => {
            buf.extend_from_slice(&[INT24_FIRST_SECOND_AND_FOURTH_BYTES, b3, b1, b0])
        }
        [b3, b2, 0, b0] if b3 != 0 && b2 != 0 && b0 != 0 => {
            buf.extend_from_slice(&[INT24_FIRST_THIRD_AND_FOURTH_BYTES, b3, b2, b0])
        }
        [b3, b2, b1, 0] if b3 != 0 && b2 != 0 && b1 != 0 => {
            buf.extend_from_slice(&[INT24_SHIFTED_8, b3, b2, b1])
        }
        [b3, b2, b1, b0] if b3 != 0 && b2 != 0 && b1 != 0 => {
            buf.extend_from_slice(&[INT32, b3, b2, b1, b0])
        }
        _ => return Err(EncodeError::UnrepresentableInteger), // is this unreachable now?
    }
    Ok(())
}

fn encode_expression(buf: &mut Vec<u8>, expr: &Expression) -> Result<(), EncodeError> {
    use crate::types::expr::*;

    match expr {
        Expression::GreaterThanOrEqual(boite) => {
            buf.push(GEQ);
            encode_expression(buf, &boite.0)?;
            encode_expression(buf, &boite.1)?;
        }
        Expression::GreaterThan(boite) => {
            buf.push(GT);
            encode_expression(buf, &boite.0)?;
            encode_expression(buf, &boite.1)?;
        }
        Expression::LessThanOrEqual(boite) => {
            buf.push(LEQ);
            encode_expression(buf, &boite.0)?;
            encode_expression(buf, &boite.1)?;
        }
        Expression::LessThan(boite) => {
            buf.push(LT);
            encode_expression(buf, &boite.0)?;
            encode_expression(buf, &boite.1)?;
        }
        Expression::Equal(boite) => {
            buf.push(EQ);
            encode_expression(buf, &boite.0)?;
            encode_expression(buf, &boite.1)?;
        }
        Expression::NotEqual(boite) => {
            buf.push(NEQ);
            encode_expression(buf, &boite.0)?;
            encode_expression(buf, &boite.1)?;
        }
        Expression::TopLevelParameter(index) => buf.push(0xD0 | (*index & 0x0F)),
        Expression::InputParameter(parameter_index) => {
            buf.push(INPUT_PARAM);
            encode_integer(buf, *parameter_index)?;
        }
        Expression::PlayerParameter(parameter_index) => {
            buf.push(PLAYER_PARAM);
            encode_integer(buf, *parameter_index)?;
        }
        Expression::StringParameter(parameter_index) => {
            buf.push(STRING_PARAM);
            encode_integer(buf, *parameter_index)?;
        }
        Expression::ObjectParameter(parameter_index) => {
            buf.push(OBJECT_PARAM);
            encode_integer(buf, *parameter_index)?;
        }
        Expression::TodoEC => buf.push(TODO_EC),
        Expression::Integer(value) => match value.to_be_bytes() {
            [0, 0, 0, b] if b <= 0xCE => buf.push(b + 1),
            [0, 0, 0, b] if b != 0 => buf.extend_from_slice(&[BYTE, b]),
            [0, 0, b, 0] if b != 0 => buf.extend_from_slice(&[BYTE_SHIFTED_8, b]),
            [0, b, 0, 0] if b != 0 => buf.extend_from_slice(&[BYTE_SHIFTED_16, b]),
            [b, 0, 0, 0] if b != 0 => buf.extend_from_slice(&[BYTE_SHIFTED_24, b]),
            [0, 0, b1, b0] if b1 != 0 && b0 != 0 => buf.extend_from_slice(&[INT16, b1, b0]),
            [0, b2, b1, 0] if b2 != 0 && b1 != 0 => {
                buf.extend_from_slice(&[INT16_SHIFTED_8, b2, b1])
            }
            [b3, b2, 0, 0] if b3 != 0 && b2 != 0 => {
                buf.extend_from_slice(&[INT16_SHIFTED_16, b3, b2])
            }
            [0, b2, 0, b0] if b2 != 0 && b0 != 0 => {
                buf.extend_from_slice(&[INT16_FIRST_AND_THIRD_BYTES, b2, b0])
            }
            [b3, 0, 0, b0] if b3 != 0 && b0 != 0 => {
                buf.extend_from_slice(&[INT16_FIRST_AND_LAST_BYTES, b3, b0])
            }
            [b3, 0, b1, 0] if b3 != 0 && b1 != 0 => {
                buf.extend_from_slice(&[INT16_SECOND_AND_FOURTH_BYTES, b3, b1])
            }
            [0, b2, b1, b0] if b2 != 0 && b1 != 0 && b0 != 0 => {
                buf.extend_from_slice(&[INT24, b2, b1, b0])
            }
            [b3, 0, b1, b0] if b3 != 0 && b1 != 0 && b0 != 0 => {
                buf.extend_from_slice(&[INT24_FIRST_SECOND_AND_FOURTH_BYTES, b3, b1, b0])
            }
            [b3, b2, 0, b0] if b3 != 0 && b2 != 0 && b0 != 0 => {
                buf.extend_from_slice(&[INT24_FIRST_THIRD_AND_FOURTH_BYTES, b3, b2, b0])
            }
            [b3, b2, b1, 0] if b3 != 0 && b2 != 0 && b1 != 0 => {
                buf.extend_from_slice(&[INT24_SHIFTED_8, b3, b2, b1])
            }
            [b3, b2, b1, b0] if b3 != 0 && b2 != 0 && b1 != 0 => {
                buf.extend_from_slice(&[INT32, b3, b2, b1, b0])
            }
            _ => return Err(EncodeError::UnrepresentableInteger), // is this unreachable now?
        },
        Expression::Text(text) => {
            buf.push(TAGGED_TEXT);
            // TODO: could eliminate allocations by calculating lengths separately
            let mut encoded_text = encode(text)?;
            encode_integer(buf, encoded_text.len().try_into().unwrap())?;
            buf.append(&mut encoded_text);
        }
    }
    Ok(())
}

fn encode_tag(buf: &mut Vec<u8>, tag: &Segment) -> Result<(), EncodeError> {
    use crate::types::tag::*;

    match tag {
        Segment::Literal(string) => {
            if string.chars().any(|c| c == '\u{0}') {
                return Err(EncodeError::NullByte);
            }
            buf.extend_from_slice(string.as_bytes())
        }
        Segment::TodoResetTime(data) => {
            buf.extend_from_slice(&[2, TODO_RESET_TIME]);
            encode_integer(buf, data.len().try_into().unwrap())?;
            buf.extend(data.iter().map(|byte| byte.get()));
            buf.push(3);
        }
        Segment::Time(expr) => {
            buf.extend_from_slice(&[2, TIME]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, expr)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::If {
            condition,
            true_value,
            false_value,
        } => {
            buf.extend_from_slice(&[2, IF]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, condition)?;
            encode_expression(&mut tag_data, true_value)?;
            encode_expression(&mut tag_data, false_value)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::Switch {
            discriminant,
            cases,
        } => {
            buf.extend_from_slice(&[2, SWITCH]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, discriminant)?;
            for case in cases.iter() {
                encode_expression(&mut tag_data, case)?;
            }
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::Todo0A(expr) => {
            buf.extend_from_slice(&[2, TODO_0A]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, expr)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::IfEquals {
            left,
            right,
            true_value,
            false_value,
        } => {
            buf.extend_from_slice(&[2, IF_EQUALS]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, left)?;
            encode_expression(&mut tag_data, right)?;
            encode_expression(&mut tag_data, true_value)?;
            encode_expression(&mut tag_data, false_value)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::Todo0F {
            player,
            self_value,
            other_value,
        } => {
            buf.extend_from_slice(&[2, TODO_0F]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, player)?;
            encode_expression(&mut tag_data, self_value)?;
            encode_expression(&mut tag_data, other_value)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::NewLine => {
            buf.extend_from_slice(&[2, NEW_LINE, 1, 3]);
        }
        Segment::GuiIcon(expr) => {
            buf.extend_from_slice(&[2, GUI_ICON]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, expr)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::ColorChange(expr) => {
            buf.extend_from_slice(&[2, COLOR_CHANGE]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, expr)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::Todo14(expr) => {
            buf.extend_from_slice(&[2, TODO_14]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, expr)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::SoftHyphen => {
            buf.extend_from_slice(&[2, SOFT_HYPHEN, 1, 3]);
        }
        Segment::Todo17 => {
            buf.extend_from_slice(&[2, TODO_17, 1, 3]);
        }
        Segment::Todo19(value) => {
            buf.extend_from_slice(&[2, TODO_19]);
            let mut tag_data = vec![];
            encode_integer(&mut tag_data, *value as u32)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::Emphasis(value) => {
            buf.extend_from_slice(&[2, EMPHASIS]);
            let mut tag_data = vec![];
            encode_integer(&mut tag_data, *value as u32)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::Todo1B(data) => {
            buf.extend_from_slice(&[2, TODO_1B]);
            encode_integer(buf, data.len().try_into().unwrap())?;
            buf.extend(data.iter().map(|byte| byte.get()));
            buf.push(3);
        }
        Segment::Todo1C(data) => {
            buf.extend_from_slice(&[2, TODO_1C]);
            encode_integer(buf, data.len().try_into().unwrap())?;
            buf.extend(data.iter().map(|byte| byte.get()));
            buf.push(3);
        }
        Segment::NonBreakingSpace => {
            buf.extend_from_slice(&[2, NON_BREAKING_SPACE, 1, 3]);
        }
        Segment::CommandIcon(expr) => {
            buf.extend_from_slice(&[2, COMMAND_ICON]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, expr)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::Dash => {
            buf.extend_from_slice(&[2, DASH, 1, 3]);
        }
        Segment::IntegerValue(expr) => {
            buf.extend_from_slice(&[2, INTEGER_VALUE]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, expr)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::TodoFormat(expr, data) => {
            buf.extend_from_slice(&[2, TODO_FORMAT]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, expr)?;
            tag_data.extend(data.iter().map(|byte| byte.get()));
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::TwoDigitValue(expr) => {
            buf.extend_from_slice(&[2, TWO_DIGIT_VALUE]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, expr)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::Todo26(arg1, arg2, arg3) => {
            buf.extend_from_slice(&[2, TODO_26]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, arg1)?;
            encode_expression(&mut tag_data, arg2)?;
            encode_expression(&mut tag_data, arg3)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::Sheet {
            name,
            row_index,
            column_index,
            parameters,
        } => {
            buf.extend_from_slice(&[2, SHEET]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, name)?;
            encode_expression(&mut tag_data, row_index)?;
            if let Some(column_index) = column_index {
                encode_expression(&mut tag_data, column_index)?;
            }
            for param in parameters.iter() {
                encode_expression(&mut tag_data, param)?;
            }
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::TodoStringValue1(expr) => {
            buf.extend_from_slice(&[2, TODO_STRING_VALUE_1]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, expr)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::TodoStringValue2(expr) => {
            buf.extend_from_slice(&[2, TODO_STRING_VALUE_2]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, expr)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::Split {
            input,
            separator,
            index,
        } => {
            buf.extend_from_slice(&[2, SPLIT]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, input)?;
            encode_expression(&mut tag_data, separator)?;
            encode_expression(&mut tag_data, index)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::StringValueTitleCase(expr) => {
            buf.extend_from_slice(&[2, STRING_VALUE_TITLE_CASE]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, expr)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::AutoTranslate(arg1, arg2) => {
            buf.extend_from_slice(&[2, AUTO_TRANSLATE]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, arg1)?;
            encode_expression(&mut tag_data, arg2)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::StringValueLowerCase(expr) => {
            buf.extend_from_slice(&[2, STRING_VALUE_LOWER_CASE]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, expr)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::SheetJa(args) => {
            buf.extend_from_slice(&[2, SHEET_JA]);
            let mut tag_data = vec![];
            for arg in args.iter() {
                encode_expression(&mut tag_data, arg)?;
            }
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::SheetEn(args) => {
            buf.extend_from_slice(&[2, SHEET_EN]);
            let mut tag_data = vec![];
            for arg in args.iter() {
                encode_expression(&mut tag_data, arg)?;
            }
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::SheetDe(args) => {
            buf.extend_from_slice(&[2, SHEET_DE]);
            let mut tag_data = vec![];
            for arg in args.iter() {
                encode_expression(&mut tag_data, arg)?;
            }
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::SheetFr(args) => {
            buf.extend_from_slice(&[2, SHEET_FR]);
            let mut tag_data = vec![];
            for arg in args.iter() {
                encode_expression(&mut tag_data, arg)?;
            }
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::Todo40(expr) => {
            buf.extend_from_slice(&[2, TODO_40]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, expr)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::Foreground(expr) => {
            buf.extend_from_slice(&[2, FOREGROUND]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, expr)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::Glow(expr) => {
            buf.extend_from_slice(&[2, GLOW]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, expr)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::Ruby {
            annotated,
            annotation,
        } => {
            buf.extend_from_slice(&[2, RUBY]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, annotated)?;
            encode_expression(&mut tag_data, annotation)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::ZeroPaddedValue { value, digits } => {
            buf.extend_from_slice(&[2, ZERO_PADDED_VALUE]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, value)?;
            encode_expression(&mut tag_data, digits)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::Todo51(expr) => {
            buf.extend_from_slice(&[2, TODO_51]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, expr)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
        Segment::Todo60(data) => {
            buf.extend_from_slice(&[2, TODO_60]);
            encode_integer(buf, data.len().try_into().unwrap())?;
            buf.extend(data.iter().map(|byte| byte.get()));
            buf.push(3);
        }
        Segment::Todo61(expr) => {
            buf.extend_from_slice(&[2, TODO_61]);
            let mut tag_data = vec![];
            encode_expression(&mut tag_data, expr)?;
            encode_integer(buf, tag_data.len().try_into().unwrap())?;
            buf.append(&mut tag_data);
            buf.push(3);
        }
    }
    Ok(())
}

fn encode_text(buf: &mut Vec<u8>, text: &Text) -> Result<(), EncodeError> {
    for segment in text.segments.iter() {
        encode_tag(buf, segment)?;
    }
    Ok(())
}

pub fn encode(text: &Text) -> Result<Vec<u8>, EncodeError> {
    let mut buf = vec![];
    encode_text(&mut buf, text)?;
    Ok(buf)
}
