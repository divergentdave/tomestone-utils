//! Manually-implemented serde serialization and deserialization for tagged text ASTs. This is
//! manually implemented so that only the well-understood parts of the format can be serialized,
//! and other enum variants can be freely changed in the future. If needed, deserialization could
//! be customized to prevent backwards compatibility issues.

use std::{marker::PhantomData, mem::MaybeUninit};

use crate::{Expression, Segment, Text};
use serde::{
    de::{EnumAccess, Error as DeError, MapAccess, SeqAccess, VariantAccess, Visitor},
    ser::{Error as SerError, SerializeSeq, SerializeStructVariant, SerializeTupleVariant},
    Deserialize, Deserializer, Serialize, Serializer,
};

static EXPRESSION_NAME: &str = "expr";
static EXPRESSION_VARIANTS: &[&str] = &[
    "int",              // 0
    "param",            // 1
    "geq",              // 2
    "gt",               // 3
    "leq",              // 4
    "lt",               // 5
    "eq",               // 6
    "neq",              // 7
    "input_param",      // 8
    "player_param",     // 9
    "string_param",     // 10
    "object_param",     // 11
    "placeholder_0xec", // 12
    "text",             // 13
];
static SEGMENT_NAME: &str = "segment";
static SEGMENT_VARIANTS: &[&str] = &[
    "literal",            // 0
    "placeholder_0x06",   // 1
    "time",               // 2
    "if",                 // 3
    "switch",             // 4
    "placeholder_0x0a",   // 5
    "if_equals",          // 6
    "placeholder_0x0f",   // 7
    "new_line",           // 8
    "gui_icon",           // 9
    "color_change",       // 10
    "placeholder_0x14",   // 11
    "soft_hyphen",        // 12
    "placeholder_0x17",   // 13
    "placeholder_0x19",   // 14
    "emphasis",           // 15
    "placeholder_0x1b",   // 16
    "placeholder_0x1c",   // 17
    "non_breaking_space", // 18
    "command_icon",       // 19
    "dash",               // 20
    "value",              // 21
    "placeholder_0x22",   // 22
    "two_digit_value",    // 23
    "placeholder_0x26",   // 24
    "sheet",              // 25
    "placeholder_0x29",   // 26
    "link",               // 27
    "split",              // 28
    "placeholder_0x2d",   // 29
    "auto_translate",     // 30
    "placeholder_0x2f",   // 31
    "sheet_ja",           // 32
    "sheet_en",           // 33
    "sheet_de",           // 34
    "sheet_fr",           // 35
    "placeholder_0x40",   // 36
    "foreground",         // 37
    "glow",               // 38
    "ruby",               // 39
    "zero_padded_value",  // 40
    "placeholder_0x51",   // 41
    "placeholder_0x60",   // 42
    "placeholder_0x61",   // 43
];
static IF_SEGMENT_FIELDS: &[&str] = &["condition", "true_value", "false_value"];
static SHEET_SEGMENT_FIELDS: &[&str] = &["name", "row_index", "column_index", "parameters"];
static SPLIT_SEGMENT_FIELDS: &[&str] = &["input", "separator", "index"];
static RUBY_SEGMENT_FIELDS: &[&str] = &["annotated", "annotation"];

impl Serialize for Expression {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Expression::Integer(value) => serializer.serialize_newtype_variant(
                EXPRESSION_NAME,
                0,
                EXPRESSION_VARIANTS[0],
                value,
            ),
            Expression::TopLevelParameter(value) => serializer.serialize_newtype_variant(
                EXPRESSION_NAME,
                1,
                EXPRESSION_VARIANTS[1],
                value,
            ),
            Expression::GreaterThanOrEqual(boite) => {
                let mut variant = serializer.serialize_tuple_variant(
                    EXPRESSION_NAME,
                    2,
                    EXPRESSION_VARIANTS[2],
                    2,
                )?;
                variant.serialize_field(&boite.0)?;
                variant.serialize_field(&boite.1)?;
                variant.end()
            }
            Expression::GreaterThan(boite) => {
                let mut variant = serializer.serialize_tuple_variant(
                    EXPRESSION_NAME,
                    3,
                    EXPRESSION_VARIANTS[3],
                    2,
                )?;
                variant.serialize_field(&boite.0)?;
                variant.serialize_field(&boite.1)?;
                variant.end()
            }
            Expression::LessThanOrEqual(boite) => {
                let mut variant = serializer.serialize_tuple_variant(
                    EXPRESSION_NAME,
                    4,
                    EXPRESSION_VARIANTS[4],
                    2,
                )?;
                variant.serialize_field(&boite.0)?;
                variant.serialize_field(&boite.1)?;
                variant.end()
            }
            Expression::LessThan(boite) => {
                let mut variant = serializer.serialize_tuple_variant(
                    EXPRESSION_NAME,
                    5,
                    EXPRESSION_VARIANTS[5],
                    2,
                )?;
                variant.serialize_field(&boite.0)?;
                variant.serialize_field(&boite.1)?;
                variant.end()
            }
            Expression::Equal(boite) => {
                let mut variant = serializer.serialize_tuple_variant(
                    EXPRESSION_NAME,
                    6,
                    EXPRESSION_VARIANTS[6],
                    2,
                )?;
                variant.serialize_field(&boite.0)?;
                variant.serialize_field(&boite.1)?;
                variant.end()
            }
            Expression::NotEqual(boite) => {
                let mut variant = serializer.serialize_tuple_variant(
                    EXPRESSION_NAME,
                    7,
                    EXPRESSION_VARIANTS[7],
                    2,
                )?;
                variant.serialize_field(&boite.0)?;
                variant.serialize_field(&boite.1)?;
                variant.end()
            }
            Expression::InputParameter(value) => serializer.serialize_newtype_variant(
                EXPRESSION_NAME,
                8,
                EXPRESSION_VARIANTS[8],
                value,
            ),
            Expression::PlayerParameter(value) => serializer.serialize_newtype_variant(
                EXPRESSION_NAME,
                9,
                EXPRESSION_VARIANTS[9],
                value,
            ),
            Expression::StringParameter(value) => serializer.serialize_newtype_variant(
                EXPRESSION_NAME,
                10,
                EXPRESSION_VARIANTS[10],
                value,
            ),
            Expression::ObjectParameter(value) => serializer.serialize_newtype_variant(
                EXPRESSION_NAME,
                11,
                EXPRESSION_VARIANTS[11],
                value,
            ),
            Expression::TodoEC => Err(S::Error::custom(
                "serialization of expressions with tag 0xec is not yet supported",
            )),
            Expression::Text(boxed_text) => serializer.serialize_newtype_variant(
                EXPRESSION_NAME,
                13,
                EXPRESSION_VARIANTS[13],
                &*boxed_text,
            ),
        }
    }
}

/// Marker to indicate which variant of `Expression` should be deserialized.
enum ExpressionVariant {
    Integer,
    TopLevelParameter,
    GreaterThanOrEqual,
    GreaterThan,
    LessThanOrEqual,
    LessThan,
    Equal,
    NotEqual,
    InputParameter,
    PlayerParameter,
    StringParameter,
    ObjectParameter,
    Text,
}

struct ExpressionVariantVisitor;

impl<'de> Visitor<'de> for ExpressionVariantVisitor {
    type Value = ExpressionVariant;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str(
            "`int`, `param`, `geq`, `gt`, `leq`, `lt`, `eq`, `neq`, `input_param`, \
            `player_param`, `string_param`, `object_param`, or `text`",
        )
    }

    fn visit_str<E>(self, s: &str) -> Result<ExpressionVariant, E>
    where
        E: DeError,
    {
        match s {
            "int" => Ok(ExpressionVariant::Integer),
            "param" => Ok(ExpressionVariant::TopLevelParameter),
            "geq" => Ok(ExpressionVariant::GreaterThanOrEqual),
            "gt" => Ok(ExpressionVariant::GreaterThan),
            "leq" => Ok(ExpressionVariant::LessThanOrEqual),
            "lt" => Ok(ExpressionVariant::LessThan),
            "eq" => Ok(ExpressionVariant::Equal),
            "neq" => Ok(ExpressionVariant::NotEqual),
            "input_param" => Ok(ExpressionVariant::InputParameter),
            "player_param" => Ok(ExpressionVariant::PlayerParameter),
            "string_param" => Ok(ExpressionVariant::StringParameter),
            "object_param" => Ok(ExpressionVariant::ObjectParameter),
            "text" => Ok(ExpressionVariant::Text),
            _ => Err(E::unknown_variant(s, EXPRESSION_VARIANTS)),
        }
    }
}

impl<'de> Deserialize<'de> for ExpressionVariant {
    fn deserialize<D>(deserializer: D) -> Result<ExpressionVariant, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_identifier(ExpressionVariantVisitor)
    }
}

/// Fully generic serde array visitor.
struct ArrayVisitor<A> {
    marker: PhantomData<A>,
}

impl<A> ArrayVisitor<A> {
    fn new() -> ArrayVisitor<A> {
        ArrayVisitor {
            marker: PhantomData,
        }
    }
}

impl<'de, T, const N: usize> Visitor<'de> for ArrayVisitor<[T; N]>
where
    T: Deserialize<'de> + Sized,
{
    type Value = [T; N];

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("an array of length ")?;
        <usize as std::fmt::Display>::fmt(&N, formatter)
    }

    fn visit_seq<S>(self, mut access: S) -> Result<[T; N], S::Error>
    where
        S: SeqAccess<'de>,
    {
        // Store the partially uninitialized array and initialization progress in a drop guard,
        // so that we can clean up to avoid memory leaks in the event of a panic or error in
        // deserialization.
        struct DropGuard<T, const N: usize> {
            valid_idx: Option<usize>,
            array: Option<[MaybeUninit<T>; N]>,
        }

        impl<T, const N: usize> DropGuard<T, N> {
            fn new() -> DropGuard<T, N> {
                DropGuard {
                    valid_idx: None,
                    // It is safe to assume_init on an array of MaybeUninit<T>, as they don't have
                    // initialization requirements.
                    array: Some(unsafe { MaybeUninit::uninit().assume_init() }),
                }
            }
        }

        impl<T, const N: usize> Drop for DropGuard<T, N> {
            fn drop(&mut self) {
                if let (Some(valid_idx), Some(array)) = (self.valid_idx, &mut self.array) {
                    for elem in &mut array.as_mut()[..=valid_idx] {
                        unsafe {
                            std::ptr::drop_in_place(elem.as_mut_ptr());
                        }
                    }
                }
            }
        }

        let mut drop_guard = DropGuard::<T, N>::new();

        for (i, elem) in drop_guard.array.as_mut().unwrap()[..]
            .iter_mut()
            .enumerate()
        {
            let value = match access.next_element()? {
                Some(value) => value,
                None => return Err(S::Error::invalid_length(i, &self)),
            };
            unsafe {
                std::ptr::write(elem.as_mut_ptr(), value);
            }
            drop_guard.valid_idx = Some(i);
        }
        // No more panics from user code, nor early exit for error handling, so take the array out
        // of the drop guard. Cast its pointer to the final type now that it has been fully
        // initialized, and read from it.
        let array_of_maybeuninit = drop_guard.array.take().unwrap();
        let ptr = &array_of_maybeuninit as *const [MaybeUninit<T>; N] as *const [T; N];
        let output_array = unsafe { ptr.read() };
        Ok(output_array)
    }
}

struct ExpressionVisitor;

impl<'de> Visitor<'de> for ExpressionVisitor {
    type Value = Expression;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("an enum representing an expression")
    }

    fn visit_enum<E>(self, access: E) -> Result<Expression, E::Error>
    where
        E: EnumAccess<'de>,
    {
        let (variant, variant_access) = access.variant()?;
        match variant {
            ExpressionVariant::Integer => {
                Ok(Expression::Integer(variant_access.newtype_variant()?))
            }
            ExpressionVariant::TopLevelParameter => Ok(Expression::TopLevelParameter(
                variant_access.newtype_variant()?,
            )),
            ExpressionVariant::GreaterThanOrEqual => Ok(Expression::GreaterThanOrEqual(Box::new(
                variant_access
                    .tuple_variant(2, ArrayVisitor::<[Expression; 2]>::new())
                    .map(|[left, right]| (left, right))?,
            ))),
            ExpressionVariant::GreaterThan => Ok(Expression::GreaterThan(Box::new(
                variant_access
                    .tuple_variant(2, ArrayVisitor::<[Expression; 2]>::new())
                    .map(|[left, right]| (left, right))?,
            ))),
            ExpressionVariant::LessThanOrEqual => Ok(Expression::LessThanOrEqual(Box::new(
                variant_access
                    .tuple_variant(2, ArrayVisitor::<[Expression; 2]>::new())
                    .map(|[left, right]| (left, right))?,
            ))),
            ExpressionVariant::LessThan => Ok(Expression::LessThan(Box::new(
                variant_access
                    .tuple_variant(2, ArrayVisitor::<[Expression; 2]>::new())
                    .map(|[left, right]| (left, right))?,
            ))),
            ExpressionVariant::Equal => Ok(Expression::Equal(Box::new(
                variant_access
                    .tuple_variant(2, ArrayVisitor::<[Expression; 2]>::new())
                    .map(|[left, right]| (left, right))?,
            ))),
            ExpressionVariant::NotEqual => Ok(Expression::NotEqual(Box::new(
                variant_access
                    .tuple_variant(2, ArrayVisitor::<[Expression; 2]>::new())
                    .map(|[left, right]| (left, right))?,
            ))),
            ExpressionVariant::InputParameter => Ok(Expression::InputParameter(
                variant_access.newtype_variant()?,
            )),
            ExpressionVariant::PlayerParameter => Ok(Expression::PlayerParameter(
                variant_access.newtype_variant()?,
            )),
            ExpressionVariant::StringParameter => Ok(Expression::StringParameter(
                variant_access.newtype_variant()?,
            )),
            ExpressionVariant::ObjectParameter => Ok(Expression::ObjectParameter(
                variant_access.newtype_variant()?,
            )),
            ExpressionVariant::Text => Ok(Expression::Text(variant_access.newtype_variant()?)),
        }
    }
}

impl<'de> Deserialize<'de> for Expression {
    fn deserialize<D>(deserializer: D) -> Result<Expression, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_enum("expr", EXPRESSION_VARIANTS, ExpressionVisitor)
    }
}

impl Serialize for Segment {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Segment::Literal(literal_text) => serializer.serialize_newtype_variant(
                SEGMENT_NAME,
                0,
                SEGMENT_VARIANTS[0],
                literal_text,
            ),
            Segment::TodoResetTime(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x06 is not yet supported",
            )),
            Segment::Time(timestamp) => serializer.serialize_newtype_variant(
                SEGMENT_NAME,
                2,
                SEGMENT_VARIANTS[2],
                timestamp,
            ),
            Segment::If {
                condition,
                true_value,
                false_value,
            } => {
                let mut variant =
                    serializer.serialize_struct_variant(SEGMENT_NAME, 3, SEGMENT_VARIANTS[3], 3)?;
                variant.serialize_field("condition", condition)?;
                variant.serialize_field("true_value", true_value)?;
                variant.serialize_field("false_value", false_value)?;
                variant.end()
            }
            Segment::Switch {
                discriminant: _discriminant,
                cases: _cases,
            } => Err(S::Error::custom(
                "serialization of segments with tag 0x09 is not yet supported",
            )),
            Segment::Todo0A(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x0a is not yet supported",
            )),
            Segment::IfEquals {
                left: _left,
                right: _right,
                true_value: _true_value,
                false_value: _false_value,
            } => Err(S::Error::custom(
                "serialization of segments with tag 0x0c is not yet supported",
            )),
            Segment::Todo0F { .. } => Err(S::Error::custom(
                "serialization of segments with tag 0x0f is not yet supported",
            )),
            Segment::NewLine => {
                serializer.serialize_unit_variant(SEGMENT_NAME, 8, SEGMENT_VARIANTS[8])
            }
            Segment::GuiIcon(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x12 is not yet supported",
            )),
            Segment::ColorChange(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x13 is not yet supported",
            )),
            Segment::Todo14(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x14 is not yet supported",
            )),
            Segment::SoftHyphen => {
                serializer.serialize_unit_variant(SEGMENT_NAME, 12, SEGMENT_VARIANTS[12])
            }
            Segment::Todo17 => Err(S::Error::custom(
                "serialization of segments with tag 0x17 is not yet supported",
            )),
            Segment::Todo19(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x19 is not yet supported",
            )),
            Segment::Emphasis(flag) => {
                serializer.serialize_newtype_variant(SEGMENT_NAME, 15, SEGMENT_VARIANTS[15], flag)
            }
            Segment::Todo1B(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x1b is not yet supported",
            )),
            Segment::Todo1C(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x1c is not yet supported",
            )),
            Segment::NonBreakingSpace => {
                serializer.serialize_unit_variant(SEGMENT_NAME, 18, SEGMENT_VARIANTS[18])
            }
            Segment::CommandIcon(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x1e is not yet supported",
            )),
            Segment::Dash => {
                serializer.serialize_unit_variant(SEGMENT_NAME, 20, SEGMENT_VARIANTS[20])
            }
            Segment::IntegerValue(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x20 is not yet supported",
            )),
            Segment::TodoFormat(_, _) => Err(S::Error::custom(
                "serialization of segments with tag 0x22 is not yet supported",
            )),
            Segment::TwoDigitValue(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x24 is not yet supported",
            )),
            Segment::Todo26(_, _, _) => Err(S::Error::custom(
                "serialization of segments with tag 0x26 is not yet supported",
            )),
            Segment::Sheet {
                name,
                row_index,
                column_index,
                parameters,
            } => {
                let mut variant = serializer.serialize_struct_variant(
                    SEGMENT_NAME,
                    25,
                    SEGMENT_VARIANTS[25],
                    4,
                )?;
                variant.serialize_field("name", name)?;
                variant.serialize_field("row_index", row_index)?;
                variant.serialize_field("column_index", column_index)?;
                variant.serialize_field("parameters", parameters)?;
                variant.end()
            }
            Segment::TodoStringValue1(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x29 is not yet supported",
            )),
            Segment::TodoStringValue2(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x2b is not yet supported",
            )),
            Segment::Split {
                input,
                separator,
                index,
            } => {
                let mut variant = serializer.serialize_struct_variant(
                    SEGMENT_NAME,
                    28,
                    SEGMENT_VARIANTS[28],
                    3,
                )?;
                variant.serialize_field("input", input)?;
                variant.serialize_field("separator", separator)?;
                variant.serialize_field("index", index)?;
                variant.end()
            }
            Segment::StringValueTitleCase(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x2d is not yet supported",
            )),
            Segment::AutoTranslate(_, _) => Err(S::Error::custom(
                "serialization of segments with tag 0x2e is not yet supported",
            )),
            Segment::StringValueLowerCase(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x2f is not yet supported",
            )),
            Segment::SheetJa(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x30 is not yet supported",
            )),
            Segment::SheetEn(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x31 is not yet supported",
            )),
            Segment::SheetDe(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x32 is not yet supported",
            )),
            Segment::SheetFr(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x33 is not yet supported",
            )),
            Segment::Todo40(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x40 is not yet supported",
            )),
            Segment::Foreground(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x48 is not yet supported",
            )),
            Segment::Glow(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x49 is not yet supported",
            )),
            Segment::Ruby {
                annotated,
                annotation,
            } => {
                let mut variant = serializer.serialize_struct_variant(
                    SEGMENT_NAME,
                    39,
                    SEGMENT_VARIANTS[39],
                    2,
                )?;
                variant.serialize_field("annotated", annotated)?;
                variant.serialize_field("annotation", annotation)?;
                variant.end()
            }
            Segment::ZeroPaddedValue {
                value: _value,
                digits: _digits,
            } => Err(S::Error::custom(
                "serialization of segments with tag 0x50 is not yet supported",
            )),
            Segment::Todo51(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x51 is not yet supported",
            )),
            Segment::Todo60(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x60 is not yet supported",
            )),
            Segment::Todo61(_) => Err(S::Error::custom(
                "serialization of segments with tag 0x61 is not yet supported",
            )),
        }
    }
}

/// Marker to differentiate between the fields of `Segment::If` when deserializing.
enum IfSegmentField {
    Condition,
    TrueValue,
    FalseValue,
}

struct IfSegmentFieldVisitor;

impl<'de> Visitor<'de> for IfSegmentFieldVisitor {
    type Value = IfSegmentField;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("`condition`, `true_value`, or `false_value`")
    }

    fn visit_str<E>(self, value: &str) -> Result<IfSegmentField, E>
    where
        E: DeError,
    {
        match value {
            "condition" => Ok(IfSegmentField::Condition),
            "true_value" => Ok(IfSegmentField::TrueValue),
            "false_value" => Ok(IfSegmentField::FalseValue),
            _ => Err(E::unknown_field(value, IF_SEGMENT_FIELDS)),
        }
    }
}

impl<'de> Deserialize<'de> for IfSegmentField {
    fn deserialize<D>(deserializer: D) -> Result<IfSegmentField, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_identifier(IfSegmentFieldVisitor)
    }
}

struct IfSegmentVisitor;

impl<'de> Visitor<'de> for IfSegmentVisitor {
    type Value = Segment;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a struct with fields `condition`, `true_value`, and `false_value`")
    }

    fn visit_map<M>(self, mut access: M) -> Result<Segment, M::Error>
    where
        M: MapAccess<'de>,
    {
        let mut condition = None;
        let mut true_value = None;
        let mut false_value = None;
        while let Some(key) = access.next_key()? {
            match key {
                IfSegmentField::Condition => {
                    if condition.is_some() {
                        return Err(M::Error::duplicate_field("condition"));
                    }
                    condition = Some(access.next_value()?);
                }
                IfSegmentField::TrueValue => {
                    if true_value.is_some() {
                        return Err(M::Error::duplicate_field("true_value"));
                    }
                    true_value = Some(access.next_value()?);
                }
                IfSegmentField::FalseValue => {
                    if false_value.is_some() {
                        return Err(M::Error::duplicate_field("false_value"));
                    }
                    false_value = Some(access.next_value()?);
                }
            }
        }
        match (condition, true_value, false_value) {
            (None, _, _) => Err(M::Error::missing_field("condition")),
            (Some(_), None, _) => Err(M::Error::missing_field("true_value")),
            (Some(_), Some(_), None) => Err(M::Error::missing_field("false_value")),
            (Some(condition), Some(true_value), Some(false_value)) => Ok(Segment::If {
                condition,
                true_value,
                false_value,
            }),
        }
    }
}

/// Marker to differentiate between the fields of `Segment::Sheet` when deserializing.
enum SheetSegmentField {
    Name,
    RowIndex,
    ColumnIndex,
    Parameters,
}

struct SheetSegmentFieldVisitor;

impl<'de> Visitor<'de> for SheetSegmentFieldVisitor {
    type Value = SheetSegmentField;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("`name`, `row_index`, `column_index`, or `parameters`")
    }

    fn visit_str<E>(self, value: &str) -> Result<SheetSegmentField, E>
    where
        E: DeError,
    {
        match value {
            "name" => Ok(SheetSegmentField::Name),
            "row_index" => Ok(SheetSegmentField::RowIndex),
            "column_index" => Ok(SheetSegmentField::ColumnIndex),
            "parameters" => Ok(SheetSegmentField::Parameters),
            _ => Err(E::unknown_field(value, SPLIT_SEGMENT_FIELDS)),
        }
    }
}

impl<'de> Deserialize<'de> for SheetSegmentField {
    fn deserialize<D>(deserializer: D) -> Result<SheetSegmentField, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_identifier(SheetSegmentFieldVisitor)
    }
}

struct SheetSegmentVisitor;

impl<'de> Visitor<'de> for SheetSegmentVisitor {
    type Value = Segment;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter
            .write_str("a struct with fields `name`, `row_index`, `column_index`, and `parameters`")
    }

    fn visit_map<M>(self, mut access: M) -> Result<Segment, M::Error>
    where
        M: MapAccess<'de>,
    {
        let mut name = None;
        let mut row_index = None;
        let mut column_index = None;
        let mut parameters = None;
        while let Some(key) = access.next_key()? {
            match key {
                SheetSegmentField::Name => {
                    if name.is_some() {
                        return Err(M::Error::duplicate_field("name"));
                    }
                    name = Some(access.next_value()?);
                }
                SheetSegmentField::RowIndex => {
                    if row_index.is_some() {
                        return Err(M::Error::duplicate_field("row_index"));
                    }
                    row_index = Some(access.next_value()?);
                }
                SheetSegmentField::ColumnIndex => {
                    if column_index.is_some() {
                        return Err(M::Error::duplicate_field("column_index"));
                    }
                    column_index = Some(access.next_value()?);
                }
                SheetSegmentField::Parameters => {
                    if parameters.is_some() {
                        return Err(M::Error::duplicate_field("parameters"));
                    }
                    parameters = Some(access.next_value()?);
                }
            }
        }
        match (name, row_index, column_index, parameters) {
            (None, _, _, _) => Err(M::Error::missing_field("name")),
            (Some(_), None, _, _) => Err(M::Error::missing_field("row_index")),
            (Some(_), Some(_), None, _) => Err(M::Error::missing_field("column_index")),
            (Some(_), Some(_), Some(_), None) => Err(M::Error::missing_field("parameters")),
            (Some(name), Some(row_index), Some(column_index), Some(parameters)) => {
                Ok(Segment::Sheet {
                    name,
                    row_index,
                    column_index,
                    parameters,
                })
            }
        }
    }
}

/// Marker to differentiate between the fields of `Segment::Split` when deserializing.
enum SplitSegmentField {
    Input,
    Separator,
    Index,
}

struct SplitSegmentFieldVisitor;

impl<'de> Visitor<'de> for SplitSegmentFieldVisitor {
    type Value = SplitSegmentField;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("`input`, `separator`, or `index`")
    }

    fn visit_str<E>(self, value: &str) -> Result<SplitSegmentField, E>
    where
        E: DeError,
    {
        match value {
            "input" => Ok(SplitSegmentField::Input),
            "separator" => Ok(SplitSegmentField::Separator),
            "index" => Ok(SplitSegmentField::Index),
            _ => Err(E::unknown_field(value, SPLIT_SEGMENT_FIELDS)),
        }
    }
}

impl<'de> Deserialize<'de> for SplitSegmentField {
    fn deserialize<D>(deserializer: D) -> Result<SplitSegmentField, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_identifier(SplitSegmentFieldVisitor)
    }
}

struct SplitSegmentVisitor;

impl<'de> Visitor<'de> for SplitSegmentVisitor {
    type Value = Segment;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a struct with fields `input`, `separator`, and `index`")
    }

    fn visit_map<M>(self, mut access: M) -> Result<Segment, M::Error>
    where
        M: MapAccess<'de>,
    {
        let mut input = None;
        let mut separator = None;
        let mut index = None;
        while let Some(key) = access.next_key()? {
            match key {
                SplitSegmentField::Input => {
                    if input.is_some() {
                        return Err(M::Error::duplicate_field("input"));
                    }
                    input = Some(access.next_value()?);
                }
                SplitSegmentField::Separator => {
                    if separator.is_some() {
                        return Err(M::Error::duplicate_field("separator"));
                    }
                    separator = Some(access.next_value()?);
                }
                SplitSegmentField::Index => {
                    if index.is_some() {
                        return Err(M::Error::duplicate_field("index"));
                    }
                    index = Some(access.next_value()?);
                }
            }
        }
        match (input, separator, index) {
            (None, _, _) => Err(M::Error::missing_field("input")),
            (Some(_), None, _) => Err(M::Error::missing_field("separator")),
            (Some(_), Some(_), None) => Err(M::Error::missing_field("index")),
            (Some(input), Some(separator), Some(index)) => Ok(Segment::Split {
                input,
                separator,
                index,
            }),
        }
    }
}

/// Marker to differentiate between the fields of `Segment::Ruby` when deserializing.
enum RubySegmentField {
    Annotated,
    Annotation,
}

struct RubySegmentFieldVisitor;

impl<'de> Visitor<'de> for RubySegmentFieldVisitor {
    type Value = RubySegmentField;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("`annotated` or `annotation`")
    }

    fn visit_str<E>(self, value: &str) -> Result<RubySegmentField, E>
    where
        E: DeError,
    {
        match value {
            "annotated" => Ok(RubySegmentField::Annotated),
            "annotation" => Ok(RubySegmentField::Annotation),
            _ => Err(E::unknown_field(value, RUBY_SEGMENT_FIELDS)),
        }
    }
}

impl<'de> Deserialize<'de> for RubySegmentField {
    fn deserialize<D>(deserializer: D) -> Result<RubySegmentField, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_identifier(RubySegmentFieldVisitor)
    }
}

struct RubySegmentVisitor;

impl<'de> Visitor<'de> for RubySegmentVisitor {
    type Value = Segment;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a struct with fields `annotated` and `annotation`")
    }

    fn visit_map<M>(self, mut access: M) -> Result<Segment, M::Error>
    where
        M: MapAccess<'de>,
    {
        let mut annotated = None;
        let mut annotation = None;
        while let Some(key) = access.next_key()? {
            match key {
                RubySegmentField::Annotated => {
                    if annotated.is_some() {
                        return Err(M::Error::duplicate_field("annotated"));
                    }
                    annotated = Some(access.next_value()?);
                }
                RubySegmentField::Annotation => {
                    if annotation.is_some() {
                        return Err(M::Error::duplicate_field("annotation"));
                    }
                    annotation = Some(access.next_value()?);
                }
            }
        }
        match (annotated, annotation) {
            (None, _) => Err(M::Error::missing_field("annotated")),
            (Some(_), None) => Err(M::Error::missing_field("annotation")),
            (Some(annotated), Some(annotation)) => Ok(Segment::Ruby {
                annotated,
                annotation,
            }),
        }
    }
}

/// Marker to indicate which variant of `Segment` should be deserialized.
enum SegmentVariant {
    Literal,
    Time,
    If,
    NewLine,
    SoftHyphen,
    Emphasis,
    NonBreakingSpace,
    Dash,
    Sheet,
    Split,
    Ruby,
}

struct SegmentVariantVisitor;

impl<'de> Visitor<'de> for SegmentVariantVisitor {
    type Value = SegmentVariant;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str(
            "`literal`, `time`, `if`, `new_line`, `soft_hyphen`, `emphasis`, \
            `non_breaking_space`, `dash`, `sheet`, `split`, or `ruby`",
        )
    }

    fn visit_str<E>(self, s: &str) -> Result<SegmentVariant, E>
    where
        E: DeError,
    {
        match s {
            "literal" => Ok(SegmentVariant::Literal),
            "time" => Ok(SegmentVariant::Time),
            "if" => Ok(SegmentVariant::If),
            "new_line" => Ok(SegmentVariant::NewLine),
            "soft_hyphen" => Ok(SegmentVariant::SoftHyphen),
            "emphasis" => Ok(SegmentVariant::Emphasis),
            "non_breaking_space" => Ok(SegmentVariant::NonBreakingSpace),
            "dash" => Ok(SegmentVariant::Dash),
            "sheet" => Ok(SegmentVariant::Sheet),
            "split" => Ok(SegmentVariant::Split),
            "ruby" => Ok(SegmentVariant::Ruby),
            _ => Err(E::unknown_variant(
                s,
                &[
                    "literal",
                    "time",
                    "if",
                    "new_line",
                    "soft_hyphen",
                    "emphasis",
                    "non_breaking_space",
                    "dash",
                    "sheet",
                    "split",
                    "ruby",
                ],
            )),
        }
    }
}

impl<'de> Deserialize<'de> for SegmentVariant {
    fn deserialize<D>(deserializer: D) -> Result<SegmentVariant, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_identifier(SegmentVariantVisitor)
    }
}

struct SegmentVisitor;

impl<'de> Visitor<'de> for SegmentVisitor {
    type Value = Segment;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("an enum representing a text segment")
    }

    fn visit_enum<E>(self, access: E) -> Result<Segment, E::Error>
    where
        E: EnumAccess<'de>,
    {
        let (variant, variant_access) = access.variant()?;
        match variant {
            SegmentVariant::Literal => Ok(Segment::Literal(variant_access.newtype_variant()?)),
            SegmentVariant::Time => Ok(Segment::Time(variant_access.newtype_variant()?)),
            SegmentVariant::If => {
                variant_access.struct_variant(IF_SEGMENT_FIELDS, IfSegmentVisitor)
            }
            SegmentVariant::NewLine => {
                variant_access.unit_variant()?;
                Ok(Segment::NewLine)
            }
            SegmentVariant::SoftHyphen => {
                variant_access.unit_variant()?;
                Ok(Segment::SoftHyphen)
            }
            SegmentVariant::Emphasis => Ok(Segment::Emphasis(variant_access.newtype_variant()?)),
            SegmentVariant::NonBreakingSpace => {
                variant_access.unit_variant()?;
                Ok(Segment::NonBreakingSpace)
            }
            SegmentVariant::Dash => {
                variant_access.unit_variant()?;
                Ok(Segment::Dash)
            }
            SegmentVariant::Sheet => {
                variant_access.struct_variant(SHEET_SEGMENT_FIELDS, SheetSegmentVisitor)
            }
            SegmentVariant::Split => {
                variant_access.struct_variant(SPLIT_SEGMENT_FIELDS, SplitSegmentVisitor)
            }
            SegmentVariant::Ruby => {
                variant_access.struct_variant(RUBY_SEGMENT_FIELDS, RubySegmentVisitor)
            }
        }
    }
}

impl<'de> Deserialize<'de> for Segment {
    fn deserialize<D>(deserializer: D) -> Result<Segment, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_enum("segment", SEGMENT_VARIANTS, SegmentVisitor)
    }
}

impl Serialize for Text {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.segments.len()))?;
        for segment in self.segments.iter() {
            seq.serialize_element(segment)?;
        }
        seq.end()
    }
}

struct TextVisitor;

impl<'de> Visitor<'de> for TextVisitor {
    type Value = Text;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a sequence representing a tagged text string")
    }

    fn visit_seq<S>(self, mut access: S) -> Result<Text, S::Error>
    where
        S: SeqAccess<'de>,
    {
        let mut segments = if let Some(size) = access.size_hint() {
            Vec::with_capacity(size)
        } else {
            Vec::new()
        };
        while let Some(segment) = access.next_element::<Segment>()? {
            segments.push(segment);
        }
        Ok(Text::new(segments))
    }
}

impl<'de> Deserialize<'de> for Text {
    fn deserialize<D>(deserializer: D) -> Result<Text, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_seq(TextVisitor)
    }
}

#[cfg(test)]
mod tests {
    use serde_test::{assert_ser_tokens_error, assert_tokens, Token};

    use crate::{Expression, Segment, Text};

    #[test]
    fn test_expression() {
        assert_tokens(
            &Expression::Integer(10),
            &[
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "int",
                },
                Token::U32(10),
            ],
        );

        assert_tokens(
            &Expression::TopLevelParameter(15),
            &[
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "param",
                },
                Token::U8(15),
            ],
        );

        assert_tokens(
            &Expression::GreaterThanOrEqual(Box::new((
                Expression::Integer(0),
                Expression::Integer(1),
            ))),
            &[
                Token::TupleVariant {
                    name: "expr",
                    variant: "geq",
                    len: 2,
                },
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "int",
                },
                Token::U32(0),
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "int",
                },
                Token::U32(1),
                Token::TupleVariantEnd,
            ],
        );

        assert_tokens(
            &Expression::GreaterThan(Box::new((Expression::Integer(0), Expression::Integer(1)))),
            &[
                Token::TupleVariant {
                    name: "expr",
                    variant: "gt",
                    len: 2,
                },
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "int",
                },
                Token::U32(0),
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "int",
                },
                Token::U32(1),
                Token::TupleVariantEnd,
            ],
        );

        assert_tokens(
            &Expression::LessThanOrEqual(Box::new((
                Expression::Integer(0),
                Expression::Integer(1),
            ))),
            &[
                Token::TupleVariant {
                    name: "expr",
                    variant: "leq",
                    len: 2,
                },
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "int",
                },
                Token::U32(0),
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "int",
                },
                Token::U32(1),
                Token::TupleVariantEnd,
            ],
        );

        assert_tokens(
            &Expression::LessThan(Box::new((Expression::Integer(0), Expression::Integer(1)))),
            &[
                Token::TupleVariant {
                    name: "expr",
                    variant: "lt",
                    len: 2,
                },
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "int",
                },
                Token::U32(0),
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "int",
                },
                Token::U32(1),
                Token::TupleVariantEnd,
            ],
        );

        assert_tokens(
            &Expression::Equal(Box::new((Expression::Integer(0), Expression::Integer(1)))),
            &[
                Token::TupleVariant {
                    name: "expr",
                    variant: "eq",
                    len: 2,
                },
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "int",
                },
                Token::U32(0),
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "int",
                },
                Token::U32(1),
                Token::TupleVariantEnd,
            ],
        );

        assert_tokens(
            &Expression::NotEqual(Box::new((Expression::Integer(0), Expression::Integer(1)))),
            &[
                Token::TupleVariant {
                    name: "expr",
                    variant: "neq",
                    len: 2,
                },
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "int",
                },
                Token::U32(0),
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "int",
                },
                Token::U32(1),
                Token::TupleVariantEnd,
            ],
        );

        assert_tokens(
            &Expression::InputParameter(9),
            &[
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "input_param",
                },
                Token::U32(9),
            ],
        );

        assert_tokens(
            &Expression::PlayerParameter(4),
            &[
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "player_param",
                },
                Token::U32(4),
            ],
        );

        assert_tokens(
            &Expression::StringParameter(8),
            &[
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "string_param",
                },
                Token::U32(8),
            ],
        );

        assert_tokens(
            &Expression::ObjectParameter(82),
            &[
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "object_param",
                },
                Token::U32(82),
            ],
        );

        assert_ser_tokens_error(
            &Expression::TodoEC,
            &[],
            "serialization of expressions with tag 0xec is not yet supported",
        );

        assert_tokens(
            &Expression::Text(Text::new(vec![Segment::Literal(
                "Hello, world".to_string(),
            )])),
            &[
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "text",
                },
                Token::Seq { len: Some(1) },
                Token::NewtypeVariant {
                    name: "segment",
                    variant: "literal",
                },
                Token::Str("Hello, world"),
                Token::SeqEnd,
            ],
        );
    }

    #[test]
    fn test_segment() {
        assert_tokens(
            &Segment::Literal("Test message".to_string()),
            &[
                Token::NewtypeVariant {
                    name: "segment",
                    variant: "literal",
                },
                Token::Str("Test message"),
            ],
        );

        assert_ser_tokens_error(
            &Segment::TodoResetTime(vec![]),
            &[],
            "serialization of segments with tag 0x06 is not yet supported",
        );

        assert_tokens(
            &Segment::Time(Expression::Integer(1285056000)),
            &[
                Token::NewtypeVariant {
                    name: "segment",
                    variant: "time",
                },
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "int",
                },
                Token::U32(1285056000),
            ],
        );

        assert_tokens(
            &Segment::If {
                condition: Expression::Integer(1),
                true_value: Expression::Text(Text::new(vec![Segment::Literal("true".to_string())])),
                false_value: Expression::Text(Text::new(vec![Segment::Literal(
                    "false".to_string(),
                )])),
            },
            &[
                Token::StructVariant {
                    name: "segment",
                    variant: "if",
                    len: 3,
                },
                Token::Str("condition"),
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "int",
                },
                Token::U32(1),
                Token::Str("true_value"),
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "text",
                },
                Token::Seq { len: Some(1) },
                Token::NewtypeVariant {
                    name: "segment",
                    variant: "literal",
                },
                Token::Str("true"),
                Token::SeqEnd,
                Token::Str("false_value"),
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "text",
                },
                Token::Seq { len: Some(1) },
                Token::NewtypeVariant {
                    name: "segment",
                    variant: "literal",
                },
                Token::Str("false"),
                Token::SeqEnd,
                Token::StructVariantEnd,
            ],
        );

        assert_ser_tokens_error(
            &Segment::Switch {
                discriminant: Expression::Integer(0),
                cases: vec![Expression::Integer(0), Expression::Integer(0)],
            },
            &[],
            "serialization of segments with tag 0x09 is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::Todo0A(Expression::Integer(0)),
            &[],
            "serialization of segments with tag 0x0a is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::IfEquals {
                left: Expression::Integer(0),
                right: Expression::Integer(0),
                true_value: Expression::Integer(0),
                false_value: Expression::Integer(0),
            },
            &[],
            "serialization of segments with tag 0x0c is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::Todo0F {
                player: Expression::Integer(0),
                self_value: Expression::Integer(0),
                other_value: Expression::Integer(0),
            },
            &[],
            "serialization of segments with tag 0x0f is not yet supported",
        );

        assert_tokens(
            &Segment::NewLine,
            &[Token::UnitVariant {
                name: "segment",
                variant: "new_line",
            }],
        );

        assert_ser_tokens_error(
            &Segment::GuiIcon(Expression::Integer(0)),
            &[],
            "serialization of segments with tag 0x12 is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::ColorChange(Expression::Integer(0)),
            &[],
            "serialization of segments with tag 0x13 is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::Todo14(Expression::Integer(0)),
            &[],
            "serialization of segments with tag 0x14 is not yet supported",
        );

        assert_tokens(
            &Segment::SoftHyphen,
            &[Token::UnitVariant {
                name: "segment",
                variant: "soft_hyphen",
            }],
        );

        assert_ser_tokens_error(
            &Segment::Todo17,
            &[],
            "serialization of segments with tag 0x17 is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::Todo19(false),
            &[],
            "serialization of segments with tag 0x19 is not yet supported",
        );

        assert_tokens(
            &Segment::Emphasis(false),
            &[
                Token::NewtypeVariant {
                    name: "segment",
                    variant: "emphasis",
                },
                Token::Bool(false),
            ],
        );

        assert_ser_tokens_error(
            &Segment::Todo1B(vec![]),
            &[],
            "serialization of segments with tag 0x1b is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::Todo1C(vec![]),
            &[],
            "serialization of segments with tag 0x1c is not yet supported",
        );

        assert_tokens(
            &Segment::NonBreakingSpace,
            &[Token::UnitVariant {
                name: "segment",
                variant: "non_breaking_space",
            }],
        );

        assert_ser_tokens_error(
            &Segment::CommandIcon(Expression::Integer(0)),
            &[],
            "serialization of segments with tag 0x1e is not yet supported",
        );

        assert_tokens(
            &Segment::Dash,
            &[Token::UnitVariant {
                name: "segment",
                variant: "dash",
            }],
        );

        assert_ser_tokens_error(
            &Segment::IntegerValue(Expression::Integer(0)),
            &[],
            "serialization of segments with tag 0x20 is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::TodoFormat(Expression::Integer(0), vec![]),
            &[],
            "serialization of segments with tag 0x22 is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::TwoDigitValue(Expression::Integer(0)),
            &[],
            "serialization of segments with tag 0x24 is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::Todo26(
                Expression::Integer(0),
                Expression::Integer(0),
                Expression::Integer(0),
            ),
            &[],
            "serialization of segments with tag 0x26 is not yet supported",
        );

        assert_tokens(
            &Segment::Sheet {
                name: Expression::Text(Text::new(vec![Segment::Literal("SheetName".to_string())])),
                row_index: Expression::Integer(1),
                column_index: Some(Expression::Integer(2)),
                parameters: vec![Expression::Integer(5)],
            },
            &[
                Token::StructVariant {
                    name: "segment",
                    variant: "sheet",
                    len: 4,
                },
                Token::Str("name"),
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "text",
                },
                Token::Seq { len: Some(1) },
                Token::NewtypeVariant {
                    name: "segment",
                    variant: "literal",
                },
                Token::Str("SheetName"),
                Token::SeqEnd,
                Token::Str("row_index"),
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "int",
                },
                Token::U32(1),
                Token::Str("column_index"),
                Token::Some,
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "int",
                },
                Token::U32(2),
                Token::Str("parameters"),
                Token::Seq { len: Some(1) },
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "int",
                },
                Token::U32(5),
                Token::SeqEnd,
                Token::StructVariantEnd,
            ],
        );
        assert_tokens(
            &Segment::Sheet {
                name: Expression::Text(Text::new(vec![Segment::Literal("SheetName".to_string())])),
                row_index: Expression::Integer(1),
                column_index: None,
                parameters: vec![],
            },
            &[
                Token::StructVariant {
                    name: "segment",
                    variant: "sheet",
                    len: 4,
                },
                Token::Str("name"),
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "text",
                },
                Token::Seq { len: Some(1) },
                Token::NewtypeVariant {
                    name: "segment",
                    variant: "literal",
                },
                Token::Str("SheetName"),
                Token::SeqEnd,
                Token::Str("row_index"),
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "int",
                },
                Token::U32(1),
                Token::Str("column_index"),
                Token::None,
                Token::Str("parameters"),
                Token::Seq { len: Some(0) },
                Token::SeqEnd,
                Token::StructVariantEnd,
            ],
        );

        assert_ser_tokens_error(
            &Segment::TodoStringValue1(Expression::Integer(0)),
            &[],
            "serialization of segments with tag 0x29 is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::TodoStringValue2(Expression::Integer(0)),
            &[],
            "serialization of segments with tag 0x2b is not yet supported",
        );

        assert_tokens(
            &Segment::Split {
                input: Expression::Text(Text::new(vec![Segment::Literal(
                    "First Last".to_string(),
                )])),
                separator: Expression::Text(Text::new(vec![Segment::Literal(" ".to_string())])),
                index: Expression::Integer(1),
            },
            &[
                Token::StructVariant {
                    name: "segment",
                    variant: "split",
                    len: 3,
                },
                Token::Str("input"),
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "text",
                },
                Token::Seq { len: Some(1) },
                Token::NewtypeVariant {
                    name: "segment",
                    variant: "literal",
                },
                Token::Str("First Last"),
                Token::SeqEnd,
                Token::Str("separator"),
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "text",
                },
                Token::Seq { len: Some(1) },
                Token::NewtypeVariant {
                    name: "segment",
                    variant: "literal",
                },
                Token::Str(" "),
                Token::SeqEnd,
                Token::Str("index"),
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "int",
                },
                Token::U32(1),
                Token::StructVariantEnd,
            ],
        );

        assert_ser_tokens_error(
            &Segment::StringValueTitleCase(Expression::Integer(0)),
            &[],
            "serialization of segments with tag 0x2d is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::AutoTranslate(Expression::Integer(0), Expression::Integer(0)),
            &[],
            "serialization of segments with tag 0x2e is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::StringValueLowerCase(Expression::Integer(0)),
            &[],
            "serialization of segments with tag 0x2f is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::SheetJa(vec![Expression::Integer(0)]),
            &[],
            "serialization of segments with tag 0x30 is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::SheetEn(vec![Expression::Integer(0)]),
            &[],
            "serialization of segments with tag 0x31 is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::SheetDe(vec![Expression::Integer(0)]),
            &[],
            "serialization of segments with tag 0x32 is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::SheetFr(vec![Expression::Integer(0)]),
            &[],
            "serialization of segments with tag 0x33 is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::Todo40(Expression::Integer(0)),
            &[],
            "serialization of segments with tag 0x40 is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::Foreground(Expression::Integer(0)),
            &[],
            "serialization of segments with tag 0x48 is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::Glow(Expression::Integer(0)),
            &[],
            "serialization of segments with tag 0x49 is not yet supported",
        );

        assert_tokens(
            &Segment::Ruby {
                annotated: Expression::Text(Text::new(vec![Segment::Literal(
                    "main text".to_string(),
                )])),
                annotation: Expression::Text(Text::new(vec![Segment::Literal("ruby".to_string())])),
            },
            &[
                Token::StructVariant {
                    name: "segment",
                    variant: "ruby",
                    len: 2,
                },
                Token::Str("annotated"),
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "text",
                },
                Token::Seq { len: Some(1) },
                Token::NewtypeVariant {
                    name: "segment",
                    variant: "literal",
                },
                Token::Str("main text"),
                Token::SeqEnd,
                Token::Str("annotation"),
                Token::NewtypeVariant {
                    name: "expr",
                    variant: "text",
                },
                Token::Seq { len: Some(1) },
                Token::NewtypeVariant {
                    name: "segment",
                    variant: "literal",
                },
                Token::Str("ruby"),
                Token::SeqEnd,
                Token::StructVariantEnd,
            ],
        );

        assert_ser_tokens_error(
            &Segment::ZeroPaddedValue {
                value: Expression::Integer(0),
                digits: Expression::Integer(0),
            },
            &[],
            "serialization of segments with tag 0x50 is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::Todo51(Expression::Integer(0)),
            &[],
            "serialization of segments with tag 0x51 is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::Todo60(vec![]),
            &[],
            "serialization of segments with tag 0x60 is not yet supported",
        );

        assert_ser_tokens_error(
            &Segment::Todo61(Expression::Integer(0)),
            &[],
            "serialization of segments with tag 0x61 is not yet supported",
        );
    }
}

#[cfg(test)]
mod proptests {
    use quickcheck::{QuickCheck, TestResult};

    use crate::Segment;

    fn property_yaml_round_trip(tag: Segment) -> TestResult {
        if let Ok(yaml) = serde_yaml::to_string(&tag) {
            match serde_yaml::from_str::<Segment>(&yaml) {
                Ok(deserialized) => {
                    if deserialized == tag {
                        TestResult::passed()
                    } else {
                        eprintln!(
                            "round trip failed, {:?} => {:?} => {:?}",
                            tag, yaml, deserialized
                        );
                        TestResult::failed()
                    }
                }
                Err(e) => {
                    eprintln!(
                        "round trip failed, {:?} => {:?} => parse error {:?}",
                        tag, yaml, e
                    );
                    TestResult::failed()
                }
            }
        } else {
            quickcheck::TestResult::discard()
        }
    }

    #[test]
    fn serde_round_trip() {
        QuickCheck::new().quickcheck(property_yaml_round_trip as fn(Segment) -> TestResult);
    }
}
