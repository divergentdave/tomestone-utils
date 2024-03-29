pub const GEQ: u8 = 0xe0;
pub const GT: u8 = 0xe1;
pub const LEQ: u8 = 0xe2;
pub const LT: u8 = 0xe3;
pub const EQ: u8 = 0xe4;
pub const NEQ: u8 = 0xe5;

pub const INPUT_PARAM: u8 = 0xe8;
pub const PLAYER_PARAM: u8 = 0xe9;
pub const STRING_PARAM: u8 = 0xea;
pub const OBJECT_PARAM: u8 = 0xeb;
pub const TODO_EC: u8 = 0xec;

pub const BYTE: u8 = 0xf0;
pub const BYTE_SHIFTED_8: u8 = 0xf1;
pub const INT16: u8 = 0xf2;
pub const BYTE_SHIFTED_16: u8 = 0xf3;
pub const INT16_FIRST_AND_THIRD_BYTES: u8 = 0xf4;
pub const INT16_SHIFTED_8: u8 = 0xf5;
pub const INT24: u8 = 0xf6;
pub const BYTE_SHIFTED_24: u8 = 0xf7;
pub const INT16_FIRST_AND_LAST_BYTES: u8 = 0xf8;
pub const INT16_SECOND_AND_FOURTH_BYTES: u8 = 0xf9;
pub const INT24_FIRST_SECOND_AND_FOURTH_BYTES: u8 = 0xfa;
pub const INT16_SHIFTED_16: u8 = 0xfb;
pub const INT24_FIRST_THIRD_AND_FOURTH_BYTES: u8 = 0xfc;
pub const INT24_SHIFTED_8: u8 = 0xfd;
pub const INT32: u8 = 0xfe;

pub const TAGGED_TEXT: u8 = 0xff;
