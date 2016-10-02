
// Numeric Literal

use std::fmt;
use common::StringPosition;
use message::Message;
use message::MessageEmitter;

#[cfg(test)]
#[derive(PartialEq, Clone)]
pub enum NumericLiteralValue {
    U64(u64),
    U32(u32),
    I32(i32),
    U8(u8),
    F32(f32),
    F64(f64),
}
#[cfg(not(test))]
#[derive(Clone)]
pub enum NumericLiteralValue {
    U64(u64),
    U32(u32),
    I32(i32),
    U8(u8),
    F32(f32),
    F64(f64),
}
#[cfg(test)]
impl Eq for NumericLiteralValue {
}

#[cfg(test)]
#[derive(Eq, PartialEq, Clone)]
pub struct NumericLiteral {
    pub value: Option<NumericLiteralValue>,
    pub pos: StringPosition,
}
#[cfg(not(test))]
#[derive(Clone)]
pub struct NumericLiteral {
    pub value: Option<NumericLiteralValue>,
    pub pos: StringPosition,
}

impl fmt::Debug for NumericLiteral {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NumericLiteral at {:?}{}",
            self.pos,
            match self.value {
                Some(NumericLiteralValue::U64(ref value)) => format!(", with value (u64){:?}", value),
                Some(NumericLiteralValue::U32(ref value)) => format!(", with value (u32){:?}", value),
                Some(NumericLiteralValue::I32(ref value)) => format!(", with value (i32){:?}", value),
                Some(NumericLiteralValue::U8(ref value)) => format!(", with value (u8){:?}", value),
                Some(NumericLiteralValue::F32(ref value)) => format!(", with value (f32){:?}", value),
                Some(NumericLiteralValue::F64(ref value)) => format!(", with value (f64){:?}", value),
                None => ", invalid".to_owned(),
            }
        )
    }
}

impl NumericLiteral {

    pub fn new(_raw: &str, pos: StringPosition, _messages: &mut MessageEmitter) -> NumericLiteral {
        NumericLiteral { 
            pos: pos, 
            value: Some(NumericLiteralValue::I32(0)),
         }
    }
    
    // pub fn from_hex_to_u8(hex: &str) -> Result<u8, NumericLiteralValueParseError> {

    //     match hex.len() {
    //         0 => Err(NumericLiteralValueParseError::Empty),
    //         1 => {
    //             let char1 = hex.chars().next().unwrap();
    //             match char1.to_digit(16) {
    //                 Some(digit) => Ok(digit as u8),
    //                 None => Err(NumericLiteralValueParseError::InvalidChar(char1))
    //             }
    //         }
    //         2 => { 
    //             let mut chars = hex.chars();
    //             let char1 = chars.next().unwrap();
    //             let char2 = chars.next().unwrap();
    //             match char1.to_digit(16) {
    //                 None => Err(NumericLiteralValueParseError::InvalidChar(char1)),
    //                 Some(digit1) => match char2.to_digit(16) {
    //                     None => Err(NumericLiteralValueParseError::InvalidChar(char2)),
    //                     Some(digit2) => Ok((digit1 * 16 + digit2) as u8),
    //                 }
    //             }
    //         }
    //         _ => Err(NumericLiteralValueParseError::TooLong),
    //     }
    // }

    // pub fn from_hex_to_u32(hex: &str) -> Result<u32, NumericLiteralValueParseError> {
    //     const F_POWERED: [u32; 8] = 
    //         [1_u32, 0x10_u32, 0x100_u32, 0x1000_u32, 0x1000_0_u32, 0x1000_00_u32, 0x1000_000_u32, 0x1000_0000_u32];

    //     match hex.len() {
    //         0 => Err(NumericLiteralValueParseError::Empty),
    //         mut n @ 1...8 => {
    //             let mut ret_val = 0_u32;
    //             for ch in hex.chars() {
    //                 match ch.to_digit(16) {
    //                     None => return Err(NumericLiteralValueParseError::InvalidChar(ch)),
    //                     Some(digit) => ret_val += digit * F_POWERED[n - 1], 
    //                 }
    //                 n -= 1;
    //             }
    //             return Ok(ret_val);
    //         }
    //         _ => Err(NumericLiteralValueParseError::TooLong),
    //     }
    // } 

    // /// Attention that 0x8000+ will be negative value
    // pub fn from_hex_to_i32(hex: &str) -> Result<i32, NumericLiteralValueParseError> {
        
    //     NumericLiteralValue::from_hex_to_u32(hex)
    //         .map(|u| match u {
    //                 0x8000_0000_u32 => i32::min_value(),
    //                 u @ 0x8000_0000_u32...0xFFFF_FFFF_u32 => -((0xFFFF_FFFFu32 - u + 1) as i32 ),
    //                 u => u as i32
    //             }
    //         )
    // }

    // pub fn from_hex_to_u64(hex: &str) -> Result<u64, NumericLiteralValueParseError> {
    //     const F_POWERED: [u64; 16] = 
    //         [0x1_u64, 0x10_u64, 0x100_u64, 0x1000_u64, 
    //          0x1000_0_u64, 0x1000_00_u64, 0x1000_000_u64, 0x1000_0000_u64,  
    //          0x1000_0000_0_u64, 0x1000_0000_00_u64, 0x1000_0000_000_u64, 0x1000_0000_0000_u64,
    //          0x1000_0000_0000_0_u64, 0x1000_0000_0000_00_u64, 0x1000_0000_0000_000_u64, 0x1000_0000_0000_0000_u64];

    //     match hex.len() {
    //         0 => Err(NumericLiteralValueParseError::Empty),
    //         mut n @ 1...16 => {
    //             let mut ret_val = 0_u64;
    //             for ch in hex.chars() {
    //                 match ch.to_digit(16) {
    //                     None => return Err(NumericLiteralValueParseError::InvalidChar(ch)),
    //                     Some(digit) => ret_val += digit as u64 * F_POWERED[n - 1], 
    //                 }
    //                 n -= 1;
    //             }
    //             return Ok(ret_val);
    //         }
    //         _ => Err(NumericLiteralValueParseError::TooLong),
    //     }
    // }

}

// ignore _ and ignore i32 postfix
fn numeric_literal_to_value(raw: &str, pos: StringPosition, messages: &mut MessageEmitter) -> (i32, bool) { // value, has_failed
    
    let no_postfix = if raw.len() > 3 && &raw[(raw.len() - 3)..] == "i32" {
        &raw[..(raw.len() - 3)]
    } else {
        raw
    };

    let mut digits = Vec::new();
    for ch in no_postfix.chars() {
        if ch == '_' {
            continue;
        } else if ch.is_digit(10) {
            digits.push(ch);
        } else {
            messages.push(Message::UnexpectedIdentifierCharInNumericLiteral { 
                literal_start: pos.start_pos,
                unexpected_char: ch
            });
            return (0, true);
        }
    }

    // digits will not be empty because first character must be [0-9]
    if digits.len() > 10 {
        messages.push(Message::NumericLiteralTooLong { literal_start: pos.start_pos });
        return (0, true);
    }
    
    const TENS: [i32; 10] = [
        1, 
        10, 
        100, 
        1000, 
        10000,
        100000, 
        1000000,
        10000000, 
        100000000, 
        1000000000
    ];

    let mut value = 0_i32;
    let length = digits.len();
    for i in 0..length {
        value = match (digits[i].to_digit(10).unwrap() as i32).checked_mul(TENS[length - i - 1]) {
            None => {
                messages.push(Message::NumericLiteralTooLarge { literal_start: pos.start_pos });
                return (0, true);
            }
            Some(middle) => {
                match value.checked_add(middle) {
                    Some(value) => value, 
                    None => {
                        messages.push(Message::NumericLiteralTooLarge { literal_start: pos.start_pos });
                        return (0, true);
                    }
                }
            }
        };
    }

    (value, false)
}

#[cfg(test)]
mod tests {

    #[test]
    fn v3_see_rustc() {
        // let a = 1;
        // let a = 1_u256;
        // let a = 0xCCC;
        // let a = 0xCCHA;
        // let a = 0a123;
        // let a = 1i_32;
        // let a = 100000i8;
    }

    // #[test]
    // fn num_lit_value_hex_to_u8() {
    //     use super::NumericLiteralValue;
    //     use super::NumericLiteralValueParseError::*;

    //     macro_rules! test_case {
    //         ($input: expr, ok: $result: expr) => (
    //             match NumericLiteralValue::from_hex_to_u8($input) {
    //                 Ok(result) => assert_eq!(result, $result),
    //                 Err(e) => panic!("Unexpected error: {:?}", e),
    //             }
    //         );
    //         ($input: expr, err: $error: expr) => (
    //             match NumericLiteralValue::from_hex_to_u8($input) {
    //                 Ok(result) => panic!("Unexpected ok: {}", result),
    //                 Err(e) => assert_eq!(e, $error),
    //             }
    //         )
    //     }

    //     test_case!("", err: Empty);
    //     test_case!("A", ok: 0xAu8);
    //     test_case!("AC", ok: 0xACu8);
    //     test_case!("BFF", err: TooLong);
    //     test_case!("Z", err: InvalidChar('Z'));
    //     test_case!("BZ", err: InvalidChar('Z'));
    //     test_case!("GB", err: InvalidChar('G'));
    //     test_case!("a", ok: 0xAu8);
    //     test_case!("ac", ok: 0xACu8);
    //     test_case!("bff", err: TooLong);
    //     test_case!("z", err: InvalidChar('z'));
    //     test_case!("bz", err: InvalidChar('z'));
    //     test_case!("gb", err: InvalidChar('g'));
    // }

    // #[test]
    // fn num_lit_value_hex_to_u32() {
    //     use super::NumericLiteralValue;
    //     use super::NumericLiteralValueParseError::*;

    //     macro_rules! test_case {
    //         ($input: expr, ok: $result: expr) => (
    //             match NumericLiteralValue::from_hex_to_u32($input) {
    //                 Ok(result) => assert_eq!(result, $result),
    //                 Err(e) => panic!("Unexpected error: {:?}", e),
    //             }
    //         );
    //         ($input: expr, err: $error: expr) => (
    //             match NumericLiteralValue::from_hex_to_u32($input) {
    //                 Ok(result) => panic!("Unexpected ok: {}", result),
    //                 Err(e) => assert_eq!(e, $error),
    //             }
    //         )
    //     }
        
    //     test_case!("", err: Empty);
    //     test_case!("A", ok: 0xAu32);
    //     test_case!("AC", ok: 0xACu32);
    //     test_case!("BFF", ok: 0xBFFu32);
    //     test_case!("Z", err: InvalidChar('Z'));
    //     test_case!("BZ", err: InvalidChar('Z'));
    //     test_case!("GB", err: InvalidChar('G'));
    //     test_case!("a", ok: 0xAu32);
    //     test_case!("ac", ok: 0xACu32);
    //     test_case!("bff", ok: 0xBFFu32);
    //     test_case!("z", err: InvalidChar('z'));
    //     test_case!("bz", err: InvalidChar('z'));
    //     test_case!("gb", err: InvalidChar('g'));
    //     test_case!("AAAA0000", ok: 0xAAAA0000u32);
    //     test_case!("FFFFFFFF", ok: 0xFFFFFFFFu32);
    //     test_case!("80000000", ok: 0x80000000u32);
    //     test_case!("7FFFFFFF", ok: 0x7FFFFFFFu32);
    //     test_case!("AAAAAAAAA", err: TooLong);
    //     test_case!("efgh", err: InvalidChar('g'));
    //     test_case!("0000", ok: 0x0000u32);
    // }

    // #[test]
    // fn num_lit_value_hex_to_i32() {
    //     use super::NumericLiteralValue;
    //     use super::NumericLiteralValueParseError::*;

    //     macro_rules! test_case {
    //         ($input: expr, ok: $result: expr) => (
    //             match NumericLiteralValue::from_hex_to_i32($input) {
    //                 Ok(result) => assert_eq!(result, $result),
    //                 Err(e) => panic!("Unexpected error: {:?}", e),
    //             }
    //         );
    //         ($input: expr, err: $error: expr) => (
    //             match NumericLiteralValue::from_hex_to_i32($input) {
    //                 Ok(result) => panic!("Unexpected ok: {}", result),
    //                 Err(e) => assert_eq!(e, $error),
    //             }
    //         )
    //     }
        
    //     test_case!("", err: Empty);
    //     test_case!("A", ok: 0xAi32);
    //     test_case!("AC", ok: 0xACi32);
    //     test_case!("BFF", ok: 0xBFFi32);
    //     test_case!("Z", err: InvalidChar('Z'));
    //     test_case!("BZ", err: InvalidChar('Z'));
    //     test_case!("GB", err: InvalidChar('G'));
    //     test_case!("a", ok: 0xAi32);
    //     test_case!("ac", ok: 0xACi32);
    //     test_case!("bff", ok: 0xBFFi32);
    //     test_case!("z", err: InvalidChar('z'));
    //     test_case!("bz", err: InvalidChar('z'));
    //     test_case!("gb", err: InvalidChar('g'));
    //     test_case!("AAAA0000", ok: -1431699456i32);
    //     test_case!("FFFFFFFF", ok: -1i32);
    //     test_case!("80000000", ok: i32::min_value());
    //     test_case!("7FFFFFFF", ok: 0x7FFFFFFFi32);
    //     test_case!("AAAAAAAAA", err: TooLong);
    //     test_case!("efgh", err: InvalidChar('g'));
    //     test_case!("0000", ok: 0x0000i32);
    // }

    // #[test]
    // fn num_lit_value_hex_to_u64() {
    //     use super::NumericLiteralValue;
    //     use super::NumericLiteralValueParseError::*;

    //     macro_rules! test_case {
    //         ($input: expr, ok: $result: expr) => (
    //             match NumericLiteralValue::from_hex_to_u64($input) {
    //                 Ok(result) => assert_eq!(result, $result),
    //                 Err(e) => panic!("Unexpected error: {:?}", e),
    //             }
    //         );
    //         ($input: expr, err: $error: expr) => (
    //             match NumericLiteralValue::from_hex_to_u64($input) {
    //                 Ok(result) => panic!("Unexpected ok: {}", result),
    //                 Err(e) => assert_eq!(e, $error),
    //             }
    //         )
    //     }
        
    //     test_case!("", err: Empty);
    //     test_case!("A", ok: 0xAu64);
    //     test_case!("AC", ok: 0xACu64);
    //     test_case!("BFF", ok: 0xBFFu64);
    //     test_case!("Z", err: InvalidChar('Z'));
    //     test_case!("BZ", err: InvalidChar('Z'));
    //     test_case!("GB", err: InvalidChar('G'));
    //     test_case!("AAAA0000", ok: 0xAAAA0000u64);
    //     test_case!("FFFFFFFF", ok: 0xFFFFFFFFu64);
    //     test_case!("80000000", ok: 0x80000000u64);
    //     test_case!("7FFFFFFF", ok: 0x7FFFFFFFu64);
    //     test_case!("AAAAAAAAA", ok: 0xAAAAAAAAAu64);
    //     test_case!("efgh", err: InvalidChar('g'));
    //     test_case!("0000", ok: 0x0000u64);
    //     test_case!("1234567890ABCDEFG", err: TooLong);
    //     test_case!("1234567890ABCDEF", ok: 0x1234567890ABCDEFu64);
    //     test_case!("1234567890ABCDEG", err: InvalidChar('G'));
    //     test_case!("1234567890abcdeg", err: InvalidChar('g'))
    // }

    #[test]
    fn v3_numeric_literal() {
        // use super::pub_numeric_literal;
        // use message::MessageEmitter;

        // let messages = &mut MessageEmitter::new();
        // perrorln!("{:?}", pub_numeric_literal("123", messages));
        // perrorln!("{:?}", pub_numeric_literal("123_i32", messages));
        // perrorln!("{:?}", pub_numeric_literal("1_2_3i32", messages));
        // perrorln!("{:?}", pub_numeric_literal("123_456_789_012", messages));
        // perrorln!("{:?}", pub_numeric_literal("999_999_999_9_i32", messages));

        // perrorln!("Messages: {:?}", messages);
    }
}