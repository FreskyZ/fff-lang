
// numeric literal parser
// TODO: ' as seperator, multi seperator not supported, full test

use common::StringPosition;
use message::LexicalMessage as Message;
use message::MessageEmitter;

use lexical::symbol_type::numeric_literal::NumericLiteral;
use lexical::NumLitValue;

// Final to value, check too large
macro_rules! values_to_integral {
    ($fn_name: ident, $ty: ty, $result_path: path) => (

        fn $fn_name(radix: u32, content: Vec<u32>, pos: StringPosition) -> Result<NumLitValue, Message> {
            let mut ret_val: $ty = 0;
            let mut digit_weight: $ty = 1;
            let len = content.len();
            let mut index = 0;
            for digit in content.into_iter().rev() {
                let digit = digit as $ty;
                match digit.checked_mul(digit_weight) {
                    Some(temp1) => match ret_val.checked_add(temp1) {
                        Some(result) => ret_val = result,
                        None => return Err(Message::NumericLiteralTooLarge{ literal_pos: pos }),
                    }, 
                    None => return Err(Message::NumericLiteralTooLarge{ literal_pos: pos }),
                }

                if index + 1 != len {
                    digit_weight = match digit_weight.checked_mul(radix as $ty) {
                        Some(result) => result,
                        None => return Err(Message::NumericLiteralTooLarge{ literal_pos: pos }),
                    }
                }
                index += 1;
            }

            Ok($result_path(ret_val))
        }
    )
}
values_to_integral!{ values_to_i8, i8, NumLitValue::I8 }
values_to_integral!{ values_to_u8, u8, NumLitValue::U8 }
values_to_integral!{ values_to_i16, i16, NumLitValue::I16 }
values_to_integral!{ values_to_u16, u16, NumLitValue::U16 }
values_to_integral!{ values_to_i32, i32, NumLitValue::I32 }
values_to_integral!{ values_to_u32, u32, NumLitValue::U32 }
values_to_integral!{ values_to_i64, i64, NumLitValue::I64 }
values_to_integral!{ values_to_u64, u64, NumLitValue::U64 }

macro_rules! values_to_float {
    ($fn_name: ident, $ty: ty, $result_path: path) => (

        fn $fn_name(radix: u32, content1: Vec<u32>, content2: Vec<u32>, _pos: StringPosition) -> Result<NumLitValue, Message> {

            let mut ret_val: $ty = 0 as $ty;
            let mut digit_weight: $ty = 1 as $ty;
            let len = content1.len();
            let mut index = 0;
            for digit in content1.into_iter().rev() {
                let digit = digit as $ty;
                ret_val += digit * digit_weight;

                if index + 1 != len {
                    digit_weight *= radix as $ty;
                }
                index += 1;
            }

            digit_weight = 1 as $ty;    
            for digit in content2.into_iter() {
                let digit = digit as $ty;
                digit_weight /= radix as $ty;
                ret_val += digit * digit_weight;
            }

            Ok($result_path(ret_val))
        }
    )
}
values_to_float!{ values_to_f32, f32, NumLitValue::F32 }
values_to_float!{ values_to_f64, f64, NumLitValue::F64 }

test_only_attr!{
    [derive(Debug, Eq, PartialEq)]
    ![derive(Eq, PartialEq)]
    enum Prefix {
        Binary,
        Octal,
        Decimal,
        Hex,
        NotSet, // not set maybe float, not same as decimal
    }
}
impl Prefix {
    // Length for the prefix, notset is 0
    fn len(&self) -> usize {
        match *self {
            Prefix::Binary | Prefix::Octal | Prefix::Decimal | Prefix::Hex => 2,
            Prefix::NotSet => 0,
        }
    }
    // get radix of the prefix
    fn radix(&self) -> u32 {
        match *self {
            Prefix::Binary => 2,
            Prefix::Octal => 8,
            Prefix::Decimal => 10,
            Prefix::Hex => 16,
            Prefix::NotSet => 10,
        }
    }
}
test_only_attr!{
    [derive(Debug, Eq, PartialEq)]
    ![derive(Eq, PartialEq)]
    enum Postfix {
        I8,
        U8,
        I16,
        U16, 
        I32,
        U32,
        I64,
        U64,
        F32,
        F64,
        NotSet, // I32 for maybe integral, F64 for floating point
    }
}
impl Postfix {
    fn len(&self) -> usize {
        match *self {
            Postfix::NotSet => 0,
            Postfix::I8 => 2,
            Postfix::U8 => 2,
            _ => 3,
        }
    }
    fn is_float(&self) -> bool {
        match *self {
            Postfix::F32 | Postfix::F64 => true,
            _ => false,
        }
    }
    fn is_not_set(&self) -> bool {
        match *self{
            Postfix::NotSet => true,
            _ => false,
        }
    }
    fn is_integral(&self) -> bool {
        match *self {
            Postfix::I8 | Postfix::U8
            | Postfix::I16 | Postfix::U16 
            | Postfix::I32 | Postfix::U32 
            | Postfix::I64| Postfix::U64 => true, 
            _ => false,  
        }
    }
}
test_only_attr!{
    [derive(Debug, Eq, PartialEq)]
    ![derive(Eq, PartialEq)]
    enum Content {
        Integral(Vec<u32>),
        Float(Vec<u32>, Vec<u32>),
    }
}

// get prefix
//     start with 0 is prefix, if not the 4 prefix is invliad prefix because in C start with 0 is Octal         // InvalidPrefixInNumericLiteral
//     check if rest is empty, if not, throw messages here not furthur
fn get_prefix(raw: &str, pos: StringPosition) -> Result<Prefix, Message> {

    if raw.len() < 2 {
        return Ok(Prefix::NotSet);
    }

    let mut chars = raw.chars();
    let ret_val = match chars.next().unwrap() {
        '0' => match chars.next().unwrap() {    // length > 2, safe to unwrap these 2
            'x' => Ok(Prefix::Hex),
            'd' => Ok(Prefix::Decimal),
            'o' => Ok(Prefix::Octal),
            'b' => Ok(Prefix::Binary),
            // unexpected char after first char '0' and length >= 2
            maybe_start_of_postfix @ 'i' 
            | maybe_start_of_postfix @ 'u' 
            | maybe_start_of_postfix @ 'f' => {
                // directly special check `0f32` cases
                match (maybe_start_of_postfix, chars.next(), chars.next(), chars.next()) {
                    ('u', Some('8'), None, None)
                    | ('i', Some('8'), None, None)
                    | ('i', Some('1'), Some('6'), None)
                    | ('u', Some('1'), Some('6'), None)
                    | ('i', Some('3'), Some('2'), None)
                    | ('u', Some('3'), Some('2'), None)
                    | ('i', Some('6'), Some('4'), None)
                    | ('u', Some('6'), Some('4'), None)
                    | ('f', Some('3'), Some('2'), None)
                    | ('f', Some('6'), Some('4'), None) => Ok(Prefix::NotSet),
                    _other_cases => Err(Message::InvalidPrefixInNumericLiteral{ literal_pos: pos, prefix: maybe_start_of_postfix }),
                }
            }
            other => Err(Message::InvalidPrefixInNumericLiteral{ literal_pos: pos, prefix: other }),
        },
        _ => Ok(Prefix::NotSet),
    };

    match ret_val {
        Ok(Prefix::NotSet) => Ok(Prefix::NotSet),
        ref _other if raw.len() == 2 => Err(Message::EmptyNumericLiteral{ literal_pos: pos }), // remove prefix is none
        other => other,
    }
}

// get postfix
//     floating point postfix should not have prefix, even 0d is not, because prefix means interger             // FloatingPointPostfixNotWithPrefix
fn get_postfix(raw: &str, pos: StringPosition, has_prefix: bool) -> Result<Postfix, Message> {

    match raw {                                  // find nothing after removing postfix, which is like "0xu8"
        "i8" | "u8"
        | "i16" | "u16" 
        | "u32" | "i32" 
        | "i64" | "u64" 
        | "f32" | "f64" => return Err(Message::EmptyNumericLiteral{ literal_pos: pos }),
        _ => (),
    }

    let mut revchars = raw.chars().rev();
    match (revchars.next(), revchars.next(), revchars.next()) {
        (Some('8'), Some('i'), _) => Ok(Postfix::I8),
        (Some('8'), Some('u'), _) => Ok(Postfix::U8),
        (Some('6'), Some('1'), Some('i')) => Ok(Postfix::I16),
        (Some('6'), Some('1'), Some('u')) => Ok(Postfix::U16),
        (Some('2'), Some('3'), Some('i')) => Ok(Postfix::I32),
        (Some('2'), Some('3'), Some('u')) => Ok(Postfix::U32),
        (Some('4'), Some('6'), Some('i')) => Ok(Postfix::I64),
        (Some('4'), Some('6'), Some('u')) => Ok(Postfix::U64),

        (Some('2'), Some('3'), Some('f')) | (Some('4'), Some('6'), Some('f')) if has_prefix => 
            Err(Message::PrefixNotSupportedForFloatLiteral{ literal_pos: pos }),

        (Some('2'), Some('3'), Some('f')) => Ok(Postfix::F32),
        (Some('4'), Some('6'), Some('f')) => Ok(Postfix::F64),
        _ => Ok(Postfix::NotSet),
    }
}

// iterate content to get content before decimal dot and after decimal dot
//     ignore underscore, check char according to radix of prefix
//     with prefix or with integral postfix should not have any decimal point                                   // IntegralPostfixForFloatingPointLiteral
//     prefix limits the max char value,                                                                        // InvalidCharInNumericLiteral     
//     can not use underscore and single quote at same time                                                     // MultiSeperatorInNumericLiteral       
fn get_content(raw: &str, pos: StringPosition, radix: u32, mut postfix: Postfix) -> Result<(Content, Postfix), Message> {
    // that is, prefix only has radix meaning, postfix notset is decided by whether have decimal dot
    // so, iterate content to vec of u32 and final postfix 
    // and report message on unexpected char of this radix and on expected decimal dot or unexpected multi decimal dot
    // and then dispatch to different methods to get final value

    let mut after_a_decimal_dot = false;
    let mut ret_val1 = Vec::new();
    let mut ret_val2 = Vec::new();

    let mut has_underscore = false;
    let mut has_singlequote = false;
    for ch in raw.chars() {   
        if ch == '_' { // ignore underscore
            if has_singlequote {
                return Err(Message::MultiSeperatorInNumericLiteral{ literal_pos: pos });
            }
            has_underscore = true;
            continue; 
        } else if ch == '\'' {
            if has_underscore {
                return Err(Message::MultiSeperatorInNumericLiteral{ literal_pos: pos });
            }
            has_singlequote = true;
            continue;
        } else if ch == '.' {
            if postfix.is_integral() { // integral and '.', immediate error
                return Err(Message::UnexpectedDecimalPointInIntegralLiteral{ literal_pos: pos });       // C1, 123.456i32
            } else if postfix.is_not_set() {
                postfix = Postfix::F64; // not set will absolutely change to f64 if met decimal dot,    // C2, 123.456 => 123, 456, F64
                after_a_decimal_dot = true;      // so there is no possiblity that after a decimal dot and met is not set
            } else {
                if after_a_decimal_dot { // already one decimal dot
                    return Err(Message::UnexpectedMultiDecimalPointInFloatLiteral{ literal_pos: pos });   // C3, 123.456.789
                }
                after_a_decimal_dot = true;
            }
        } else {
            match ch.to_digit(radix) {
                Some(digit) => { 
                    if after_a_decimal_dot {                                                                // C5, normal
                        ret_val2.push(digit);
                    } else {
                        ret_val1.push(digit);
                    }
                }
                None => return Err(Message::UnexpectedCharInNumericLiteral{ literal_pos: pos }),            // C4, 123ABC
            }
        }
    }

    if postfix.is_not_set() {
        postfix = Postfix::I32;                                                                         // C6, 123 => 123, I32
    }

    if postfix.is_integral() {
        return Ok((Content::Integral(ret_val1), postfix));
    } else if postfix.is_float() {
        return Ok((Content::Float(ret_val1, ret_val2), postfix));
    } else {
        unreachable!()
    }
}

fn delegate(raw: String, pos: StringPosition) -> Result<NumLitValue, Message> {

    let prefix = get_prefix(&*raw, pos)?;

    // prefix length != 0 means has prefix
    let postfix = get_postfix(&raw[prefix.len()..], pos, prefix.len() != 0)?;

    match get_content(&raw[prefix.len()..(raw.len() - postfix.len())], pos, prefix.radix(), postfix) {
        Err(msg) => Err(msg),
        Ok((Content::Integral(content), Postfix::U8)) => values_to_u8(prefix.radix(), content, pos),
        Ok((Content::Integral(content), Postfix::U32)) => values_to_u32(prefix.radix(), content, pos),
        Ok((Content::Integral(content), Postfix::I32)) => values_to_i32(prefix.radix(), content, pos),
        Ok((Content::Integral(content), Postfix::U64)) => values_to_u64(prefix.radix(), content, pos),
        Ok((Content::Integral(content), Postfix::I8)) => values_to_i8(prefix.radix(), content, pos),
        Ok((Content::Integral(content), Postfix::U16)) => values_to_u16(prefix.radix(), content, pos),
        Ok((Content::Integral(content), Postfix::I16)) => values_to_i16(prefix.radix(), content, pos),
        Ok((Content::Integral(content), Postfix::I64)) => values_to_i64(prefix.radix(), content, pos),
        Ok((Content::Float(content1, content2), Postfix::F32)) => values_to_f32(prefix.radix(), content1, content2, pos),
        Ok((Content::Float(content1, content2), Postfix::F64)) => values_to_f64(prefix.radix(), content1, content2, pos),
        _ => unreachable!()
    }
}

/// NumericLiteral => [0b|0o|0d|0x|][\._0-9]*[u8|u32|i32|u64|f32|f64|]
pub fn parse_numeric_literal(raw: String, pos: StringPosition, messages: &mut MessageEmitter) -> NumericLiteral {
    
    match delegate(raw, pos) {
        Ok(value) => NumericLiteral{ value: Some(value), pos: pos },
        Err(msg) => {
            messages.push(msg);
            NumericLiteral{ value: None, pos: pos }
        }
    }

}

#[cfg(test)]
#[test]
fn num_lit_prefix() {

    macro_rules! test_case {
        ([$raw: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr] [$expect: expr]) => ({
            assert_eq!(get_prefix($raw, make_str_pos!($row1, $col1, $row2, $col2)), $expect);
        })
    }

    test_case!{ ["0", 1, 1, 1, 1] [Ok(Prefix::NotSet)] };
    test_case!{ ["0x1", 1, 1, 1, 3] [Ok(Prefix::Hex)] };
    test_case!{ ["0b1", 1, 1, 1, 3] [Ok(Prefix::Binary)] };
    test_case!{ ["0o1", 1, 1, 1, 3] [Ok(Prefix::Octal)] };
    test_case!{ ["0d1", 1, 1, 1, 3] [Ok(Prefix::Decimal)] };
    test_case!{ ["1234567", 1, 1, 1, 7] [Ok(Prefix::NotSet)] };
    test_case!{ ["12", 1, 1, 1, 2] [Ok(Prefix::NotSet)] };
    test_case!{ ["1234567", 1, 1, 1, 7] [Ok(Prefix::NotSet)] };
    test_case!{ ["12", 1, 1, 1, 2] [Ok(Prefix::NotSet)] };
    test_case!{ ["0f32", 1, 1, 1, 4] [Ok(Prefix::NotSet)] };
    test_case!{ ["0u8", 1, 1, 1, 3] [Ok(Prefix::NotSet)] };
    test_case!{ ["0u78", 1, 1, 1, 2] [Err(Message::InvalidPrefixInNumericLiteral{ literal_pos: StringPosition::from((1, 1, 1, 2)), prefix: 'u' })] };
    test_case!{ ["0x", 1, 1, 1, 2] [Err(Message::EmptyNumericLiteral{ literal_pos: StringPosition::from((1, 1, 1, 2)) })] };
    test_case!{ ["001", 1, 1, 1, 3] [Err(Message::InvalidPrefixInNumericLiteral{ literal_pos: StringPosition::from((1, 1, 1, 3)), prefix: '0' })] };
    test_case!{ ["0X1", 1, 1, 1, 450] [Err(Message::InvalidPrefixInNumericLiteral{ literal_pos: StringPosition::from((1, 1, 1, 450)), prefix: 'X' })] };
}

#[cfg(test)]
#[test]
fn num_lit_postfix() {

    assert_eq!(
        get_postfix("u8", StringPosition::from((1, 1, 1, 2)), false), 
        Err(Message::EmptyNumericLiteral{ literal_pos: StringPosition::from((1, 1, 1, 2)) }));
    assert_eq!(
        get_postfix("i8", StringPosition::from((1, 1, 1, 2)), false), 
        Err(Message::EmptyNumericLiteral{ literal_pos: StringPosition::from((1, 1, 1, 2)) }));
    assert_eq!(
        get_postfix("u16", StringPosition::from((1, 1, 1, 3)), false), 
        Err(Message::EmptyNumericLiteral{ literal_pos: StringPosition::from((1, 1, 1, 3)) }));
    assert_eq!(
        get_postfix("i16", StringPosition::from((1, 1, 1, 3)), false), 
        Err(Message::EmptyNumericLiteral{ literal_pos: StringPosition::from((1, 1, 1, 3)) }));
    assert_eq!(
        get_postfix("i64", StringPosition::from((1, 1, 1, 3)), false), 
        Err(Message::EmptyNumericLiteral{ literal_pos: StringPosition::from((1, 1, 1, 3)) }));
    assert_eq!(
        get_postfix("u32", StringPosition::from((1, 1, 1, 3)), false), 
        Err(Message::EmptyNumericLiteral{ literal_pos: StringPosition::from((1, 1, 1, 3)) }));
    assert_eq!(
        get_postfix("i32", StringPosition::from((1, 1, 1, 3)), false), 
        Err(Message::EmptyNumericLiteral{ literal_pos: StringPosition::from((1, 1, 1, 3)) }));
    assert_eq!(
        get_postfix("u64", StringPosition::from((1, 1, 1, 3)), false), 
        Err(Message::EmptyNumericLiteral{ literal_pos: StringPosition::from((1, 1, 1, 3)) }));
    assert_eq!(
        get_postfix("f32", StringPosition::from((1, 1, 1, 3)), false), 
        Err(Message::EmptyNumericLiteral{ literal_pos: StringPosition::from((1, 1, 1, 3)) }));
    assert_eq!(
        get_postfix("f64", StringPosition::from((1, 1, 1, 3)), false), 
        Err(Message::EmptyNumericLiteral{ literal_pos: StringPosition::from((1, 1, 1, 3)) }));

    assert_eq!(
        get_postfix("1u8", StringPosition::from((1, 1, 1, 3)), false), 
        Ok(Postfix::U8));
    assert_eq!(
        get_postfix("1i8", StringPosition::from((1, 1, 1, 3)), false), 
        Ok(Postfix::I8));
    assert_eq!(
        get_postfix("1i64", StringPosition::from((1, 1, 1, 3)), false), 
        Ok(Postfix::I64));
    assert_eq!(
        get_postfix("1u16", StringPosition::from((1, 1, 1, 3)), false), 
        Ok(Postfix::U16));
    assert_eq!(
        get_postfix("1i16", StringPosition::from((1, 1, 1, 3)), false), 
        Ok(Postfix::I16));
    assert_eq!(
        get_postfix("1u64", StringPosition::from((1, 1, 1, 3)), false), 
        Ok(Postfix::U64));
    assert_eq!(
        get_postfix("1u32", StringPosition::from((1, 1, 1, 3)), false), 
        Ok(Postfix::U32));
    assert_eq!(
        get_postfix("1i32", StringPosition::from((1, 1, 1, 3)), false), 
        Ok(Postfix::I32));
    assert_eq!(
        get_postfix("1f64", StringPosition::from((1, 1, 1, 3)), false), 
        Ok(Postfix::F64));
    assert_eq!(
        get_postfix("1f32", StringPosition::from((1, 1, 1, 3)), false), 
        Ok(Postfix::F32));
    
    assert_eq!(
        get_postfix("1u18", StringPosition::from((1, 1, 1, 3)), false), 
        Ok(Postfix::NotSet));
    assert_eq!(
        get_postfix("1xxx", StringPosition::from((1, 1, 1, 3)), false), 
        Ok(Postfix::NotSet));
    assert_eq!(
        get_postfix("1abc", StringPosition::from((1, 1, 1, 3)), false), 
        Ok(Postfix::NotSet));
        
    assert_eq!(
        get_postfix("1u8", StringPosition::from((1, 1, 1, 3)), true), 
        Ok(Postfix::U8));
    assert_eq!(
        get_postfix("1u64", StringPosition::from((1, 1, 1, 3)), true), 
        Ok(Postfix::U64));
    assert_eq!(
        get_postfix("1u32", StringPosition::from((1, 1, 1, 3)), true), 
        Ok(Postfix::U32));
    assert_eq!(
        get_postfix("1i32", StringPosition::from((1, 1, 1, 3)), true), 
        Ok(Postfix::I32));
    assert_eq!(
        get_postfix("1f64", StringPosition::from((1, 1, 1, 3)), true), 
        Err(Message::PrefixNotSupportedForFloatLiteral{ literal_pos: StringPosition::from((1, 1, 1, 3)) }));
    assert_eq!(
        get_postfix("1f32", StringPosition::from((1, 1, 1, 3)), true), 
        Err(Message::PrefixNotSupportedForFloatLiteral{ literal_pos: StringPosition::from((1, 1, 1, 3)) }));
}

#[cfg(test)]
#[test]
fn num_lit_content() {
    let pos = StringPosition::from((1, 1, 1, 8));
    
    assert_eq!(get_content("123", pos, 10, Postfix::I32), Ok((Content::Integral(vec![1, 2, 3]), Postfix::I32)));
    assert_eq!(get_content("123.456", pos, 10, Postfix::I32), Err(Message::UnexpectedDecimalPointInIntegralLiteral{ literal_pos: pos }));
    assert_eq!(get_content("123", pos, 10, Postfix::NotSet), Ok((Content::Integral(vec![1, 2, 3]), Postfix::I32)));
    assert_eq!(get_content("123.456", pos, 10, Postfix::NotSet), Ok((Content::Float(vec![1, 2, 3], vec![4, 5, 6]), Postfix::F64)));
    assert_eq!(get_content("123.456.789", pos, 10, Postfix::F64), Err(Message::UnexpectedMultiDecimalPointInFloatLiteral{ literal_pos: pos }));
    assert_eq!(get_content("123ABC", pos, 10, Postfix::I32), Err(Message::UnexpectedCharInNumericLiteral{ literal_pos: pos }));
    assert_eq!(get_content("123321", pos, 2, Postfix::I32), Err(Message::UnexpectedCharInNumericLiteral{ literal_pos: pos }));
    assert_eq!(get_content("ABCDEFG", pos, 16, Postfix::I32), Err(Message::UnexpectedCharInNumericLiteral{ literal_pos: pos }));
    assert_eq!(get_content("123", pos, 10, Postfix::F32), Ok((Content::Float(vec![1, 2, 3], vec![]), Postfix::F32)));
    assert_eq!(get_content("123.", pos, 10, Postfix::F32), Ok((Content::Float(vec![1, 2, 3], vec![]), Postfix::F32)));

    // underscore
    assert_eq!(get_content("1_23", pos, 10, Postfix::I32), Ok((Content::Integral(vec![1, 2, 3]), Postfix::I32)));
    assert_eq!(get_content("1____23", pos, 10, Postfix::I32), Ok((Content::Integral(vec![1, 2, 3]), Postfix::I32)));
    assert_eq!(get_content("_1_23", pos, 10, Postfix::I32), Ok((Content::Integral(vec![1, 2, 3]), Postfix::I32)));
    assert_eq!(get_content("1_2_3_", pos, 10, Postfix::I32), Ok((Content::Integral(vec![1, 2, 3]), Postfix::I32)));
    assert_eq!(get_content("123.456", pos, 10, Postfix::I32), Err(Message::UnexpectedDecimalPointInIntegralLiteral{ literal_pos: pos }));
    assert_eq!(get_content("_____123", pos, 10, Postfix::NotSet), Ok((Content::Integral(vec![1, 2, 3]), Postfix::I32)));
    assert_eq!(get_content("123_._456_____", pos, 10, Postfix::NotSet), Ok((Content::Float(vec![1, 2, 3], vec![4, 5, 6]), Postfix::F64))); // strange but supported
    assert_eq!(get_content("123.456.789", pos, 10, Postfix::F64), Err(Message::UnexpectedMultiDecimalPointInFloatLiteral{ literal_pos: pos }));
    assert_eq!(get_content("123ABC", pos, 10, Postfix::I32), Err(Message::UnexpectedCharInNumericLiteral{ literal_pos: pos }));
    assert_eq!(get_content("123321", pos, 2, Postfix::I32), Err(Message::UnexpectedCharInNumericLiteral{ literal_pos: pos }));
    assert_eq!(get_content("ABCDEFG", pos, 16, Postfix::I32), Err(Message::UnexpectedCharInNumericLiteral{ literal_pos: pos }));
    assert_eq!(get_content("____123", pos, 10, Postfix::F32), Ok((Content::Float(vec![1, 2, 3], vec![]), Postfix::F32)));
    assert_eq!(get_content("123.________", pos, 10, Postfix::F32), Ok((Content::Float(vec![1, 2, 3], vec![]), Postfix::F32)));

    // single quote
    assert_eq!(get_content("1'23", pos, 10, Postfix::I32), Ok((Content::Integral(vec![1, 2, 3]), Postfix::I32)));
    assert_eq!(get_content("1''''23", pos, 10, Postfix::I32), Ok((Content::Integral(vec![1, 2, 3]), Postfix::I32)));
    assert_eq!(get_content("'1'23", pos, 10, Postfix::I32), Ok((Content::Integral(vec![1, 2, 3]), Postfix::I32)));
    assert_eq!(get_content("1'2'3'", pos, 10, Postfix::I32), Ok((Content::Integral(vec![1, 2, 3]), Postfix::I32)));
    assert_eq!(get_content("123.456", pos, 10, Postfix::I32), Err(Message::UnexpectedDecimalPointInIntegralLiteral{ literal_pos: pos }));
    assert_eq!(get_content("'''''123", pos, 10, Postfix::NotSet), Ok((Content::Integral(vec![1, 2, 3]), Postfix::I32)));
    assert_eq!(get_content("123'.'456'''''", pos, 10, Postfix::NotSet), Ok((Content::Float(vec![1, 2, 3], vec![4, 5, 6]), Postfix::F64))); // more strange but supported
    assert_eq!(get_content("123.456.789", pos, 10, Postfix::F64), Err(Message::UnexpectedMultiDecimalPointInFloatLiteral{ literal_pos: pos }));
    assert_eq!(get_content("123ABC", pos, 10, Postfix::I32), Err(Message::UnexpectedCharInNumericLiteral{ literal_pos: pos }));
    assert_eq!(get_content("123321", pos, 2, Postfix::I32), Err(Message::UnexpectedCharInNumericLiteral{ literal_pos: pos }));
    assert_eq!(get_content("ABCDEFG", pos, 16, Postfix::I32), Err(Message::UnexpectedCharInNumericLiteral{ literal_pos: pos }));
    assert_eq!(get_content("''''123", pos, 10, Postfix::F32), Ok((Content::Float(vec![1, 2, 3], vec![]), Postfix::F32)));
    assert_eq!(get_content("123.''''''''", pos, 10, Postfix::F32), Ok((Content::Float(vec![1, 2, 3], vec![]), Postfix::F32)));

    assert_eq!(get_content("1'2_3", pos, 10, Postfix::I32), Err(Message::MultiSeperatorInNumericLiteral{ literal_pos: pos }));
    assert_eq!(get_content("1_2'3", pos, 10, Postfix::I32), Err(Message::MultiSeperatorInNumericLiteral{ literal_pos: pos }));
}

#[cfg(test)]
#[test]
fn num_lit_u32() {
    let pos = StringPosition::from((1, 2, 3, 4));

    assert_eq!(values_to_u32(2, vec![1, 0, 1, 0], pos), Ok(NumLitValue::U32(0b1010)));
    assert_eq!(values_to_u32(8, vec![1, 2, 3], pos), Ok(NumLitValue::U32(0o123)));
    assert_eq!(values_to_u32(10, vec![1, 2, 3], pos), Ok(NumLitValue::U32(123)));
    assert_eq!(values_to_u32(16, vec![1, 2, 3], pos), Ok(NumLitValue::U32(0x123)));

    // Not too long max and too long mins
    assert_eq!(values_to_u32(2, vec![1; 32], pos), Ok(NumLitValue::U32(0xFFFFFFFF)));
    assert_eq!(values_to_u32(8, vec![3, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7], pos), Ok(NumLitValue::U32(0xFFFFFFFF)));
    assert_eq!(values_to_u32(10, vec![4, 2, 9, 4, 9, 6, 7, 2, 9, 5], pos), Ok(NumLitValue::U32(0xFFFFFFFF)));
    assert_eq!(values_to_u32(16, vec![15; 8], pos), Ok(NumLitValue::U32(0xFFFFFFFF)));

    assert_eq!(values_to_u32(2, vec![1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], pos), Err(Message::NumericLiteralTooLarge{ literal_pos: pos }));
    assert_eq!(values_to_u32(8, vec![4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], pos), Err(Message::NumericLiteralTooLarge{ literal_pos: pos }));
    assert_eq!(values_to_u32(10, vec![4, 2, 9, 4, 9, 6, 7, 2, 9, 6], pos), Err(Message::NumericLiteralTooLarge{ literal_pos: pos }));
    assert_eq!(values_to_u32(16, vec![16, 0, 0, 0, 0, 0, 0, 0, 0], pos), Err(Message::NumericLiteralTooLarge{ literal_pos: pos }));
}

#[cfg(test)]
#[test]
fn num_lit_f32() {
    let pos = StringPosition::from((1, 2, 3, 4));

    assert_eq!(values_to_f32(10, vec![1, 2, 3], vec![4, 5, 6], pos), Ok(NumLitValue::F32(123.456f32)));
    assert_eq!(values_to_f32(10, vec![0], vec![], pos), Ok(NumLitValue::F32(0f32)));
    assert_eq!(values_to_f32(10, vec![0], vec![1, 2, 3], pos), Ok(NumLitValue::F32(0.123f32)));
}

#[cfg(test)]
#[test]
fn num_lit_inter() {
    let pos = StringPosition::from((1, 2, 3, 4));
    macro_rules! test_case {
        ($input: expr, $pos: expr, ok: $expect: expr) => (
            assert_eq!(delegate($input.to_owned(), $pos), Ok(NumLitValue::from($expect)));
        );
        ($input: expr, $pos: expr, err: $expect: expr) => (
            assert_eq!(delegate($input.to_owned(), $pos), Err($expect));
        )
    }

    test_case!{ "123", pos, ok: NumLitValue::I32(123) };
    test_case!{ "0123", pos, err: Message::InvalidPrefixInNumericLiteral{ literal_pos: pos, prefix: '1' } };
    test_case!{ "123.", pos, ok: NumLitValue::F64(123f64) };
    test_case!{ "0x123", pos, ok: NumLitValue::I32(0x123) };
    test_case!{ "0o123u64", pos, ok: NumLitValue::U64(0o123) };
    test_case!{ "0b1010", pos, ok: NumLitValue::I32(0b1010) };
    test_case!{ "0f32", pos, ok: NumLitValue::F32(0f32) };
    test_case!{ "0u32", pos, ok: NumLitValue::U32(0u32) };
    test_case!{ "___123", pos, ok: NumLitValue::I32(123) };
    test_case!{ "01_23", pos, err: Message::InvalidPrefixInNumericLiteral{ literal_pos: pos, prefix: '1' } };
    test_case!{ "123_____.", pos, ok: NumLitValue::F64(123f64) };
    test_case!{ "0x1'2'3", pos, ok: NumLitValue::I32(0x123) };
    test_case!{ "0o12_3u64", pos, ok: NumLitValue::U64(0o123) };
    test_case!{ "0b101_0", pos, ok: NumLitValue::I32(0b1010) };
    test_case!{ "0f3_2", pos, err: Message::InvalidPrefixInNumericLiteral{ literal_pos: pos, prefix: 'f' } };
}