#![allow(dead_code)]
///! fff-lang
///! numeric literal parser
// TODO: ' as seperator, multi seperator not supported, full test

// Syntax:
// NumericLiteral => IntegralLiteral | FloatingPointLiteral
//
// IntegralLiteral => ['-'] ['0d'] [Char0To9]+ [SignedIntegralPostfix]
//                    | ['-'] '0b' [Char0Or1]+ [SignedIntegralPostifx]
//                    | ['-'] '0o' [Char0To7]+ [SignedIntegralPostifx]
//                    | ['-'] '0x' [Char0ToF]+ [SignedIntegralPostifx]
//                    | ['0d'] [Char0To9]+ [UnsignedIntegralPostfix]
//                    | '0b' [Char0Or1]+ [UnsignedIntegralPostfix]
//                    | '0o' [Char0To7]+ [UnsignedIntegralPostfix]
//                    | '0x' [Char0ToF]+ [UnsignedIntegralPostfix]
// SignedIntegralPostfix = 'i8' | 'i16' | 'i32' | 'i64' 
// UnsignedIntegralPostfix = 'u8' | 'u16' | 'u32' | 'u64'
//
// FloatingPointLiteral => [-] [Char0To9]+ '.' [Char0To9]+ [FloatingPointExponential] [FloatingPointPostfix]
//                         | [-] [Char0To9]+ [FloatingPointExponential] FloatingPointPostfix
// FloatingPointPostfix = 'f32' | 'f64'
// FloatingPointExponential => ['e' | 'E'] ['+' | '-'] [Char0To9]+
// 
// Char0To9 = '0'...'9'
// Char0Or1 = '0' | '1'
// Char0To7 = '0'...'7'
// Char0ToF = '0'...'9' | 'A'...'F' | 'a'...'f'

use codepos::StringPosition;
use message::Message;
use message::MessageCollection;

use super::super::NumLitValue;

// // Final to value, check too large
// macro_rules! values_to_integral {
//     ($fn_name: ident, $ty: ty, $result_path: path) => (

//         fn $fn_name(radix: u32, content: Vec<u32>, pos: StringPosition) -> Result<NumLitValue, LexicalMessage> {
//             let mut ret_val: $ty = 0;
//             let mut digit_weight: $ty = 1;
//             let len = content.len();
//             let mut index = 0;
//             for digit in content.into_iter().rev() {
//                 let digit = digit as $ty;
//                 match digit.checked_mul(digit_weight) {
//                     Some(temp1) => match ret_val.checked_add(temp1) {
//                         Some(result) => ret_val = result,
//                         None => return Err(LexicalMessage::NumericLiteralTooLarge{ literal_pos: pos }),
//                     }, 
//                     None => return Err(LexicalMessage::NumericLiteralTooLarge{ literal_pos: pos }),
//                 }

//                 if index + 1 != len {
//                     digit_weight = match digit_weight.checked_mul(radix as $ty) {
//                         Some(result) => result,
//                         None => return Err(LexicalMessage::NumericLiteralTooLarge{ literal_pos: pos }),
//                     }
//                 }
//                 index += 1;
//             }

//             Ok($result_path(ret_val))
//         }
//     )
// }
// values_to_integral!{ values_to_i8, i8, NumLitValue::I8 }
// values_to_integral!{ values_to_u8, u8, NumLitValue::U8 }
// values_to_integral!{ values_to_i16, i16, NumLitValue::I16 }
// values_to_integral!{ values_to_u16, u16, NumLitValue::U16 }
// values_to_integral!{ values_to_i32, i32, NumLitValue::I32 }
// values_to_integral!{ values_to_u32, u32, NumLitValue::U32 }
// values_to_integral!{ values_to_i64, i64, NumLitValue::I64 }
// values_to_integral!{ values_to_u64, u64, NumLitValue::U64 }

// macro_rules! values_to_float {
//     ($fn_name: ident, $ty: ty, $result_path: path) => (

//         fn $fn_name(radix: u32, content1: Vec<u32>, content2: Vec<u32>, _pos: StringPosition) -> Result<NumLitValue, LexicalMessage> {

//             let mut ret_val: $ty = 0 as $ty;
//             let mut digit_weight: $ty = 1 as $ty;
//             let len = content1.len();
//             let mut index = 0;
//             for digit in content1.into_iter().rev() {
//                 let digit = digit as $ty;
//                 ret_val += digit * digit_weight;

//                 if index + 1 != len {
//                     digit_weight *= radix as $ty;
//                 }
//                 index += 1;
//             }

//             digit_weight = 1 as $ty;    
//             for digit in content2.into_iter() {
//                 let digit = digit as $ty;
//                 digit_weight /= radix as $ty;
//                 ret_val += digit * digit_weight;
//             }

//             Ok($result_path(ret_val))
//         }
//     )
// }
// values_to_float!{ values_to_f32, f32, NumLitValue::F32 }
// values_to_float!{ values_to_f64, f64, NumLitValue::F64 }

// test_only_attr!{
//     [derive(Debug, Eq, PartialEq)]
//     ![derive(Eq, PartialEq)]
//     enum Prefix {
//         Binary,
//         Octal,
//         Decimal,
//         Hex,
//         NotSet, // not set maybe float, not same as decimal
//     }
// }
// impl Prefix {
//     // Length for the prefix, notset is 0
//     fn len(&self) -> usize {
//         match *self {
//             Prefix::Binary | Prefix::Octal | Prefix::Decimal | Prefix::Hex => 2,
//             Prefix::NotSet => 0,
//         }
//     }
//     // get radix of the prefix
//     fn radix(&self) -> u32 {
//         match *self {
//             Prefix::Binary => 2,
//             Prefix::Octal => 8,
//             Prefix::Decimal => 10,
//             Prefix::Hex => 16,
//             Prefix::NotSet => 10,
//         }
//     }
// }
// test_only_attr!{
//     [derive(Debug, Eq, PartialEq)]
//     ![derive(Eq, PartialEq)]
//     enum Postfix {
//         I8,
//         U8,
//         I16,
//         U16, 
//         I32,
//         U32,
//         I64,
//         U64,
//         F32,
//         F64,
//         NotSet, // I32 for maybe integral, F64 for floating point
//     }
// }
// impl Postfix {
//     fn len(&self) -> usize {
//         match *self {
//             Postfix::NotSet => 0,
//             Postfix::I8 => 2,
//             Postfix::U8 => 2,
//             _ => 3,
//         }
//     }
//     fn is_float(&self) -> bool {
//         match *self {
//             Postfix::F32 | Postfix::F64 => true,
//             _ => false,
//         }
//     }
//     fn is_not_set(&self) -> bool {
//         match *self{
//             Postfix::NotSet => true,
//             _ => false,
//         }
//     }
//     fn is_integral(&self) -> bool {
//         match *self {
//             Postfix::I8 | Postfix::U8
//             | Postfix::I16 | Postfix::U16 
//             | Postfix::I32 | Postfix::U32 
//             | Postfix::I64| Postfix::U64 => true, 
//             _ => false,  
//         }
//     }
// }
// test_only_attr!{
//     [derive(Debug, Eq, PartialEq)]
//     ![derive(Eq, PartialEq)]
//     enum Content {
//         Integral(Vec<u32>),
//         Float(Vec<u32>, Vec<u32>),
//     }
// }

// // get prefix
// //     start with 0 is prefix, if not the 4 prefix is invliad prefix because in C start with 0 is Octal         // InvalidPrefixInNumericLiteral
// //     check if rest is empty, if not, throw messages here not furthur
// fn get_prefix(raw: &str, pos: StringPosition) -> Result<Prefix, LexicalMessage> {

//     if raw.len() < 2 {
//         return Ok(Prefix::NotSet);
//     }

//     let mut chars = raw.chars();
//     let ret_val = match chars.next().unwrap() {
//         '0' => match chars.next().unwrap() {    // length > 2, safe to unwrap these 2
//             'x' => Ok(Prefix::Hex),
//             'd' => Ok(Prefix::Decimal),
//             'o' => Ok(Prefix::Octal),
//             'b' => Ok(Prefix::Binary),
//             // unexpected char after first char '0' and length >= 2
//             maybe_start_of_postfix @ 'i' 
//             | maybe_start_of_postfix @ 'u' 
//             | maybe_start_of_postfix @ 'f' => {
//                 // directly special check `0f32` cases
//                 match (maybe_start_of_postfix, chars.next(), chars.next(), chars.next()) {
//                     ('u', Some('8'), None, None)
//                     | ('i', Some('8'), None, None)
//                     | ('i', Some('1'), Some('6'), None)
//                     | ('u', Some('1'), Some('6'), None)
//                     | ('i', Some('3'), Some('2'), None)
//                     | ('u', Some('3'), Some('2'), None)
//                     | ('i', Some('6'), Some('4'), None)
//                     | ('u', Some('6'), Some('4'), None)
//                     | ('f', Some('3'), Some('2'), None)
//                     | ('f', Some('6'), Some('4'), None) => Ok(Prefix::NotSet),
//                     _other_cases => Err(LexicalMessage::InvalidPrefixInNumericLiteral{ literal_pos: pos, prefix: maybe_start_of_postfix }),
//                 }
//             }
//             other => Err(LexicalMessage::InvalidPrefixInNumericLiteral{ literal_pos: pos, prefix: other }),
//         },
//         _ => Ok(Prefix::NotSet),
//     };

//     match ret_val {
//         Ok(Prefix::NotSet) => Ok(Prefix::NotSet),
//         ref _other if raw.len() == 2 => Err(LexicalMessage::EmptyNumericLiteral{ literal_pos: pos }), // remove prefix is none
//         other => other,
//     }
// }

// // get postfix
// //     floating point postfix should not have prefix, even 0d is not, because prefix means interger             // FloatingPointPostfixNotWithPrefix
// fn get_postfix(raw: &str, pos: StringPosition, has_prefix: bool) -> Result<Postfix, LexicalMessage> {

//     match raw {                                  // find nothing after removing postfix, which is like "0xu8"
//         "i8" | "u8"
//         | "i16" | "u16" 
//         | "u32" | "i32" 
//         | "i64" | "u64" 
//         | "f32" | "f64" => return Err(LexicalMessage::EmptyNumericLiteral{ literal_pos: pos }),
//         _ => (),
//     }

//     let mut revchars = raw.chars().rev();
//     match (revchars.next(), revchars.next(), revchars.next()) {
//         (Some('8'), Some('i'), _) => Ok(Postfix::I8),
//         (Some('8'), Some('u'), _) => Ok(Postfix::U8),
//         (Some('6'), Some('1'), Some('i')) => Ok(Postfix::I16),
//         (Some('6'), Some('1'), Some('u')) => Ok(Postfix::U16),
//         (Some('2'), Some('3'), Some('i')) => Ok(Postfix::I32),
//         (Some('2'), Some('3'), Some('u')) => Ok(Postfix::U32),
//         (Some('4'), Some('6'), Some('i')) => Ok(Postfix::I64),
//         (Some('4'), Some('6'), Some('u')) => Ok(Postfix::U64),

//         (Some('2'), Some('3'), Some('f')) | (Some('4'), Some('6'), Some('f')) if has_prefix => 
//             Err(LexicalMessage::PrefixNotSupportedForFloatLiteral{ literal_pos: pos }),

//         (Some('2'), Some('3'), Some('f')) => Ok(Postfix::F32),
//         (Some('4'), Some('6'), Some('f')) => Ok(Postfix::F64),
//         _ => Ok(Postfix::NotSet),
//     }
// }

// // iterate content to get content before decimal dot and after decimal dot
// //     ignore underscore, check char according to radix of prefix
// //     with prefix or with integral postfix should not have any decimal point                                   // IntegralPostfixForFloatingPointLiteral
// //     prefix limits the max char value,                                                                        // InvalidCharInNumericLiteral     
// //     can not use underscore and single quote at same time                                                     // MultiSeperatorInNumericLiteral       
// fn get_content(raw: &str, pos: StringPosition, radix: u32, mut postfix: Postfix) -> Result<(Content, Postfix), LexicalMessage> {
//     // that is, prefix only has radix meaning, postfix notset is decided by whether have decimal dot
//     // so, iterate content to vec of u32 and final postfix 
//     // and report message on unexpected char of this radix and on expected decimal dot or unexpected multi decimal dot
//     // and then dispatch to different methods to get final value

//     let mut after_a_decimal_dot = false;
//     let mut ret_val1 = Vec::new();
//     let mut ret_val2 = Vec::new();

//     let mut has_underscore = false;
//     let mut has_singlequote = false;
//     for ch in raw.chars() {   
//         if ch == '_' { // ignore underscore
//             if has_singlequote {
//                 return Err(LexicalMessage::MultiSeperatorInNumericLiteral{ literal_pos: pos });
//             }
//             has_underscore = true;
//             continue; 
//         } else if ch == '\'' {
//             if has_underscore {
//                 return Err(LexicalMessage::MultiSeperatorInNumericLiteral{ literal_pos: pos });
//             }
//             has_singlequote = true;
//             continue;
//         } else if ch == '.' {
//             if postfix.is_integral() { // integral and '.', immediate error
//                 return Err(LexicalMessage::UnexpectedDecimalPointInIntegralLiteral{ literal_pos: pos });       // C1, 123.456i32
//             } else if postfix.is_not_set() {
//                 postfix = Postfix::F64; // not set will absolutely change to f64 if met decimal dot,    // C2, 123.456 => 123, 456, F64
//                 after_a_decimal_dot = true;      // so there is no possiblity that after a decimal dot and met is not set
//             } else {
//                 if after_a_decimal_dot { // already one decimal dot
//                     return Err(LexicalMessage::UnexpectedMultiDecimalPointInFloatLiteral{ literal_pos: pos });   // C3, 123.456.789
//                 }
//                 after_a_decimal_dot = true;
//             }
//         } else {
//             match ch.to_digit(radix) {
//                 Some(digit) => { 
//                     if after_a_decimal_dot {                                                                // C5, normal
//                         ret_val2.push(digit);
//                     } else {
//                         ret_val1.push(digit);
//                     }
//                 }
//                 None => return Err(LexicalMessage::UnexpectedCharInNumericLiteral{ literal_pos: pos }),            // C4, 123ABC
//             }
//         }
//     }

//     if postfix.is_not_set() {
//         postfix = Postfix::I32;                                                                         // C6, 123 => 123, I32
//     }

//     if postfix.is_integral() {
//         return Ok((Content::Integral(ret_val1), postfix));
//     } else if postfix.is_float() {
//         return Ok((Content::Float(ret_val1, ret_val2), postfix));
//     } else {
//         unreachable!()
//     }
// }

// fn delegate_legacy(raw: String, pos: StringPosition) -> Result<NumLitValue, LexicalMessage> {

//     let prefix = try!(get_prefix(&*raw, pos));

//     // prefix length != 0 means has prefix
//     let postfix = try!(get_postfix(&raw[prefix.len()..], pos, prefix.len() != 0));

//     match get_content(&raw[prefix.len()..(raw.len() - postfix.len())], pos, prefix.radix(), postfix) {
//         Err(msg) => Err(msg),
//         Ok((Content::Integral(content), Postfix::U8)) => values_to_u8(prefix.radix(), content, pos),
//         Ok((Content::Integral(content), Postfix::U32)) => values_to_u32(prefix.radix(), content, pos),
//         Ok((Content::Integral(content), Postfix::I32)) => values_to_i32(prefix.radix(), content, pos),
//         Ok((Content::Integral(content), Postfix::U64)) => values_to_u64(prefix.radix(), content, pos),
//         Ok((Content::Integral(content), Postfix::I8)) => values_to_i8(prefix.radix(), content, pos),
//         Ok((Content::Integral(content), Postfix::U16)) => values_to_u16(prefix.radix(), content, pos),
//         Ok((Content::Integral(content), Postfix::I16)) => values_to_i16(prefix.radix(), content, pos),
//         Ok((Content::Integral(content), Postfix::I64)) => values_to_i64(prefix.radix(), content, pos),
//         Ok((Content::Float(content1, content2), Postfix::F32)) => values_to_f32(prefix.radix(), content1, content2, pos),
//         Ok((Content::Float(content1, content2), Postfix::F64)) => values_to_f64(prefix.radix(), content1, content2, pos),
//         _ => unreachable!()
//     }
// }

mod error_strings {
    #![allow(non_upper_case_globals)]
    #![allow(dead_code)]
    pub const InvalidNumericLiteral: &'static str = "Invalid numeric literal";
}

// Buf char helper
use super::super::v0lexer::EOFCHAR;
use std::cell::Cell;
struct BufChars<T> {
    chars: T,
    skips: Cell<i32>,
    m_current: char,
    m_next: char,
    m_nextnext: char,
}
#[allow(dead_code)]
impl<T> BufChars<T> where T : Iterator<Item = char> {

    fn new(mut chars: T) -> BufChars<T> {

        macro_rules! some_char_to_char { ($ch: expr) => (if let Some(ch) = $ch { ch } else { EOFCHAR }) }

        let current = some_char_to_char!(chars.next());
        let (next, nextnext) = if current != EOFCHAR {
            let next = some_char_to_char!(chars.next());
            (next, if next != EOFCHAR {
                some_char_to_char!(chars.next())
            } else {
                EOFCHAR
            })
        } else {
            (EOFCHAR, EOFCHAR)
        };
        
        BufChars{ 
            chars: chars,
            m_current: current,
            m_next: next,
            m_nextnext: nextnext,
            skips: Cell::new(1),
        }
    }
    fn actual_move_next(&mut self) {

        if self.m_current != EOFCHAR {
            self.m_current = self.m_next;
            if self.m_next != EOFCHAR {
                self.m_next = self.m_nextnext;
                if self.m_nextnext != EOFCHAR {
                    self.m_nextnext = if let Some(nextnext) = self.chars.next() { nextnext } else { EOFCHAR };
                }
            }
        }
    }
    fn move_next(&mut self) {

        if self.skips.get() > 0 {
            self.skips.set(self.skips.get() - 1);
            return;
        } else {
            while self.skips.get() < 0 {
                self.actual_move_next();
                self.skips.set(self.skips.get() + 1);
            }
        }
        self.actual_move_next();
    }
    fn dummy1(&self) {
        self.skips.set(self.skips.get() - 1)
    }
    fn skip1(&self) {
        self.skips.set(self.skips.get() + 1)
    }
    fn current(&self) -> char {
        self.m_current
    }
    fn current_with_preview(&self) -> (char, char) {
        (self.m_current, self.m_next)
    }
    fn current_with_preview2(&self) -> (char, char, char) {
        (self.m_current, self.m_next, self.m_nextnext)
    }
    fn current_with_state<TState>(&self, state: TState) -> (TState, char, char, char) {
        (state, self.m_current, self.m_next, self.m_nextnext)
    }
}

enum Prefix {
    Binary,
    Octal,
    Decimal,
    Hex,
    NotSet,
}

fn f64_checked_mul<T: Into<f64>>(lhs: f64, rhs: T) -> Option<f64> {
    use std::f64;
    let rhs = rhs.into();
    if f64::MAX / lhs < rhs {
        return None;
    }
    Some(lhs * rhs)
}
fn f64_checked_add<T: Into<f64>>(lhs: f64, rhs: T) -> Option<f64> {
    use std::f64;
    let rhs = rhs.into();
    if f64::MAX - lhs < rhs {
        return None;
    }
    Some(lhs + rhs)
}
// for pure number conditions, use i32 or u32 or i64 or u64 or f64 to hold the value
fn f64_final_value(value: f64, is_negative: bool) -> NumLitValue {
    use std::{i32, u32, i64, u64, f64};
    if value <= i32::MAX as f64 {
        return NumLitValue::I32(value as i32);
    } else if value <= u32::MAX as f64 && !is_negative {
        return NumLitValue::U32(value as u32);
    } else if value <= i64::MAX as f64{
        return NumLitValue::I64(value as i64);
    } else if value <= u64::MAX as f64 && !is_negative {
        return NumLitValue::U64(value as u64);
    } else {
        return NumLitValue::F64(value);
    }
}
fn u64_final_value(value: u64, is_negative: bool) -> NumLitValue {
    use std::{i32, u32, i64, u64 };
    if value <= i32::MAX as u64 {
        return NumLitValue::I32(value as i32);
    } else if value <= u32::MAX as u64 && !is_negative {
        return NumLitValue::U32(value as u32);
    } else if value <= i64::MAX as u64 {
        return NumLitValue::I64(value as i64);
    } else {
        return NumLitValue::U64(value as u64);
    }
}

fn delegate(raw: String, strpos: StringPosition) -> Result<NumLitValue, Message> {
    use std::{ i8, u8, i16, u16, i32, u32, i64, u64, f32, f64 };

    let new_empty_error = |i: i32| { 
        Err(Message::new(error_strings::InvalidNumericLiteral.to_owned(), vec![(strpos, format!("{}", i))])) 
    };

    // enum State {
    //     Nothing, // no char processed
    //     CheckedNegative(bool), // first char processed, negative and move next or nothing, has_negative_prefix
    //     // after a char after checked int prefix, value stored f64 and name is maybe int because it may expand to f64, normally record current value with base by prefix
    //     // meet dot, e to error, meet EOF to return, meet u and i to check more
    //     MaybeIntNormal(bool, Prefix, bool, f64),  // has_negative, int prefix, previous is underscore, current value
    //     // after a char after checked int prefix which is notset, normally record current value with base 10
    //     UnknownNormal(bool, f64),
    //     // current char is u or i, check if is some int postfix
    //     // if not, error, if is, check current value overflow, if success, return
    //     ExpectingIntPostfix(bool, f64),
    //     // after dot, meet e to FloatExpo
    //     FloatFraction(bool, f64, u32), // is_negative, current value, current after dot length
    //     // after e
    //     FloatExpo(bool, f64, u32), // is_negative, current value, current expo
    //     // currenc char is f, check if is some float postfix
    //     ExpectingFloatPostfix(bool, f64, u32),
    // }
    // let mut state = State::Nothing;
    
    enum State {
        Nothing,
        SomeValue(f64),
        IntPrefix(u32), // base
        ExpectInt(u32, u64), // base, current value
        ExpectIntPostfix(u64),
        ExpectFloatPostfix(f64),
        ExpectEOF(NumLitValue),  // retval
    }

    let mut state = State::Nothing;
    let mut chars = BufChars::new(raw.chars());
    loop {
        chars.move_next();
        match chars.current_with_state(state) {
            (State::Nothing, '0', EOFCHAR, _) => {
                println!("ret 0");
                return Ok(NumLitValue::I32(0));
            }

            (State::Nothing, '0', 'i', _)
            | (State::Nothing, '0', 'u', _) => {
                println!("conv 0");
                state = State::ExpectIntPostfix(0);
            }
            (State::Nothing, '0', 'f', _) => {
                println!("conv 1");
                state = State::ExpectFloatPostfix(0f64);
            }

            (State::Nothing, '0', 'b', EOFCHAR)
            | (State::Nothing, '0', 'o', EOFCHAR)
            | (State::Nothing, '0', 'd', EOFCHAR)
            | (State::Nothing, '0', 'x', EOFCHAR) 
                => return new_empty_error(18),           

            (State::Nothing, '0', 'b', _) => {
                println!("conv 11");
                chars.dummy1();
                state = State::IntPrefix(2);
            }
            (State::Nothing, '0', 'o', _) => {
                println!("conv 12");
                chars.dummy1();
                state = State::IntPrefix(8);
            }
            (State::Nothing, '0', 'd', _) => {
                println!("conv 13");
                chars.dummy1();
                state = State::IntPrefix(10);
            }
            (State::Nothing, '0', 'x', _) => {
                println!("conv 14");
                chars.dummy1();
                state = State::IntPrefix(16);
            }

            (State::Nothing, '0', _, _) => return new_empty_error(0),
            (State::Nothing, ch, _, _) => match ch.to_digit(10) {
                Some(digit) => {
                    println!("conv 2");
                    state = State::SomeValue(digit as f64);
                }
                None => return new_empty_error(1),
            },

            (State::SomeValue(value), EOFCHAR, _, _) => {
                println!("ret 1");
                return Ok(f64_final_value(value, false));
            }
            (State::SomeValue(value), 'i', _, _)
            | (State::SomeValue(value), 'u', _, _) => if value > u64::max_value() as f64 {
                return new_empty_error(28);                                                          // LAST ERROR
            } else {
                println!("conv 18");
                chars.skip1();
                state = State::ExpectIntPostfix(value as u64);
            },
            (State::SomeValue(value), 'f', _, _) => {
                println!("conv 19");                                                                     // LAST CONV
                chars.skip1();
                state = State::ExpectFloatPostfix(value);
            }
            (State::SomeValue(value), ch, _, _) => match ch.to_digit(10) {
                Some(digit) => match f64_checked_mul(value, 10) {
                    Some(value) => match f64_checked_add(value, digit) {
                        Some(value) => {
                            println!("conv 3");
                            state = State::SomeValue(value);
                        }
                        None => return new_empty_error(2),
                    },
                    None => return new_empty_error(3),
                },
                None => return new_empty_error(4),
            },

            (State::IntPrefix(_), 'e', _, _) 
            | (State::IntPrefix(_), 'E', _, _) => return new_empty_error(20),
            (State::IntPrefix(_), '.', _, _) => return new_empty_error(21),
            (State::IntPrefix(base), ch, _, _) => match ch.to_digit(base) {
                Some(digit) => {
                    println!("conv 15"); 
                    state = State::ExpectInt(base, digit as u64);
                }
                None => return new_empty_error(19),
            },

            (State::ExpectInt(_, _), 'e', _, _) 
            | (State::ExpectInt(_, _), 'E', _, _) => return new_empty_error(22),
            (State::ExpectInt(_, _), 'f', _, _) => return new_empty_error(27),
            (State::ExpectInt(_, _), '.', _, _) => return new_empty_error(23),
            (State::ExpectInt(_, value), EOFCHAR, _, _) => {
                println!("ret 5");                                                                      // LAST RET
                return Ok(u64_final_value(value, false))
            }
            (State::ExpectInt(_, value), 'i', _, _)
            | (State::ExpectInt(_, value), 'u', _, _) => {
                println!("conv 17");
                chars.skip1();
                state = State::ExpectIntPostfix(value);
            }
            (State::ExpectInt(base, value), ch, _, _) => match ch.to_digit(base) {
                Some(digit) => match value.checked_mul(base as u64) {
                    Some(value) => match value.checked_add(digit as u64) {
                        Some(value) => {
                            println!("conv 16");
                            state = State::ExpectInt(base, value);
                        }
                        None => return new_empty_error(25),
                    },
                    None => return new_empty_error(26),                        
                },
                None => return new_empty_error(24),
            },
            
            (State::ExpectIntPostfix(value), 'i', '8', EOFCHAR) => 
                return if value > i8::max_value() as u64 { 
                    new_empty_error(5) 
                } else { 
                    println!("ret 2"); 
                    chars.dummy1(); 
                    Ok(NumLitValue::I8(value as i8))
                },
            (State::ExpectIntPostfix(value), 'u', '8', EOFCHAR) => 
                return if value > u8::max_value() as u64 { 
                    new_empty_error(6) 
                } else { 
                    println!("ret 3");
                    chars.dummy1(); 
                    Ok(NumLitValue::U8(value as u8)) 
                },
            (State::ExpectIntPostfix(value), 'i', '1', '6') => 
                if value > i16::max_value() as u64 { 
                    return new_empty_error(7);
                } else {
                    println!("conv 4");
                    chars.dummy1();
                    chars.dummy1();
                    state = State::ExpectEOF(NumLitValue::I16(value as i16)); 
                },
            (State::ExpectIntPostfix(value), 'u', '1', '6') => 
                if value > u16::max_value() as u64 { 
                    return new_empty_error(8);
                } else { 
                    println!("conv 5");
                    chars.dummy1();
                    chars.dummy1();
                    state = State::ExpectEOF(NumLitValue::U16(value as u16)); 
                },
            (State::ExpectIntPostfix(value), 'i', '3', '2') => 
                if value > i32::max_value() as u64 { 
                    return new_empty_error(9);
                } else { 
                    println!("conv 6");
                    chars.dummy1();
                    chars.dummy1();
                    state = State::ExpectEOF(NumLitValue::I32(value as i32)); 
                },
            (State::ExpectIntPostfix(value), 'u', '3', '2') => 
                if value > u32::max_value() as u64 { 
                    return new_empty_error(10);
                } else { 
                    println!("conv 7");
                    chars.dummy1();
                    chars.dummy1();
                    state = State::ExpectEOF(NumLitValue::U32(value as u32)); 
                },
            (State::ExpectIntPostfix(value), 'i', '6', '4') => 
                if value > i64::max_value() as u64 { 
                    return new_empty_error(11);
                } else { 
                    println!("conv 8");
                    chars.dummy1();
                    chars.dummy1();
                    state = State::ExpectEOF(NumLitValue::I64(value as i64)); 
                },
            (State::ExpectIntPostfix(value), 'u', '6', '4') => 
                if value > u64::max_value() { 
                    return new_empty_error(12);
                } else { 
                    println!("conv 9");
                    chars.dummy1();
                    chars.dummy1();
                    state = State::ExpectEOF(NumLitValue::U64(value as u64)); 
                },
            (State::ExpectIntPostfix(_), _, _, _) => return new_empty_error(13),

            (State::ExpectFloatPostfix(value), 'f', '3', '2') => 
                if value > f32::MAX as f64 { 
                    return new_empty_error(14);
                } else { 
                    println!("conv 10");
                    chars.dummy1();
                    chars.dummy1();
                    state = State::ExpectEOF(NumLitValue::F32(value as f32)); 
                },
            (State::ExpectFloatPostfix(value), 'f', '6', '4') => 
                if value > f64::MAX { 
                    return new_empty_error(15);
                } else { 
                    println!("conv 10");
                    chars.dummy1();
                    chars.dummy1();
                    state = State::ExpectEOF(NumLitValue::F64(value as f64)); 
                },
            (State::ExpectFloatPostfix(_), _, _, _) => return new_empty_error(16),

            (State::ExpectEOF(ret_val), EOFCHAR, _, _) => {
                println!("ret 4");
                return Ok(ret_val);
            }
            (State::ExpectEOF(_), _, _, _) => return new_empty_error(17),
        }
    }

}

pub fn parse_numeric_literal(raw: String, pos: StringPosition, messages: &mut MessageCollection) -> (Option<NumLitValue>, StringPosition) {
    
    match delegate(raw, pos) {
        Ok(value) => (Some(value), pos),
        Err(msg) => {
            messages.push(msg);
            (None, pos)
        }
    }

}

#[cfg(test)]
#[test]
fn num_lit_buf_char() {

    let mut chars = BufChars::new("987654321".chars().rev());
    chars.move_next();
    assert_eq!(chars.current_with_state(1), (1, '1', '2', '3'));
    chars.move_next();
    assert_eq!(chars.current_with_state(2), (2, '2', '3', '4'));
    chars.move_next();
    chars.move_next();
    assert_eq!(chars.current_with_state(4), (4, '4', '5', '6'));
    assert_eq!(chars.current_with_state(5), (5, '4', '5', '6'));
    chars.skip1();
    chars.move_next();
    assert_eq!(chars.current_with_state(5), (5, '4', '5', '6'));
    chars.skip1();
    chars.skip1();
    chars.move_next();
    chars.move_next();
    chars.move_next();
    assert_eq!(chars.current_with_state(7), (7, '5', '6', '7'));
    assert_eq!(chars.current_with_state(7), (7, '5', '6', '7'));
    chars.dummy1();
    chars.move_next();
    assert_eq!(chars.current_with_state(5), (5, '7', '8', '9'));
    chars.dummy1();
    chars.dummy1();
    chars.move_next();
    assert_eq!(chars.current_with_state(1), (1, EOFCHAR, EOFCHAR, EOFCHAR));
    chars.move_next();
    assert_eq!(chars.current_with_state(1), (1, EOFCHAR, EOFCHAR, EOFCHAR));
    chars.move_next();
    assert_eq!(chars.current_with_state(123), (123, EOFCHAR, EOFCHAR, EOFCHAR));
    chars.move_next();
    chars.move_next();
    assert_eq!(chars.current_with_state(1024), (1024, EOFCHAR, EOFCHAR, EOFCHAR));
}

#[cfg(test)]
#[test]
fn num_lit_f64_checked() {
    assert_eq!(f64_checked_mul(1.5f64, 2.5f64), Some(3.75f64));
    assert_eq!(f64_checked_mul(1.7976931348623157E+308_f64, 2), None);
    assert_eq!(f64_checked_add(1.2f64, 2.3), Some(3.5));
    assert_eq!(f64_checked_add(1.7976931348623157E+308_f64, 2), None);
}

#[cfg(test)]
#[test]
fn num_lit_feature() {

    // let strpos = make_str_pos!(1, 2, 3, 4);
    fn new_empty_error(i: i32) -> Message { 
        Message::new(error_strings::InvalidNumericLiteral.to_owned(), vec![(make_str_pos!(1, 2, 3, 4), format!("{}", i))])
    }

    macro_rules! test_case {
        ($input: expr, $expect: expr) => (
            println!("\ncase {:?} at {}:", $input, line!());
            assert_eq!(delegate($input.to_owned(), make_str_pos!(1, 2, 3, 4)), Ok(NumLitValue::from($expect)));
        );
        ($input: expr, err, $expect: expr) => (
            println!("\ncase {:?} at {}:", $input, line!());
            assert_eq!(delegate($input.to_owned(), make_str_pos!(1, 2, 3, 4)), Err($expect));
        )
    }
    
    // normal i32
    test_case!("123", NumLitValue::I32(123));
    test_case!("1", NumLitValue::I32(1));
    test_case!("123456789", NumLitValue::I32(123456789));
    test_case!("2147483647", NumLitValue::I32(2147483647));
    // should not start with 0
    test_case!("0123", err, new_empty_error(0));

    // 0s
    test_case!("0", NumLitValue::I32(0));
    test_case!("0u32", NumLitValue::U32(0));
    test_case!("0f32", NumLitValue::F32(0f32));
    test_case!("0x0", NumLitValue::I32(0));
    test_case!("0o0u8", NumLitValue::U8(0));
    test_case!("0u", err, new_empty_error(13));
    test_case!("0f", err, new_empty_error(16));
    test_case!("0i888", err, new_empty_error(13));
    test_case!("0f3210", err, new_empty_error(17));
    // 1s
    test_case!("1", NumLitValue::I32(1));
    test_case!("1u32", NumLitValue::U32(1));
    test_case!("1f32", NumLitValue::F32(1f32));
    test_case!("0x1", NumLitValue::I32(1));
    test_case!("0o1u8", NumLitValue::U8(1));
    
    // normal f64
    test_case!("1.0", NumLitValue::F64(1.0));
    test_case!("1.234", NumLitValue::F64(1.234));
    test_case!("12345678901234567890.0", NumLitValue::F64(12345678901234567890f64));
    test_case!("1.79E308", NumLitValue::F64(1.79E308));
    test_case!("1.79E-308", NumLitValue::F64(1.79E-308));
    // smallflow
    test_case!("1.79E-2333", err, new_empty_error(0));

    // postfix for other integral
    test_case!("1u8", NumLitValue::U8(1));
    test_case!("234i16", NumLitValue::I16(234));
    test_case!("18446744073709551615u64", NumLitValue::U64(18446744073709551615));
    test_case!("100i8", NumLitValue::I8(100));
    test_case!("61234u16", NumLitValue::U16(61234));
    test_case!("9223372036854775807i64", NumLitValue::I64(9223372036854775807));
    // overflow and downflow is error
    test_case!("256u8", err, new_empty_error(0));     // should be overflow and explain large than u8::max
    test_case!("100000i16", err, new_empty_error(0)); // should be overflow and explain large than i16::max
    test_case!("-800i8", err, new_empty_error(0));  // should be downflow and explain smaller than i8

    // negative value
    test_case!("-123", NumLitValue::I32(-123));
    test_case!("-123.456", NumLitValue::F64(-123.456));
    test_case!("-30000i16", NumLitValue::I16(-30000));
    // negative not applicapable for unsigned

    // integral prefix
    test_case!("0xABCD", NumLitValue::I32(0xABCD));
    test_case!("0xfedc", NumLitValue::I32(0xFEDC));
    test_case!("0b101010", NumLitValue::I32(0b101010));
    test_case!("-0o777", NumLitValue::I32(-0o777));
    // invalid char
    test_case!("0xXXXX", err, new_empty_error(0)); // should be invalid char at xx:xx
    test_case!("0b1234", err, new_empty_error(0)); // should be invalid char at xx:xx
    test_case!("0daaaa", err, new_empty_error(0)); // should be invalid char at xx:xx

    // floating point no prefix 
    test_case!("0x123.0", err, new_empty_error(0)); // should be floating point no prefix
    test_case!("0b111.01", err, new_empty_error(0)); // should be floating point no prefix

    // auto expansion for no postfix
    test_case!("2147483645", NumLitValue::I32(2147483645));
    test_case!("2147483648", NumLitValue::U32(2147483648));                     // 2^31 - 1..2^32 expand to u32
    test_case!("4333333333", NumLitValue::I64(4333333333));                     // 2^32..2^63 expand to i64
    test_case!("9223372036854775807", NumLitValue::I64(9223372036854775807));   // 2^32..2^63 expand to i64
    test_case!("9223372036854775808", NumLitValue::U64(9223372036854775808));   // 2^63..2^64 expand to u64
    test_case!("18446744073709551615", NumLitValue::U64(18446744073709551615)); // 2^63..2^64 expand to u64
    test_case!("18446744073709551616", NumLitValue::F64(18446744073709551616f64)); // 2^64.. expand to f64
    // auto expansion for negative value
    test_case!("-2147483648", NumLitValue::I32(-2147483648));
    test_case!("-2147483649", NumLitValue::I64(-2147483649));
    test_case!("-9223372036854775809", NumLitValue::F64(-9223372036854775809f64));

    // floats' e
    test_case!("123e10", NumLitValue::F64(123E10));
    test_case!("123E10", NumLitValue::F64(123E10));
    test_case!("123E+10", NumLitValue::F64(123E10));
    test_case!("123E-10", NumLitValue::F64(123E-10));
    test_case!("0.456E5", NumLitValue::F64(0.456E5));
    test_case!("0.0001E-200", NumLitValue::F64(0.0001E-200));
    test_case!("-0.0001E-200", NumLitValue::F64(-0.0001E-200));
    test_case!("123E5f32", NumLitValue::F32(123E5f32));
    // e not with prefix or postfix, exp should be integer
    test_case!("0x123E-5", err, new_empty_error(0)); // should be floating point should not use prefix
    test_case!("123.456E789.0", err, new_empty_error(0)); // should be floating point scientific representation's exponential part should be integer

    // cannot first or last char is dot
    test_case!(".123", err, new_empty_error(0));
    test_case!("0x.123", err, new_empty_error(0));
    test_case!("123.", err, new_empty_error(0));
    test_case!(".", err, new_empty_error(0));

    // multi dot
    test_case!("123.456.789", err, new_empty_error(0));
    test_case!("123..456", err, new_empty_error(0));

    // underscore as sepreator
    test_case!("0b110_111_000_001u16", NumLitValue::U16(0b11011100000));
    test_case!("123_456_789u64", NumLitValue::U64(123456789));
    test_case!("1_2_3_4", NumLitValue::I32(1234));
    // underscore not at head, tail
    test_case!("_1234", err, new_empty_error(0));
    test_case!("1_", err, new_empty_error(0));
    test_case!("_", err, new_empty_error(0));
    test_case!("0b_1110_1001", NumLitValue::I32(0b11101001));
    test_case!("0x_1234_i64", NumLitValue::I64(0x1234));
    // underscore not in postfix or prefix
    test_case!("0_xABCD", err, new_empty_error(0));
    test_case!("ABCDu6_4", err, new_empty_error(0));
    test_case!("123i_32", err, new_empty_error(0));
    // underscore no double
    test_case!("123__456", err, new_empty_error(0));
    test_case!("0b__101100", err, new_empty_error(0));

    // empty
    test_case!("0xu64", err, new_empty_error(0));
    test_case!("0bf32", err, new_empty_error(0));

    // strange postfix and prefix
    test_case!("0X123", err, new_empty_error(0));
    test_case!("001", err, new_empty_error(0));
    test_case!("0u123", err, new_empty_error(0));
    test_case!("123u18", err, new_empty_error(0));
    test_case!("1u128", err, new_empty_error(0));
    test_case!("654321u1024", err, new_empty_error(0));
    test_case!("0x3f2048", err, new_empty_error(0));
}