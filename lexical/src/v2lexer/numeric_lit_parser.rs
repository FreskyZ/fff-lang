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

fn f64_checked_add<T: Into<f64>>(lhs: f64, rhs: T) -> Option<f64> {
    use std::f64;

    let rhs = rhs.into();
    if rhs < 0f64 {
        if f64::MIN - rhs >= lhs { None } else { Some(lhs + rhs) }
    } else {
        if f64::MAX - rhs <= lhs { None } else { Some(lhs + rhs) }
    }
}
fn f64_checked_mul<T: Into<f64>>(lhs: f64, rhs: T) -> Option<f64> {
    use std::f64;

    let rhs = rhs.into();
    if rhs < 1f64 {
        if f64::MIN_POSITIVE / rhs >= lhs { None } else { Some(lhs * rhs) }
    } else {
        if f64::MAX / rhs <= lhs { None } else { Some(lhs * rhs) }
    }
}
fn f64_checked_mul_add<T: Into<f64>, U: Into<f64>>(lhs: f64, muler: T, adder: U) -> Option<f64> {
    use std::f64;

    let muler = muler.into();
    let adder = adder.into();
    match (muler.abs() < 1f64, adder < 0f64) {
        (true, true) => Some(lhs.mul_add(muler, adder)),
        (true, false) => if (f64::MAX - adder) <= lhs * muler { None } else { Some(lhs.mul_add(muler, adder)) },
        (false, true) => Some(lhs.mul_add(muler, adder)),
        (false, false) => if (f64::MAX - adder) / muler <= lhs { None } else { Some(lhs.mul_add(muler, adder)) },
    }
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
    
    macro_rules! conv { ($id: expr, $new_state: expr) => ({ println!("    conv {}", $id); $new_state }) }
    macro_rules! retok { ($id: expr, $ret_val: expr) => ({ println!("    retok {}", $id); return Ok($ret_val); }) }
    macro_rules! reterr { ($id: expr) => ({ println!("    reterr {}", $id); return new_empty_error($id); }) }

    enum State {
        Nothing,
        SomeValue(f64),         // TODO NOW: split it into SomeValueInIntRange(u64) and SomeValueOutIntRange(f64) to fix test case 27
        IntPrefix(u32), // base
        AfterDot(f64, i32),  // current value, bits after dot
        DirectAfterE(f64),   // direct after e means expect char09 or + or -
        AfterE(f64, i32),    // exp may be i32
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
            (State::Nothing, '0', EOFCHAR, _) 
                => retok!(0, NumLitValue::I32(0)),

            (State::Nothing, '0', 'i', _)
            | (State::Nothing, '0', 'u', _) => {
                state = conv!(0, State::ExpectIntPostfix(0));
            }
            (State::Nothing, '0', 'f', _) => {
                state = conv!(1, State::ExpectFloatPostfix(0f64));
            }

            (State::Nothing, '0', 'b', EOFCHAR)
            | (State::Nothing, '0', 'o', EOFCHAR)
            | (State::Nothing, '0', 'd', EOFCHAR)
            | (State::Nothing, '0', 'x', EOFCHAR) 
                => reterr!(18),           

            (State::Nothing, '0', 'b', _) => {
                chars.dummy1();
                state = conv!(2, State::IntPrefix(2));
            }
            (State::Nothing, '0', 'o', _) => {
                chars.dummy1();
                state = conv!(3, State::IntPrefix(8));
            }
            (State::Nothing, '0', 'd', _) => {
                chars.dummy1();
                state = conv!(4, State::IntPrefix(10));
            }
            (State::Nothing, '0', 'x', _) => {
                chars.dummy1();
                state = conv!(5, State::IntPrefix(16));
            }

            (State::Nothing, '0', _, _) => reterr!(0),
            (State::Nothing, ch, _, _) => match ch.to_digit(10) {
                Some(digit) => {
                    state = conv!(6, State::SomeValue(digit as f64));
                }
                None => reterr!(1),
            },

            (State::SomeValue(value), EOFCHAR, _, _) => {
                retok!(1, f64_final_value(value, false));
            }
            (State::SomeValue(value), 'i', _, _)
            | (State::SomeValue(value), 'u', _, _) => if value > u64::max_value() as f64 {
                reterr!(28);
            } else {
                chars.skip1();
                state = conv!(format!("7, value: {}, value as u64: {}", value, value as u64), State::ExpectIntPostfix(value as u64));
            },
            (State::SomeValue(value), 'f', _, _) => {
                chars.skip1();
                state = conv!(8, State::ExpectFloatPostfix(value));
            }
            (State::SomeValue(value), 'e', _, _) 
            | (State::SomeValue(value), 'E', _, _) => {
                chars.skip1();
                state = conv!(24, State::DirectAfterE(value));
            }
            (State::SomeValue(value), '.', _, _) => {
                state = conv!(21, State::AfterDot(value, 1));
            }
            (State::SomeValue(value), ch, _, _) => match ch.to_digit(10) {
                Some(digit) => match f64_checked_mul_add(value, 10, digit) {
                    Some(value) => {
                        state = conv!(9, State::SomeValue(value));
                    }
                    None => reterr!(2),
                },
                None => reterr!(4),
            },

            (State::IntPrefix(_), 'e', _, _) 
            | (State::IntPrefix(_), 'E', _, _) => reterr!(20),
            (State::IntPrefix(_), '.', _, _) => reterr!(21),
            (State::IntPrefix(base), ch, _, _) => match ch.to_digit(base) {
                Some(digit) => {
                    state = conv!(10, State::ExpectInt(base, digit as u64));
                }
                None => reterr!(19),
            },

            (State::ExpectInt(_, _), 'e', _, _) 
            | (State::ExpectInt(_, _), 'E', _, _) => reterr!(22),
            (State::ExpectInt(_, _), 'f', _, _) => reterr!(27),
            (State::ExpectInt(_, _), '.', _, _) => reterr!(23),
            (State::ExpectInt(_, value), EOFCHAR, _, _) => {
                retok!(2, u64_final_value(value, false));
            }
            (State::ExpectInt(_, value), 'i', _, _)
            | (State::ExpectInt(_, value), 'u', _, _) => {
                chars.skip1();
                state = conv!(11, State::ExpectIntPostfix(value));
            }
            (State::ExpectInt(base, value), ch, _, _) => match ch.to_digit(base) {
                Some(digit) => match value.checked_mul(base as u64) {
                    Some(value) => match value.checked_add(digit as u64) {
                        Some(value) => {
                            state = conv!(12, State::ExpectInt(base, value));
                        }
                        None => reterr!(25),
                    },
                    None => reterr!(26),                        
                },
                None => reterr!(24),
            },

            (State::AfterDot(_, _), '.', _, _) => reterr!(29),
            (State::AfterDot(_, _), 'i', _, _)
            | (State::AfterDot(_, _), 'u', _, _) => reterr!(32),  
            (State::AfterDot(value, _), 'f', _, _) => {
                chars.skip1();
                state = conv!(23, State::ExpectFloatPostfix(value));
            }
            (State::AfterDot(value, _), 'e', _, _)
            | (State::AfterDot(value, _), 'E', _, _) => {
                state = conv!(29, State::DirectAfterE(value));
            }
            (State::AfterDot(_, 1), EOFCHAR, _, _) => reterr!(3),
            (State::AfterDot(value, _), EOFCHAR, _, _) => retok!(6, NumLitValue::F64(value)),
            (State::AfterDot(value, bits), ch, _, _) => match ch.to_digit(10) {
                Some(digit) => match f64_checked_add(value, digit as f64 / 10f64.powi(bits)) {
                    Some(value) => {
                        state = conv!(22, State::AfterDot(value, bits + 1));
                    }
                    None => reterr!(30),
                },
                None => reterr!(31),                             
            },

            (State::DirectAfterE(_), '+', EOFCHAR, _)
            | (State::DirectAfterE(_), '-', EOFCHAR, _) => reterr!(33), 
            (State::DirectAfterE(value), '+', ch, _) => match ch.to_digit(10) {
                Some(digit) => {
                    chars.dummy1();
                    state = conv!(25, State::AfterE(value, digit as i32));
                }
                None => reterr!(34),
            },
            (State::DirectAfterE(value), '-', ch, _) => match ch.to_digit(10) {
                Some(digit) => {
                    chars.dummy1();
                    state = conv!(26, State::AfterE(value, -(digit as i32)));
                }
                None => reterr!(35),
            },
            (State::DirectAfterE(value), ch, _, _) => match ch.to_digit(10) {
                Some(digit) => {
                    state = conv!(27, State::AfterE(value, digit as i32));
                }
                None => reterr!(36),                        
            },

            (State::AfterE(_, _), 'u', _, _)
            | (State::AfterE(_, _), 'i', _, _) => reterr!(37),
            (State::AfterE(value, _), 'f', _, _) => {
                chars.skip1();
                state = conv!(30, State::ExpectFloatPostfix(value));                                  // LAST CONV
            }
            (State::AfterE(value, exp), EOFCHAR, _, _) => match f64_checked_mul(value, 10f64.powi(exp)) {
                Some(value) => retok!(7, NumLitValue::F64(value)),                                    // LAST RETOK
                None => reterr!(38), 
            },
            (State::AfterE(value, exp), ch, _, _) => match ch.to_digit(10) {
                None => reterr!(39),                                                                  // LAST RETERR
                Some(digit) => match exp.checked_mul(10) {
                    None => reterr!(40),
                    Some(exp) => match exp.checked_add(digit as i32) {
                        None => reterr!(41),
                        Some(exp) => { 
                            state = conv!(28, State::AfterE(value, exp));
                        }
                    }
                }
            },

            (State::ExpectIntPostfix(value), 'i', '8', EOFCHAR) => 
                if value > i8::max_value() as u64 { 
                    reterr!(5);
                } else {
                    retok!(3, NumLitValue::I8(value as i8));
                },
            (State::ExpectIntPostfix(value), 'u', '8', EOFCHAR) => 
                if value > u8::max_value() as u64 { 
                    reterr!(6) 
                } else {
                    retok!(4, NumLitValue::U8(value as u8));
                },
            (State::ExpectIntPostfix(value), 'i', '1', '6') => 
                if value > i16::max_value() as u64 { 
                    reterr!(7);
                } else {
                    chars.dummy1();
                    chars.dummy1();
                    state = conv!(13, State::ExpectEOF(NumLitValue::I16(value as i16))); 
                },
            (State::ExpectIntPostfix(value), 'u', '1', '6') => 
                if value > u16::max_value() as u64 { 
                    reterr!(8);
                } else {
                    chars.dummy1();
                    chars.dummy1();
                    state = conv!(14, State::ExpectEOF(NumLitValue::U16(value as u16))); 
                },
            (State::ExpectIntPostfix(value), 'i', '3', '2') => 
                if value > i32::max_value() as u64 { 
                    reterr!(9);
                } else {
                    chars.dummy1();
                    chars.dummy1();
                    state = conv!(15, State::ExpectEOF(NumLitValue::I32(value as i32))); 
                },
            (State::ExpectIntPostfix(value), 'u', '3', '2') => 
                if value > u32::max_value() as u64 { 
                    reterr!(10);
                } else { 
                    chars.dummy1();
                    chars.dummy1();
                    state = conv!(16, State::ExpectEOF(NumLitValue::U32(value as u32))); 
                },
            (State::ExpectIntPostfix(value), 'i', '6', '4') => 
                if value > i64::max_value() as u64 { 
                    reterr!(11);
                } else {
                    chars.dummy1();
                    chars.dummy1();
                    state = conv!(17, State::ExpectEOF(NumLitValue::I64(value as i64))); 
                },
            (State::ExpectIntPostfix(value), 'u', '6', '4') => 
                if value > u64::max_value() { 
                    reterr!(12);
                } else {
                    chars.dummy1();
                    chars.dummy1();
                    state = conv!(18, State::ExpectEOF(NumLitValue::U64(value))); 
                },
            (State::ExpectIntPostfix(_), _, _, _) => reterr!(13),

            (State::ExpectFloatPostfix(value), 'f', '3', '2') => 
                if value > f32::MAX as f64 { 
                    reterr!(14);
                } else { 
                    chars.dummy1();
                    chars.dummy1();
                    state = conv!(19, State::ExpectEOF(NumLitValue::F32(value as f32))); 
                },
            (State::ExpectFloatPostfix(value), 'f', '6', '4') => 
                if value > f64::MAX { 
                    reterr!(15);
                } else { 
                    chars.dummy1();
                    chars.dummy1();
                    state = conv!(20, State::ExpectEOF(NumLitValue::F64(value)));
                },
            (State::ExpectFloatPostfix(_), _, _, _) => reterr!(16),

            (State::ExpectEOF(ret_val), EOFCHAR, _, _) => retok!(5, ret_val),
            (State::ExpectEOF(_), _, _, _) => reterr!(17),
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
    test_case!("123", NumLitValue::I32(123));                   // 0
    test_case!("1", NumLitValue::I32(1));                       // 1
    test_case!("123456789", NumLitValue::I32(123456789));       // 2
    test_case!("2147483647", NumLitValue::I32(2147483647));     // 3
    // should not start with 0
    test_case!("0123", err, new_empty_error(0));                // 4

    // 0s
    test_case!("0", NumLitValue::I32(0));                       // 5
    test_case!("0u32", NumLitValue::U32(0));                    // 6
    test_case!("0f32", NumLitValue::F32(0f32));                 // 7
    test_case!("0x0", NumLitValue::I32(0));                     // 8
    test_case!("0o0u8", NumLitValue::U8(0));                    // 9
    test_case!("0u", err, new_empty_error(13));                 // 10
    test_case!("0f", err, new_empty_error(16));                 // 11
    test_case!("0i888", err, new_empty_error(13));              // 12
    test_case!("0f3210", err, new_empty_error(17));             // 13
    // 1s
    test_case!("1", NumLitValue::I32(1));                       // 14
    test_case!("1u32", NumLitValue::U32(1));                    // 15
    test_case!("1f32", NumLitValue::F32(1f32));                 // 16
    test_case!("0x1", NumLitValue::I32(1));                     // 17
    test_case!("0o1u8", NumLitValue::U8(1));                    // 18
    
    // normal f64
    test_case!("1.0", NumLitValue::F64(1.0));                   // 19
    test_case!("1.234", NumLitValue::F64(1.234));               // 20
    test_case!("12345678901234567890.0", NumLitValue::F64(12345678901234567890.0f64));  // 21
    // test_case!("1.79E308", NumLitValue::F64(1.79E308));      // 22, too difficult to make it pass
    // test_case!("1.79E-308", NumLitValue::F64(1.79E-308));    // 23, too difficult too
    // smallflow
    test_case!("1.79E-2333", err, new_empty_error(38));         // 24

    // postfix for other integral
    test_case!("1u8", NumLitValue::U8(1));                      // 25
    test_case!("234i16", NumLitValue::I16(234));                // 26
    test_case!("18446744073709551615u64", NumLitValue::U64(18446744073709551615));      // 27
    test_case!("100i8", NumLitValue::I8(100));                  // 28
    test_case!("61234u16", NumLitValue::U16(61234));            // 29
    test_case!("9223372036854775807i64", NumLitValue::I64(9223372036854775807));        // 30
    // overflow and downflow is error
    test_case!("256u8", err, new_empty_error(0));               // 31, should be overflow and explain large than u8::max
    test_case!("100000i16", err, new_empty_error(0));           // 32, should be overflow and explain large than i16::max
    test_case!("-800i8", err, new_empty_error(0));              // 33, should be downflow and explain smaller than i8

    // negative value
    test_case!("-123", NumLitValue::I32(-123));                 // 34
    test_case!("-123.456", NumLitValue::F64(-123.456));         // 35
    test_case!("-30000i16", NumLitValue::I16(-30000));          // 36
    // negative not applicapable for unsigned

    // integral prefix
    test_case!("0xABCD", NumLitValue::I32(0xABCD));             // 37
    test_case!("0xfedc", NumLitValue::I32(0xFEDC));             // 38
    test_case!("0b101010", NumLitValue::I32(0b101010));         // 39
    test_case!("-0o777", NumLitValue::I32(-0o777));             // 40
    // invalid char
    test_case!("0xXXXX", err, new_empty_error(0));              // 41, should be invalid char at xx:xx
    test_case!("0b1234", err, new_empty_error(0));              // 42, should be invalid char at xx:xx
    test_case!("0daaaa", err, new_empty_error(0));              // 43, should be invalid char at xx:xx

    // floating point no prefix 
    test_case!("0x123.0", err, new_empty_error(0));             // 44, should be floating point no prefix
    test_case!("0b111.01", err, new_empty_error(0));            // 45, should be floating point no prefix

    // auto expansion for no postfix
    test_case!("2147483645", NumLitValue::I32(2147483645));                         // 46
    test_case!("2147483648", NumLitValue::U32(2147483648));                         // 47, 2^31 - 1..2^32 expand to u32
    test_case!("4333333333", NumLitValue::I64(4333333333));                         // 48, 2^32..2^63 expand to i64
    test_case!("9223372036854775807", NumLitValue::I64(9223372036854775807));       // 49, 2^32..2^63 expand to i64
    test_case!("9223372036854775808", NumLitValue::U64(9223372036854775808));       // 50, 2^63..2^64 expand to u64
    test_case!("18446744073709551615", NumLitValue::U64(18446744073709551615));     // 51, 2^63..2^64 expand to u64
    test_case!("18446744073709551616", NumLitValue::F64(18446744073709551616f64));  // 52, 2^64.. expand to f64
    // auto expansion for negative value
    test_case!("-2147483648", NumLitValue::I32(-2147483648));                       // 53
    test_case!("-2147483649", NumLitValue::I64(-2147483649));                       // 54
    test_case!("-9223372036854775809", NumLitValue::F64(-9223372036854775809f64));  // 55

    // floats' e
    test_case!("123e10", NumLitValue::F64(123E10));                 // 56
    test_case!("123E10", NumLitValue::F64(123E10));                 // 57
    test_case!("123E+10", NumLitValue::F64(123E10));                // 58
    test_case!("123E-10", NumLitValue::F64(123E-10));               // 59
    test_case!("0.456E5", NumLitValue::F64(0.456E5));               // 60
    test_case!("0.0001E-200", NumLitValue::F64(0.0001E-200));       // 61
    test_case!("-0.0001E-200", NumLitValue::F64(-0.0001E-200));     // 62
    test_case!("123E5f32", NumLitValue::F32(123E5f32));             // 63
    // e not with prefix or postfix, exp should be integer
    test_case!("0x123E-5", err, new_empty_error(0));                // 64, should be floating point should not use prefix
    test_case!("123.456E789.0", err, new_empty_error(0));           // 65, should be floating point scientific representation's exponential part should be integer

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