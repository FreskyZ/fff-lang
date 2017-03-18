#![deny(overflowing_literals)]
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

use super::error_strings;
use super::super::NumLitValue;

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

trait NumLitParserValue {
    fn merge_sign(self, sign: bool) -> Self;
}
impl NumLitParserValue for i32 {
    fn merge_sign(self, sign: bool) -> i32 { if sign { self } else { -self } }
}
impl NumLitParserValue for i64 {
    fn merge_sign(self, sign: bool) -> i64 { if sign { self } else { -self } }
}

trait FloatCheckedAlgorithm : Sized{

    fn checked_add<T: Into<f64>>(self, rhs: T) -> Option<Self>;
    fn checked_sub<T: Into<f64>>(self, rhs: T) -> Option<Self>;
    fn checked_mul<T: Into<f64>>(self, rhs: T) -> Option<Self>;
    fn checked_mul_add<T: Into<f64>, U: Into<f64>>(self, muler: T, adder: U) -> Option<Self>;
}
impl FloatCheckedAlgorithm for f64 {

    #[inline]
    fn checked_add<T: Into<f64>>(self, rhs: T) -> Option<f64> {
        use std::f64;

        let rhs = rhs.into();
        if rhs < 0f64 {
            if f64::MIN - rhs >= self { None } else { Some(self + rhs) }
        } else {
            if f64::MAX - rhs <= self { None } else { Some(self + rhs) }
        }
    }
    #[inline]
    fn checked_sub<T: Into<f64>>(self, rhs: T) -> Option<f64> {
        use std::f64;

        let rhs = rhs.into();
        if rhs < 0f64 {
            if f64::MAX + rhs >= self { Some(self - rhs) } else { None }
        } else {
            if f64::MIN + rhs <= self { Some(self - rhs) } else { None }
        }
    }
    #[inline]
    fn checked_mul<T: Into<f64>>(self, rhs: T) -> Option<f64> {
        use std::f64;

        let rhs = rhs.into();
        if self > 0f64 {
            if rhs < 1f64 {
                if f64::MIN_POSITIVE / rhs >= self { None } else { Some(self * rhs) }
            } else {
                if f64::MAX / rhs <= self { None } else { Some(self * rhs) }
            }
        } else {
            if rhs < 1f64 {
                if f64::MIN_POSITIVE / rhs >= -self { None } else { Some(self * rhs) }
            } else {
                if f64::MAX / rhs <= -self { None } else { Some(self * rhs) }
            }
        }
    }
    #[inline]
    fn checked_mul_add<T: Into<f64>, U: Into<f64>>(self, muler: T, adder: U) -> Option<f64> {
        use std::f64;

        let muler = muler.into();
        let adder = adder.into();
        match (muler.abs() < 1f64, adder < 0f64) {
            (true, true) => Some(self.mul_add(muler, adder)),
            (true, false) => if (f64::MAX - adder) <= self * muler { None } else { Some(self.mul_add(muler, adder)) },
            (false, true) => Some(self.mul_add(muler, adder)),
            (false, false) => if (f64::MAX - adder) / muler <= self { None } else { Some(self.mul_add(muler, adder)) },
        }
    }
}
fn u64_final_value(value: u64, is_positive: bool) -> NumLitValue {
    use std::{ i32, u32, i64, u64 };
    if value <= i32::MAX as u64 {
        return NumLitValue::I32(if is_positive { value as i32 } else { -(value as i32) });
    } else if value <= u32::MAX as u64 && is_positive {
        return NumLitValue::U32(value as u32);
    } else if value <= i64::MAX as u64 {
        return NumLitValue::I64(if is_positive { value as i64 } else { -(value as i64) });
    } else {
        return NumLitValue::U64(value);
    }
}

// Actual impl, huge state machine
fn str_to_num_lit_impl(raw: String, strpos: StringPosition) -> Result<NumLitValue, Message> {
    use std::{ i8, u8, i16, u16, i32, u32, i64, u64, f32, f64 };

    if cfg!(feature = "trace_num_lit_parse") {
        println!("\nnum lit parsing: \"{}\" at {:?}", raw, strpos);
    }

    let new_empty_error = |_: i32| { 
        Err(Message::new_by_str(error_strings::InvalidNumericLiteral, vec![(strpos, "")])) 
    };
    
    enum State {
        ReallyNothing,
        Nothing(bool),                      // is positive
        UnknownI32(i32, bool, bool),        // value (already with sign), is positive, prev is underscore
        UnknownU32(u32, bool),              // value, prev is underscore
        UnknownI64(i64, bool, bool),        // value (already with sign), is positive, prev is underscore
        UnknownU64(u64, bool),              // value, prev is underscore
        UnknownF64(f64, bool),              // value, prev is underscore
        IntPrefix(u32, bool, bool),         // base, is positive, already a underscore
        AfterDot(f64, i32, bool, bool),     // value (already with sign), bits after dot, is positive, prev is underscore
        DirectAfterE(f64, bool),            // value, already a underscore, direct after e means expect char09 or + or -
        AfterE(f64, i32, bool),             // value, exp may be i32, prev is underscore
        ExpectInt(u32, u64, bool, bool),    // base (not with sign), current value, is positive, prev is underscore
        ExpectSignedIntPostfix(i64),
        ExpectUnsignedIntPostfix(u64),
        ExpectFloatPostfix(f64),
        ExpectEOF(NumLitValue),             // retval
    }

    let mut state = State::ReallyNothing;
    let mut chars = BufChars::new(raw.chars());
    
    #[cfg(feature = "trace_num_lit_parse")] macro_rules! conv { ($id: expr, $new_state: expr) => ({ println!("    conv {}", $id); state = $new_state; }) }
    #[cfg(feature = "trace_num_lit_parse")] macro_rules! retok { ($id: expr, $ret_val: expr) => ({ println!("    retok {}", $id); return Ok($ret_val); }) }
    #[cfg(feature = "trace_num_lit_parse")] macro_rules! reterr { ($id: expr) => ({ println!("    reterr {}", $id); return new_empty_error($id); }) }
    #[cfg(not(feature = "trace_num_lit_parse"))] macro_rules! conv { ($id: expr, $new_state: expr) => ({ state = $new_state; }) }
    #[cfg(not(feature = "trace_num_lit_parse"))] macro_rules! retok { ($id: expr, $ret_val: expr) => ({ return Ok($ret_val); }) }
    #[cfg(not(feature = "trace_num_lit_parse"))] macro_rules! reterr { ($id: expr) => ({ return new_empty_error($id); }) }

    loop {
        chars.move_next();
        match chars.current_with_state(state) {

            // ---- ReallyNothing ----
            (State::ReallyNothing, '-', EOFCHAR, _) => reterr!(0),
            (State::ReallyNothing, '_', _, _) => reterr!(53),
            (State::ReallyNothing, '-', _, _) => conv!(0, State::Nothing(false)),
            (State::ReallyNothing, _, _, _) => {
                chars.skip1();
                conv!(1, State::Nothing(true)); 
            }

            // ---- Nothing(is_positive) ----
            (State::Nothing(_), '0', EOFCHAR, _) => retok!(0, NumLitValue::I32(0)),
            (State::Nothing(_), '_', _, _) => reterr!(54),

            (State::Nothing(_), '0', 'i', _) => conv!(2, State::ExpectSignedIntPostfix(0i64)),
            (State::Nothing(_), '0', 'u', _) => conv!(3, State::ExpectUnsignedIntPostfix(0u64)),
            (State::Nothing(_), '0', 'f', _) => conv!(4, State::ExpectFloatPostfix(0f64)),

            (State::Nothing(_), '0', 'b', EOFCHAR)
            | (State::Nothing(_), '0', 'o', EOFCHAR)
            | (State::Nothing(_), '0', 'd', EOFCHAR)
            | (State::Nothing(_), '0', 'x', EOFCHAR) => reterr!(1),           

            (State::Nothing(is_positive), '0', 'b', _) => {
                chars.dummy1();
                conv!(5, State::IntPrefix(2, is_positive, false));
            }
            (State::Nothing(is_positive), '0', 'o', _) => {
                chars.dummy1();
                conv!(6, State::IntPrefix(8, is_positive, false));
            }
            (State::Nothing(is_positive), '0', 'd', _) => {
                chars.dummy1();
                conv!(7, State::IntPrefix(10, is_positive, false));
            }
            (State::Nothing(is_positive), '0', 'x', _) => {
                chars.dummy1();
                conv!(8, State::IntPrefix(16, is_positive, false));
            }
            
            (State::Nothing(_), '0', '.', '_') => reterr!(67),
            (State::Nothing(is_positive), '0', '.', _) => {
                chars.dummy1();
                conv!(77, State::AfterDot(0f64, 1, is_positive, false));                       
            }
            (State::Nothing(_), '0', _, _) => reterr!(2),
            (State::Nothing(is_positive), ch, _, _) => match ch.to_digit(10) {
                Some(digit) => conv!(9, State::UnknownI32((digit as i32).merge_sign(is_positive), is_positive, false)),
                None => reterr!(3),
            },

            // ---- UnknownI32(value, is_positive, prev_is_underscore) ----
            (State::UnknownI32(_, _, true), EOFCHAR, _, _) => reterr!(68),
            (State::UnknownI32(value, _, false), EOFCHAR, _, _) => retok!(1, NumLitValue::I32(value)),
            (State::UnknownI32(value, _, _), 'i', _, _) => {
                chars.skip1();
                conv!(10, State::ExpectSignedIntPostfix(value as i64));
            },
            (State::UnknownI32(value, true, _), 'u', _, _) => {
                chars.skip1();
                conv!(11, State::ExpectUnsignedIntPostfix(value as u64));
            },
            (State::UnknownI32(_, false, _), 'u', _, _) => reterr!(4),
            (State::UnknownI32(value, _, _), 'f', _, _) => {
                chars.skip1();
                conv!(12, State::ExpectFloatPostfix(value as f64));
            }
            (State::UnknownI32(value, _, _), 'e', _, _) 
            | (State::UnknownI32(value, _, _), 'E', _, _) => conv!(13, State::DirectAfterE(value as f64, false)),
            (State::UnknownI32(_, _, true), '.', _, _) => reterr!(57),
            (State::UnknownI32(_, _, false), '.', '_', _) => reterr!(69),
            (State::UnknownI32(value, is_positive, false), '.', _, _) =>
                conv!(14, State::AfterDot(value as f64, 1, is_positive, false)),
            (State::UnknownI32(value, is_positive, prev_is_underscore), ch, _, _) => match (prev_is_underscore, ch == '_') {
                (true, true) => reterr!(58),
                (_, true) => conv!(80, State::UnknownI32(value, is_positive, true)),
                (_, false) => match ch.to_digit(10) {
                    None => reterr!(5),
                    Some(digit) => match (value.checked_mul(10), is_positive) {
                        (None, true) => { // positive i32 mul 10 overflow, that is (value >= 214748365 and value <= 21487483647)
                            let i64_value = value as i64 * 10i64 + digit as i64 ;   // these values won't overflow i64 anyway
                            if i64_value <= u32::MAX as i64 {                       // may not overflow u32
                                conv!(67, State::UnknownU32(i64_value as u32, false));
                            } else {
                                conv!(15, State::UnknownI64(i64_value, true, false));
                            }
                        }
                        (None, false) => { // negative i32 mul 10 overflow, that is (value <= -214748365 and value >= -2147483648)
                            let i64_value = value as i64 * 10i64 - digit as i64;    // these values won't overflow i64 anyway
                            conv!(71, State::UnknownI64(i64_value, false, false));
                        }
                        (Some(value), true) => match value.checked_add(digit as i32) {
                            None => conv!(17, State::UnknownU32(value as u32 + digit, false)),
                            Some(value) => conv!(18, State::UnknownI32(value, is_positive, false)),
                        },
                        (Some(value), false) => match value.checked_sub(digit as i32) {
                            None => conv!(16, State::UnknownI64(value as i64 - digit as i64, false, false)),
                            Some(value) => conv!(70, State::UnknownI32(value, is_positive, false)),
                        },
                    },
                },
            },

            // ---- UnknownU32(value, prev_is_underscore) ----
            (State::UnknownU32(_, true), EOFCHAR, _, _) => reterr!(81),
            (State::UnknownU32(value, false), EOFCHAR, _, _) => retok!(2, NumLitValue::U32(value)),
            (State::UnknownU32(value, _), 'i', _, _) => {
                chars.skip1();
                conv!(19, State::ExpectSignedIntPostfix(value as i64));
            },
            (State::UnknownU32(value, _), 'u', _, _) => {
                chars.skip1();
                conv!(20, State::ExpectUnsignedIntPostfix(value as u64));
            },
            (State::UnknownU32(value, _), 'f', _, _) => {
                chars.skip1();
                conv!(21, State::ExpectFloatPostfix(value as f64));
            }
            (State::UnknownU32(value, _), 'e', _, _) 
            | (State::UnknownU32(value, _), 'E', _, _) => conv!(22, State::DirectAfterE(value as f64, false)),
            (State::UnknownU32(_, _), '.', '_', _) => reterr!(70),
            (State::UnknownU32(_, true), '.', _, _) => reterr!(82),
            (State::UnknownU32(value, false), '.', _, _) => conv!(23, State::AfterDot(value as f64, 1, true, false)),
            (State::UnknownU32(value, prev_is_underscore), ch, _, _) => match (prev_is_underscore, ch == '_') {
                (true, true) => reterr!(83),                                                                            // LAST RETERR
                (_, true) => conv!(87, State::UnknownU32(value, true)),                                                 // LAST CONV
                (_, false) => match ch.to_digit(10) {
                    None => reterr!(6),
                    Some(digit) => match value.checked_mul(10) { 
                        None => conv!(24, State::UnknownI64(value as i64 * 10i64 + digit as i64, true, false)), // u32 mul 10 overflow must be positive i64
                        Some(value) => match value.checked_add(digit) {
                            None => conv!(25, State::UnknownI64(value as i64 + digit as i64, true, false)),     // u32 add digit overflow must be positive i64
                            Some(value) => conv!(26, State::UnknownU32(value, false)),                          // not overflow continue u32
                        },
                    },
                },
            },

            // ---- UnknownI64(value, is_positive, prev is underscore) ----
            (State::UnknownI64(_, _, true), EOFCHAR, _, _) => reterr!(61),
            (State::UnknownI64(value, _, false), EOFCHAR, _, _) => retok!(3, NumLitValue::I64(value)),
            (State::UnknownI64(value, _, _), 'i', _, _) => {
                chars.skip1();
                conv!(27, State::ExpectSignedIntPostfix(value));
            },
            (State::UnknownI64(value, true, _), 'u', _, _) => {
                chars.skip1();
                conv!(28, State::ExpectUnsignedIntPostfix(value as u64));
            },
            (State::UnknownI64(_, false, _), 'u', _, _) => reterr!(7),
            (State::UnknownI64(value, _, _), 'f', _, _) => {
                chars.skip1();
                conv!(29, State::ExpectFloatPostfix(value as f64));
            }
            (State::UnknownI64(value, _, _), 'e', _, _) 
            | (State::UnknownI64(value, _, _), 'E', _, _) => conv!(30, State::DirectAfterE(value as f64, false)),
            (State::UnknownI64(_, _, true), '.', _, _) => reterr!(62),
            (State::UnknownI64(_, _, false), '.', '_', _) => reterr!(71),
            (State::UnknownI64(value, is_positive, false), '.', _, _) => conv!(31, State::AfterDot(value as f64, 1, is_positive, false)),
            (State::UnknownI64(value, is_positive, prev_is_underscore), ch, _, _) => match (prev_is_underscore, ch == '_') {
                (true, true) => reterr!(63),
                (_, true) => conv!(82, State::UnknownI64(value, is_positive, true)),
                (_, false) => match ch.to_digit(10) {
                    None => reterr!(60), 
                    Some(digit) => match (value.checked_mul(10i64), is_positive) {
                        (None, true) => {       // positive i64 mul 10 overflow, that is (value <= 9223372036854775808 and value >= 922337203685477581)
                            match (value as u64).checked_mul(10u64) {
                                None => {       // positive i64 as u64 mul 10 overflow, that is (value <= 9223372036854775808 and value >= 1844674407370955162)
                                    conv!(32, State::UnknownF64(value as f64 * 10f64 + digit as f64, is_positive));
                                }
                                Some(value) => match value.checked_add(digit as u64) {
                                    None => {   // positive i64 as u64 mul 10 not overflow add digit overflow, that is (value = 1844674407370955161 and digit >= 6)
                                        conv!(68, State::UnknownF64(value as f64 + digit as f64, is_positive));
                                    }
                                    Some(value) => conv!(69, State::UnknownU64(value, false)),
                                },
                            }
                        }
                        (None, false) => {      // negative i64 mul 10 overflow, that is (value >= -9223372036854775808 and value <= -922337203685477581)
                            match (value as u64).checked_mul(10u64) {
                                None => {       // negative i64 as u64 mul 10 overflow, that is (value >= -9223372036854775808 and value <= -1844674407370955162)
                                    conv!(73, State::UnknownF64(value as f64 * 10f64 - digit as f64, is_positive));
                                }
                                Some(value) => match value.checked_sub(digit as u64) {
                                    None => {   // negative i64 as u64 mul 10 not overflow add digit overflow, that is (value = -1844674407370955161 and digit >= 7)
                                        conv!(74, State::UnknownF64(value as f64 - digit as f64, is_positive));
                                    }
                                    Some(value) => conv!(76, State::UnknownU64(value, false)), 
                                },
                            }
                        }
                        (Some(value), true) => match value.checked_add(digit as i64) {
                            None => conv!(34, State::UnknownU64(value as u64 + digit as u64, false)),
                            Some(value) => conv!(35, State::UnknownI64(value, true, false)), 
                        },
                        (Some(value), false) => match value.checked_sub(digit as i64) {
                            None => conv!(33, State::UnknownF64(value as f64 - digit as f64, false)),
                            Some(value) => conv!(75, State::UnknownI64(value, false, false)),
                        },
                    },
                },
            },

            // ---- UnknownU64(value, prev_is_underscore) ----
            (State::UnknownU64(_, true), EOFCHAR, _, _) => reterr!(64),
            (State::UnknownU64(value, false), EOFCHAR, _, _) => retok!(4, NumLitValue::U64(value)),
            (State::UnknownU64(_, _), 'i', _, _) => reterr!(9), // UnknownU64 must large then i64::MAX
            (State::UnknownU64(value, _), 'u', _, _) => {
                chars.skip1();
                conv!(36, State::ExpectUnsignedIntPostfix(value));
            },
            (State::UnknownU64(value, _), 'f', _, _) => {
                chars.skip1();
                conv!(37, State::ExpectFloatPostfix(value as f64));
            }
            (State::UnknownU64(value, _), 'e', _, _) 
            | (State::UnknownU64(value, _), 'E', _, _) => conv!(38, State::DirectAfterE(value as f64, false)),
            (State::UnknownU64(_, true), '.', _, _) => reterr!(65),
            (State::UnknownU64(_, false), '.', '_', _) => reterr!(72),
            (State::UnknownU64(value, false), '.', _, _) => conv!(39, State::AfterDot(value as f64, 1, true, false)),
            (State::UnknownU64(value, prev_is_underscore), ch, _, _) => match (prev_is_underscore, ch == '_') {
                (true, true) => reterr!(66),
                (_, true) => conv!(83, State::UnknownU64(value, true)),
                (_, false) => match ch.to_digit(10) {
                    None => reterr!(10),
                    Some(digit) => match value.checked_mul(10) { 
                        None => conv!(40, State::UnknownF64(value as f64 * 10f64 + digit as f64, true)),    // u64 mul 10 overflow must be positive f64
                        Some(value) => match value.checked_add(digit as u64) {
                            None => conv!(41, State::UnknownF64(value as f64 + digit as f64, true)),        // u64 add digit overflow must be positive f64
                            Some(value) => conv!(42, State::UnknownU64(value, false)),                      // not overflow continue u64
                        },
                    },
                },
            },

            // ---- UnknownF64(value, is_positive) ----
            (State::UnknownF64(value, _), EOFCHAR, _, _) => 
                retok!(5, NumLitValue::F64(value)),
            (State::UnknownF64(_, _), 'i', _, _) => reterr!(11),
            (State::UnknownF64(_, _), 'u', _, _) => reterr!(12),
            (State::UnknownF64(value, _), 'f', _, _) => {
                chars.skip1();
                conv!(43, State::ExpectFloatPostfix(value));
            }
            (State::UnknownF64(value, _), 'e', _, _) 
            | (State::UnknownF64(value, _), 'E', _, _) => conv!(44, State::DirectAfterE(value, false)),
            (State::UnknownF64(_, _), '.', '_', _) => reterr!(73),
            (State::UnknownF64(value, is_positive), '.', _, _) => conv!(45, State::AfterDot(value, 1, is_positive, false)),
            (State::UnknownF64(value, is_positive), ch, _, _) => match ch.to_digit(10) {
                Some(digit) => match value.checked_mul_add(10, if is_positive { digit as i32 } else { -(digit as i32) }) {
                    Some(value) => conv!(46, State::UnknownF64(value, is_positive)),
                    None => reterr!(13),
                },
                None => reterr!(14),
            },

            // ---- IntPrefix(base, is_postive, prev_is_underscore) ----
            (State::IntPrefix(_, _, _), 'e', _, _) 
            | (State::IntPrefix(_, _, _), 'E', _, _) => reterr!(15),
            (State::IntPrefix(_, _, _), '.', _, _) => reterr!(16),
            (State::IntPrefix(_, _, true), '_', _, _) => reterr!(80),
            (State::IntPrefix(base, is_positive, false), '_', _, _) => conv!(86, State::IntPrefix(base, is_positive, true)),
            (State::IntPrefix(base, is_positive, _), ch, _, _) => match ch.to_digit(base) {
                Some(digit) => conv!(47, State::ExpectInt(base, digit as u64, is_positive, false)),
                None => reterr!(17),
            },

            // ---- ExpectInt(base, value, is_positive) ----
            (State::ExpectInt(_, _, _, _), '.', _, _) => reterr!(20),
            (State::ExpectInt(_, value, is_positive, _), EOFCHAR, _, _) => retok!(6, u64_final_value(value, is_positive)),
            (State::ExpectInt(_, value, is_positive, _), 'i', _, _) => if value > i64::MAX as u64 {
                reterr!(21);
            } else {
                chars.skip1();
                conv!(48, State::ExpectSignedIntPostfix(if is_positive { value as i64 } else { -(value as i64) }));
            },
            (State::ExpectInt(_, value, true, _), 'u', _, _) => {
                chars.skip1();
                conv!(49, State::ExpectUnsignedIntPostfix(value));
            }
            (State::ExpectInt(_, _, false, _), 'u', _, _) => {
                reterr!(22); 
            }
            (State::ExpectInt(base, value, is_positive, prev_is_underscore), ch, _, _) => match (prev_is_underscore, ch == '_') {
                (true, true) => reterr!(55),
                (_, true) => conv!(79, State::ExpectInt(base, value, is_positive, true)),
                (_, false) => match ch.to_digit(base) {
                    None => reterr!(25),
                    Some(digit) => match value.checked_mul(base as u64) {
                        None => reterr!(24), 
                        Some(value) => match value.checked_add(digit as u64) {
                            None => reterr!(23),
                            Some(value) => {
                                if !is_positive && value >= 9223372036854775808u64 {
                                    reterr!(56); 
                                } else {
                                    conv!(50, State::ExpectInt(base, value, is_positive, false));
                                }
                            }
                        },                       
                    },
                },
            },

            // ---- AfterDot(value, bits, is_positive, prev_is_underscore) ----
            (State::AfterDot(_, _, _, _), '.', _, _) => reterr!(26),
            (State::AfterDot(_, _, _, _), 'i', _, _) => reterr!(50),
            (State::AfterDot(_, _, _, _), 'u', _, _) => reterr!(27),  
            (State::AfterDot(value, _, _, _), 'f', _, _) => {
                chars.skip1();
                conv!(51, State::ExpectFloatPostfix(value));
            }
            (State::AfterDot(value, _, _, _), 'e', _, _)
            | (State::AfterDot(value, _, _, _), 'E', _, _) => conv!(52, State::DirectAfterE(value, false)),
            (State::AfterDot(_, 1, _, _), EOFCHAR, _, _) => reterr!(28),
            (State::AfterDot(_, _, _, true), EOFCHAR, _, _) => reterr!(74),
            (State::AfterDot(value, _, _, false), EOFCHAR, _, _) => retok!(9, NumLitValue::F64(value)),
            (State::AfterDot(value, bits, is_positive, prev_is_underscore), ch, _, _) => match (prev_is_underscore, ch == '_') {
                (true, true) => reterr!(75),
                (_, true) => conv!(84, State::AfterDot(value, bits, is_positive, true)),
                (_, false) => match (ch.to_digit(10), is_positive) {
                    (None, _) => reterr!(30),
                    (Some(digit), true) => match value.checked_add(digit as f64 / 10f64.powi(bits)) {
                        Some(value) => conv!(53, State::AfterDot(value, bits + 1, is_positive, false)),
                        None => reterr!(29),
                    },                 
                    (Some(digit), false) => match value.checked_sub(digit as f64 / 10f64.powi(bits)) {
                        Some(value) => conv!(72, State::AfterDot(value, bits + 1, is_positive, false)),
                        None => reterr!(51),
                    }  
                },
            },

            // ---- DirectAfterE(value) ----
            (State::DirectAfterE(_, _), '+', EOFCHAR, _)
            | (State::DirectAfterE(_, _), '-', EOFCHAR, _) => reterr!(31),  
            (State::DirectAfterE(_, _), '+', '_', _) => reterr!(76),
            (State::DirectAfterE(value, _), '+', ch, _) => match ch.to_digit(10) {
                Some(digit) => {
                    chars.dummy1();
                    conv!(54, State::AfterE(value, digit as i32, false));
                }
                None => reterr!(32),
            },
            (State::DirectAfterE(_, _), '-', '_', _) => reterr!(77),
            (State::DirectAfterE(value, _), '-', ch, _) => match ch.to_digit(10) {
                Some(digit) => {
                    chars.dummy1();
                    conv!(55, State::AfterE(value, -(digit as i32), false));
                }
                None => reterr!(33),
            },
            (State::DirectAfterE(value, false), '_', _, _) => conv!(81, State::DirectAfterE(value, true)),
            (State::DirectAfterE(_, true), '_', _, _) => reterr!(59),
            (State::DirectAfterE(value, _), ch, _, _) => match ch.to_digit(10) {
                Some(digit) => conv!(56, State::AfterE(value, digit as i32, false)),
                None => reterr!(34),                        
            },

            // ---- AfterE(value, exp, prev_is_underscore) ----
            (State::AfterE(_, _, _), 'u', _, _)
            | (State::AfterE(_, _, _), 'i', _, _) => reterr!(35),
            (State::AfterE(value, exp, _), 'f', _, _) => match value.checked_mul(10f64.powi(exp)) {
                None => reterr!(52),
                Some(value) => {
                    chars.skip1();
                    conv!(57, State::ExpectFloatPostfix(value));
                }
            },
            (State::AfterE(_, _, true), EOFCHAR, _, _) => reterr!(78),
            (State::AfterE(value, exp, false), EOFCHAR, _, _) => match value.checked_mul(10f64.powi(exp)) {
                Some(value) => retok!(10, NumLitValue::F64(value)),
                None => reterr!(36), 
            },
            (State::AfterE(value, exp, prev_is_underscore), ch, _, _) => match (prev_is_underscore, ch == '_') {
                (true, true) => reterr!(79),
                (_, true) => conv!(85, State::AfterE(value, exp, true)),
                (_, false) => match ch.to_digit(10) {
                    None => reterr!(37),               
                    Some(digit) => match exp.checked_mul(10) {
                        None => reterr!(38),
                        Some(exp) => match exp.checked_add(digit as i32) {
                            None => reterr!(39),
                            Some(exp) => conv!(58, State::AfterE(value, exp, false)),
                        }
                    }
                },
            },

            // ---- ExpectSignedIntPostfix(value) ----
            (State::ExpectSignedIntPostfix(value), 'i', '8', EOFCHAR) => 
                if value > i8::MAX as i64 || value < i8::MIN as i64 { 
                    reterr!(40);
                } else {
                    retok!(format!("11, value: {}", value), NumLitValue::I8(value as i8));
                },
            (State::ExpectSignedIntPostfix(value), 'i', '1', '6') => 
                if value > i16::MAX as i64 || value < i64::MIN as i64 { 
                    reterr!(41);
                } else {
                    chars.dummy1();
                    chars.dummy1();
                    conv!(59, State::ExpectEOF(NumLitValue::I16(value as i16))); 
                },
            (State::ExpectSignedIntPostfix(value), 'i', '3', '2') => 
                if value > i32::MAX as i64 || value < i32::MIN as i64 { 
                    reterr!(42);
                } else {
                    chars.dummy1();
                    chars.dummy1();
                    conv!(60, State::ExpectEOF(NumLitValue::I32(value as i32))); 
                },
            (State::ExpectSignedIntPostfix(value), 'i', '6', '4') => {
                chars.dummy1();
                chars.dummy1();
                conv!(61, State::ExpectEOF(NumLitValue::I64(value as i64)));
            }
            (State::ExpectSignedIntPostfix(_), _, _, _) => reterr!(43),

            // ---- ExpectUnsignedIntPostfix(value) ---- 
            (State::ExpectUnsignedIntPostfix(value), 'u', '8', EOFCHAR) => 
                if value > u8::max_value() as u64 { 
                    reterr!(44) 
                } else {
                    retok!(12, NumLitValue::U8(value as u8));
                },
            (State::ExpectUnsignedIntPostfix(value), 'u', '1', '6') => 
                if value > u16::max_value() as u64 { 
                    reterr!(45);
                } else {
                    chars.dummy1();
                    chars.dummy1();
                    conv!(62, State::ExpectEOF(NumLitValue::U16(value as u16))); 
                },
            (State::ExpectUnsignedIntPostfix(value), 'u', '3', '2') => 
                if value > u32::max_value() as u64 { 
                    reterr!(46);
                } else { 
                    chars.dummy1();
                    chars.dummy1();
                    conv!(63, State::ExpectEOF(NumLitValue::U32(value as u32))); 
                },
            (State::ExpectUnsignedIntPostfix(value), 'u', '6', '4') => {
                chars.dummy1();
                chars.dummy1();
                conv!(64, State::ExpectEOF(NumLitValue::U64(value))); 
            }
            (State::ExpectUnsignedIntPostfix(_), _, _, _) => reterr!(47),

            // ---- ExpectFloatPostfix(value) ----
            (State::ExpectFloatPostfix(value), 'f', '3', '2') => {
                if value > f32::MAX as f64 { 
                    reterr!(48);
                } else if value == 0f32 as f64 {
                    chars.dummy1();
                    chars.dummy1();
                    conv!(78, State::ExpectEOF(NumLitValue::F32(0f32)));
                } else if value < f32::MIN_POSITIVE as f64 {
                    reterr!(49);
                } else {
                    chars.dummy1();
                    chars.dummy1();
                    conv!(65, State::ExpectEOF(NumLitValue::F32(value as f32))); 
                }
            }
            (State::ExpectFloatPostfix(value), 'f', '6', '4') => {
                chars.dummy1();
                chars.dummy1();
                conv!(66, State::ExpectEOF(NumLitValue::F64(value)));
            }
            (State::ExpectFloatPostfix(_), _, _, _) => reterr!(7), 

            // ---- ExpectEOF(value) ---- 
            (State::ExpectEOF(ret_val), EOFCHAR, _, _) => retok!(13, ret_val),          // LAST RETOK
            (State::ExpectEOF(_), _, _, _) => reterr!(8),
        }
    }

}

pub fn parse_numeric_literal(raw: String, pos: StringPosition, messages: &mut MessageCollection) -> (Option<NumLitValue>, StringPosition) {
    
    match str_to_num_lit_impl(raw, pos) {
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
    // use std::mem;
    
    let myresult = (-0.0001).checked_mul(10f64.powi(-200)).unwrap();
    let expect = -0.0001E-200;

    assert_eq!(NumLitValue::F64(myresult), NumLitValue::F64(expect));
    // println!("{}", unsafe { mem::transmute::<f64, u64>(myresult) });
    // println!("{}", unsafe { mem::transmute::<f64, u64>(expect) });
    // assert_eq!(myresult, expect);
}

#[cfg(test)]
#[test]
fn num_lit_feature() {

    // let strpos = make_str_pos!(1, 2, 3, 4);
    fn new_empty_error(_: i32) -> Message { 
        Message::new_by_str(error_strings::InvalidNumericLiteral, vec![(make_str_pos!(1, 2, 3, 4), "")])
    }

    #[cfg(feature = "trace_num_lit_parse")]
    macro_rules! test_case {
        ($input: expr, $expect: expr) => (
            print!("\ncase {:?} at {}:", $input, line!());
            assert_eq!(str_to_num_lit_impl($input.to_owned(), make_str_pos!(1, 2, 3, 4)), Ok(NumLitValue::from($expect)));
        );
        ($input: expr, err, $expect: expr) => (
            print!("\ncase {:?} at {}:", $input, line!());
            assert_eq!(str_to_num_lit_impl($input.to_owned(), make_str_pos!(1, 2, 3, 4)), Err($expect));
        )
    }
    #[cfg(not(feature = "trace_num_lit_parse"))]
    macro_rules! test_case {
        ($input: expr, $expect: expr) => (
            assert_eq!(str_to_num_lit_impl($input.to_owned(), make_str_pos!(1, 2, 3, 4)), Ok(NumLitValue::from($expect)));
        );
        ($input: expr, err, $expect: expr) => (
            assert_eq!(str_to_num_lit_impl($input.to_owned(), make_str_pos!(1, 2, 3, 4)), Err($expect));
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
    test_case!("256u8", err, new_empty_error(6));               // 31, should be overflow and explain large than u8::max
    test_case!("100000i16", err, new_empty_error(7));           // 32, should be overflow and explain large than i16::max
    test_case!("-800i8", err, new_empty_error(1023));           // 33, should be downflow and explain smaller than i8

    // negative value
    test_case!("-0d123", NumLitValue::I32(-123));               // 34
    test_case!("-123", NumLitValue::I32(-123));                 // 35
    test_case!("-30000i16", NumLitValue::I16(-30000));          // 36
    test_case!("-123.456", NumLitValue::F64(-123.456));         // 37
    // negative not applicapable for unsigned

    // integral prefix
    test_case!("0xABCD", NumLitValue::I32(0xABCD));             // 38
    test_case!("0xfedc", NumLitValue::I32(0xFEDC));             // 39
    test_case!("0b101010", NumLitValue::I32(0b101010));         // 40
    test_case!("-0o777", NumLitValue::I32(-0o777));             // 41
    // invalid char
    test_case!("0xXXXX", err, new_empty_error(0));              // 42, should be invalid char at xx:xx
    test_case!("0b1234", err, new_empty_error(0));              // 43, should be invalid char at xx:xx
    test_case!("0daaaa", err, new_empty_error(0));              // 44, should be invalid char at xx:xx

    // floating point no prefix 
    test_case!("0x123.0", err, new_empty_error(0));             // 45, should be floating point no prefix
    test_case!("0b111.01", err, new_empty_error(0));            // 46, should be floating point no prefix

    // auto expansion for no postfix
    test_case!("2147483645", NumLitValue::I32(2147483645));                         // 47
    test_case!("2147483648", NumLitValue::U32(2147483648));                         // 48, 2^31 - 1..2^32 expand to u32
    test_case!("4294967295", NumLitValue::U32(4294967295u32));                      // 49, 2^31 - 1..2^32 expand to u32,
    test_case!("4294967296", NumLitValue::I64(4294967296i64));                      // 50, 2^32..2^63 expand to i64
    test_case!("4333333333", NumLitValue::I64(4333333333i64));                      // 51, 2^32..2^63 expand to i64
    test_case!("9223372036854775807", NumLitValue::I64(9223372036854775807));       // 52, 2^32..2^63 expand to i64
    test_case!("9223372036854775808", NumLitValue::U64(9223372036854775808));       // 53, 2^63..2^64 expand to u64
    test_case!("18446744073709551615", NumLitValue::U64(18446744073709551615));     // 54, 2^63..2^64 expand to u64
    test_case!("18446744073709551616", NumLitValue::F64(18446744073709551616f64));  // 55, 2^64.. expand to f64
    // auto expansion for negative value
    test_case!("-2147483648", NumLitValue::I32(-2147483648));                       // 56
    test_case!("-2147483649", NumLitValue::I64(-2147483649));                       // 57
    test_case!("-9223372036854775808", NumLitValue::I64(-9223372036854775808i64));  // 58
    test_case!("-9223372036854775809", NumLitValue::F64(-9223372036854775809f64));  // 59

    // int with e is float
    test_case!("123e10", NumLitValue::F64(123E10));                 // 60
    test_case!("123E10", NumLitValue::F64(123E10));                 // 61
    test_case!("123E+10", NumLitValue::F64(123E10));                // 62
    test_case!("123E-10", NumLitValue::F64(123E-10));               // 63
    // e not with prefix or postfix, exp should be integer
    test_case!("0x123E-5", err, new_empty_error(0));                // 64, should be floating point should not use prefix
    test_case!("123.456E789.0", err, new_empty_error(0));           // 65, should be floating point scientific representation's exponential part should be integer
    // after e is a i32
    test_case!("123E12345678901", err, new_empty_error(0));         // 66

    // decimal dot
    test_case!("123.456", NumLitValue::F64(123.456));               // 67
    test_case!("123456.0", NumLitValue::F64(123456f64));            // 68
    test_case!("0.123", NumLitValue::F64(0.123));                   // 69
    test_case!("0.0000000123", NumLitValue::F64(0.0000000123));     // 70
    test_case!("123.456E0", NumLitValue::F64(123.456E0));           // 71, dddE0 is legal
    test_case!("123456.0E5", NumLitValue::F64(123456E5f64));        // 72
    test_case!("0.123E-10", NumLitValue::F64(0.123E-10));           // 73
    test_case!("0.0000000123E3", NumLitValue::F64(0.0000123));      // 74
    test_case!("0.0001E-200", NumLitValue::F64(0.0001E-200));       // 75
    test_case!("-0.0001E-200", NumLitValue::F64(-0.0001E-200));     // 76
    test_case!("123E5f32", NumLitValue::F32(123E5f32));             // 77
    test_case!("0.123E-10f32", NumLitValue::F32(0.123E-10f32));     // 78 
    test_case!("0.0000000123E3f64", NumLitValue::F64(0.0000123));   // 79
    test_case!("0.0001E-200f64", NumLitValue::F64(0.0001E-200f64)); // 80
    test_case!("-0.0001E-200f32", err, new_empty_error(0));         // 81

    // cannot first or last char is dot
    test_case!(".123", err, new_empty_error(0));                    // 82
    test_case!("0x.123", err, new_empty_error(0));                  // 83
    test_case!("123.", err, new_empty_error(0));                    // 84
    test_case!(".", err, new_empty_error(0));                       // 85

    // multi dot
    test_case!("123.456.789", err, new_empty_error(0));             // 86
    test_case!("123..456", err, new_empty_error(0));                // 87

    test_case!("-0x8FFF_FFFF_FFFF_FFFF", err, new_empty_error(0));  // 88, negative int downflow -i64

    // underscore as sepreator, can before or after E
    test_case!("0b110_111_000_001u16", NumLitValue::U16(0b110111000001)); // 89
    test_case!("123_456_789u64", NumLitValue::U64(123456789));      // 90
    test_case!("2147483651_u32", NumLitValue::U32(2147483651));        // 118
    test_case!("184_467_440_737_095_516_15_u64", NumLitValue::U64(18446744073709551615u64)); // 91
    test_case!("1_2_3_4", NumLitValue::I32(1234));                  // 92
    test_case!("123_456_E_12", NumLitValue::F64(123456E12));        // 93
    test_case!("123.4_5_6E1_23", NumLitValue::F64(123.456E123));    // 94
    test_case!("0.1_2_3_4_5_6E0", NumLitValue::F64(0.123456));      // 95
    // underscore not at head, tail, not around dot
    test_case!("_1234", err, new_empty_error(0));                   // 96
    test_case!("1_", err, new_empty_error(0));                      // 97
    test_case!("_", err, new_empty_error(0));                       // 98
    test_case!("123_.5", err, new_empty_error(0));                  // 99
    test_case!("0._456", err, new_empty_error(0));                  // 100
    test_case!("0.123_", err, new_empty_error(0));                  // 101
    test_case!("0b_1110_1001", NumLitValue::I32(0b11101001));       // 102
    test_case!("0x_1234_i64", NumLitValue::I64(0x1234));            // 103
    // underscore not in postfix or prefix
    test_case!("0_xABCD", err, new_empty_error(0));                 // 104
    test_case!("ABCDu6_4", err, new_empty_error(0));                // 105
    test_case!("123i_32", err, new_empty_error(0));                 // 106
    // underscore no double
    test_case!("123__456", err, new_empty_error(0));                // 107
    test_case!("0b__101100", err, new_empty_error(0));              // 108

    // empty
    test_case!("0xu64", err, new_empty_error(0));                   // 109
    test_case!("0bf32", err, new_empty_error(0));                   // 110

    // strange postfix and prefix
    test_case!("0X123", err, new_empty_error(0));                   // 111
    test_case!("001", err, new_empty_error(0));                     // 112
    test_case!("0u123", err, new_empty_error(0));                   // 113
    test_case!("123u18", err, new_empty_error(0));                  // 114
    test_case!("1u128", err, new_empty_error(0));                   // 115
    test_case!("654321u1024", err, new_empty_error(0));             // 116
    test_case!("0x3f2048", NumLitValue::I32(0x3F2048));             // 117
}