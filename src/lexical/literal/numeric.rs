#![deny(overflowing_literals)]
///! lexical::numeric: generic numeric parser

// Syntax:
// num-lit = int-lit | rational-lit
//
// int-lit = '-'? '0d'? dec-char+ signed-postfix
//         | '-'? '0b'  bin-char+ signed-postfix
//         | '-'? '0o'  oct-char+ signed-postfix
//         | '-'? '0x'  hex-char+ signed-postfix
//         |      '0d'? dec-char+ unsigned-postifx
//         |      '0b'  bin-char+ unsigned-postfix
//         |      '0o'  oct-char+ unsigned-postfix
//         |      '0x'  hex-char+ unsigned-postfix
// signed-postfix = 'i8' | 'i16' | 'i32' | 'i64'
// unsiged-postfix = 'u8' | 'u16' | 'u32' | 'u64'
//
// rational-lit = '-'? dec-char+ ('.' dec-char+) rational-exponent? rational-postfix?
//              | '-'? dec-char+ rational-exponent? rational-postfix
// rational-postfix = 'r32' | 'r64'
// rational-exponent => ('e' | 'E') ('+' | '-') dec-char+
//
// dec-char = '0'...'9'
// bin-char = '0' | '1'
// oct-char = '0'...'7'
// hex-char = '0'...'9' | 'A'...'F' | 'a'...'f'

// Interestingly, currently numeric literal support `-` prefix to declare negative literals
// while v2lexer do not use this feature and later unary expr recognize it as a operator negate
// which allows this: `1----------1` to be a
// BinaryExpr <1:1-1:12>
//    Literal (i32)1 <1:1-1:1>
//    '-' <1:2-1:2>
//    UnaryExpr <1:3-1:12>
//      UnaryExpr <1:4-1:12>
//      ...
//        '-' <1:4-1:4>
//      '-' <1:11-1:11》
//      Literal (i32)1 <1:12-1:12>

use std::cell::Cell;
use crate::source::{Span, EOF};
use crate::diagnostics::strings;
use super::super::{Parser, Token, Numeric, CharExt};

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

        macro_rules! some_char_to_char { ($ch: expr) => (if let Some(ch) = $ch { ch } else { EOF }) }

        let current = some_char_to_char!(chars.next());
        let (next, nextnext) = if current != EOF {
            let next = some_char_to_char!(chars.next());
            (next, if next != EOF {
                some_char_to_char!(chars.next())
            } else {
                EOF
            })
        } else {
            (EOF, EOF)
        };

        BufChars{
            chars,
            m_current: current,
            m_next: next,
            m_nextnext: nextnext,
            skips: Cell::new(1),
        }
    }
    fn actual_move_next(&mut self) {

        if self.m_current != EOF {
            self.m_current = self.m_next;
            if self.m_next != EOF {
                self.m_next = self.m_nextnext;
                if self.m_nextnext != EOF {
                    self.m_nextnext = if let Some(nextnext) = self.chars.next() { nextnext } else { EOF };
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

enum FloatCheckedResult {
    Ok(f64),
    Overflow,
    Underflow,
}
trait FloatCheckedAlgorithm : Sized{

    fn checked_add<T: Into<f64>>(self, rhs: T) -> FloatCheckedResult;
    fn checked_sub<T: Into<f64>>(self, rhs: T) -> FloatCheckedResult;
    fn checked_mul<T: Into<f64>>(self, rhs: T) -> FloatCheckedResult;
    fn checked_mul_add<T: Into<f64>, U: Into<f64>>(self, muler: T, adder: U) -> FloatCheckedResult;
}
impl FloatCheckedAlgorithm for f64 {

    #[inline]
    fn checked_add<T: Into<f64>>(self, rhs: T) -> FloatCheckedResult {

        let rhs = rhs.into();
        if rhs < 0f64 {
            if f64::MIN - rhs >= self { FloatCheckedResult::Overflow } else { FloatCheckedResult::Ok(self + rhs) }
        } else {
            if f64::MAX - rhs <= self { FloatCheckedResult::Overflow } else { FloatCheckedResult::Ok(self + rhs) }
        }
    }
    #[inline]
    fn checked_sub<T: Into<f64>>(self, rhs: T) -> FloatCheckedResult {

        let rhs = rhs.into();
        if rhs < 0f64 {
            if f64::MAX + rhs >= self { FloatCheckedResult::Ok(self - rhs) } else { FloatCheckedResult::Overflow }
        } else {
            if f64::MIN + rhs <= self { FloatCheckedResult::Ok(self - rhs) } else { FloatCheckedResult::Overflow }
        }
    }
    #[inline]
    fn checked_mul<T: Into<f64>>(self, rhs: T) -> FloatCheckedResult {

        let rhs = rhs.into();
        if self > 0f64 {
            if rhs < 1f64 {
                if f64::MIN_POSITIVE / rhs >= self { FloatCheckedResult::Underflow } else { FloatCheckedResult::Ok(self * rhs) }
            } else {
                if f64::MAX / rhs <= self { FloatCheckedResult::Overflow } else { FloatCheckedResult::Ok(self * rhs) }
            }
        } else {
            if rhs < 1f64 {
                if f64::MIN_POSITIVE / rhs >= -self { FloatCheckedResult::Underflow } else { FloatCheckedResult::Ok(self * rhs) }
            } else {
                if f64::MAX / rhs <= -self { FloatCheckedResult::Overflow } else { FloatCheckedResult::Ok(self * rhs) }
            }
        }
    }
    #[inline]
    fn checked_mul_add<T: Into<f64>, U: Into<f64>>(self, muler: T, adder: U) -> FloatCheckedResult {

        let muler = muler.into();
        let adder = adder.into();
        match (muler.abs() < 1f64, adder < 0f64) {
            (true, true) => FloatCheckedResult::Ok(self.mul_add(muler, adder)),
            (true, false) => if (f64::MAX - adder) <= self * muler { FloatCheckedResult::Overflow } else { FloatCheckedResult::Ok(self.mul_add(muler, adder)) },
            (false, true) => FloatCheckedResult::Ok(self.mul_add(muler, adder)),
            (false, false) => if (f64::MAX - adder) / muler <= self { FloatCheckedResult::Overflow } else { FloatCheckedResult::Ok(self.mul_add(muler, adder)) },
        }
    }
}
fn u64_final_value(value: u64, is_positive: bool) -> Numeric {
    if value <= i32::MAX as u64 {
        Numeric::I32(if is_positive { value as i32 } else { -(value as i32) })
    } else if value <= u32::MAX as u64 && is_positive {
        Numeric::U32(value as u32)
    } else if value <= i64::MAX as u64 {
        Numeric::I64(if is_positive { value as i64 } else { -(value as i64) })
    } else {
        Numeric::U64(value)
    }
}

// Actual impl, huge state machine
// error is (message, location, optional help)
fn parse_impl(raw: String) -> Result<Numeric, (String, Option<String>)> {
    trace!(scope "parse_numeric");
    trace!("  #0 input \"{}\"", raw);

    #[cfg_attr(debug_assertions, derive(Debug, Clone, Copy))]
    enum State {
        ReallyNothing,
        Nothing(bool),                      // is positive
        UnknownI32(i32, bool, bool),        // value (already with sign), is positive, prev is underscore
        UnknownU32(u32, bool),              // value, prev is underscore
        UnknownI64(i64, bool, bool),        // value (already with sign), is positive, prev is underscore
        UnknownU64(u64, bool),              // value, prev is underscore
        UnknownR64(f64, bool),              // value, prev is underscore
        IntPrefix(u32, bool, bool),         // base, is positive, already a underscore
        AfterDot(f64, i32, bool, bool),     // value (already with sign), bits after dot, is positive, prev is underscore
        DirectAfterE(f64, bool),            // value, already a underscore, direct after e means expect char09 or + or -
        AfterE(f64, i32, bool, bool),       // value, exp (already with sign), exp is positive, prev is underscore
        ExpectInt(u32, u64, bool, bool),    // base (not with sign), current value, is positive, prev is underscore
        ExpectSignedIntPostfix(i64),
        ExpectUnsignedIntPostfix(u64),
        ExpectFloatPostfix(f64),
        ExpectEOF(Numeric),             // retval
    }

    let mut state = State::ReallyNothing;
    let mut chars = BufChars::new(raw.chars());

    #[allow(unused_macro_rules)]
    macro_rules! debug {
        // state transfer
        (transfer #$id:expr, $state:expr) => {{
            trace!("{:>4} from {:?} > {:?}", format!("#{}", $id), state, $state);
            state = $state;
        }};
        // return with success
        (success #$id:expr, $value:expr) => {{
            trace!("{:>4} ok {}", format!("#{}", $id), $value);
            return Ok($value);
        }};
        // return with fail
        (fail #$id:expr) => {{
            trace!("{:>4} fail", format!("#{}", $id));
            return Err((strings::InvalidNumericLiteral.to_owned(), None));
        }};
        (fail #$id:expr, $msg:expr) => {{
            trace!("{:>4} fail {}", format!("#{}", $id), $msg);
            return Err((format!("{}, {}", strings::InvalidNumericLiteral, $msg), None));
        }};
        (fail #$id:expr, $msg:expr, $help:expr) => ({
            trace!("{:>4} fail {}", format!("#{}", $id), $msg);
            return Err((format!("{}, {}", strings::InvalidNumericLiteral, $msg), Some($help.into())));
        });
        // internal error
        (internal #$id:expr) => ({
            trace!("internal error#{}", $id);
            return Err((format!("{}, {} {}", strings::InvalidNumericLiteral, strings::InternalErrorAt, line!()), None));
        });
    }

    loop {
        chars.move_next();
        match chars.current_with_state(state) {

            // ---- ReallyNothing ----
            (State::ReallyNothing, '-', EOF, _) => debug!(fail #1, strings::EmptyLiteral),
            (State::ReallyNothing, '_', EOF, _) => debug!(fail #2, strings::EmptyLiteral),
            (State::ReallyNothing, '_', _, _) => debug!(fail #3, strings::UnderscoreAtHead),
            (State::ReallyNothing, '-', _, _) => debug!(transfer #1, State::Nothing(false)),
            (State::ReallyNothing, _, _, _) => {
                chars.skip1();
                debug!(transfer #2, State::Nothing(true));
            }

            // ---- Nothing(is_positive) ----
            (State::Nothing(_), '0', EOF, _) => debug!(success #1, Numeric::I32(0)),
            (State::Nothing(_), '_', _, _) => debug!(fail #4, strings::UnderscoreAtHead),

            (State::Nothing(_), '0', 'i', _) => debug!(transfer #3, State::ExpectSignedIntPostfix(0i64)),
            (State::Nothing(_), '0', 'u', _) => debug!(transfer #4, State::ExpectUnsignedIntPostfix(0u64)),
            (State::Nothing(_), '0', 'r', _) => debug!(transfer #5, State::ExpectFloatPostfix(0f64)),

            (State::Nothing(_), '0', 'b', EOF)
            | (State::Nothing(_), '0', 'o', EOF)
            | (State::Nothing(_), '0', 'd', EOF)
            | (State::Nothing(_), '0', 'B', EOF) // although they are not int prefix, make it this error
            | (State::Nothing(_), '0', 'O', EOF)
            | (State::Nothing(_), '0', 'D', EOF)
            | (State::Nothing(_), '0', 'X', EOF) => debug!(fail #5, strings::EmptyIntLiteral),

            (State::Nothing(is_positive), '0', 'b', _) => {
                chars.dummy1();
                debug!(transfer #6, State::IntPrefix(2, is_positive, false));
            }
            (State::Nothing(is_positive), '0', 'o', _) => {
                chars.dummy1();
                debug!(transfer #7, State::IntPrefix(8, is_positive, false));
            }
            (State::Nothing(is_positive), '0', 'd', _) => {
                chars.dummy1();
                debug!(transfer #8, State::IntPrefix(10, is_positive, false));
            }
            (State::Nothing(is_positive), '0', 'x', _) => {
                chars.dummy1();
                debug!(transfer #9, State::IntPrefix(16, is_positive, false));
            }
            (State::Nothing(_), '0', 'B', _)
            | (State::Nothing(_), '0', 'O', _)
            | (State::Nothing(_), '0', 'D', _)
            | (State::Nothing(_), '0', 'X', _) => debug!(fail #6, strings::NumLitShouldNotStartWith0, strings::IntegralPrefixIsLowerCase),

            (State::Nothing(_), '0', '.', '_') => debug!(fail #7, strings::UnderscoreArroundDot),
            (State::Nothing(is_positive), '0', '.', _) => {
                chars.dummy1();
                debug!(transfer #10, State::AfterDot(0f64, 1, is_positive, false));
            }
            (State::Nothing(_), '0', _, _) => debug!(fail #8, strings::NumLitShouldNotStartWith0, strings::CStyleOctNumLitHelp),
            (State::Nothing(_), '.', EOF, _) => debug!(fail #9, strings::EmptyLiteral),
            (State::Nothing(_), '.', _, _) => debug!(fail #10, strings::DotAtHead),
            (State::Nothing(is_positive), ch, _, _) => match ch.to_digit(10) {
                Some(digit) => debug!(transfer #11, State::UnknownI32((digit as i32).merge_sign(is_positive), is_positive, false)),
                None => debug!(fail #11, strings::InvalidChar),
            },

            // ---- UnknownI32(value, is_positive, prev_is_underscore) ----
            (State::UnknownI32(_, _, true), EOF, _, _) => debug!(fail #12, strings::UnderscoreAtEnd),
            (State::UnknownI32(value, _, false), EOF, _, _) => debug!(success #2, Numeric::I32(value)),
            (State::UnknownI32(value, _, _), 'i', _, _) => {
                chars.skip1();
                debug!(transfer #12, State::ExpectSignedIntPostfix(value as i64));
            },
            (State::UnknownI32(value, true, _), 'u', _, _) => {
                chars.skip1();
                debug!(transfer #13, State::ExpectUnsignedIntPostfix(value as u64));
            },
            (State::UnknownI32(_, false, _), 'u', _, _) => debug!(fail #13, strings::NegativeOperatorOnUnsignedInt),
            (State::UnknownI32(value, _, _), 'r', _, _) => {
                chars.skip1();
                debug!(transfer #14, State::ExpectFloatPostfix(value as f64));
            }
            (State::UnknownI32(value, _, _), 'e', _, _)
            | (State::UnknownI32(value, _, _), 'E', _, _) => debug!(transfer #15, State::DirectAfterE(value as f64, false)),
            (State::UnknownI32(_, _, true), '.', _, _) => debug!(fail #14, strings::UnderscoreArroundDot),
            (State::UnknownI32(_, _, false), '.', '_', _) => debug!(fail #15, strings::UnderscoreArroundDot),
            (State::UnknownI32(value, is_positive, false), '.', _, _) =>
                debug!(transfer #16, State::AfterDot(value as f64, 1, is_positive, false)),
            (State::UnknownI32(value, is_positive, prev_is_underscore), ch, _, _) => match (prev_is_underscore, ch == '_') {
                (true, true) => debug!(fail #16, strings::UnderscoreDouble),
                (_, true) => debug!(transfer #17, State::UnknownI32(value, is_positive, true)),
                (_, false) => match ch.to_digit(10) {
                    None => debug!(fail #17, strings::InvalidChar),
                    Some(digit) => match (value.checked_mul(10), is_positive) {
                        (None, true) => { // positive i32 mul 10 overflow, that is (value >= 214748365 and value <= 21487483647)
                            let i64_value = value as i64 * 10i64 + digit as i64 ;   // these values won't overflow i64 anyway
                            if i64_value <= u32::MAX as i64 {                       // may not overflow u32
                                debug!(transfer #18, State::UnknownU32(i64_value as u32, false));
                            } else {
                                debug!(transfer #19, State::UnknownI64(i64_value, true, false));
                            }
                        }
                        (None, false) => { // negative i32 mul 10 overflow, that is (value <= -214748365 and value >= -2147483648)
                            let i64_value = value as i64 * 10i64 - digit as i64;    // these values won't overflow i64 anyway
                            debug!(transfer #20, State::UnknownI64(i64_value, false, false));
                        }
                        (Some(value), true) => match value.checked_add(digit as i32) {
                            None => debug!(transfer #21, State::UnknownU32(value as u32 + digit, false)),
                            Some(value) => debug!(transfer #22, State::UnknownI32(value, is_positive, false)),
                        },
                        (Some(value), false) => match value.checked_sub(digit as i32) {
                            None => debug!(transfer #23, State::UnknownI64(value as i64 - digit as i64, false, false)),
                            Some(value) => debug!(transfer #24, State::UnknownI32(value, is_positive, false)),
                        },
                    },
                },
            },

            // ---- UnknownU32(value, prev_is_underscore) ----
            (State::UnknownU32(_, true), EOF, _, _) => debug!(fail #18, strings::UnderscoreAtEnd),
            (State::UnknownU32(value, false), EOF, _, _) => debug!(success #3, Numeric::U32(value)),
            (State::UnknownU32(value, _), 'i', _, _) => {
                chars.skip1();
                debug!(transfer #25, State::ExpectSignedIntPostfix(value as i64));
            },
            (State::UnknownU32(value, _), 'u', _, _) => {
                chars.skip1();
                debug!(transfer #26, State::ExpectUnsignedIntPostfix(value as u64));
            },
            (State::UnknownU32(value, _), 'r', _, _) => {
                chars.skip1();
                debug!(transfer #27, State::ExpectFloatPostfix(value as f64));
            }
            (State::UnknownU32(value, _), 'e', _, _)
            | (State::UnknownU32(value, _), 'E', _, _) => debug!(transfer #28, State::DirectAfterE(value as f64, false)),
            (State::UnknownU32(_, _), '.', '_', _) => debug!(fail #19, strings::UnderscoreArroundDot),
            (State::UnknownU32(_, true), '.', _, _) => debug!(fail #20, strings::UnderscoreArroundDot),
            (State::UnknownU32(value, false), '.', _, _) => debug!(transfer #29, State::AfterDot(value as f64, 1, true, false)),
            (State::UnknownU32(value, prev_is_underscore), ch, _, _) => match (prev_is_underscore, ch == '_') {
                (true, true) => debug!(fail #21, strings::UnderscoreDouble),
                (_, true) => debug!(transfer #30, State::UnknownU32(value, true)),
                (_, false) => match ch.to_digit(10) {
                    None => debug!(fail #22, strings::InvalidChar),
                    Some(digit) => match value.checked_mul(10) {
                        None => debug!(transfer #31, State::UnknownI64(value as i64 * 10i64 + digit as i64, true, false)), // u32 mul 10 overflow must be positive i64
                        Some(value) => match value.checked_add(digit) {
                            None => debug!(transfer #32, State::UnknownI64(value as i64 + digit as i64, true, false)),     // u32 add digit overflow must be positive i64
                            Some(value) => debug!(transfer #33, State::UnknownU32(value, false)),                          // not overflow continue u32
                        },
                    },
                },
            },

            // ---- UnknownI64(value, is_positive, prev is underscore) ----
            (State::UnknownI64(_, _, true), EOF, _, _) => debug!(fail #23, strings::UnderscoreAtEnd),
            (State::UnknownI64(value, _, false), EOF, _, _) => debug!(success #4, Numeric::I64(value)),
            (State::UnknownI64(value, _, _), 'i', _, _) => {
                chars.skip1();
                debug!(transfer #34, State::ExpectSignedIntPostfix(value));
            },
            (State::UnknownI64(value, true, _), 'u', _, _) => {
                chars.skip1();
                debug!(transfer #35, State::ExpectUnsignedIntPostfix(value as u64));
            },
            (State::UnknownI64(_, false, _), 'u', _, _) => debug!(fail #24, strings::NegativeOperatorOnUnsignedInt),
            (State::UnknownI64(value, _, _), 'r', _, _) => {
                chars.skip1();
                debug!(transfer #36, State::ExpectFloatPostfix(value as f64));
            }
            (State::UnknownI64(value, _, _), 'e', _, _)
            | (State::UnknownI64(value, _, _), 'E', _, _) => debug!(transfer #37, State::DirectAfterE(value as f64, false)),
            (State::UnknownI64(_, _, true), '.', _, _) => debug!(fail #25, strings::UnderscoreArroundDot),
            (State::UnknownI64(_, _, false), '.', '_', _) => debug!(fail #26, strings::UnderscoreArroundDot),
            (State::UnknownI64(value, is_positive, false), '.', _, _) => debug!(transfer #38, State::AfterDot(value as f64, 1, is_positive, false)),
            (State::UnknownI64(value, is_positive, prev_is_underscore), ch, _, _) => match (prev_is_underscore, ch == '_') {
                (true, true) => debug!(fail #27, strings::UnderscoreDouble),
                (_, true) => debug!(transfer #39, State::UnknownI64(value, is_positive, true)),
                (_, false) => match ch.to_digit(10) {
                    None => debug!(fail #28, strings::InvalidChar),
                    Some(digit) => match (value.checked_mul(10i64), is_positive) {
                        (None, true) => {       // positive i64 mul 10 overflow, that is (value <= 9223372036854775808 and value >= 922337203685477581)
                            match (value as u64).checked_mul(10u64) {
                                None => {       // positive i64 as u64 mul 10 overflow, that is (value <= 9223372036854775808 and value >= 1844674407370955162)
                                    debug!(transfer #40, State::UnknownR64(value as f64 * 10f64 + digit as f64, is_positive));
                                }
                                Some(value) => match value.checked_add(digit as u64) {
                                    None => {   // positive i64 as u64 mul 10 not overflow add digit overflow, that is (value = 1844674407370955161 and digit >= 6)
                                        debug!(transfer #41, State::UnknownR64(value as f64 + digit as f64, is_positive));
                                    }
                                    Some(value) => debug!(transfer #42, State::UnknownU64(value, false)),
                                },
                            }
                        }
                        (None, false) => {      // negative i64 mul 10 overflow, that is (value >= -9223372036854775808 and value <= -922337203685477581)
                            match (value as u64).checked_mul(10u64) {
                                None => {       // negative i64 as u64 mul 10 overflow, that is (value >= -9223372036854775808 and value <= -1844674407370955162)
                                    debug!(transfer #43, State::UnknownR64(value as f64 * 10f64 - digit as f64, is_positive));
                                }
                                Some(value) => match value.checked_sub(digit as u64) {
                                    None => {   // negative i64 as u64 mul 10 not overflow add digit overflow, that is (value = -1844674407370955161 and digit >= 7)
                                        debug!(transfer #44, State::UnknownR64(value as f64 - digit as f64, is_positive));
                                    }
                                    Some(value) => debug!(transfer #45, State::UnknownU64(value, false)),
                                },
                            }
                        }
                        (Some(value), true) => match value.checked_add(digit as i64) {
                            None => debug!(transfer #46, State::UnknownU64(value as u64 + digit as u64, false)),
                            Some(value) => debug!(transfer #47, State::UnknownI64(value, true, false)),
                        },
                        (Some(value), false) => match value.checked_sub(digit as i64) {
                            None => debug!(transfer #48, State::UnknownR64(value as f64 - digit as f64, false)),
                            Some(value) => debug!(transfer #49, State::UnknownI64(value, false, false)),
                        },
                    },
                },
            },

            // ---- UnknownU64(value, prev_is_underscore) ----
            (State::UnknownU64(_, true), EOF, _, _) => debug!(fail #29, strings::UnderscoreAtEnd),
            (State::UnknownU64(value, false), EOF, _, _) => debug!(success #5, Numeric::U64(value)),
            (State::UnknownU64(_, _), 'i', _, _) => debug!(fail #30, strings::IntegralOverflow),
            (State::UnknownU64(value, _), 'u', _, _) => {
                chars.skip1();
                debug!(transfer #50, State::ExpectUnsignedIntPostfix(value));
            },
            (State::UnknownU64(value, _), 'r', _, _) => {
                chars.skip1();
                debug!(transfer #51, State::ExpectFloatPostfix(value as f64));
            }
            (State::UnknownU64(value, _), 'e', _, _)
            | (State::UnknownU64(value, _), 'E', _, _) => debug!(transfer #52, State::DirectAfterE(value as f64, false)),
            (State::UnknownU64(_, true), '.', _, _) => debug!(fail #31, strings::UnderscoreArroundDot),
            (State::UnknownU64(_, false), '.', '_', _) => debug!(fail #32, strings::UnderscoreArroundDot),
            (State::UnknownU64(value, false), '.', _, _) => debug!(transfer #53, State::AfterDot(value as f64, 1, true, false)),
            (State::UnknownU64(value, prev_is_underscore), ch, _, _) => match (prev_is_underscore, ch == '_') {
                (true, true) => debug!(fail #33, strings::UnderscoreDouble),
                (_, true) => debug!(transfer #54, State::UnknownU64(value, true)),
                (_, false) => match ch.to_digit(10) {
                    None => debug!(fail #34, strings::InvalidChar),
                    Some(digit) => match value.checked_mul(10) {
                        None => debug!(transfer #55, State::UnknownR64(value as f64 * 10f64 + digit as f64, true)),    // u64 mul 10 overflow must be positive f64
                        Some(value) => match value.checked_add(digit as u64) {
                            None => debug!(transfer #56, State::UnknownR64(value as f64 + digit as f64, true)),        // u64 add digit overflow must be positive f64
                            Some(value) => debug!(transfer #57, State::UnknownU64(value, false)),                      // not overflow continue u64
                        },
                    },
                },
            },

            // TODO: need prev is underscore?
            // ---- UnknownR64(value, is_positive) ----
            (State::UnknownR64(value, _), EOF, _, _) => debug!(success #6, Numeric::R64(value)),
            (State::UnknownR64(_, _), 'i', _, _) => debug!(fail #35, strings::IntegralOverflow),
            (State::UnknownR64(_, _), 'u', _, _) => debug!(fail #36, strings::IntegralOverflow),
            (State::UnknownR64(value, _), 'r', _, _) => {
                chars.skip1();
                debug!(transfer #58, State::ExpectFloatPostfix(value));
            }
            (State::UnknownR64(value, _), 'e', _, _)
            | (State::UnknownR64(value, _), 'E', _, _) => debug!(transfer #59, State::DirectAfterE(value, false)),
            (State::UnknownR64(_, _), '.', '_', _) => debug!(fail #37, strings::UnderscoreArroundDot),
            (State::UnknownR64(value, is_positive), '.', _, _) => debug!(transfer #60, State::AfterDot(value, 1, is_positive, false)),
            (State::UnknownR64(value, is_positive), ch, _, _) => match ch.to_digit(10) {
                None => debug!(fail #38, strings::InvalidChar),
                Some(digit) => match value.checked_mul_add(10, if is_positive { digit as i32 } else { -(digit as i32) }) {
                    FloatCheckedResult::Ok(value) => debug!(transfer #61, State::UnknownR64(value, is_positive)),
                    FloatCheckedResult::Overflow => debug!(fail #39, strings::FloatPointOverflow),
                    FloatCheckedResult::Underflow => debug!(fail #40, strings::FloatPointUnderflow),
                },
            },

            // ---- IntPrefix(base, is_postive, prev_is_underscore) ----
            (State::IntPrefix(_, _, _), 'e', _, _)
            | (State::IntPrefix(_, _, _), 'E', _, _) => debug!(fail #41, strings::ExponentInIntLiteral),
            (State::IntPrefix(_, _, _), '.', _, _) => debug!(fail #42, strings::DotAtHead),
            (State::IntPrefix(_, _, true), '_', _, _) => debug!(fail #43, strings::UnderscoreDouble),
            (State::IntPrefix(_, _, _), 'i', '8', EOF)
            | (State::IntPrefix(_, _, _), 'i', '1', '6')
            | (State::IntPrefix(_, _, _), 'i', '3', '2')
            | (State::IntPrefix(_, _, _), 'i', '6', '4')
            | (State::IntPrefix(_, _, _), 'u', '8', EOF)
            | (State::IntPrefix(_, _, _), 'u', '1', '6')
            | (State::IntPrefix(_, _, _), 'u', '3', '2')
            | (State::IntPrefix(_, _, _), 'u', '6', '4') => debug!(fail #44, strings::EmptyIntLiteral),
            (State::IntPrefix(_, _, _), 'r', '3', '2')
            | (State::IntPrefix(_, _, _), 'r', '6', '4') => debug!(fail #45, strings::EmptyIntLiteral, strings::AndFloatPostfixInIntLiteral),
            // u, i, f
            (State::IntPrefix(base, is_positive, false), '_', _, _) => debug!(transfer #62, State::IntPrefix(base, is_positive, true)),
            (State::IntPrefix(base, is_positive, _), ch, _, _) => match ch.to_digit(base) {
                Some(digit) => debug!(transfer #63, State::ExpectInt(base, digit as u64, is_positive, false)),
                None => debug!(fail #46, strings::InvalidCharInIntLiteral, strings::IntLiteralAllowedChars[match base { 2 => 0, 8 => 1, 10 => 2, 16 => 3, _ => debug!(internal #2) }]),
            },

            // ---- ExpectInt(base, value, is_positive, prev_is_underscore) ----
            (State::ExpectInt(_, _, _, _), '.', _, _) => debug!(fail #47, strings::DotInIntLiteral),
            (State::ExpectInt(_, value, is_positive, _), EOF, _, _) => debug!(success #7, u64_final_value(value, is_positive)),
            (State::ExpectInt(_, value, is_positive, _), 'i', _, _) => if value > i64::MAX as u64 {
                debug!(fail #48, strings::IntegralOverflow);
            } else {
                chars.skip1();
                debug!(transfer #64, State::ExpectSignedIntPostfix(if is_positive { value as i64 } else { -(value as i64) }));
            },
            (State::ExpectInt(_, value, true, _), 'u', _, _) => {
                chars.skip1();
                debug!(transfer #65, State::ExpectUnsignedIntPostfix(value));
            }
            (State::ExpectInt(_, _, false, _), 'u', _, _) => debug!(fail #49, strings::NegativeOperatorOnUnsignedInt),
            (State::ExpectInt(2, _, _, _), 'e', _, _)
            | (State::ExpectInt(2, _, _, _), 'E', _, _)
            | (State::ExpectInt(8, _, _, _), 'e', _, _)     // because E is allowed in 16
            | (State::ExpectInt(8, _, _, _), 'E', _, _)
            | (State::ExpectInt(10, _, _, _), 'e', _, _)
            | (State::ExpectInt(10, _, _, _), 'E', _, _)
            | (State::ExpectInt(16, _, _, _), 'e', '+', _)  // because E is allowed in 16
            | (State::ExpectInt(16, _, _, _), 'E', '+', _)
            | (State::ExpectInt(16, _, _, _), 'e', '-', _)
            | (State::ExpectInt(16, _, _, _), 'E', '-', _) => debug!(fail #50, strings::ExponentInIntLiteral),
            (State::ExpectInt(base, value, is_positive, prev_is_underscore), ch, _, _) => match (prev_is_underscore, ch == '_') {
                (true, true) => debug!(fail #51, strings::UnderscoreDouble),
                (_, true) => debug!(transfer #66, State::ExpectInt(base, value, is_positive, true)),
                (_, false) => match ch.to_digit(base) {
                    None => debug!(fail #52, strings::InvalidCharInIntLiteral, strings::IntLiteralAllowedChars[match base { 2 => 0, 8 => 1, 10 => 2, 16 => 3, _ => debug!(internal #1) }]),
                    Some(digit) => match value.checked_mul(base as u64) {
                        None => debug!(fail #53, strings::IntegralOverflow),
                        Some(value) => match value.checked_add(digit as u64) {
                            None => debug!(fail #54, strings::IntegralOverflow),
                            Some(value) => {
                                if !is_positive && value >= 9223372036854775808u64 {
                                    debug!(fail #55, strings::IntegralOverflow);
                                } else {
                                    debug!(transfer #67, State::ExpectInt(base, value, is_positive, false));
                                }
                            }
                        },
                    },
                },
            },

            // ---- AfterDot(value, bits, is_positive, prev_is_underscore) ----
            (State::AfterDot(_, _, _, _), '.', _, _) => debug!(fail #56, strings::DotDouble),
            (State::AfterDot(_, _, _, _), 'i', _, _)
            | (State::AfterDot(_, _, _, _), 'u', _, _) => debug!(fail #57, strings::MaybeIntPostfixInFloatPoint),
            (State::AfterDot(value, _, _, _), 'r', _, _) => {
                chars.skip1();
                debug!(transfer #68, State::ExpectFloatPostfix(value));
            }
            (State::AfterDot(value, _, _, _), 'e', _, _)
            | (State::AfterDot(value, _, _, _), 'E', _, _) => debug!(transfer #69, State::DirectAfterE(value, false)),
            (State::AfterDot(_, 1, _, _), EOF, _, _) => debug!(fail #58, strings::DotAtEnd),
            (State::AfterDot(_, _, _, true), EOF, _, _) => debug!(fail #59, strings::UnderscoreAtEnd),
            (State::AfterDot(value, _, _, false), EOF, _, _) => debug!(success #8, Numeric::R64(value)),
            (State::AfterDot(value, bits, is_positive, prev_is_underscore), ch, _, _) => match (prev_is_underscore, ch == '_') {
                (true, true) => debug!(fail #60, strings::UnderscoreDouble),
                (_, true) => debug!(transfer #70, State::AfterDot(value, bits, is_positive, true)),
                (_, false) => match (ch.to_digit(10), is_positive) {
                    (None, _) => debug!(fail #61, strings::InvalidChar),
                    (Some(digit), true) => match value.checked_add(digit as f64 / 10f64.powi(bits)) {
                        FloatCheckedResult::Ok(value) => debug!(transfer #71, State::AfterDot(value, bits + 1, is_positive, false)),
                        FloatCheckedResult::Overflow => debug!(fail #62, strings::FloatPointOverflow),
                        FloatCheckedResult::Underflow => debug!(fail #63, strings::FloatPointUnderflow),
                    },
                    (Some(digit), false) => match value.checked_sub(digit as f64 / 10f64.powi(bits)) {
                        FloatCheckedResult::Ok(value) => debug!(transfer #72, State::AfterDot(value, bits + 1, is_positive, false)),
                        FloatCheckedResult::Overflow => debug!(fail #64, strings::FloatPointOverflow),
                        FloatCheckedResult::Underflow => debug!(fail #65, strings::FloatPointUnderflow),
                    }
                },
            },

            // ---- DirectAfterE(value, prev_is_underscore) ----
            (State::DirectAfterE(_, _), '+', EOF, _)
            | (State::DirectAfterE(_, _), '-', EOF, _) => debug!(fail #66, strings::UnexpectedEOFInExponent),
            (State::DirectAfterE(_, _), '+', '_', _) => debug!(fail #67, strings::UnderscoreAtExponentHead),
            (State::DirectAfterE(_, _), '-', '_', _) => debug!(fail #68, strings::UnderscoreAtExponentHead),
            (State::DirectAfterE(value, _), '+', ch, _) => match ch.to_digit(10) {
                None => debug!(fail #69, strings::InvalidChar),
                Some(digit) => {
                    chars.dummy1();
                    debug!(transfer #73, State::AfterE(value, digit as i32, true, false));
                }
            },
            (State::DirectAfterE(value, _), '-', ch, _) => match ch.to_digit(10) {
                None => debug!(fail #70, strings::InvalidChar),
                Some(digit) => {
                    chars.dummy1();
                    debug!(transfer #74, State::AfterE(value, -(digit as i32), false, false));
                }
            },
            (State::DirectAfterE(value, false), '_', _, _) => debug!(transfer #75, State::DirectAfterE(value, true)),
            (State::DirectAfterE(_, true), '_', _, _) => debug!(fail #71, strings::UnderscoreDouble),
            (State::DirectAfterE(value, _), ch, _, _) => match ch.to_digit(10) {
                None => debug!(fail #72, strings::InvalidChar),
                Some(digit) => debug!(transfer #76, State::AfterE(value, digit as i32, true, false)),
            },

            // ---- AfterE(value, exp, exp_is_positive, prev_is_underscore) ----
            (State::AfterE(_, _, _, _), 'u', _, _)
            | (State::AfterE(_, _, _, _), 'i', _, _) => debug!(fail #73, strings::MaybeIntPostfixInFloatPoint),
            (State::AfterE(_, _, _, _), '.', _, _)
            | (State::AfterE(_, _, _, _), 'e', _, _)
            | (State::AfterE(_, _, _, _), 'E', _, _) => debug!(fail #74, strings::FloatExponentFloat),
            (State::AfterE(value, exp, _, _), 'r', _, _) => match value.checked_mul(10f64.powi(exp)) {
                FloatCheckedResult::Overflow => debug!(fail #75, strings::FloatPointOverflow),
                FloatCheckedResult::Underflow => debug!(fail #76, strings::FloatPointUnderflow),
                FloatCheckedResult::Ok(value) => {
                    chars.skip1();
                    debug!(transfer #77, State::ExpectFloatPostfix(value));
                }
            },
            (State::AfterE(_, _, _, true), EOF, _, _) => debug!(fail #77, strings::UnderscoreAtEnd),
            (State::AfterE(value, exp, _, false), EOF, _, _) => match value.checked_mul(10f64.powi(exp)) {
                FloatCheckedResult::Ok(value) => debug!(success #9, Numeric::R64(value)),
                FloatCheckedResult::Overflow => debug!(fail #78, strings::FloatPointOverflow,
                    strings::FloatPointOverflowHelpMaxValue[if value > 0f64 { 2 } else { 3 }]),
                FloatCheckedResult::Underflow => debug!(fail #79, strings::FloatPointUnderflow,
                    strings::FloatPointUnderflowHelpMinValue[if value > 0f64 { 2 } else { 3 }]),
            },
            (State::AfterE(value, exp, exp_is_positive, prev_is_underscore), ch, _, _) => match (prev_is_underscore, ch == '_') {
                (true, true) => debug!(fail #80, strings::UnderscoreDouble),
                (_, true) => debug!(transfer #78, State::AfterE(value, exp, exp_is_positive, true)),
                (_, false) => match ch.to_digit(10) {
                    None => debug!(fail #81, strings::InvalidChar),
                    Some(digit) => match (exp.checked_mul(10), exp_is_positive) {
                        (None, true) => debug!(fail #82, strings::FloatPointOverflow, strings::FloatPointOverflowHelpMaxValue[if value > 0f64 { 2 } else { 3 }]),
                        (None, false) => debug!(fail #83, strings::FloatPointUnderflow, strings::FloatPointUnderflowHelpMinValue[if value > 0f64 { 2 } else { 3 }]),
                        (Some(exp), true) => match exp.checked_add(digit as i32) {
                            None => debug!(fail #84, strings::FloatPointUnderflow),
                            Some(exp) => debug!(transfer #79, State::AfterE(value, exp, true, false)),
                        },
                        (Some(exp), false) => match exp.checked_sub(digit as i32) {
                            None => debug!(fail #85, strings::FloatPointUnderflow),             // LAST RETERR
                            Some(exp) => debug!(transfer #80, State::AfterE(value, exp, false, false)),
                        }
                    }
                },
            },

            // ---- ExpectSignedIntPostfix(value) ----
            (State::ExpectSignedIntPostfix(value), 'i', '8', EOF) =>
                if value > i8::MAX as i64 {
                    debug!(fail #86, strings::IntegralOverflow, strings::IntegralOverflowHelpMaxValue[0]);
                } else if value < i8::MIN as i64 {
                    debug!(fail #87, strings::IntegralUnderflow, strings::IntegralUnderflowHelpMinValue[0]);
                } else {
                    debug!(success #10, Numeric::I8(value as i8));
                },
            (State::ExpectSignedIntPostfix(value), 'i', '1', '6') =>
                if value > i16::MAX as i64 {
                    debug!(fail #88, strings::IntegralOverflow, strings::IntegralOverflowHelpMaxValue[2]);
                } else if value < i64::MIN as i64 {
                    debug!(fail #89, strings::IntegralUnderflow, strings::IntegralUnderflowHelpMinValue[1]);
                } else {
                    chars.dummy1();
                    chars.dummy1();
                    debug!(transfer #81, State::ExpectEOF(Numeric::I16(value as i16)));
                },
            (State::ExpectSignedIntPostfix(value), 'i', '3', '2') =>
                if value > i32::MAX as i64 {
                    debug!(fail #90, strings::IntegralOverflow, strings::IntegralOverflowHelpMaxValue[5]);
                } else if value < i32::MIN as i64 {
                    debug!(fail #91, strings::IntegralUnderflow, strings::IntegralUnderflowHelpMinValue[1]);
                } else {
                    chars.dummy1();
                    chars.dummy1();
                    debug!(transfer #82, State::ExpectEOF(Numeric::I32(value as i32)));
                },
            (State::ExpectSignedIntPostfix(value), 'i', '6', '4') => {
                chars.dummy1();
                chars.dummy1();
                debug!(transfer #83, State::ExpectEOF(Numeric::I64(value as i64)));
            }
            (State::ExpectSignedIntPostfix(_), 'i', EOF, _) => debug!(fail #92, strings::UnexpectedEOFInMaybeSignedIntPostfix),
            (State::ExpectSignedIntPostfix(_), 'i', '_', _)
            | (State::ExpectSignedIntPostfix(_), 'i', _, '_') => debug!(fail #93, strings::UnderscoreInMaybeSignedIntPostfix),
            (State::ExpectSignedIntPostfix(_), _, _, _) => debug!(fail #94, strings::UnexpectedValueAfterMaybeSignedIntPostfix),

            // ---- ExpectUnsignedIntPostfix(value) ----
            (State::ExpectUnsignedIntPostfix(value), 'u', '8', EOF) =>
                if value > u8::max_value() as u64 {
                    debug!(fail #95, strings::IntegralOverflow, strings::IntegralOverflowHelpMaxValue[1]);
                } else {
                    debug!(success #11, Numeric::U8(value as u8));
                },
            (State::ExpectUnsignedIntPostfix(value), 'u', '1', '6') =>
                if value > u16::max_value() as u64 {
                    debug!(fail #96, strings::IntegralOverflow, strings::IntegralOverflowHelpMaxValue[3]);
                } else {
                    chars.dummy1();
                    chars.dummy1();
                    debug!(transfer #84, State::ExpectEOF(Numeric::U16(value as u16)));
                },
            (State::ExpectUnsignedIntPostfix(value), 'u', '3', '2') =>
                if value > u32::max_value() as u64 {
                    debug!(fail #97, strings::IntegralOverflow, strings::IntegralOverflowHelpMaxValue[5]);
                } else {
                    chars.dummy1();
                    chars.dummy1();
                    debug!(transfer #85, State::ExpectEOF(Numeric::U32(value as u32)));
                },
            (State::ExpectUnsignedIntPostfix(value), 'u', '6', '4') => {
                chars.dummy1();
                chars.dummy1();
                debug!(transfer #86, State::ExpectEOF(Numeric::U64(value)));
            }
            (State::ExpectUnsignedIntPostfix(_), 'u', EOF, _) => debug!(fail #98, strings::UnexpectedEOFInMaybeUnsignedIntPostfix),
            (State::ExpectUnsignedIntPostfix(_), 'u', '_', _)
            | (State::ExpectUnsignedIntPostfix(_), 'u', _, '_') => debug!(fail #99, strings::UnderscoreInMaybeUnsignedIntPostfix),
            (State::ExpectUnsignedIntPostfix(_), _, _, _) => debug!(fail #100, strings::UnexpectedValueAfterMaybeUnsignedIntPostfix),

            // ---- ExpectFloatPostfix(value) ----
            (State::ExpectFloatPostfix(value), 'r', '3', '2') => {
                if value > f32::MAX as f64 {
                    debug!(fail #101, strings::FloatPointOverflow, strings::FloatPointOverflowHelpMaxValue[0]);
                } else if value < -f32::MAX as f64 {
                    debug!(fail #102, strings::FloatPointOverflow, strings::FloatPointOverflowHelpMaxValue[1]);
                } else if value == 0f32 as f64 {
                    chars.dummy1();
                    chars.dummy1();
                    debug!(transfer #87, State::ExpectEOF(Numeric::R32(0f32)));
                } else if value < f32::MIN_POSITIVE as f64 && value > 0f64 {
                    debug!(fail #103, strings::FloatPointUnderflow, strings::FloatPointUnderflowHelpMinValue[0]);
                } else if value > -f32::MIN_POSITIVE as f64 && value < 0f64 {
                    debug!(fail #104, strings::FloatPointUnderflow, strings::FloatPointUnderflowHelpMinValue[1]);
                } else {
                    chars.dummy1();
                    chars.dummy1();
                    debug!(transfer #88, State::ExpectEOF(Numeric::R32(value as f32)));
                }
            }
            (State::ExpectFloatPostfix(value), 'r', '6', '4') => {
                chars.dummy1();
                chars.dummy1();
                debug!(transfer #89, State::ExpectEOF(Numeric::R64(value)));
            }
            (State::ExpectFloatPostfix(_), 'r', EOF, _) =>
                debug!(fail #105, strings::UnexpectedEOFInMaybeFloatingPostfix),
            (State::ExpectFloatPostfix(_), 'u', '_', _)
            | (State::ExpectFloatPostfix(_), 'u', _, '_') => debug!(fail #106, strings::UnderscoreInMaybeFloatPointPostfix),
            (State::ExpectFloatPostfix(_), _, _, _) =>
                debug!(fail #107, strings::UnexpectedValueAfterMaybeFloatingPostfix),

            // ---- ExpectEOF(value) ----
            (State::ExpectEOF(ret_val), EOF, _, _) => debug!(success #12, ret_val),
            (State::ExpectEOF(_), _, _, _) => debug!(fail #108, strings::UnexpectedNotEOF),
        }
    }
}

impl<'ecx, 'scx> Parser<'ecx, 'scx> {

    // only digit, no dot, and no hyphen,
    // numeric parser supports hyphen but not used in lexical parser,
    // but keep for generic numeric parser (e.g. the one in standard library)
    pub(in super::super) fn is_numeric_start(&self) -> bool {
        self.buf[0].is_digit(10)
    }

    // Only digit or ASCII letters or underscore, or hyphen or plus for exponent
    fn is_numeric_continue(&self) -> bool {
        self.buf[0].is_digit(36) || matches!(self.buf[0], '_' | '.' | '-' | '+')
    }

    pub(in super::super) fn parse_numeric(&mut self) -> (Token, Span) {

        let mut string_value = String::new();
        let mut span = self.pos[0].into();
        while self.is_numeric_continue() {
            // 1. exclude 1..2 for range expression, parse_impl checks for that but not used in lexical parser, keep for generic numeric parser
            // 2. exclude 1.to_string(), parse_impl checks for that but not used in lexical parser, keep for generic numeric parser
            if self.buf[0] == '.' && (self.buf[1] == '.' || self.buf[1].is_id_start()) {
                break;
            }
            string_value.push(self.buf[0]);
            span += self.pos[0];
            self.eat();
        }

        match parse_impl(string_value) {
            Ok(result) => (Token::Num(result), span),
            Err((name, None)) => { self.diagnostics.emit(name).span(span); (Token::Num(Numeric::I32(0)), span) },
            Err((name, Some(help))) => { self.diagnostics.emit(name).span(span).help(help); (Token::Num(Numeric::I32(0)), span) },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // some test case require this check equal
    macro_rules! rational_eq {
        ($a:expr, $b:expr) => {
            match ($a, $b) {
                (Numeric::R32(a), Numeric::R32(b)) if a.is_normal() && b.is_normal() => {
                    let (a, b) = (a.to_bits(), b.to_bits());
                    if a > b { a - b <= 2 } else { b - a <= 2 }
                },
                (Numeric::R64(a), Numeric::R64(b)) if a.is_normal() && b.is_normal() => {
                    let (a, b) = (a.to_bits(), b.to_bits());
                    if a > b { a - b <= 2 } else { b - a <= 2 }
                },
                (a, b) => a == b,
            }
        }
    }

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
        assert_eq!(chars.current_with_state(1), (1, EOF, EOF, EOF));
        chars.move_next();
        assert_eq!(chars.current_with_state(1), (1, EOF, EOF, EOF));
        chars.move_next();
        assert_eq!(chars.current_with_state(123), (123, EOF, EOF, EOF));
        chars.move_next();
        chars.move_next();
        assert_eq!(chars.current_with_state(1024), (1024, EOF, EOF, EOF));
    }

    #[test]
    fn num_lit_f64_checked() {
        // use std::mem;

        if let FloatCheckedResult::Ok(myresult) = (-0.0001).checked_mul(10f64.powi(-200)) {
            let expect = -0.0001E-200;
            rational_eq!(Numeric::R64(myresult), Numeric::R64(expect));
        }
        // println!("{}", unsafe { mem::transmute::<f64, u64>(myresult) });
        // println!("{}", unsafe { mem::transmute::<f64, u64>(expect) });
        // assert_eq!(myresult, expect);
    }

    #[test]
    fn num_lit_feature() {

        macro_rules! make_err {
            ($msg:expr) => ((format!("{}, {}", strings::InvalidNumericLiteral, $msg), None));
            ($msg:expr, $help:expr) => ((format!("{}, {}", strings::InvalidNumericLiteral, $msg), Some($help.into())));
        }
        macro_rules! test_case {
            ($input:expr, $expect:expr) => {{
                assert!(rational_eq!(parse_impl($input.to_owned()).unwrap(), $expect));
            }};
            ($input:expr, err, $expect:expr) => {{
                assert_eq!(parse_impl($input.to_owned()), Err($expect));
            }};
        }

        // 64/99 reterr to be done
        // 18/45 make_err to be done, that means, many error not tested

        // normal i32
        test_case!("123", Numeric::I32(123));                                  // 1
        test_case!("1", Numeric::I32(1));                                      // 2
        test_case!("123456789", Numeric::I32(123456789));                      // 3
        test_case!("2147483647", Numeric::I32(2147483647));                    // 4
        // should not start with 0
        test_case!("0123", err, make_err!(
            strings::NumLitShouldNotStartWith0,
            strings::CStyleOctNumLitHelp));                                    // 5

        // 0s
        test_case!("0", Numeric::I32(0));                                      // 6
        test_case!("0u32", Numeric::U32(0));                                   // 7
        test_case!("0r32", Numeric::R32(0f32));                                // 8
        test_case!("0x0", Numeric::I32(0));                                    // 9
        test_case!("0o0u8", Numeric::U8(0));                                   // 10
        test_case!("0u", err, make_err!(
            strings::UnexpectedEOFInMaybeUnsignedIntPostfix));                 // 11
        test_case!("0r", err, make_err!(
            strings::UnexpectedEOFInMaybeFloatingPostfix));                    // 12
        test_case!("0i888", err, make_err!(
            strings::UnexpectedValueAfterMaybeSignedIntPostfix));              // 13
        test_case!("0r3210", err, make_err!(strings::UnexpectedNotEOF));       // 14
        // 1s
        test_case!("1", Numeric::I32(1));                                      // 15
        test_case!("1u32", Numeric::U32(1));                                   // 16
        test_case!("1r32", Numeric::R32(1f32));                                // 17
        test_case!("0x1", Numeric::I32(1));                                    // 18
        test_case!("0o1u8", Numeric::U8(1));                                   // 19

        // normal f64
        test_case!("1.0", Numeric::R64(1.0));                                  // 20
        test_case!("1.234", Numeric::R64(1.234));                              // 21
        test_case!("12345678901234567890.0",
            Numeric::R64(12345678901234567890.0f64));                          // 22
        // test_case!("1.78E308", Numeric::R64(1.79E308));                     // 23, too difficult to make it pass
        // test_case!("1.78E-308", Numeric::R64(1.79E-308));                   // 24, too difficult too
        // underflow
        test_case!("1.79E-2333", err, make_err!(
            strings::FloatPointUnderflow,
            strings::FloatPointUnderflowHelpMinValue[2]));                     // 25
        test_case!("1.79E2333", err, make_err!(
            strings::FloatPointOverflow,
            strings::FloatPointOverflowHelpMaxValue[2]));                      // 26

        // postfix for other integral
        test_case!("1u8", Numeric::U8(1));                                     // 27
        test_case!("234i16", Numeric::I16(234));                               // 28
        test_case!("18446744073709551615u64",
            Numeric::U64(18446744073709551615));                               // 29
        test_case!("100i8", Numeric::I8(100));                                 // 30
        test_case!("61234u16", Numeric::U16(61234));                           // 31
        test_case!("9223372036854775807i64",
            Numeric::I64(9223372036854775807));                                // 32
        test_case!("10223372036854775807i64", err, make_err!(
            strings::IntegralOverflow));                                       // 0, this overflow do not include help
        // overflow and downflow is error
        test_case!("256u8", err, make_err!(
            strings::IntegralOverflow,
            strings::IntegralOverflowHelpMaxValue[1]));                        // 33
        test_case!("100000i16", err, make_err!(
            strings::IntegralOverflow,
            strings::IntegralOverflowHelpMaxValue[2]));                        // 34
        test_case!("-800i8", err, make_err!(
            strings::IntegralUnderflow,
            strings::IntegralUnderflowHelpMinValue[0]));                       // 35

        // negative value
        test_case!("-0d123", Numeric::I32(-123));                              // 36
        test_case!("-123", Numeric::I32(-123));                                // 37
        test_case!("-30000i16", Numeric::I16(-30000));                         // 38
        test_case!("-123.456", Numeric::R64(-123.456));                        // 39
        test_case!("-123u8", err, make_err!(
            strings::NegativeOperatorOnUnsignedInt));                          // 40
        test_case!("-1u64", err, make_err!(
            strings::NegativeOperatorOnUnsignedInt));                          // 41

        // integral prefix
        test_case!("0xABCD", Numeric::I32(0xABCD));                            // 42
        test_case!("0xfedc", Numeric::I32(0xFEDC));                            // 43
        test_case!("0b101010", Numeric::I32(0b101010));                        // 44
        test_case!("-0o777", Numeric::I32(-0o777));                            // 44
        // invalid char
        test_case!("0xXXXX", err, make_err!(
            strings::InvalidCharInIntLiteral,
            strings::IntLiteralAllowedChars[3]));                              // 46
        test_case!("0b1234", err, make_err!(
            strings::InvalidCharInIntLiteral,
            strings::IntLiteralAllowedChars[0]));                              // 47
        test_case!("0daaaa", err, make_err!(
            strings::InvalidCharInIntLiteral,
            strings::IntLiteralAllowedChars[2]));                              // 48

        // floating point no prefix
        test_case!("0x123.0", err, make_err!(strings::DotInIntLiteral));       // 49
        test_case!("0b111.01", err, make_err!(strings::DotInIntLiteral));      // 50

        // auto expansion for no postfix
        test_case!("2147483645", Numeric::I32(2147483645));                    // 51
        test_case!("2147483648", Numeric::U32(2147483648));                    // 52, 2^31 - 1..2^32 expand to u32
        test_case!("4294967295", Numeric::U32(4294967295u32));                 // 53, 2^31 - 1..2^32 expand to u32,
        test_case!("4294967296", Numeric::I64(4294967296i64));                 // 54, 2^32..2^63 expand to i64
        test_case!("4333333333", Numeric::I64(4333333333i64));                 // 55, 2^32..2^63 expand to i64
        test_case!("9223372036854775807", Numeric::I64(9223372036854775807));  // 56, 2^32..2^63 expand to i64
        test_case!("9223372036854775808", Numeric::U64(9223372036854775808));  // 57, 2^63..2^64 expand to u64
        test_case!("18446744073709551615",
            Numeric::U64(18446744073709551615));                               // 58, 2^63..2^64 expand to u64
        test_case!("18446744073709551616",
            Numeric::R64(18446744073709551616f64));                            // 59, 2^64.. expand to f64
        // auto expansion for negative value
        test_case!("-2147483648", Numeric::I32(-2147483648));                  // 60
        test_case!("-2147483649", Numeric::I64(-2147483649));                  // 61
        test_case!("-9223372036854775808",
            Numeric::I64(-9223372036854775808));                               // 62
        test_case!("-9223372036854775809",
            Numeric::R64(-9223372036854775809f64));                            // 63

        // int with e is float
        test_case!("123e10", Numeric::R64(123E10));                            // 64
        test_case!("123E10", Numeric::R64(123E10));                            // 65
        test_case!("123E+10", Numeric::R64(123E10));                           // 66
        test_case!("123E-10", Numeric::R64(123E-10));                          // 67
        test_case!("123E-16", Numeric::R64(123E-16));                          // 68
        test_case!("123E+12", Numeric::R64(123E+12));                          // 69
        test_case!("123E17", Numeric::R64(123E17));                            // 70
        // e not with prefix or postfix, exp should be integer
        test_case!("0d123E-5", err, make_err!(strings::ExponentInIntLiteral)); // 71
        test_case!("0x123E-5", err, make_err!(strings::ExponentInIntLiteral)); // 72
        test_case!("123.456E789.0", err, make_err!(
            strings::FloatExponentFloat));                                     // 73
        // after e is a i32
        test_case!("123E12345678901", err, make_err!(
            strings::FloatPointOverflow,
            strings::FloatPointOverflowHelpMaxValue[2]));                      // 74
        test_case!("123E-12345678901", err, make_err!(
            strings::FloatPointUnderflow,
            strings::FloatPointUnderflowHelpMinValue[2]));                     // 75
        test_case!("1E200r32", err, make_err!(
            strings::FloatPointOverflow,
            strings::FloatPointOverflowHelpMaxValue[0]));                      // 0

        // decimal dot
        test_case!("123.456", Numeric::R64(123.456));                          // 76
        test_case!("123456.0", Numeric::R64(123456f64));                       // 77
        test_case!("0.123", Numeric::R64(0.123));                              // 78
        test_case!("0.0000000123", Numeric::R64(0.0000000123));                // 79
        test_case!("123.456E0", Numeric::R64(123.456E0));                      // 80, dddE0 is legal
        test_case!("123456.0E5", Numeric::R64(123456E5f64));                   // 81
        test_case!("0.123E-10", Numeric::R64(0.123E-10));                      // 82
        test_case!("0.0000000123E3", Numeric::R64(0.0000123));                 // 83
        test_case!("0.0001E-200", Numeric::R64(0.0001E-200));                  // 84
        test_case!("-0.0001E-200", Numeric::R64(-0.0001E-200));                // 85
        test_case!("123E5r32", Numeric::R32(123E5f32));                        // 86
        test_case!("0.123E-10r32", Numeric::R32(0.123E-10f32));                // 87
        test_case!("0.0000000123E3r64", Numeric::R64(0.0000123));              // 88
        test_case!("0.0001E-200r64", Numeric::R64(0.0001E-200f64));            // 89
        test_case!("-0.0001E-200r32", err, make_err!(
            strings::FloatPointUnderflow,
            strings::FloatPointUnderflowHelpMinValue[1]));                     // 90

        // cannot first or last char is dot
        test_case!(".123", err, make_err!(strings::DotAtHead));                // 91
        test_case!("0x.123", err, make_err!( strings::DotAtHead));             // 92
        test_case!("123.", err, make_err!(strings::DotAtEnd));                 // 93
        test_case!(".", err, make_err!(strings::EmptyLiteral));                // 94

        // multi dot
        test_case!("123.456.789", err, make_err!(strings::DotDouble));         // 95
        test_case!("123..456", err, make_err!(strings::DotDouble));            // 96

        test_case!("-0x8FFF_FFFF_FFFF_FFFF", err, make_err!(
            strings::IntegralOverflow));                                       // 97

        // underscore as separator, can before or after E
        test_case!("0b110_111_000_001u16", Numeric::U16(0b110111000001));      // 98
        test_case!("123_456_789u64", Numeric::U64(123456789));                 // 99
        test_case!("2147483651_u32", Numeric::U32(2147483651));                // 100
        test_case!("184_467_440_737_095_516_15_u64",
            Numeric::U64(18446744073709551615u64));                            // 101
        test_case!("1_2_3_4", Numeric::I32(1234));                             // 102
        test_case!("123_456_E_12", Numeric::R64(123456E12));                   // 103
        test_case!("123.4_5_6E1_23", Numeric::R64(123.456E123));               // 104
        test_case!("0.1_2_3_4_5_6E0", Numeric::R64(0.123456));                 // 105
        // underscore not at head, tail, not around dot
        test_case!("_1234", err, make_err!(strings::UnderscoreAtHead));        // 106
        test_case!("1_", err, make_err!(strings::UnderscoreAtEnd));            // 107
        test_case!("_", err, make_err!(strings::EmptyLiteral));                // 108
        test_case!("123_.5", err, make_err!(strings::UnderscoreArroundDot));   // 109
        test_case!("0._456", err, make_err!(strings::UnderscoreArroundDot));   // 110
        test_case!("0.123_", err, make_err!(strings::UnderscoreAtEnd));        // 111
        test_case!("0b_1110_1001", Numeric::I32(0b11101001));                  // 112
        test_case!("0x_1234_i64", Numeric::I64(0x1234));                       // 113
        // underscore not in postfix or prefix
        test_case!("0_xABCD", err, make_err!(
            strings::NumLitShouldNotStartWith0,
            strings::CStyleOctNumLitHelp));                                    // 114
        test_case!("0xABCDu6_4", err, make_err!(
            strings::UnderscoreInMaybeUnsignedIntPostfix));                    // 115
        test_case!("123i_32", err, make_err!(
            strings::UnderscoreInMaybeSignedIntPostfix));                      // 116
        // underscore no double
        test_case!("123__456", err, make_err!(strings::UnderscoreDouble));     // 117
        test_case!("0b__101100", err, make_err!(strings::UnderscoreDouble));   // 118

        // empty
        test_case!("0xu64", err, make_err!(strings::EmptyIntLiteral));         // 119
        test_case!("0br32", err, make_err!(
            strings::EmptyIntLiteral,
            strings::AndFloatPostfixInIntLiteral));                            // 120

        // strange postfix and prefix
        test_case!("0X123", err, make_err!(
            strings::NumLitShouldNotStartWith0,
            strings::IntegralPrefixIsLowerCase));                              // 121
        test_case!("001", err, make_err!(
            strings::NumLitShouldNotStartWith0,
            strings::CStyleOctNumLitHelp));                                    // 122
        test_case!("0u123", err, make_err!(
            strings::UnexpectedValueAfterMaybeUnsignedIntPostfix));            // 123
        test_case!("123u18", err, make_err!(
            strings::UnexpectedValueAfterMaybeUnsignedIntPostfix));            // 124
        test_case!("654321i1024", err, make_err!(
            strings::UnexpectedValueAfterMaybeSignedIntPostfix));              // 126
        test_case!("0x3f2048", Numeric::I32(0x3F2048));                        // 127

        // from some auto generated
        test_case!("22.58387167E-6", Numeric::R64(22.58387167E-6));

        // 0xE+FOO
        // 0xE
    }
}
