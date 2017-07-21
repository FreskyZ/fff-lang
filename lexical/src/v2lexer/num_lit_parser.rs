#![deny(overflowing_literals)]
///! fff-lang
///! numeric literal parser
// TODO: ' as seperator, multi seperator not supported, full test

// Syntax:
// NumericLiteral => IntegralLiteral | FloatPointLiteral
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
// FloatPointLiteral => [-] [Char0To9]+ '.' [Char0To9]+ [FloatPointExponent] [FloatPointPostfix]
//                         | [-] [Char0To9]+ [FloatPointExponent] FloatPointPostfix
// FloatPointPostfix = 'f32' | 'f64'
// FloatPointExponent => ['e' | 'E'] ['+' | '-'] [Char0To9]+
// 
// Char0To9 = '0'...'9'
// Char0Or1 = '0' | '1'
// Char0To7 = '0'...'7'
// Char0ToF = '0'...'9' | 'A'...'F' | 'a'...'f'

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
// ...let it go

use std::cell::Cell;

use codemap::Span;
use message::Message;
use message::MessageCollection;
use codemap::EOF_CHAR;

use super::error_strings;
use super::super::NumLitValue;

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

        macro_rules! some_char_to_char { ($ch: expr) => (if let Some(ch) = $ch { ch } else { EOF_CHAR }) }

        let current = some_char_to_char!(chars.next());
        let (next, nextnext) = if current != EOF_CHAR {
            let next = some_char_to_char!(chars.next());
            (next, if next != EOF_CHAR {
                some_char_to_char!(chars.next())
            } else {
                EOF_CHAR
            })
        } else {
            (EOF_CHAR, EOF_CHAR)
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

        if self.m_current != EOF_CHAR {
            self.m_current = self.m_next;
            if self.m_next != EOF_CHAR {
                self.m_next = self.m_nextnext;
                if self.m_nextnext != EOF_CHAR {
                    self.m_nextnext = if let Some(nextnext) = self.chars.next() { nextnext } else { EOF_CHAR };
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
        use std::f64;

        let rhs = rhs.into();
        if rhs < 0f64 {
            if f64::MIN - rhs >= self { FloatCheckedResult::Overflow } else { FloatCheckedResult::Ok(self + rhs) }
        } else {
            if f64::MAX - rhs <= self { FloatCheckedResult::Overflow } else { FloatCheckedResult::Ok(self + rhs) }
        }
    }
    #[inline]
    fn checked_sub<T: Into<f64>>(self, rhs: T) -> FloatCheckedResult {
        use std::f64;

        let rhs = rhs.into();
        if rhs < 0f64 {
            if f64::MAX + rhs >= self { FloatCheckedResult::Ok(self - rhs) } else { FloatCheckedResult::Overflow }
        } else {
            if f64::MIN + rhs <= self { FloatCheckedResult::Ok(self - rhs) } else { FloatCheckedResult::Overflow }
        }
    }
    #[inline]
    fn checked_mul<T: Into<f64>>(self, rhs: T) -> FloatCheckedResult {
        use std::f64;

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
        use std::f64;

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
fn str_to_num_lit_impl(raw: String, strpos: Span) -> Result<NumLitValue, Message> {
    use std::{ i8, u8, i16, u16, i32, u32, i64, u64, f32, f64 };

    if cfg!(feature = "trace_num_lit_parse") {
        println!("\nnum lit parsing: \"{}\" at {:?}", raw, strpos);
    }

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
        AfterE(f64, i32, bool, bool),       // value, exp (already with sign), exp is positive, prev is underscore
        ExpectInt(u32, u64, bool, bool),    // base (not with sign), current value, is positive, prev is underscore
        ExpectSignedIntPostfix(i64),
        ExpectUnsignedIntPostfix(u64),
        ExpectFloatPostfix(f64),
        ExpectEOF(NumLitValue),             // retval
    }

    let mut state = State::ReallyNothing;
    let mut chars = BufChars::new(raw.chars());
    
    // use with --features trace_num_lit_parse
    #[cfg(feature = "trace_num_lit_parse")] macro_rules! conv { ($id: expr, $new_state: expr) => ({ println!("    conv {}", $id); state = $new_state; }) }
    #[cfg(feature = "trace_num_lit_parse")] macro_rules! retok { ($id: expr, $ret_val: expr) => ({ println!("    retok {}", $id); return Ok($ret_val); }) }
    #[cfg(feature = "trace_num_lit_parse")] 
    macro_rules! reterr { 
        ($id: expr) => ({ 
            println!("    reterr {}", $id); 
            return Err(Message::new_by_str(error_strings::InvalidNumericLiteral, vec![(strpos, "")])); 
        });
        ($id: expr, $extra_msg: expr) => ({ 
            println!("    reterr {}", $id); 
            return Err(Message::new(
                format!("{}, {}", error_strings::InvalidNumericLiteral, $extra_msg),
                vec![(strpos, String::new())]
            )); 
        });
        ($id: expr, $extra_msg: expr, $help: expr) => ({ 
            println!("    reterr {}", $id); 
            return Err(Message::with_help(
                format!("{}, {}", error_strings::InvalidNumericLiteral, $extra_msg),
                vec![(strpos, String::new())],
                $help,
            )); 
        });
    }
    #[cfg(feature = "trace_num_lit_parse")] 
    macro_rules! reterr_internal {
        ($id: expr) => ({
            println!("    reterr_internal {}", $id);
            return Err(Message::new(
                format!("{}, {} {}", error_strings::InvalidNumericLiteral, error_strings::InternalErrorAt, line!()),
                vec![(strpos, String::new())],
            ))
        });
    }

    #[cfg(not(feature = "trace_num_lit_parse"))] macro_rules! conv { ($id: expr, $new_state: expr) => ({ state = $new_state; }) }
    #[cfg(not(feature = "trace_num_lit_parse"))] macro_rules! retok { ($id: expr, $ret_val: expr) => ({ return Ok($ret_val); }) }
    #[cfg(not(feature = "trace_num_lit_parse"))] 
    macro_rules! reterr { 
        ($id: expr) => ({ return Err(Message::new_by_str(error_strings::InvalidNumericLiteral, vec![(strpos, "")])); });
        ($id: expr, $extra_msg: expr) => ({
            return Err(Message::new(
                format!("{}, {}", error_strings::InvalidNumericLiteral, $extra_msg),
                vec![(strpos, String::new())]
            ));  
        });
        ($id: expr, $extra_msg: expr, $help: expr) => ({ 
            return Err(Message::with_help(
                format!("{}, {}", error_strings::InvalidNumericLiteral, $extra_msg),
                vec![(strpos, String::new())],
                $help,
            )); 
        });
    }
    #[cfg(not(feature = "trace_num_lit_parse"))] 
    macro_rules! reterr_internal {
        ($id: expr) => ({
            return Err(Message::new(
                format!("{}, {} {}", error_strings::InvalidNumericLiteral, error_strings::InternalErrorAt, line!()),
                vec![(strpos, String::new())],
            ))
        });
    }

    loop {
        chars.move_next();
        match chars.current_with_state(state) {

            // ---- ReallyNothing ----
            (State::ReallyNothing, '-', EOF_CHAR, _) => reterr!(1, error_strings::EmptyLiteral),
            (State::ReallyNothing, '_', EOF_CHAR, _) => reterr!(2, error_strings::EmptyLiteral),
            (State::ReallyNothing, '_', _, _) => reterr!(3, error_strings::UnderscoreAtHead),
            (State::ReallyNothing, '-', _, _) => conv!(1, State::Nothing(false)),
            (State::ReallyNothing, _, _, _) => {
                chars.skip1();
                conv!(2, State::Nothing(true)); 
            }

            // ---- Nothing(is_positive) ----
            (State::Nothing(_), '0', EOF_CHAR, _) => retok!(1, NumLitValue::I32(0)),
            (State::Nothing(_), '_', _, _) => reterr!(4, error_strings::UnderscoreAtHead),

            (State::Nothing(_), '0', 'i', _) => conv!(3, State::ExpectSignedIntPostfix(0i64)),
            (State::Nothing(_), '0', 'u', _) => conv!(4, State::ExpectUnsignedIntPostfix(0u64)),
            (State::Nothing(_), '0', 'f', _) => conv!(5, State::ExpectFloatPostfix(0f64)),

            (State::Nothing(_), '0', 'b', EOF_CHAR)
            | (State::Nothing(_), '0', 'o', EOF_CHAR)
            | (State::Nothing(_), '0', 'd', EOF_CHAR)
            | (State::Nothing(_), '0', 'B', EOF_CHAR) // although they are not int prefix, make it this error
            | (State::Nothing(_), '0', 'O', EOF_CHAR)
            | (State::Nothing(_), '0', 'D', EOF_CHAR)
            | (State::Nothing(_), '0', 'X', EOF_CHAR) => reterr!(5, error_strings::EmptyIntLiteral),           

            (State::Nothing(is_positive), '0', 'b', _) => {
                chars.dummy1();
                conv!(6, State::IntPrefix(2, is_positive, false));
            }
            (State::Nothing(is_positive), '0', 'o', _) => {
                chars.dummy1();
                conv!(7, State::IntPrefix(8, is_positive, false));
            }
            (State::Nothing(is_positive), '0', 'd', _) => {
                chars.dummy1();
                conv!(8, State::IntPrefix(10, is_positive, false));
            }
            (State::Nothing(is_positive), '0', 'x', _) => {
                chars.dummy1();
                conv!(9, State::IntPrefix(16, is_positive, false));
            }
            (State::Nothing(_), '0', 'B', _)
            | (State::Nothing(_), '0', 'O', _)
            | (State::Nothing(_), '0', 'D', _)
            | (State::Nothing(_), '0', 'X', _) => reterr!(6, error_strings::NumLitShouldNotStartWith0, 
                vec![error_strings::IntegralPrefixIsLowerCase.to_owned()]
            ),
            
            (State::Nothing(_), '0', '.', '_') => reterr!(7, error_strings::UnderscoreArroundDot),
            (State::Nothing(is_positive), '0', '.', _) => {
                chars.dummy1();
                conv!(10, State::AfterDot(0f64, 1, is_positive, false));                       
            }
            (State::Nothing(_), '0', _, _) => reterr!(8, error_strings::NumLitShouldNotStartWith0, 
                vec![error_strings::CStyleOctNumLitHelp.to_owned()]
            ),
            (State::Nothing(_), '.', EOF_CHAR, _) => reterr!(9, error_strings::EmptyLiteral),
            (State::Nothing(_), '.', _, _) => reterr!(10, error_strings::DotAtHead),
            (State::Nothing(is_positive), ch, _, _) => match ch.to_digit(10) {
                Some(digit) => conv!(11, State::UnknownI32((digit as i32).merge_sign(is_positive), is_positive, false)),
                None => reterr!(11, error_strings::InvalidChar),
            },

            // ---- UnknownI32(value, is_positive, prev_is_underscore) ----
            (State::UnknownI32(_, _, true), EOF_CHAR, _, _) => reterr!(12, error_strings::UnderscoreAtEnd),
            (State::UnknownI32(value, _, false), EOF_CHAR, _, _) => retok!(2, NumLitValue::I32(value)),
            (State::UnknownI32(value, _, _), 'i', _, _) => {
                chars.skip1();
                conv!(12, State::ExpectSignedIntPostfix(value as i64));
            },
            (State::UnknownI32(value, true, _), 'u', _, _) => {
                chars.skip1();
                conv!(13, State::ExpectUnsignedIntPostfix(value as u64));
            },
            (State::UnknownI32(_, false, _), 'u', _, _) => reterr!(13, error_strings::NegativeOperatorOnUnsignedInt),
            (State::UnknownI32(value, _, _), 'f', _, _) => {
                chars.skip1();
                conv!(14, State::ExpectFloatPostfix(value as f64));
            }
            (State::UnknownI32(value, _, _), 'e', _, _) 
            | (State::UnknownI32(value, _, _), 'E', _, _) => conv!(15, State::DirectAfterE(value as f64, false)),
            (State::UnknownI32(_, _, true), '.', _, _) => reterr!(14, error_strings::UnderscoreArroundDot),
            (State::UnknownI32(_, _, false), '.', '_', _) => reterr!(15, error_strings::UnderscoreArroundDot),
            (State::UnknownI32(value, is_positive, false), '.', _, _) =>
                conv!(16, State::AfterDot(value as f64, 1, is_positive, false)),
            (State::UnknownI32(value, is_positive, prev_is_underscore), ch, _, _) => match (prev_is_underscore, ch == '_') {
                (true, true) => reterr!(16, error_strings::UnderscoreDouble),
                (_, true) => conv!(17, State::UnknownI32(value, is_positive, true)),
                (_, false) => match ch.to_digit(10) {
                    None => reterr!(17, error_strings::InvalidChar),
                    Some(digit) => match (value.checked_mul(10), is_positive) {
                        (None, true) => { // positive i32 mul 10 overflow, that is (value >= 214748365 and value <= 21487483647)
                            let i64_value = value as i64 * 10i64 + digit as i64 ;   // these values won't overflow i64 anyway
                            if i64_value <= u32::MAX as i64 {                       // may not overflow u32
                                conv!(18, State::UnknownU32(i64_value as u32, false));
                            } else {
                                conv!(19, State::UnknownI64(i64_value, true, false));
                            }
                        }
                        (None, false) => { // negative i32 mul 10 overflow, that is (value <= -214748365 and value >= -2147483648)
                            let i64_value = value as i64 * 10i64 - digit as i64;    // these values won't overflow i64 anyway
                            conv!(20, State::UnknownI64(i64_value, false, false));
                        }
                        (Some(value), true) => match value.checked_add(digit as i32) {
                            None => conv!(21, State::UnknownU32(value as u32 + digit, false)),
                            Some(value) => conv!(22, State::UnknownI32(value, is_positive, false)),
                        },
                        (Some(value), false) => match value.checked_sub(digit as i32) {
                            None => conv!(23, State::UnknownI64(value as i64 - digit as i64, false, false)),
                            Some(value) => conv!(24, State::UnknownI32(value, is_positive, false)),
                        },
                    },
                },
            },

            // ---- UnknownU32(value, prev_is_underscore) ----
            (State::UnknownU32(_, true), EOF_CHAR, _, _) => reterr!(18, error_strings::UnderscoreAtEnd),
            (State::UnknownU32(value, false), EOF_CHAR, _, _) => retok!(3, NumLitValue::U32(value)),
            (State::UnknownU32(value, _), 'i', _, _) => {
                chars.skip1();
                conv!(25, State::ExpectSignedIntPostfix(value as i64));
            },
            (State::UnknownU32(value, _), 'u', _, _) => {
                chars.skip1();
                conv!(26, State::ExpectUnsignedIntPostfix(value as u64));
            },
            (State::UnknownU32(value, _), 'f', _, _) => {
                chars.skip1();
                conv!(27, State::ExpectFloatPostfix(value as f64));
            }
            (State::UnknownU32(value, _), 'e', _, _) 
            | (State::UnknownU32(value, _), 'E', _, _) => conv!(28, State::DirectAfterE(value as f64, false)),
            (State::UnknownU32(_, _), '.', '_', _) => reterr!(19, error_strings::UnderscoreArroundDot),
            (State::UnknownU32(_, true), '.', _, _) => reterr!(20, error_strings::UnderscoreArroundDot),
            (State::UnknownU32(value, false), '.', _, _) => conv!(29, State::AfterDot(value as f64, 1, true, false)),
            (State::UnknownU32(value, prev_is_underscore), ch, _, _) => match (prev_is_underscore, ch == '_') {
                (true, true) => reterr!(21, error_strings::UnderscoreDouble),
                (_, true) => conv!(30, State::UnknownU32(value, true)),
                (_, false) => match ch.to_digit(10) {
                    None => reterr!(22, error_strings::InvalidChar),
                    Some(digit) => match value.checked_mul(10) { 
                        None => conv!(31, State::UnknownI64(value as i64 * 10i64 + digit as i64, true, false)), // u32 mul 10 overflow must be positive i64
                        Some(value) => match value.checked_add(digit) {
                            None => conv!(32, State::UnknownI64(value as i64 + digit as i64, true, false)),     // u32 add digit overflow must be positive i64
                            Some(value) => conv!(33, State::UnknownU32(value, false)),                          // not overflow continue u32
                        },
                    },
                },
            },

            // ---- UnknownI64(value, is_positive, prev is underscore) ----
            (State::UnknownI64(_, _, true), EOF_CHAR, _, _) => reterr!(23, error_strings::UnderscoreAtEnd),
            (State::UnknownI64(value, _, false), EOF_CHAR, _, _) => retok!(4, NumLitValue::I64(value)),
            (State::UnknownI64(value, _, _), 'i', _, _) => {
                chars.skip1();
                conv!(34, State::ExpectSignedIntPostfix(value));
            },
            (State::UnknownI64(value, true, _), 'u', _, _) => {
                chars.skip1();
                conv!(35, State::ExpectUnsignedIntPostfix(value as u64));
            },
            (State::UnknownI64(_, false, _), 'u', _, _) => reterr!(24, error_strings::NegativeOperatorOnUnsignedInt),
            (State::UnknownI64(value, _, _), 'f', _, _) => {
                chars.skip1();
                conv!(36, State::ExpectFloatPostfix(value as f64));
            }
            (State::UnknownI64(value, _, _), 'e', _, _) 
            | (State::UnknownI64(value, _, _), 'E', _, _) => conv!(37, State::DirectAfterE(value as f64, false)),
            (State::UnknownI64(_, _, true), '.', _, _) => reterr!(25, error_strings::UnderscoreArroundDot),
            (State::UnknownI64(_, _, false), '.', '_', _) => reterr!(26, error_strings::UnderscoreArroundDot),
            (State::UnknownI64(value, is_positive, false), '.', _, _) => conv!(38, State::AfterDot(value as f64, 1, is_positive, false)),
            (State::UnknownI64(value, is_positive, prev_is_underscore), ch, _, _) => match (prev_is_underscore, ch == '_') {
                (true, true) => reterr!(27, error_strings::UnderscoreDouble),
                (_, true) => conv!(39, State::UnknownI64(value, is_positive, true)),
                (_, false) => match ch.to_digit(10) {
                    None => reterr!(28, error_strings::InvalidChar), 
                    Some(digit) => match (value.checked_mul(10i64), is_positive) {
                        (None, true) => {       // positive i64 mul 10 overflow, that is (value <= 9223372036854775808 and value >= 922337203685477581)
                            match (value as u64).checked_mul(10u64) {
                                None => {       // positive i64 as u64 mul 10 overflow, that is (value <= 9223372036854775808 and value >= 1844674407370955162)
                                    conv!(40, State::UnknownF64(value as f64 * 10f64 + digit as f64, is_positive));
                                }
                                Some(value) => match value.checked_add(digit as u64) {
                                    None => {   // positive i64 as u64 mul 10 not overflow add digit overflow, that is (value = 1844674407370955161 and digit >= 6)
                                        conv!(41, State::UnknownF64(value as f64 + digit as f64, is_positive));
                                    }
                                    Some(value) => conv!(42, State::UnknownU64(value, false)),
                                },
                            }
                        }
                        (None, false) => {      // negative i64 mul 10 overflow, that is (value >= -9223372036854775808 and value <= -922337203685477581)
                            match (value as u64).checked_mul(10u64) {
                                None => {       // negative i64 as u64 mul 10 overflow, that is (value >= -9223372036854775808 and value <= -1844674407370955162)
                                    conv!(43, State::UnknownF64(value as f64 * 10f64 - digit as f64, is_positive));
                                }
                                Some(value) => match value.checked_sub(digit as u64) {
                                    None => {   // negative i64 as u64 mul 10 not overflow add digit overflow, that is (value = -1844674407370955161 and digit >= 7)
                                        conv!(44, State::UnknownF64(value as f64 - digit as f64, is_positive));
                                    }
                                    Some(value) => conv!(45, State::UnknownU64(value, false)), 
                                },
                            }
                        }
                        (Some(value), true) => match value.checked_add(digit as i64) {
                            None => conv!(46, State::UnknownU64(value as u64 + digit as u64, false)),
                            Some(value) => conv!(47, State::UnknownI64(value, true, false)), 
                        },
                        (Some(value), false) => match value.checked_sub(digit as i64) {
                            None => conv!(48, State::UnknownF64(value as f64 - digit as f64, false)),
                            Some(value) => conv!(49, State::UnknownI64(value, false, false)),
                        },
                    },
                },
            },

            // ---- UnknownU64(value, prev_is_underscore) ----
            (State::UnknownU64(_, true), EOF_CHAR, _, _) => reterr!(29, error_strings::UnderscoreAtEnd),
            (State::UnknownU64(value, false), EOF_CHAR, _, _) => retok!(5, NumLitValue::U64(value)),
            (State::UnknownU64(_, _), 'i', _, _) => reterr!(30, error_strings::IntegralOverflow),
            (State::UnknownU64(value, _), 'u', _, _) => {
                chars.skip1();
                conv!(50, State::ExpectUnsignedIntPostfix(value));
            },
            (State::UnknownU64(value, _), 'f', _, _) => {
                chars.skip1();
                conv!(51, State::ExpectFloatPostfix(value as f64));
            }
            (State::UnknownU64(value, _), 'e', _, _) 
            | (State::UnknownU64(value, _), 'E', _, _) => conv!(52, State::DirectAfterE(value as f64, false)),
            (State::UnknownU64(_, true), '.', _, _) => reterr!(31, error_strings::UnderscoreArroundDot),
            (State::UnknownU64(_, false), '.', '_', _) => reterr!(32, error_strings::UnderscoreArroundDot),
            (State::UnknownU64(value, false), '.', _, _) => conv!(53, State::AfterDot(value as f64, 1, true, false)),
            (State::UnknownU64(value, prev_is_underscore), ch, _, _) => match (prev_is_underscore, ch == '_') {
                (true, true) => reterr!(33, error_strings::UnderscoreDouble),
                (_, true) => conv!(54, State::UnknownU64(value, true)),
                (_, false) => match ch.to_digit(10) {
                    None => reterr!(34, error_strings::InvalidChar),
                    Some(digit) => match value.checked_mul(10) { 
                        None => conv!(55, State::UnknownF64(value as f64 * 10f64 + digit as f64, true)),    // u64 mul 10 overflow must be positive f64
                        Some(value) => match value.checked_add(digit as u64) {
                            None => conv!(56, State::UnknownF64(value as f64 + digit as f64, true)),        // u64 add digit overflow must be positive f64
                            Some(value) => conv!(57, State::UnknownU64(value, false)),                      // not overflow continue u64
                        },
                    },
                },
            },

            // TODO: nned prev is underscore?
            // ---- UnknownF64(value, is_positive) ----
            (State::UnknownF64(value, _), EOF_CHAR, _, _) => retok!(6, NumLitValue::F64(value)),
            (State::UnknownF64(_, _), 'i', _, _) => reterr!(35, error_strings::IntegralOverflow),
            (State::UnknownF64(_, _), 'u', _, _) => reterr!(36, error_strings::IntegralOverflow),
            (State::UnknownF64(value, _), 'f', _, _) => {
                chars.skip1();
                conv!(58, State::ExpectFloatPostfix(value));
            }
            (State::UnknownF64(value, _), 'e', _, _) 
            | (State::UnknownF64(value, _), 'E', _, _) => conv!(59, State::DirectAfterE(value, false)),
            (State::UnknownF64(_, _), '.', '_', _) => reterr!(37, error_strings::UnderscoreArroundDot),
            (State::UnknownF64(value, is_positive), '.', _, _) => conv!(60, State::AfterDot(value, 1, is_positive, false)),
            (State::UnknownF64(value, is_positive), ch, _, _) => match ch.to_digit(10) {
                None => reterr!(38, error_strings::InvalidChar),
                Some(digit) => match value.checked_mul_add(10, if is_positive { digit as i32 } else { -(digit as i32) }) {
                    FloatCheckedResult::Ok(value) => conv!(61, State::UnknownF64(value, is_positive)),
                    FloatCheckedResult::Overflow => reterr!(39, error_strings::FloatPointOverflow),
                    FloatCheckedResult::Underflow => reterr!(40, error_strings::FloatPointUnderflow),
                },
            },

            // ---- IntPrefix(base, is_postive, prev_is_underscore) ----
            (State::IntPrefix(_, _, _), 'e', _, _) 
            | (State::IntPrefix(_, _, _), 'E', _, _) => reterr!(41, error_strings::ExponentInIntLiteral),
            (State::IntPrefix(_, _, _), '.', _, _) => reterr!(42, error_strings::DotAtHead),
            (State::IntPrefix(_, _, true), '_', _, _) => reterr!(43, error_strings::UnderscoreDouble),
            (State::IntPrefix(_, _, _), 'i', '8', EOF_CHAR)
            | (State::IntPrefix(_, _, _), 'i', '1', '6')
            | (State::IntPrefix(_, _, _), 'i', '3', '2')
            | (State::IntPrefix(_, _, _), 'i', '6', '4')
            | (State::IntPrefix(_, _, _), 'u', '8', EOF_CHAR)
            | (State::IntPrefix(_, _, _), 'u', '1', '6')
            | (State::IntPrefix(_, _, _), 'u', '3', '2')
            | (State::IntPrefix(_, _, _), 'u', '6', '4') => reterr!(44, error_strings::EmptyIntLiteral),
            (State::IntPrefix(_, _, _), 'f', '3', '2') 
            | (State::IntPrefix(_, _, _), 'f', '6', '4') => reterr!(45, error_strings::EmptyIntLiteral,
                vec![error_strings::AndFloatPostfixInIntLiteral.to_owned()]
            ),
            // u, i, f
            (State::IntPrefix(base, is_positive, false), '_', _, _) => conv!(62, State::IntPrefix(base, is_positive, true)),
            (State::IntPrefix(base, is_positive, _), ch, _, _) => match ch.to_digit(base) {
                Some(digit) => conv!(63, State::ExpectInt(base, digit as u64, is_positive, false)),
                None => reterr!(46, error_strings::InvalidCharInIntLiteral,
                    vec![error_strings::IntLiteralAllowedChars[
                        match base { 2 => 0, 8 => 1, 10 => 2, 16 => 3, _ => reterr_internal!(2) }
                    ].to_owned()]
                ),
            },

            // ---- ExpectInt(base, value, is_positive, prev_is_underscore) ----
            (State::ExpectInt(_, _, _, _), '.', _, _) => reterr!(47, error_strings::DotInIntLiteral),
            (State::ExpectInt(_, value, is_positive, _), EOF_CHAR, _, _) => retok!(7, u64_final_value(value, is_positive)),
            (State::ExpectInt(_, value, is_positive, _), 'i', _, _) => if value > i64::MAX as u64 {
                reterr!(48, error_strings::IntegralOverflow);
            } else {
                chars.skip1();
                conv!(64, State::ExpectSignedIntPostfix(if is_positive { value as i64 } else { -(value as i64) }));
            },
            (State::ExpectInt(_, value, true, _), 'u', _, _) => {
                chars.skip1();
                conv!(65, State::ExpectUnsignedIntPostfix(value));
            }
            (State::ExpectInt(_, _, false, _), 'u', _, _) => reterr!(49, error_strings::NegativeOperatorOnUnsignedInt),
            (State::ExpectInt(2, _, _, _), 'e', _, _) 
            | (State::ExpectInt(2, _, _, _), 'E', _, _)
            | (State::ExpectInt(8, _, _, _), 'e', _, _)     // because E is allowed in 16
            | (State::ExpectInt(8, _, _, _), 'E', _, _) 
            | (State::ExpectInt(10, _, _, _), 'e', _, _) 
            | (State::ExpectInt(10, _, _, _), 'E', _, _)
            | (State::ExpectInt(16, _, _, _), 'e', '+', _)  // because E is allowed in 16
            | (State::ExpectInt(16, _, _, _), 'E', '+', _) 
            | (State::ExpectInt(16, _, _, _), 'e', '-', _) 
            | (State::ExpectInt(16, _, _, _), 'E', '-', _) => reterr!(50, error_strings::ExponentInIntLiteral),
            (State::ExpectInt(base, value, is_positive, prev_is_underscore), ch, _, _) => match (prev_is_underscore, ch == '_') {
                (true, true) => reterr!(51, error_strings::UnderscoreDouble),
                (_, true) => conv!(66, State::ExpectInt(base, value, is_positive, true)),
                (_, false) => match ch.to_digit(base) {
                    None => reterr!(52, error_strings::InvalidCharInIntLiteral,
                        vec![error_strings::IntLiteralAllowedChars[
                            match base { 2 => 0, 8 => 1, 10 => 2, 16 => 3, _ => reterr_internal!(1) }
                        ].to_owned()]
                    ),
                    Some(digit) => match value.checked_mul(base as u64) {
                        None => reterr!(53, error_strings::IntegralOverflow), 
                        Some(value) => match value.checked_add(digit as u64) {
                            None => reterr!(54, error_strings::IntegralOverflow),
                            Some(value) => {
                                if !is_positive && value >= 9223372036854775808u64 {
                                    reterr!(55, error_strings::IntegralOverflow); 
                                } else {
                                    conv!(67, State::ExpectInt(base, value, is_positive, false));
                                }
                            }
                        },                       
                    },
                },
            },

            // ---- AfterDot(value, bits, is_positive, prev_is_underscore) ----
            (State::AfterDot(_, _, _, _), '.', _, _) => reterr!(56, error_strings::DotDouble),
            (State::AfterDot(_, _, _, _), 'i', _, _)
            | (State::AfterDot(_, _, _, _), 'u', _, _) => reterr!(57, error_strings::MaybeIntPostfixInFloatPoint),  
            (State::AfterDot(value, _, _, _), 'f', _, _) => {
                chars.skip1();
                conv!(68, State::ExpectFloatPostfix(value));
            }
            (State::AfterDot(value, _, _, _), 'e', _, _)
            | (State::AfterDot(value, _, _, _), 'E', _, _) => conv!(69, State::DirectAfterE(value, false)),
            (State::AfterDot(_, 1, _, _), EOF_CHAR, _, _) => reterr!(58, error_strings::DotAtEnd),
            (State::AfterDot(_, _, _, true), EOF_CHAR, _, _) => reterr!(59, error_strings::UnderscoreAtEnd),
            (State::AfterDot(value, _, _, false), EOF_CHAR, _, _) => retok!(8, NumLitValue::F64(value)),
            (State::AfterDot(value, bits, is_positive, prev_is_underscore), ch, _, _) => match (prev_is_underscore, ch == '_') {
                (true, true) => reterr!(60, error_strings::UnderscoreDouble),
                (_, true) => conv!(70, State::AfterDot(value, bits, is_positive, true)),
                (_, false) => match (ch.to_digit(10), is_positive) {
                    (None, _) => reterr!(61, error_strings::InvalidChar),
                    (Some(digit), true) => match value.checked_add(digit as f64 / 10f64.powi(bits)) {
                        FloatCheckedResult::Ok(value) => conv!(71, State::AfterDot(value, bits + 1, is_positive, false)),
                        FloatCheckedResult::Overflow => reterr!(62, error_strings::FloatPointOverflow),
                        FloatCheckedResult::Underflow => reterr!(63, error_strings::FloatPointUnderflow),
                    },                 
                    (Some(digit), false) => match value.checked_sub(digit as f64 / 10f64.powi(bits)) {
                        FloatCheckedResult::Ok(value) => conv!(72, State::AfterDot(value, bits + 1, is_positive, false)),
                        FloatCheckedResult::Overflow => reterr!(64, error_strings::FloatPointOverflow),
                        FloatCheckedResult::Underflow => reterr!(65, error_strings::FloatPointUnderflow),
                    }  
                },
            },

            // ---- DirectAfterE(value, prev_is_underscore) ----
            (State::DirectAfterE(_, _), '+', EOF_CHAR, _)
            | (State::DirectAfterE(_, _), '-', EOF_CHAR, _) => reterr!(66, error_strings::UnexpectedEOFInExponent),  
            (State::DirectAfterE(_, _), '+', '_', _) => reterr!(67, error_strings::UnderscoreAtExponentHead),
            (State::DirectAfterE(_, _), '-', '_', _) => reterr!(68, error_strings::UnderscoreAtExponentHead),
            (State::DirectAfterE(value, _), '+', ch, _) => match ch.to_digit(10) {
                None => reterr!(69, error_strings::InvalidChar),
                Some(digit) => {
                    chars.dummy1();
                    conv!(73, State::AfterE(value, digit as i32, true, false));
                }
            },
            (State::DirectAfterE(value, _), '-', ch, _) => match ch.to_digit(10) {
                None => reterr!(70, error_strings::InvalidChar),
                Some(digit) => {
                    chars.dummy1();
                    conv!(74, State::AfterE(value, -(digit as i32), false, false));
                }
            },
            (State::DirectAfterE(value, false), '_', _, _) => conv!(75, State::DirectAfterE(value, true)),
            (State::DirectAfterE(_, true), '_', _, _) => reterr!(71, error_strings::UnderscoreDouble),
            (State::DirectAfterE(value, _), ch, _, _) => match ch.to_digit(10) {
                None => reterr!(72, error_strings::InvalidChar),                        
                Some(digit) => conv!(76, State::AfterE(value, digit as i32, true, false)),
            },

            // ---- AfterE(value, exp, exp_is_positive, prev_is_underscore) ----
            (State::AfterE(_, _, _, _), 'u', _, _)
            | (State::AfterE(_, _, _, _), 'i', _, _) => reterr!(73, error_strings::MaybeIntPostfixInFloatPoint),
            (State::AfterE(_, _, _, _), '.', _, _)
            | (State::AfterE(_, _, _, _), 'e', _, _)
            | (State::AfterE(_, _, _, _), 'E', _, _) => reterr!(74, error_strings::FloatExponentFloat),
            (State::AfterE(value, exp, _, _), 'f', _, _) => match value.checked_mul(10f64.powi(exp)) {
                FloatCheckedResult::Overflow => reterr!(75, error_strings::FloatPointOverflow),
                FloatCheckedResult::Underflow => reterr!(76, error_strings::FloatPointUnderflow),
                FloatCheckedResult::Ok(value) => {
                    chars.skip1();
                    conv!(77, State::ExpectFloatPostfix(value));
                }
            },
            (State::AfterE(_, _, _, true), EOF_CHAR, _, _) => reterr!(77, error_strings::UnderscoreAtEnd),
            (State::AfterE(value, exp, _, false), EOF_CHAR, _, _) => match value.checked_mul(10f64.powi(exp)) {
                FloatCheckedResult::Ok(value) => retok!(9, NumLitValue::F64(value)),
                FloatCheckedResult::Overflow => reterr!(78, error_strings::FloatPointOverflow,
                    vec![error_strings::FloatPointOverflowHelpMaxValue[if value > 0f64 { 2 } else { 3 }].to_owned()]
                ),
                FloatCheckedResult::Underflow => reterr!(79, error_strings::FloatPointUnderflow, 
                    vec![error_strings::FloatPointUnderflowHelpMinValue[if value > 0f64 { 2 } else { 3 }].to_owned()]
                ),
            },
            (State::AfterE(value, exp, exp_is_positive, prev_is_underscore), ch, _, _) => match (prev_is_underscore, ch == '_') {
                (true, true) => reterr!(80, error_strings::UnderscoreDouble),
                (_, true) => conv!(78, State::AfterE(value, exp, exp_is_positive, true)),
                (_, false) => match ch.to_digit(10) {
                    None => reterr!(81, error_strings::InvalidChar),               
                    Some(digit) => match (exp.checked_mul(10), exp_is_positive) {
                        (None, true) => reterr!(82, 
                            error_strings::FloatPointOverflow,
                            vec![error_strings::FloatPointOverflowHelpMaxValue[if value > 0f64 { 2 } else { 3 }].to_owned()]
                        ),
                        (None, false) => reterr!(83, 
                            error_strings::FloatPointUnderflow,
                            vec![error_strings::FloatPointUnderflowHelpMinValue[if value > 0f64 { 2 } else { 3 }].to_owned()]
                        ),
                        (Some(exp), true) => match exp.checked_add(digit as i32) {
                            None => reterr!(84, error_strings::FloatPointUnderflow),
                            Some(exp) => conv!(79, State::AfterE(value, exp, true, false)),
                        },
                        (Some(exp), false) => match exp.checked_sub(digit as i32) {
                            None => reterr!(85, error_strings::FloatPointUnderflow),                                                                                         // LAST RETERR
                            Some(exp) => conv!(80, State::AfterE(value, exp, false, false)),
                        }
                    }
                },
            },

            // ---- ExpectSignedIntPostfix(value) ----
            (State::ExpectSignedIntPostfix(value), 'i', '8', EOF_CHAR) => 
                if value > i8::MAX as i64 { 
                    reterr!(86, error_strings::IntegralOverflow,
                        vec![error_strings::IntegralOverflowHelpMaxValue[0].to_owned()]
                    );
                } else if value < i8::MIN as i64 {
                    reterr!(87, error_strings::IntegralUnderflow,
                        vec![error_strings::IntegralUnderflowHelpMinValue[0].to_owned()]
                    );
                } else {
                    retok!(10, NumLitValue::I8(value as i8));
                },
            (State::ExpectSignedIntPostfix(value), 'i', '1', '6') => 
                if value > i16::MAX as i64 { 
                    reterr!(88, error_strings::IntegralOverflow,
                        vec![error_strings::IntegralOverflowHelpMaxValue[2].to_owned()]
                    );
                } else if value < i64::MIN as i64 {
                    reterr!(89, error_strings::IntegralUnderflow,
                        vec![error_strings::IntegralUnderflowHelpMinValue[1].to_owned()]
                    );
                } else {
                    chars.dummy1();
                    chars.dummy1();
                    conv!(81, State::ExpectEOF(NumLitValue::I16(value as i16))); 
                },
            (State::ExpectSignedIntPostfix(value), 'i', '3', '2') => 
                if value > i32::MAX as i64 { 
                    reterr!(90, error_strings::IntegralOverflow,
                        vec![error_strings::IntegralOverflowHelpMaxValue[5].to_owned()]
                    );
                } else if value < i32::MIN as i64 {
                    reterr!(91, error_strings::IntegralUnderflow,
                        vec![error_strings::IntegralUnderflowHelpMinValue[1].to_owned()]
                    );
                } else {
                    chars.dummy1();
                    chars.dummy1();
                    conv!(82, State::ExpectEOF(NumLitValue::I32(value as i32))); 
                },
            (State::ExpectSignedIntPostfix(value), 'i', '6', '4') => {
                chars.dummy1();
                chars.dummy1();
                conv!(83, State::ExpectEOF(NumLitValue::I64(value as i64)));
            }
            (State::ExpectSignedIntPostfix(_), 'i', EOF_CHAR, _) => reterr!(92, error_strings::UnexpectedEOFInMaybeSignedIntPostfix),
            (State::ExpectSignedIntPostfix(_), 'i', '_', _)
            | (State::ExpectSignedIntPostfix(_), 'i', _, '_') => reterr!(93, error_strings::UnderscoreInMaybeSignedIntPostfix),
            (State::ExpectSignedIntPostfix(_), _, _, _) => reterr!(94, error_strings::UnexpectedValueAfterMaybeSignedIntPostfix),

            // ---- ExpectUnsignedIntPostfix(value) ---- 
            (State::ExpectUnsignedIntPostfix(value), 'u', '8', EOF_CHAR) => 
                if value > u8::max_value() as u64 { 
                    reterr!(95, error_strings::IntegralOverflow,
                        vec![error_strings::IntegralOverflowHelpMaxValue[1].to_owned()]
                    ); 
                } else {
                    retok!(11, NumLitValue::U8(value as u8));
                },
            (State::ExpectUnsignedIntPostfix(value), 'u', '1', '6') => 
                if value > u16::max_value() as u64 { 
                    reterr!(96, error_strings::IntegralOverflow,
                        vec![error_strings::IntegralOverflowHelpMaxValue[3].to_owned()]
                    );
                } else {
                    chars.dummy1();
                    chars.dummy1();
                    conv!(84, State::ExpectEOF(NumLitValue::U16(value as u16))); 
                },
            (State::ExpectUnsignedIntPostfix(value), 'u', '3', '2') => 
                if value > u32::max_value() as u64 { 
                    reterr!(97, error_strings::IntegralOverflow,
                        vec![error_strings::IntegralOverflowHelpMaxValue[5].to_owned()]
                    );
                } else { 
                    chars.dummy1();
                    chars.dummy1();
                    conv!(85, State::ExpectEOF(NumLitValue::U32(value as u32))); 
                },
            (State::ExpectUnsignedIntPostfix(value), 'u', '6', '4') => {
                chars.dummy1();
                chars.dummy1();
                conv!(86, State::ExpectEOF(NumLitValue::U64(value))); 
            }
            (State::ExpectUnsignedIntPostfix(_), 'u', EOF_CHAR, _) => reterr!(98, error_strings::UnexpectedEOFInMaybeUnsignedIntPostfix),
            (State::ExpectUnsignedIntPostfix(_), 'u', '_', _)
            | (State::ExpectUnsignedIntPostfix(_), 'u', _, '_') => reterr!(99, error_strings::UnderscoreInMaybeUnsignedIntPostfix),
            (State::ExpectUnsignedIntPostfix(_), _, _, _) => reterr!(100, error_strings::UnexpectedValueAfterMaybeUnsignedIntPostfix),

            // ---- ExpectFloatPostfix(value) ----
            (State::ExpectFloatPostfix(value), 'f', '3', '2') => {
                if value > f32::MAX as f64 { 
                    reterr!(101, error_strings::FloatPointOverflow,
                        vec![error_strings::FloatPointOverflowHelpMaxValue[0].to_owned()]
                    );
                } else if value < -f32::MAX as f64 {
                    reterr!(102, error_strings::FloatPointOverflow,
                        vec![error_strings::FloatPointOverflowHelpMaxValue[1].to_owned()]
                    );
                } else if value == 0f32 as f64 {
                    chars.dummy1();
                    chars.dummy1();
                    conv!(87, State::ExpectEOF(NumLitValue::F32(0f32)));
                } else if value < f32::MIN_POSITIVE as f64 && value > 0f64 {
                    reterr!(103, error_strings::FloatPointUnderflow,
                        vec![error_strings::FloatPointUnderflowHelpMinValue[0].to_owned()]
                    );
                } else if value > -f32::MIN_POSITIVE as f64 && value < 0f64 {
                    reterr!(104, error_strings::FloatPointUnderflow,
                        vec![error_strings::FloatPointUnderflowHelpMinValue[1].to_owned()]
                    );
                } else {
                    chars.dummy1();
                    chars.dummy1();
                    conv!(88, State::ExpectEOF(NumLitValue::F32(value as f32))); 
                }
            }
            (State::ExpectFloatPostfix(value), 'f', '6', '4') => {
                chars.dummy1();
                chars.dummy1();
                conv!(89, State::ExpectEOF(NumLitValue::F64(value)));
            }
            (State::ExpectFloatPostfix(_), 'f', EOF_CHAR, _) => 
                reterr!(105, error_strings::UnexpectedEOFInMaybeFloatingPostfix),
            (State::ExpectFloatPostfix(_), 'u', '_', _)
            | (State::ExpectFloatPostfix(_), 'u', _, '_') => reterr!(106, error_strings::UnderscoreInMaybeFloatPointPostfix),
            (State::ExpectFloatPostfix(_), _, _, _) => 
                reterr!(107, error_strings::UnexpectedValueAfterMaybeFloatingPostfix),

            // ---- ExpectEOF(value) ---- 
            (State::ExpectEOF(ret_val), EOF_CHAR, _, _) => retok!(12, ret_val),
            (State::ExpectEOF(_), _, _, _) => reterr!(108, error_strings::UnexpectedNotEOF),
        }
    }
}

pub fn parse_numeric_literal(raw: String, pos: Span, messages: &mut MessageCollection) -> (Option<NumLitValue>, Span) {
    
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
    assert_eq!(chars.current_with_state(1), (1, EOF_CHAR, EOF_CHAR, EOF_CHAR));
    chars.move_next();
    assert_eq!(chars.current_with_state(1), (1, EOF_CHAR, EOF_CHAR, EOF_CHAR));
    chars.move_next();
    assert_eq!(chars.current_with_state(123), (123, EOF_CHAR, EOF_CHAR, EOF_CHAR));
    chars.move_next();
    chars.move_next();
    assert_eq!(chars.current_with_state(1024), (1024, EOF_CHAR, EOF_CHAR, EOF_CHAR));
}

#[cfg(test)]
#[test]
fn num_lit_f64_checked() {
    // use std::mem;
    
    if let FloatCheckedResult::Ok(myresult) = (-0.0001).checked_mul(10f64.powi(-200)) {
        let expect = -0.0001E-200;
        assert_eq!(NumLitValue::F64(myresult), NumLitValue::F64(expect));
    }
    // println!("{}", unsafe { mem::transmute::<f64, u64>(myresult) });
    // println!("{}", unsafe { mem::transmute::<f64, u64>(expect) });
    // assert_eq!(myresult, expect);
}

#[cfg(test)]
#[test]
#[allow(unused_variables)]
fn num_lit_feature() {

    let strpos = make_span!(2, 4);
    let refposinfo = vec![(strpos, "")];
    let posinfo = vec![(strpos, String::new())];

    macro_rules! make_err { 
        () => (Message::new_by_str(error_strings::InvalidNumericLiteral, vec![(strpos, "")]));
        ($msg2: expr) => (Message::new(
            format!("{}, {}", error_strings::InvalidNumericLiteral, $msg2), 
            vec![(strpos, String::new())],
        ));
        ($msg2: expr, $help: expr) => (Message::with_help(
            format!("{}, {}", error_strings::InvalidNumericLiteral, $msg2), 
            vec![(strpos, String::new())],
            $help,
        ));
        (raw, $msg: expr) => ($msg);
    }

    #[cfg(feature = "trace_num_lit_parse")]
    macro_rules! test_case {
        ($input: expr, $expect: expr) => (
            print!("\ncase {:?} at {}:", $input, line!());
            assert_eq!(str_to_num_lit_impl($input.to_owned(), strpos), Ok(NumLitValue::from($expect)));
        );
        ($input: expr, err, $expect: expr) => (
            print!("\ncase {:?} at {}:", $input, line!());
            assert_eq!(str_to_num_lit_impl($input.to_owned(), strpos), Err($expect));
        )
    }
    #[cfg(not(feature = "trace_num_lit_parse"))]
    macro_rules! test_case {
        ($input: expr, $expect: expr) => (
            assert_eq!(str_to_num_lit_impl($input.to_owned(), strpos), Ok(NumLitValue::from($expect)));
        );
        ($input: expr, err, $expect: expr) => (
            assert_eq!(str_to_num_lit_impl($input.to_owned(), strpos), Err($expect));
        )
    }
    
    // 64/99 reterr to be done
    // 18/45 make_err to be done, that means, many error not tested

    // normal i32
    test_case!("123", NumLitValue::I32(123));                       // 1
    test_case!("1", NumLitValue::I32(1));                           // 2
    test_case!("123456789", NumLitValue::I32(123456789));           // 3
    test_case!("2147483647", NumLitValue::I32(2147483647));         // 4
    // should not start with 0
    test_case!("0123", err, make_err!(
        error_strings::NumLitShouldNotStartWith0, 
        vec![error_strings::CStyleOctNumLitHelp.to_owned()]
    ));                                                             // 5

    // 0s
    test_case!("0", NumLitValue::I32(0));                           // 6
    test_case!("0u32", NumLitValue::U32(0));                        // 7
    test_case!("0f32", NumLitValue::F32(0f32));                     // 8
    test_case!("0x0", NumLitValue::I32(0));                         // 9
    test_case!("0o0u8", NumLitValue::U8(0));                        // 10
    test_case!("0u", err, make_err!(
        error_strings::UnexpectedEOFInMaybeUnsignedIntPostfix));    // 11
    test_case!("0f", err, make_err!(
        error_strings::UnexpectedEOFInMaybeFloatingPostfix));       // 12
    test_case!("0i888", err, make_err!(
        error_strings::UnexpectedValueAfterMaybeSignedIntPostfix)); // 13
    test_case!("0f3210", err, make_err!(
        error_strings::UnexpectedNotEOF));                          // 14
    // 1s
    test_case!("1", NumLitValue::I32(1));                           // 15
    test_case!("1u32", NumLitValue::U32(1));                        // 16
    test_case!("1f32", NumLitValue::F32(1f32));                     // 17
    test_case!("0x1", NumLitValue::I32(1));                         // 18
    test_case!("0o1u8", NumLitValue::U8(1));                        // 19

    // normal f64
    test_case!("1.0", NumLitValue::F64(1.0));                       // 20
    test_case!("1.234", NumLitValue::F64(1.234));                   // 21
    test_case!("12345678901234567890.0", 
        NumLitValue::F64(12345678901234567890.0f64));               // 22
    // test_case!("1.78E308", NumLitValue::F64(1.79E308));          // 23, too difficult to make it pass
    // test_case!("1.78E-308", NumLitValue::F64(1.79E-308));        // 24, too difficult too
    // smallflow
    test_case!("1.79E-2333", err, make_err!(
        error_strings::FloatPointUnderflow,
        vec![error_strings::FloatPointUnderflowHelpMinValue[2].to_owned()]
    ));                                                             // 25
    test_case!("1.79E2333", err, make_err!(
        error_strings::FloatPointOverflow, 
        vec![error_strings::FloatPointOverflowHelpMaxValue[2].to_owned()]
    ));                                                             // 26

    // postfix for other integral
    test_case!("1u8", NumLitValue::U8(1));                          // 27
    test_case!("234i16", NumLitValue::I16(234));                    // 28
    test_case!("18446744073709551615u64", 
        NumLitValue::U64(18446744073709551615));                    // 29
    test_case!("100i8", NumLitValue::I8(100));                      // 30
    test_case!("61234u16", NumLitValue::U16(61234));                // 31
    test_case!("9223372036854775807i64", 
        NumLitValue::I64(9223372036854775807));                     // 32
    test_case!("10223372036854775807i64", err, make_err!(
        error_strings::IntegralOverflow));                          // 0, this overflow do not include help
    // overflow and downflow is error
    test_case!("256u8", err, make_err!(
        error_strings::IntegralOverflow,
        vec![error_strings::IntegralOverflowHelpMaxValue[1].to_owned()]
    ));                                                             // 33
    test_case!("100000i16", err, make_err!(
        error_strings::IntegralOverflow,
        vec![error_strings::IntegralOverflowHelpMaxValue[2].to_owned()]
    ));                                                             // 34
    test_case!("-800i8", err, make_err!(
        error_strings::IntegralUnderflow,
        vec![error_strings::IntegralUnderflowHelpMinValue[0].to_owned()]
    ));                                                             // 35

    // negative value
    test_case!("-0d123", NumLitValue::I32(-123));                   // 36
    test_case!("-123", NumLitValue::I32(-123));                     // 37
    test_case!("-30000i16", NumLitValue::I16(-30000));              // 38
    test_case!("-123.456", NumLitValue::F64(-123.456));             // 39
    test_case!("-123u8", err, make_err!(
        error_strings::NegativeOperatorOnUnsignedInt));             // 40
    test_case!("-1u64", err, make_err!(
        error_strings::NegativeOperatorOnUnsignedInt));             // 41
    
    // integral prefix
    test_case!("0xABCD", NumLitValue::I32(0xABCD));                 // 42
    test_case!("0xfedc", NumLitValue::I32(0xFEDC));                 // 43
    test_case!("0b101010", NumLitValue::I32(0b101010));             // 44
    test_case!("-0o777", NumLitValue::I32(-0o777));                 // 44
    // invalid char
    test_case!("0xXXXX", err, make_err!(
        error_strings::InvalidCharInIntLiteral,
        vec![error_strings::IntLiteralAllowedChars[3].to_owned()]
    ));                                                             // 46
    test_case!("0b1234", err, make_err!(
        error_strings::InvalidCharInIntLiteral,
        vec![error_strings::IntLiteralAllowedChars[0].to_owned()]
    ));                                                             // 47
    test_case!("0daaaa", err, make_err!(
        error_strings::InvalidCharInIntLiteral,
        vec![error_strings::IntLiteralAllowedChars[2].to_owned()]
    ));                                                             // 48

    // floating point no prefix 
    test_case!("0x123.0", err, make_err!(
        error_strings::DotInIntLiteral));                           // 49
    test_case!("0b111.01", err, make_err!(
        error_strings::DotInIntLiteral));                           // 50

    // auto expansion for no postfix
    test_case!("2147483645", NumLitValue::I32(2147483645));                         // 51
    test_case!("2147483648", NumLitValue::U32(2147483648));                         // 52, 2^31 - 1..2^32 expand to u32
    test_case!("4294967295", NumLitValue::U32(4294967295u32));                      // 53, 2^31 - 1..2^32 expand to u32,
    test_case!("4294967296", NumLitValue::I64(4294967296i64));                      // 54, 2^32..2^63 expand to i64
    test_case!("4333333333", NumLitValue::I64(4333333333i64));                      // 55, 2^32..2^63 expand to i64
    test_case!("9223372036854775807", NumLitValue::I64(9223372036854775807));       // 56, 2^32..2^63 expand to i64
    test_case!("9223372036854775808", NumLitValue::U64(9223372036854775808));       // 57, 2^63..2^64 expand to u64
    test_case!("18446744073709551615", NumLitValue::U64(18446744073709551615));     // 58, 2^63..2^64 expand to u64
    test_case!("18446744073709551616", NumLitValue::F64(18446744073709551616f64));  // 59, 2^64.. expand to f64
    // auto expansion for negative value
    test_case!("-2147483648", NumLitValue::I32(-2147483648));                       // 60
    test_case!("-2147483649", NumLitValue::I64(-2147483649));                       // 61
    test_case!("-9223372036854775808", NumLitValue::I64(-9223372036854775808i64));  // 62
    test_case!("-9223372036854775809", NumLitValue::F64(-9223372036854775809f64));  // 63

    // int with e is float
    test_case!("123e10", NumLitValue::F64(123E10));                 // 64
    test_case!("123E10", NumLitValue::F64(123E10));                 // 65
    test_case!("123E+10", NumLitValue::F64(123E10));                // 66
    test_case!("123E-10", NumLitValue::F64(123E-10));               // 67
    test_case!("123E-16", NumLitValue::F64(123E-16));               // 68
    test_case!("123E+12", NumLitValue::F64(123E+12));               // 69
    test_case!("123E17", NumLitValue::F64(123E17));                 // 70
    // e not with prefix or postfix, exp should be integer
    test_case!("0d123E-5", err, make_err!(
        error_strings::ExponentInIntLiteral));                   // 71
    test_case!("0x123E-5", err, make_err!(
        error_strings::ExponentInIntLiteral));                   // 72
    test_case!("123.456E789.0", err, make_err!(
        error_strings::FloatExponentFloat));                     // 73
    // after e is a i32
    test_case!("123E12345678901", err, make_err!(
        error_strings::FloatPointOverflow, 
        vec![error_strings::FloatPointOverflowHelpMaxValue[2].to_owned()]
    ));                                                             // 74
    test_case!("123E-12345678901", err, make_err!(
        error_strings::FloatPointUnderflow,
        vec![error_strings::FloatPointUnderflowHelpMinValue[2].to_owned()]
    ));                                                             // 75
    test_case!("1E200f32", err, make_err!(
        error_strings::FloatPointOverflow,
        vec![error_strings::FloatPointOverflowHelpMaxValue[0].to_owned()]
    ));                                                             // 0

    // decimal dot
    test_case!("123.456", NumLitValue::F64(123.456));               // 76
    test_case!("123456.0", NumLitValue::F64(123456f64));            // 77
    test_case!("0.123", NumLitValue::F64(0.123));                   // 78
    test_case!("0.0000000123", NumLitValue::F64(0.0000000123));     // 79
    test_case!("123.456E0", NumLitValue::F64(123.456E0));           // 80, dddE0 is legal
    test_case!("123456.0E5", NumLitValue::F64(123456E5f64));        // 81
    test_case!("0.123E-10", NumLitValue::F64(0.123E-10));           // 82
    test_case!("0.0000000123E3", NumLitValue::F64(0.0000123));      // 83
    test_case!("0.0001E-200", NumLitValue::F64(0.0001E-200));       // 84
    test_case!("-0.0001E-200", NumLitValue::F64(-0.0001E-200));     // 85
    test_case!("123E5f32", NumLitValue::F32(123E5f32));             // 86
    test_case!("0.123E-10f32", NumLitValue::F32(0.123E-10f32));     // 87 
    test_case!("0.0000000123E3f64", NumLitValue::F64(0.0000123));   // 88
    test_case!("0.0001E-200f64", NumLitValue::F64(0.0001E-200f64)); // 89
    test_case!("-0.0001E-200f32", err, make_err!(
        error_strings::FloatPointUnderflow,
        vec![error_strings::FloatPointUnderflowHelpMinValue[1].to_owned()]
    ));                                                             // 90

    // cannot first or last char is dot
    test_case!(".123", err, make_err!(
        error_strings::DotAtHead));                                 // 91
    test_case!("0x.123", err, make_err!(
        error_strings::DotAtHead));                                 // 92
    test_case!("123.", err, make_err!(
        error_strings::DotAtEnd));                                  // 93
    test_case!(".", err, make_err!(
        error_strings::EmptyLiteral));                              // 94

    // multi dot
    test_case!("123.456.789", err, make_err!(
        error_strings::DotDouble));                                 // 95
    test_case!("123..456", err, make_err!(
        error_strings::DotDouble));                                 // 96

    test_case!("-0x8FFF_FFFF_FFFF_FFFF", err, make_err!(
        error_strings::IntegralOverflow));                          // 97

    // underscore as sepreator, can before or after E
    test_case!("0b110_111_000_001u16", 
        NumLitValue::U16(0b110111000001));                          // 98
    test_case!("123_456_789u64", NumLitValue::U64(123456789));      // 99
    test_case!("2147483651_u32", NumLitValue::U32(2147483651));     // 100
    test_case!("184_467_440_737_095_516_15_u64", 
        NumLitValue::U64(18446744073709551615u64));                 // 101
    test_case!("1_2_3_4", NumLitValue::I32(1234));                  // 102
    test_case!("123_456_E_12", NumLitValue::F64(123456E12));        // 103
    test_case!("123.4_5_6E1_23", NumLitValue::F64(123.456E123));    // 104
    test_case!("0.1_2_3_4_5_6E0", NumLitValue::F64(0.123456));      // 105
    // underscore not at head, tail, not around dot
    test_case!("_1234", err, make_err!(
        error_strings::UnderscoreAtHead));                          // 106
    test_case!("1_", err, make_err!(
        error_strings::UnderscoreAtEnd));                           // 107
    test_case!("_", err, make_err!(
        error_strings::EmptyLiteral));                              // 108
    test_case!("123_.5", err, make_err!(
        error_strings::UnderscoreArroundDot));                      // 109
    test_case!("0._456", err, make_err!(
        error_strings::UnderscoreArroundDot));                      // 110
    test_case!("0.123_", err, make_err!(
        error_strings::UnderscoreAtEnd));                           // 111
    test_case!("0b_1110_1001", NumLitValue::I32(0b11101001));       // 112
    test_case!("0x_1234_i64", NumLitValue::I64(0x1234));            // 113
    // underscore not in postfix or prefix
    test_case!("0_xABCD", err, make_err!(
        error_strings::NumLitShouldNotStartWith0, 
        vec![error_strings::CStyleOctNumLitHelp.to_owned()]
    ));                                                             // 114
    test_case!("0xABCDu6_4", err, make_err!(
        error_strings::UnderscoreInMaybeUnsignedIntPostfix));       // 115
    test_case!("123i_32", err, make_err!(
        error_strings::UnderscoreInMaybeSignedIntPostfix));         // 116
    // underscore no double
    test_case!("123__456", err, make_err!(
        error_strings::UnderscoreDouble));                          // 117
    test_case!("0b__101100", err, make_err!(
        error_strings::UnderscoreDouble));                          // 118

    // empty
    test_case!("0xu64", err, make_err!(
        error_strings::EmptyIntLiteral));                           // 119
    test_case!("0bf32", err, make_err!(
        error_strings::EmptyIntLiteral,
        vec![error_strings::AndFloatPostfixInIntLiteral.to_owned()]
    ));                                                             // 120

    // strange postfix and prefix
    test_case!("0X123", err, make_err!(
        error_strings::NumLitShouldNotStartWith0, 
        vec![error_strings::IntegralPrefixIsLowerCase.to_owned()]
    ));                                                             // 121
    test_case!("001", err, make_err!(
        error_strings::NumLitShouldNotStartWith0,
        vec![error_strings::CStyleOctNumLitHelp.to_owned()]
    ));                                                             // 122
    test_case!("0u123", err, make_err!(
        error_strings::UnexpectedValueAfterMaybeUnsignedIntPostfix
    ));                                                             // 123
    test_case!("123u18", err, make_err!(
        error_strings::UnexpectedValueAfterMaybeUnsignedIntPostfix
    ));                                                             // 124
    test_case!("654321i1024", err, make_err!(
        error_strings::UnexpectedValueAfterMaybeSignedIntPostfix)); // 126
    test_case!("0x3f2048", NumLitValue::I32(0x3F2048));             // 127
}