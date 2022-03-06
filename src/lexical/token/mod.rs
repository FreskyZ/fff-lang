///! fff-lang
///!
///! lexical/token

use std::fmt;
use crate::source::IsId;

#[cfg(test)]
mod tests;

mod keyword;
mod separator;
mod num_lit_value;

pub use keyword::{Keyword, KeywordKind};
pub use separator::{Separator, SeparatorKind};
pub use num_lit_value::NumLitValue;

#[derive(Clone, Eq, PartialEq)]
pub enum LitValue {
    Str(Option<IsId>),
    Num(Option<NumLitValue>),
    Char(Option<char>),
    Bool(bool),
}
impl fmt::Debug for LitValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LitValue::Str(Some(ref val)) => write!(f, "{:?}", val),
            LitValue::Str(None) => write!(f, "\"<invalid>\""),
            LitValue::Char(Some(ref val)) => write!(f, "{:?}", val),
            LitValue::Char(None) => write!(f, "'<invalid>'"),
            LitValue::Num(Some(ref val)) => write!(f, "{:?}", val),
            LitValue::Num(None) => write!(f, "<invalid-num>"),
            LitValue::Bool(val) => write!(f, "{}", val),
        }
    }
}

// from
impl From<char> for LitValue { fn from(val: char) -> LitValue { LitValue::Char(Some(val)) } }
impl From<bool> for LitValue { fn from(val: bool) -> LitValue { LitValue::Bool(val) } }
impl From<IsId> for LitValue { fn from(val: IsId) -> LitValue { LitValue::Str(Some(val)) } }

#[cfg(test)]
impl LitValue {
    pub fn new_str_lit_simple(sid: IsId) -> LitValue { LitValue::Str(Some(sid)) }
    pub fn new_str_lit_simple_usize(sid: u32) -> LitValue { LitValue::Str(Some(IsId::new(sid))) }
}

macro_rules! impl_from_num {
    ($($ty: ty => $pa: path,)*) => ($(impl From<$ty> for LitValue { fn from(value: $ty) -> LitValue { LitValue::Num(Some($pa(value))) } })*)
}
impl_from_num!{
    i8 => NumLitValue::I8,
    u8 => NumLitValue::U8,
    i16 => NumLitValue::I16,
    u16 => NumLitValue::U16,
    i32 => NumLitValue::I32,
    u32 => NumLitValue::U32,
    i64 => NumLitValue::I64,
    u64 => NumLitValue::U64,
    f32 => NumLitValue::R32,
    f64 => NumLitValue::R64,
}

impl LitValue {

    pub fn is_str(&self) -> bool { match self { &LitValue::Str(_) => true, _ => false } }
    pub fn is_num(&self) -> bool { match self { &LitValue::Num(_) => true, _ => false } }
    pub fn is_char(&self) -> bool { match self { &LitValue::Char(_) => true, _ => false } }
    pub fn is_bool(&self) -> bool { match self { &LitValue::Bool(_) => true, _ => false } }

    pub fn is_valid(&self) -> bool { 
        match self {
            | &LitValue::Bool(_)
            | &LitValue::Str(Some(_))
            | &LitValue::Num(Some(_))
            | &LitValue::Char(Some(_)) => true,
            _ => false,
        }
    }

    pub fn get_char(&self) -> char { match self { &LitValue::Char(Some(val)) => val, _ => '\0' } }
    pub fn get_bool(&self) -> bool { match self { &LitValue::Bool(val) => val, _ => false } }
    pub fn get_num(&self) -> NumLitValue { match self { &LitValue::Num(Some(val)) => val, _ => NumLitValue::I32(0) } }
}
