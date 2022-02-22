///! fff-lang
///!
///! lexical/token

use std::fmt;
use crate::codemap::SymbolID;

mod keyword;
mod seperator;
mod num_lit_value;
mod str_lit_value;

pub use keyword::Keyword;
pub use seperator::{Seperator, SeperatorCategory};
pub use num_lit_value::NumLitValue;
pub use str_lit_value::{StrLitValue, FormatStrLitSegment};

#[derive(Clone, Eq, PartialEq)]
pub enum LitValue {
    Unit,                // unit is not generated here in v2 or some other, because for cases like `1.to_string()`, this is function call not unit
    Str(Option<StrLitValue>),
    Num(Option<NumLitValue>),
    Char(Option<char>),
    Bool(bool),
}
impl fmt::Debug for LitValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LitValue::Unit => write!(f, "unit"),
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

#[cfg(test)]
impl LitValue {
    pub fn new_str_lit_simple(sid: SymbolID) -> LitValue { LitValue::Str(Some(StrLitValue::Simple(sid))) }
    pub fn new_str_lit_simple_usize(sid: usize) -> LitValue { LitValue::Str(Some(StrLitValue::Simple(SymbolID::new(sid)))) }
    pub fn new_str_lit_format(segments: Vec<FormatStrLitSegment>) -> LitValue { LitValue::Str(Some(StrLitValue::Format(segments))) }
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

    pub fn is_unit(&self) -> bool { match self { &LitValue::Unit => true, _ => false } }
    pub fn is_str(&self) -> bool { match self { &LitValue::Str(_) => true, _ => false } }
    pub fn is_num(&self) -> bool { match self { &LitValue::Num(_) => true, _ => false } }
    pub fn is_char(&self) -> bool { match self { &LitValue::Char(_) => true, _ => false } }
    pub fn is_bool(&self) -> bool { match self { &LitValue::Bool(_) => true, _ => false } }

    pub fn is_valid(&self) -> bool { 
        match self {
            &LitValue::Unit
            | &LitValue::Bool(_)
            | &LitValue::Str(Some(_))
            | &LitValue::Num(Some(_))
            | &LitValue::Char(Some(_)) => true,
            _ => false,
        }
    }

    pub fn get_char(&self) -> char { match self { &LitValue::Char(Some(val)) => val, _ => '\0' } }
    pub fn get_bool(&self) -> bool { match self { &LitValue::Bool(val) => val, _ => false } }
    pub fn get_str(&self) -> StrLitValue { match self { &LitValue::Str(Some(ref val)) => val.clone(), _ => StrLitValue::Simple(SymbolID::new(!1)) } }
    pub fn get_num(&self) -> NumLitValue { match self { &LitValue::Num(Some(val)) => val, _ => NumLitValue::I32(0) } }
}

/// Lexical token
#[derive(Eq, PartialEq, Clone)]  
pub enum Token {
    EOF,
    Lit(LitValue),
    Ident(SymbolID),
    Label(SymbolID),
    Sep(Seperator),
    Keyword(Keyword),
}
impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Token::EOF => write!(f, "EOF"), 
            &Token::Lit(ref lit) => write!(f, "{:?}", lit),
            &Token::Ident(ref sid) => write!(f, "ident {:?}", sid),  // Ident #1
            &Token::Label(ref sid) => write!(f, "label @{:?}", sid), // Label @#2
            &Token::Sep(ref sep) => write!(f, "separator {:?}", sep),
            &Token::Keyword(ref kw) => write!(f, "keyword {:?}", kw),
        }
    }
}

#[cfg(test)] #[test]
fn token_use() {

}

#[cfg(test)] #[test]
fn token_format() {

}