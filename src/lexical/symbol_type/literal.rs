
// lexical literal, for lexical and syntax parser convenience

use std::fmt;
use lexical::NumLitValue;

#[derive(Clone, Eq, PartialEq)]
pub enum LexicalLiteral {
    Str(Option<String>),
    Num(Option<NumLitValue>),
    Char(Option<char>),
    Bool(bool),
}

impl LexicalLiteral {

    pub fn get_str_lit(&self) -> Option<&Option<String>> {
        match self {
            &LexicalLiteral::Str(ref val) => Some(val),
            _ => None,
        }
    }
    pub fn get_char_lit(&self) -> Option<&Option<char>> {
        match self {
            &LexicalLiteral::Char(ref val) => Some(val),
            _ => None,
        }
    }
    pub fn get_num_lit(&self) -> Option<&Option<NumLitValue>> {
        match self {
            &LexicalLiteral::Num(ref val) => Some(val),
            _ => None,
        }
    }
    pub fn get_bool_lit(&self) -> Option<bool> {
        match self {
            &LexicalLiteral::Bool(val) => Some(val),
            _ => None,
        }
    }
}

impl fmt::Debug for LexicalLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LexicalLiteral::Str(Some(ref val)) => write!(f, "String literal {:?}", val),
            LexicalLiteral::Str(None) => write!(f, "String literal \"<invalid>\""),
            LexicalLiteral::Char(Some(ref val)) => write!(f, "Char literal {:?}", val),
            LexicalLiteral::Char(None) => write!(f, "Char literal '<invalid>'"),
            LexicalLiteral::Num(Some(ref val)) => write!(f, "Numeric literal {:?}", val),
            LexicalLiteral::Num(None) => write!(f, "Numeric literal <invalid>"),
            LexicalLiteral::Bool(val) => write!(f, "Boolean literal {}", val),
        }
    }
}
impl fmt::Display for LexicalLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LexicalLiteral::Str(Some(ref val)) => write!(f, "{:?}", val),
            LexicalLiteral::Str(None) => write!(f, "<invalid>"),
            LexicalLiteral::Char(Some(ref val)) => write!(f, "{:?}", val),
            LexicalLiteral::Char(None) => write!(f, "<invalid>"),
            LexicalLiteral::Num(Some(ref val)) => write!(f, "{:?}", val),
            LexicalLiteral::Num(None) => write!(f, "<invalid>"),
            LexicalLiteral::Bool(val) => write!(f, "{}", val),
        }
    }
}

impl From<String> for LexicalLiteral {
    fn from(val: String) -> LexicalLiteral {
        LexicalLiteral::Str(Some(val))
    }
}
#[cfg(test)]
impl<'a> From<&'a str> for LexicalLiteral {
    fn from(val: &'a str) -> LexicalLiteral {
        LexicalLiteral::Str(Some(val.to_owned()))
    }
}

impl From<char> for LexicalLiteral {
    fn from(ch: char) -> LexicalLiteral {
        LexicalLiteral::Char(Some(ch))
    }
}
impl From<bool> for LexicalLiteral {
    fn from(val: bool) -> LexicalLiteral {
        LexicalLiteral::Bool(val)
    }
}

macro_rules! from_for_lexical_lit_num {
    ($($ty: ty)*) => (
        $(
            impl From<$ty> for LexicalLiteral {
                fn from(value: $ty) -> LexicalLiteral {
                    LexicalLiteral::Num(Some(NumLitValue::from(value)))
                }
            }
        )*
    )
}
from_for_lexical_lit_num!{ i8  u8  i16  u16  i32  u32  i64  u64  f32  f64  }