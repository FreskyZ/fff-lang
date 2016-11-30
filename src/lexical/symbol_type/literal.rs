
// lexical literal, for lexical and syntax parser convenience

use std::fmt;
use common::StringPosition;
use lexical::NumLitValue;

#[derive(Clone, Eq, PartialEq)]
pub enum LexicalLiteral {
    Unit,
    Str(Option<String>),
    Num(Option<NumLitValue>),
    Char(Option<char>),
    Bool(bool),
}

impl LexicalLiteral {

    pub fn is_unit(&self) -> bool {
        match *self {
            LexicalLiteral::Unit => true,
            _ => false,
        }
    }
    pub fn is_str(&self) -> bool {
        match *self {
            LexicalLiteral::Str(_) => true,
            _ => false,
        }
    }
    pub fn is_num(&self) -> bool {
        match *self {
            LexicalLiteral::Num(_) => true,
            _ => false,
        }
    }
    pub fn is_char(&self) -> bool {
        match *self {
            LexicalLiteral::Char(_) => true,
            _ => false,
        }
    }
    pub fn is_bool(&self) -> bool {
        match *self {
            LexicalLiteral::Bool(_) => true,
            _ => false,
        }
    }

    pub fn get_str(&self) -> Option<&Option<String>> {
        match self {
            &LexicalLiteral::Str(ref val) => Some(val),
            _ => None,
        }
    }
    pub fn get_char(&self) -> Option<&Option<char>> {
        match self {
            &LexicalLiteral::Char(ref val) => Some(val),
            _ => None,
        }
    }
    pub fn get_num(&self) -> Option<&Option<NumLitValue>> {
        match self {
            &LexicalLiteral::Num(ref val) => Some(val),
            _ => None,
        }
    }
    pub fn get_bool(&self) -> Option<bool> {
        match self {
            &LexicalLiteral::Bool(val) => Some(val),
            _ => None,
        }
    }

    /// replace error content with <error-content>, do not call on not Str
    pub fn get_str_not_option(self) -> String {
        match self {
            LexicalLiteral::Str(Some(val)) => val,
            LexicalLiteral::Str(None) => "<error-content>".to_owned(),
            _ => unreachable!(),
        }
    }
    pub fn get_num_not_option(self) -> NumLitValue {
        match self {
            LexicalLiteral::Num(Some(val)) => val,
            LexicalLiteral::Num(None) => NumLitValue::I32(0),
            _ => unreachable!(),
        }
    }
    pub fn get_char_not_option(self) -> char {
        match self {
            LexicalLiteral::Char(Some(val)) => val,
            LexicalLiteral::Char(None) => '\u{FEFF}',
            _ => unreachable!()
        }
    }
    pub fn get_bool_not_option(self) -> bool {
        match self {
            LexicalLiteral::Bool(val) => val,
            _ => false,
        }
    }
}

impl fmt::Debug for LexicalLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LexicalLiteral::Unit => write!(f, "Unit literal"),
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
            LexicalLiteral::Unit => write!(f, "()"),
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
    fn from(val: char) -> LexicalLiteral {
        LexicalLiteral::Char(Some(val))
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
                fn from(val: $ty) -> LexicalLiteral {
                    LexicalLiteral::Num(Some(NumLitValue::from(val)))
                }
            }
        )*
    )
}
from_for_lexical_lit_num!{ i8  u8  i16  u16  i32  u32  i64  u64  f32  f64  }
