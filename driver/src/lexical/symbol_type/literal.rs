
// lexical literal, for lexical and syntax parser convenience

use std::fmt;
use codepos::StringPosition;
use lexical::NumLitValue;

#[derive(Clone, Eq, PartialEq)]
pub enum LitValue {
    Unit,
    Str(Option<String>),
    Num(Option<NumLitValue>),
    Char(Option<char>),
    Bool(bool),
}

impl LitValue {

    pub fn is_unit(&self) -> bool {
        match *self {
            LitValue::Unit => true,
            _ => false,
        }
    }
    pub fn is_str(&self) -> bool {
        match *self {
            LitValue::Str(_) => true,
            _ => false,
        }
    }
    pub fn is_num(&self) -> bool {
        match *self {
            LitValue::Num(_) => true,
            _ => false,
        }
    }
    pub fn is_char(&self) -> bool {
        match *self {
            LitValue::Char(_) => true,
            _ => false,
        }
    }
    pub fn is_bool(&self) -> bool {
        match *self {
            LitValue::Bool(_) => true,
            _ => false,
        }
    }

    pub fn get_str(&self) -> Option<&Option<String>> {
        match self {
            &LitValue::Str(ref val) => Some(val),
            _ => None,
        }
    }
    pub fn get_char(&self) -> Option<&Option<char>> {
        match self {
            &LitValue::Char(ref val) => Some(val),
            _ => None,
        }
    }
    pub fn get_num(&self) -> Option<&Option<NumLitValue>> {
        match self {
            &LitValue::Num(ref val) => Some(val),
            _ => None,
        }
    }
    pub fn get_bool(&self) -> Option<bool> {
        match self {
            &LitValue::Bool(val) => Some(val),
            _ => None,
        }
    }

    /// replace error content with <error-content>, do not call on not Str
    pub fn get_str_not_option(self) -> String {
        match self {
            LitValue::Str(Some(val)) => val,
            LitValue::Str(None) => "<error-content>".to_owned(),
            _ => unreachable!(),
        }
    }
    pub fn get_num_not_option(self) -> NumLitValue {
        match self {
            LitValue::Num(Some(val)) => val,
            LitValue::Num(None) => NumLitValue::I32(0),
            _ => unreachable!(),
        }
    }
    pub fn get_char_not_option(self) -> char {
        match self {
            LitValue::Char(Some(val)) => val,
            LitValue::Char(None) => '\u{FEFF}',
            _ => unreachable!()
        }
    }
    pub fn get_bool_not_option(self) -> bool {
        match self {
            LitValue::Bool(val) => val,
            _ => false,
        }
    }
}

impl fmt::Debug for LitValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LitValue::Unit => write!(f, "Unit literal"),
            LitValue::Str(Some(ref val)) => write!(f, "String literal {:?}", val),
            LitValue::Str(None) => write!(f, "String literal \"<invalid>\""),
            LitValue::Char(Some(ref val)) => write!(f, "Char literal {:?}", val),
            LitValue::Char(None) => write!(f, "Char literal '<invalid>'"),
            LitValue::Num(Some(ref val)) => write!(f, "Numeric literal {:?}", val),
            LitValue::Num(None) => write!(f, "Numeric literal <invalid>"),
            LitValue::Bool(val) => write!(f, "Boolean literal {}", val),
        }
    }
}
impl fmt::Display for LitValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LitValue::Unit => write!(f, "()"),
            LitValue::Str(Some(ref val)) => write!(f, "{:?}", val),
            LitValue::Str(None) => write!(f, "<invalid>"),
            LitValue::Char(Some(ref val)) => write!(f, "{:?}", val),
            LitValue::Char(None) => write!(f, "<invalid>"),
            LitValue::Num(Some(ref val)) => write!(f, "{:?}", val),
            LitValue::Num(None) => write!(f, "<invalid>"),
            LitValue::Bool(val) => write!(f, "{}", val),
        }
    }
}

impl From<String> for LitValue {
    fn from(val: String) -> LitValue {
        LitValue::Str(Some(val))
    }
}
#[cfg(test)]
impl<'a> From<&'a str> for LitValue {
    fn from(val: &'a str) -> LitValue {
        LitValue::Str(Some(val.to_owned()))
    }
}

impl From<char> for LitValue {
    fn from(val: char) -> LitValue {
        LitValue::Char(Some(val))
    }
}
impl From<bool> for LitValue {
    fn from(val: bool) -> LitValue {
        LitValue::Bool(val)
    }
}

macro_rules! from_for_lexical_lit_num {
    ($($ty: ty)*) => (
        $(
            impl From<$ty> for LitValue {
                fn from(val: $ty) -> LitValue {
                    LitValue::Num(Some(NumLitValue::from(val)))
                }
            }
        )*
    )
}
from_for_lexical_lit_num!{ i8  u8  i16  u16  i32  u32  i64  u64  f32  f64  }
