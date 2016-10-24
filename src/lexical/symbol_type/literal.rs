
// lexical literal, for lexical and syntax parser convenience

use std::fmt;
use lexical::NumLitValue;

test_only_attr!{
    [derive(Clone, Eq, PartialEq)]
    ![derive(Clone)]
    pub enum LexicalLiteral {
        Str(Option<String>),
        Num(Option<NumLitValue>),
        Char(Option<char>),
        Bool(bool),
    }
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

impl_display_by_debug!{ LexicalLiteral }