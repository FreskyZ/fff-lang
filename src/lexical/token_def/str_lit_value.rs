///! fff-lang
///!
///! lexical/string literal value

use std::fmt;

use crate::source::Sym;
use super::Token;

#[derive(Clone, Eq, PartialEq)]
pub enum FormatStrLitSegment {
    Str(Sym),
    Other(Token),
}
impl fmt::Debug for FormatStrLitSegment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &FormatStrLitSegment::Str(id) => write!(f, "str {:?}", id),
            &FormatStrLitSegment::Other(ref token) => write!(f, "token {:?}", token),
        }
    }
}
impl From<u32> for FormatStrLitSegment { fn from(id: u32) -> FormatStrLitSegment { FormatStrLitSegment::Str(Sym::new(id)) } }
impl From<Sym> for FormatStrLitSegment { fn from(id: Sym) -> FormatStrLitSegment { FormatStrLitSegment::Str(id) } }
impl From<Token> for FormatStrLitSegment { fn from(token: Token) -> FormatStrLitSegment { FormatStrLitSegment::Other(token) } }

#[derive(Clone, Eq, PartialEq)]
pub enum StrLitValue {
    Simple(Sym),
    Format(Vec<FormatStrLitSegment>),
}
impl fmt::Debug for StrLitValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &StrLitValue::Simple(id) => write!(f, "str-lit {:?}", id),
            &StrLitValue::Format(ref segments) => {
                // no check segment.len() > 0 because lexer will not allow it
                let mut result: String = segments.into_iter().fold("format-str-lit(".to_owned(), |buf, segment| format!("{}{:?}, ", buf, segment));
                let _ = result.pop();
                let _ = result.pop();
                result.push(')');
                write!(f, "{}", result)
            }
        }
    }
}
