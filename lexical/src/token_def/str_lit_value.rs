///! fff-lang
///!
///! lexical/string literal value

use std::fmt;

use codemap::SymbolID;
use super::Token;

#[derive(Clone, Eq, PartialEq)]
pub enum FormatStrLitSegment {
    Str(SymbolID),
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
impl From<usize> for FormatStrLitSegment { fn from(id: usize) -> FormatStrLitSegment { FormatStrLitSegment::Str(SymbolID::new(id)) } }
impl From<SymbolID> for FormatStrLitSegment { fn from(id: SymbolID) -> FormatStrLitSegment { FormatStrLitSegment::Str(id) } }
impl From<Token> for FormatStrLitSegment { fn from(token: Token) -> FormatStrLitSegment { FormatStrLitSegment::Other(token) } }

#[derive(Clone, Eq, PartialEq)]
pub enum StrLitValue {
    Simple(SymbolID),
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
