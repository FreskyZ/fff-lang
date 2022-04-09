
///! lexical::token: the lexical token

use std::fmt;
use crate::source::{SourceContext, IsId};

#[cfg(test)]
mod tests;
mod keyword;
mod separator;

pub use keyword::{Keyword, KeywordKind};
pub use separator::{Separator, SeparatorKind};

// numeric value for all integers and rational values
// - it is designed to be used cross the whole program because
//   not only writing this list of variant is boring, but also you need to 
//   write this list several times in different form for each one more type
//   and there is not #define USE_NUMTYPE #include "numtype" similar mechanism in this normal macro (not by proc macro)
// - only PartialEq but no Eq because f32/f64 does not have that, PartialEq should be enough for assert_eq
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Numeric {
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    R32(f32),
    R64(f64),
}

impl fmt::Display for Numeric {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Numeric::I8(v) => write!(f, "i8 {}", v),
            Numeric::U8(v) => write!(f, "u8 {}", v),
            Numeric::I16(v) => write!(f, "i16 {}", v),
            Numeric::U16(v) => write!(f, "u16 {}", v),
            Numeric::I32(v) => write!(f, "i32 {}", v),
            Numeric::U32(v) => write!(f, "u32 {}", v),
            Numeric::I64(v) => write!(f, "i64 {}", v),
            Numeric::U64(v) => write!(f, "u64 {}", v),
            Numeric::R32(v) => write!(f, "r32 {}", v),
            Numeric::R64(v) => write!(f, "r64 {}", v),
        }
    }
}

#[allow(dead_code)] // binary string literal and format string literal not implemented
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum StringLiteralType {
    Normal,
    Raw,
    Binary,
    RawBinary,
    FormatStart,
    FormatSpecifier,
    FormatIntermediate,
    FormatEnd,
}

impl fmt::Display for StringLiteralType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StringLiteralType::Normal => Ok(()),
            StringLiteralType::Raw => f.write_str("(r)"),
            StringLiteralType::Binary => f.write_str("(b)"),
            StringLiteralType::RawBinary => f.write_str("(rb)"),
            StringLiteralType::FormatStart => f.write_str("({)"),
            StringLiteralType::FormatSpecifier => f.write_str("(:{)"),
            StringLiteralType::FormatIntermediate => f.write_str("(}{)"),
            StringLiteralType::FormatEnd => f.write_str("(})"),
        }
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub enum Token {
    EOF,
    // identifier, [_a-zA-Z][_a-zA-Z0-9]*
    Ident(IsId),
    // keyword
    Keyword(Keyword),
    // label, @[_a-zA-Z0-9@]*
    Label(IsId),
    // separator
    Sep(Separator),

    // bool literal
    Bool(bool),
    // char literal
    Char(char),
    // numeric literals
    Num(Numeric),

    // string literal, include format string literal segment
    // 1. format segment is not in form of `enum Segment { Str(IsId), Other(Token) }` and add a `Format(Vec<Semgent>)` variant
    //    because Vec is 3 size_t while other variants are max 8 bytes so it waste a lot of memory saving many tokens in vector
    // 2. format segment is not in form of `FormatSegmentStr(IsId)` and `FormatSegmentOther(Box<Token>)` variant because of the box
    Str(IsId, StringLiteralType),
}

// pretty print replace symbol id with string content
pub struct TokenDisplay<'t, 'scx>(&'t Token, &'scx SourceContext);

impl Token {
    #[allow(dead_code)] // after syntax add back, before --print tokens argument added, this is dead code
    pub fn display<'t, 'scx>(&'t self, scx: &'scx SourceContext) -> TokenDisplay<'t, 'scx> {
        TokenDisplay(self, scx)
    }
}

impl<'a, 'scx> fmt::Display for TokenDisplay<'a, 'scx> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            Token::EOF => f.write_str("EOF"),
            Token::Ident(id) => write!(f, "ident {}", id.display(self.1)),
            Token::Keyword(kw) => write!(f, "keyword {}", kw.display()),
            Token::Label(id) => write!(f, "label @{}", id.display(self.1)),
            Token::Sep(sep) => write!(f, "sep {}", sep.display()),
            Token::Bool(v) => write!(f, "bool {}", v),
            Token::Char(v) => write!(f, "char {:?}", v),
            Token::Num(v) => write!(f, "{}", v),
            Token::Str(id, ty) => write!(f, "str{} {:?}", ty, id.display(self.1)),
        }
    }
}
