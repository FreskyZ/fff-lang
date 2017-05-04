///! fff-lang
///!
///! lexical/token

use std::fmt;

mod keyword;
mod seperator;
mod lit_value;

pub use self::keyword::KeywordKind;
pub use self::seperator::SeperatorKind;
pub use self::seperator::SeperatorCategory;
pub use self::lit_value::LitValue;
pub use self::lit_value::NumLitValue;

/// Lexical token
#[derive(Eq, PartialEq)]  
pub enum Token {
    Lit(LitValue),
    Ident(String),
    Label(String),
    Sep(SeperatorKind),
    Keyword(KeywordKind),
    EOF,
}
impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Token::Lit(ref lit) => write!(f, "{:?}", lit),
            &Token::Ident(ref name) => write!(f, "Ident '{}'", name),
            &Token::Label(ref name) => write!(f, "Lable '@{}'", name),
            &Token::Sep(ref sep) => write!(f, "Seperator {:?}", sep),
            &Token::Keyword(ref kw) => write!(f, "Keyword {:?}", kw),
            &Token::EOF => write!(f, "EOF"), 
        }
    }
}
