///! fff-lang
///!
///! lexical/token

use std::fmt;
use codemap::SymbolID;

#[path = "keyword2.rs"] mod keyword;
#[path = "seperator2.rs"] mod seperator;
mod lit_value;

pub use self::keyword::Keyword;
pub use self::seperator::Seperator;
pub use self::seperator::SeperatorCategory;
pub use self::lit_value::LitValue;
pub use self::lit_value::NumLitValue;

/// Lexical token
#[derive(Eq, PartialEq)]  
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
            &Token::Ident(ref sid) => write!(f, "Ident {:?}", sid),  // Ident #1
            &Token::Label(ref sid) => write!(f, "Lable @{:?}", sid), // Label @#2
            &Token::Sep(ref sep) => write!(f, "Seperator {:?}", sep),
            &Token::Keyword(ref kw) => write!(f, "Keyword {:?}", kw),
        }
    }
}
