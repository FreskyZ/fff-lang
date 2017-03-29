///! fff-lang
///! 
///! lexical parser
///!
///! TokenStream::new(codechars, messages) for formal use
///! lexical::parse_test_str(program) for test use

#[macro_use] extern crate util;
#[macro_use] extern crate messages as message; 
#[cfg_attr(test, macro_use)] extern crate codepos;
extern crate codemap;

use std::fmt;
use codepos::StringPosition;

mod symbol_type;
mod buf_lexer;
mod token_stream;
mod v1lexer;
mod v2lexer;

pub use self::symbol_type::seperator::SeperatorKind;
pub use self::symbol_type::seperator::SeperatorCategory;
pub use self::symbol_type::keyword::KeywordKind;
pub use self::symbol_type::literal::NumLitValue;
pub use self::symbol_type::literal::LitValue;
pub use self::token_stream::TokenStream;

pub type Lexer = TokenStream;

pub trait IToken : fmt::Debug {

    fn is_keyword(&self, kind: KeywordKind) -> bool;
    fn is_seperator(&self, kind: SeperatorKind) -> bool;
    fn is_spec_ident(&self, name: &str) -> bool;
    fn is_ident(&self) -> bool;
    fn is_eof(&self) -> bool;
    fn is_eofs(&self) -> bool;

    fn is_lit(&self) -> bool;
    fn is_str_lit(&self) -> bool;
    fn is_num_lit(&self) -> bool;
    fn is_char_lit(&self) -> bool;
    fn is_bool_lit(&self) -> bool;

    fn is_seperator_category(&self, category: SeperatorCategory) -> bool;

    // clone them if need
    fn get_keyword(&self) -> Option<KeywordKind>;
    fn get_seperator(&self) -> Option<SeperatorKind>;
    fn get_identifier(&self) -> Option<String>;
    fn get_lit_val(&self) -> Option<LitValue>;

    fn get_position(&self) -> StringPosition;
}

// test helper, may panic
pub fn parse_test_str(program: &str) -> TokenStream {
    use codemap::CodeMap;
    use message::MessageCollection;
    let mut codemap = CodeMap::with_test_str(program);
    let mut messages = MessageCollection::new();
    let ret_val = TokenStream::new(codemap.iter(), &mut messages);
    check_messages_continuable!(messages);
    return ret_val;
}
