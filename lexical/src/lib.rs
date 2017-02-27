
// Lexer public interface

#[macro_use]
extern crate util;
#[macro_use]
extern crate codepos;
extern crate messages as message; // TODO: legacy remove

use std::fmt;
use codepos::StringPosition;

mod symbol_type;
mod buf_lexer;
mod v0lexer;
mod v1lexer;
mod v2lexer;
mod v4lexer;

pub use self::symbol_type::seperator::SeperatorKind;
pub use self::symbol_type::seperator::SeperatorCategory;
pub use self::symbol_type::keyword::KeywordKind;
pub use self::symbol_type::literal::NumLitValue;
pub use self::symbol_type::literal::LitValue;

pub use self::v4lexer::V4Lexer as Lexer;

// token interface definition
pub trait IToken : fmt::Debug {

    fn is_keyword(&self, kind: KeywordKind) -> bool;
    fn is_seperator(&self, kind: SeperatorKind) -> bool;
    fn is_spec_ident(&self, name: &str) -> bool;
    fn is_ident(&self) -> bool;
    fn is_eof(&self) -> bool;

    fn is_lit(&self) -> bool;
    fn is_str_lit(&self) -> bool;
    fn is_num_lit(&self) -> bool;
    fn is_char_lit(&self) -> bool;
    fn is_bool_lit(&self) -> bool;

    // clone them if need
    fn get_keyword(&self) -> Option<KeywordKind>;
    fn get_seperator(&self) -> Option<SeperatorKind>;
    fn get_identifier(&self) -> Option<String>;
    fn get_lit_val(&self) -> Option<LitValue>;

    fn get_position(&self) -> StringPosition;
}

// TODO: 
// numeric literal refactor, use new message, add 1.1E10 feature, remove ' seperator， remove double _ seperator
