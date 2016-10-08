
// Lexer public interface
mod symbol_type;
mod buf_lexer;
mod v0lexer;
mod v1lexer;
mod v2lexer;
mod v3lexer;
mod v4lexer;

use common::StringPosition;

pub use lexical::symbol_type::SeperatorKind;
pub use lexical::symbol_type::KeywordKind;
pub use lexical::symbol_type::NumericLiteralValue;

pub use lexical::v4lexer::V4Token as Token;
pub use lexical::v4lexer::V4Lexer as Lexer;

// token interface definition
pub trait IToken {

    fn is_keyword(&self, kind: KeywordKind) -> bool;
    fn is_seperator(&self, kind: SeperatorKind) -> bool;
    fn is_identifier(&self, name: &str) -> bool;
    fn is_str_lit(&self) -> bool;
    fn is_raw_str_lit(&self) -> bool;
    fn is_any_str_lit(&self) -> bool;
    fn is_num_lit(&self) -> bool;
    fn is_char_lit(&self) -> bool;
    fn is_bool_lit(&self) -> bool;
    fn is_eof(&self) -> bool;

    // clone them if need
    fn get_keyword(&self) -> Option<&KeywordKind>;
    fn get_seperator(&self) -> Option<&SeperatorKind>;
    fn get_identifier(&self) -> Option<&String>;

    fn get_position(&self) -> StringPosition;
}
