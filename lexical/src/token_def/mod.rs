///! fff-lang
///!
///! lexical/token

mod keyword;
mod seperator;
mod lit_value;

pub use self::keyword::KeywordKind;
pub use self::seperator::SeperatorKind;
pub use self::seperator::SeperatorCategory;
pub use self::lit_value::LitValue;
pub use self::lit_value::NumLitValue;

use std::fmt;
use codepos::StringPosition;
pub trait IToken: fmt::Debug {

    fn is_eof(&self) -> bool;
    fn is_lit(&self) -> bool;
    fn is_eofs(&self) -> bool;
    fn is_label(&self) -> bool;
    fn is_identifier(&self) -> bool;
    fn is_keyword(&self, kind: KeywordKind) -> bool;
    fn is_seperator(&self, kind: SeperatorKind) -> bool;
    fn is_seperator_category(&self, category: SeperatorCategory) -> bool;

    fn get_lit(&self) -> Option<LitValue>;
    fn get_label(&self) -> Option<String>;
    fn get_keyword(&self) -> Option<KeywordKind>;
    fn get_seperator(&self) -> Option<SeperatorKind>;
    fn get_identifier(&self) -> Option<String>;

    fn get_strpos(&self) -> StringPosition;
}