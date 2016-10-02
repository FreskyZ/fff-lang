
// Keyword kind

#[derive(Debug, Clone)]
pub enum KeywordKind {
    FnDef,
    If,
    Else,
    While,
    Break,
    Continue,
    Struct,
    For,
    Return,
    Namespace,
}

use common::TryFrom;
impl<'a> TryFrom<&'a str> for KeywordKind {

    fn try_from(name: &'a str) -> Option<KeywordKind> {
        match name {
            "fn" => Some(KeywordKind::FnDef),
            "if" => Some(KeywordKind::If),
            "else" => Some(KeywordKind::Else),
            "while" => Some(KeywordKind::While),
            "break" => Some(KeywordKind::Break),
            "continue" => Some(KeywordKind::Continue),
            "struct" => Some(KeywordKind::Struct),
            "for" => Some(KeywordKind::For),
            "return" => Some(KeywordKind::Return),
            "namespace" => Some(KeywordKind::Namespace),
            _ => None,
        }
    }
}