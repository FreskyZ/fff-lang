
// v4 lexer, input v3, act as lexer interface

use common::StringPosition;

use lexical::KeywordKind;
use lexical::SeperatorKind;
use lexical::NumericLiteralValue;

pub enum TokenValue {
    StringLiteral(Option<String>),
    RawStringLiteral(Option<String>),
    NumericLiteral(Option<NumericLiteralValue>),
    CharLiteral(Option<char>),
    BooleanLiteral(bool),
    Identifier(String),
    Keyword(KeywordKind),
    Seperator(SeperatorKind),
}

pub struct Token {
    pub value: TokenValue,
    pub pos: StringPosition,
}