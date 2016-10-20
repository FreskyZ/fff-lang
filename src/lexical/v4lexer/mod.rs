
// v4 lexer, input v3, act as lexer interface

use std::fmt;
use common::From2;
use common::StringPosition;
use message::Message;
use message::MessageEmitter;

use lexical::symbol_type::StringLiteral;
use lexical::symbol_type::CharLiteral;
use lexical::symbol_type::KeywordKind;
use lexical::symbol_type::SeperatorKind;
use lexical::symbol_type::NumericLiteral;
use lexical::symbol_type::NumericLiteralValue;

use lexical::v3lexer::V3Lexer;
use lexical::v3lexer::V3Token;

use lexical::IToken;

// Token
test_only_attr!{
    [derive(Clone, Eq, PartialEq)]
    ![derive(Clone)]
    pub enum TokenValue {
        StringLiteral(Option<String>),
        RawStringLiteral(Option<String>),
        NumericLiteral(Option<NumericLiteralValue>),
        CharLiteral(Option<char>),
        BooleanLiteral(bool),
        Identifier(String),
        Keyword(KeywordKind),
        Seperator(SeperatorKind),
        EndofFile,
    }
}

test_only_attr!{
    [derive(Clone, Eq, PartialEq)]
    ![derive(Clone)]    
    pub struct V4Token {
        value: TokenValue,
        pos: StringPosition,
    }
}

impl From<V3Token> for V4Token {

    // map None to EOF
    fn from(v3: V3Token) -> V4Token {
        match v3 {
            V3Token::StringLiteral(StringLiteral{ value, pos, is_raw: true }) => V4Token{ value: TokenValue::RawStringLiteral(value), pos: pos },
            V3Token::StringLiteral(StringLiteral{ value, pos, is_raw: false }) => V4Token{ value: TokenValue::StringLiteral(value), pos: pos },
            V3Token::NumericLiteral(NumericLiteral{ value, pos }) => V4Token{ value: TokenValue::NumericLiteral(value), pos: pos },
            V3Token::CharLiteral(CharLiteral{ value, pos }) => V4Token{ value: TokenValue::CharLiteral(value), pos: pos },
            V3Token::BooleanLiteral(value, pos) => V4Token{ value: TokenValue::BooleanLiteral(value), pos: pos },
            V3Token::Identifier(name, pos) => V4Token{ value: TokenValue::Identifier(name), pos: pos },
            V3Token::Keyword(kind, pos) => V4Token{ value: TokenValue::Keyword(kind), pos: pos },
            V3Token::Seperator(kind, pos) => V4Token{ value: TokenValue::Seperator(kind), pos: pos },
        }
    }
}

impl fmt::Debug for V4Token {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(match self.value {
            TokenValue::StringLiteral(Some(ref value)) => write!(f, "String literal {:?} at ", value),
            TokenValue::StringLiteral(None) => write!(f, "String literal <invalid> at "),
            TokenValue::RawStringLiteral(Some(ref value)) => write!(f, "Raw string literal {:?} at ", value),
            TokenValue::RawStringLiteral(None) => write!(f, "Raw string literal <invalid> at "),
            TokenValue::CharLiteral(Some(ref value)) => write!(f, "Char literal {:?} at ", value),
            TokenValue::CharLiteral(None) => write!(f, "Char literal <invalid> at "),
            TokenValue::NumericLiteral(Some(ref value)) => write!(f, "Numeric literal {:?} at ", value),
            TokenValue::NumericLiteral(None) => write!(f, "Numeric literal <invalid> at "),
            TokenValue::BooleanLiteral(ref value) => write!(f, "Boolean literal {} at ", value),
            TokenValue::Identifier(ref name) => write!(f, "Identifier `{}` at ", name),
            TokenValue::Keyword(ref kind) => write!(f, "Keyword {:?} at ", kind),
            TokenValue::Seperator(ref kind) => write!(f, "Seperator {:?} at ", kind),
            TokenValue::EndofFile => write!(f, "<EOF> at "), 
        });
        write!(f, "{:?}", self.pos)
    }
}

impl IToken for V4Token {

    fn is_keyword(&self, kind: KeywordKind) -> bool {
        match self.value {
            TokenValue::Keyword(ref self_kind) => *self_kind == kind,
            _ => false,
        }
    }
    fn is_seperator(&self, kind: SeperatorKind) -> bool {
        match self.value {
            TokenValue::Seperator(ref self_kind) => *self_kind == kind,
            _ => false,
        }
    }
    fn is_identifier(&self, name: &str) -> bool {
        match self.value {
            TokenValue::Identifier(ref self_name) => self_name == name,
            _ => false,
        }
    }
    fn is_str_lit(&self) -> bool {
        match self.value {
            TokenValue::StringLiteral(_) => true,
            _ => false,
        }
    }
    fn is_raw_str_lit(&self) -> bool {
        match self.value {
            TokenValue::RawStringLiteral(_) => true,
            _ => false,
        }
    }
    fn is_any_str_lit(&self) -> bool {
        match self.value {
            TokenValue::StringLiteral(_) | TokenValue::RawStringLiteral(_) => true,
            _ => false,
        }
    }
    fn is_num_lit(&self) -> bool {
        match self.value {
            TokenValue::NumericLiteral(_) => true,
            _ => false,
        }
    }
    fn is_char_lit(&self) -> bool {
        match self.value {
            TokenValue::CharLiteral(_) => true,
            _ => false,
        }
    }
    fn is_bool_lit(&self) -> bool {
        match self.value {
            TokenValue::BooleanLiteral(_) => true,
            _ => false,
        }
    }
    fn is_eof(&self) -> bool {
        match self.value {
            TokenValue::EndofFile => true, 
            _ => false,
        }
    }

    fn get_keyword(&self) -> Option<&KeywordKind> {
        match self.value {
            TokenValue::Keyword(ref kind) => Some(kind),
            _ => None
        }
    }
    fn get_seperator(&self) -> Option<&SeperatorKind> {
        match self.value {
            TokenValue::Seperator(ref kind) => Some(kind),
            _ => None
        }
    }
    fn get_identifier(&self) -> Option<&String> {
        match self.value {
            TokenValue::Identifier(ref name) => Some(name),
            _ => None
        }
    }
    fn get_str_lit_val(&self) -> Option<&Option<String>> {
        match self.value {
            TokenValue::StringLiteral(ref val) => Some(val),
            TokenValue::RawStringLiteral(ref val) => Some(val),
            _ => None,
        }
    }
    fn get_char_lit_val(&self) -> Option<&Option<char>> {
        match self.value {
            TokenValue::CharLiteral(ref val) => Some(val),
            _ => None,
        }
    }
    fn get_num_lit_val(&self) -> Option<&Option<NumericLiteralValue>> {
        match self.value {
            TokenValue::NumericLiteral(ref val) => Some(val),
            _ => None,
        }
    }
    fn get_bool_lit_val(&self) -> Option<bool> {
        match self.value {
            TokenValue::BooleanLiteral(val) => Some(val),
            _ => None
        }
    }

    fn get_position(&self) -> StringPosition {
        self.pos
    }
}

// Lexer
pub struct V4Lexer {
    buf: Vec<V4Token>,
    eof_token: V4Token,
    messages: MessageEmitter,
}

impl V4Lexer {
    
    pub fn new(content: String) -> V4Lexer {
        use lexical::buf_lexer::ILexer;

        let mut messages = MessageEmitter::new();
        let mut v3lexer = V3Lexer::from(content);
        let mut buf = Vec::new();
        loop {
            match v3lexer.next(&mut messages) {
                Some(token) => buf.push(V4Token::from(token)),
                None => break,
            }
        }

        V4Lexer{ 
            buf: buf, 
            messages: messages,
            eof_token: V4Token{ value: TokenValue::EndofFile, pos: StringPosition::from2(v3lexer.position(), v3lexer.position()) },
        }
    }
    #[cfg(test)] // for test, receive string literal, provide new messages for checking
    pub fn new_test(content: &str, messages: MessageEmitter) -> V4Lexer {
        let mut lexer = V4Lexer::new(content.to_owned());
        lexer.messages = messages;
        lexer
    }

    pub fn len(&self) -> usize {
        self.buf.len()
    }
    pub fn pos(&self, idx: usize) -> StringPosition { 
        if idx >= self.buf.len() { 
            self.eof_token.get_position()
        } else {
            self.buf[idx].get_position()
        }
    }
    pub fn nth(&self, idx: usize) -> V4Token {
        if idx >= self.buf.len() { 
            self.eof_token.clone() 
        } else { 
            self.buf[idx].clone()
        }
    }

    // MessageEmitter
    pub fn push(&mut self, message: Message) {
        self.messages.push(message);
    }
    pub fn push_expect<T>(&mut self, final_token: &str, index: usize, sym_size: usize) -> (Option<T>, usize) {
        self.push_expects(vec![final_token], index, sym_size)
    }
    pub fn push_expects<T>(&mut self, final_tokens: Vec<&str>, index: usize, sym_size: usize) -> (Option<T>, usize) {

        let mut desc = final_tokens.into_iter().fold(String::new(), |mut buf, token| {
            buf.push_str(token);
            buf.push_str(", ");
            buf
        });
        if desc.len() > 2 {
            let target_len = desc.len() - 2; 
            desc.truncate(target_len); 
        }

        self.messages.push(Message::ExpectSymbol{ 
            expect: desc,
            actual: format!("{:?}", self.buf[index]), 
            pos: if index >= self.buf.len() { self.eof_token.get_position().start_pos } else { self.buf[index].get_position().start_pos },
        });
        return (None, sym_size);
    }
    pub fn messages(&self) -> &MessageEmitter {
        &self.messages
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn v4_base() {
        use common::StringPosition;
        use message::MessageEmitter;
        use super::TokenValue;
        use super::V4Lexer;
        use super::V4Token;
        use lexical::IToken;
        use lexical::symbol_type::NumericLiteralValue;
        use lexical::symbol_type::SeperatorKind;

        // numeric, 123, 1:1-1:3
        // identifier, abc, 1:5-1:7
        // char, 'd', 1:9-1:11
        // seperator, comma, 1:12-1:12
        // seperator, leftbracket, 1:14-1:14
        // numeric, 1, 1:15-1:15
        // seperator, rightbracket, 1:16-1:16
        let messages = MessageEmitter::new();
        let lexer = V4Lexer::new_test("123 abc 'd', [1]", messages);

        assert_eq!(lexer.nth(0), V4Token{ value: TokenValue::NumericLiteral(Some(NumericLiteralValue::I32(123))), pos: StringPosition::from((1, 1, 1, 3)) });
        assert_eq!(lexer.pos(0), StringPosition::from((1, 1, 1, 3)));
        assert_eq!(lexer.nth(0).is_num_lit(), true);

        assert_eq!(lexer.nth(1), V4Token{ value: TokenValue::Identifier("abc".to_owned()), pos: StringPosition::from((1, 5, 1, 7)) });
        assert_eq!(lexer.pos(1), StringPosition::from((1, 5, 1, 7)));
        assert_eq!(lexer.nth(1).is_identifier("abc"), true);
        assert_eq!(lexer.nth(1).get_identifier(), Some(&("abc".to_owned())));

        assert_eq!(lexer.nth(2), V4Token{ value: TokenValue::CharLiteral(Some('d')), pos: StringPosition::from((1, 9, 1, 11)) });
        assert_eq!(lexer.pos(2), StringPosition::from((1, 9, 1, 11)));
        assert_eq!(lexer.nth(2).is_char_lit(), true);

        assert_eq!(lexer.nth(3), V4Token{ value: TokenValue::Seperator(SeperatorKind::Comma), pos: StringPosition::from((1, 12, 1, 12)) });
        assert_eq!(lexer.pos(3), StringPosition::from((1, 12, 1, 12)));
        assert_eq!(lexer.nth(3).is_seperator(SeperatorKind::Comma), true);

        assert_eq!(lexer.nth(4), V4Token{ value: TokenValue::Seperator(SeperatorKind::LeftBracket), pos: StringPosition::from((1, 14, 1, 14)) });
        assert_eq!(lexer.pos(4), StringPosition::from((1, 14, 1, 14)));
        assert_eq!(lexer.nth(4).is_seperator(SeperatorKind::LeftBracket), true);

        assert_eq!(lexer.nth(5), V4Token{ value: TokenValue::NumericLiteral(Some(NumericLiteralValue::I32(1))), pos: StringPosition::from((1, 15, 1, 15)) });
        assert_eq!(lexer.pos(5), StringPosition::from((1, 15, 1, 15)));
        assert_eq!(lexer.nth(5).is_num_lit(), true);

        assert_eq!(lexer.nth(6), V4Token{ value: TokenValue::Seperator(SeperatorKind::RightBracket), pos: StringPosition::from((1, 16, 1, 16)) });
        assert_eq!(lexer.pos(6), StringPosition::from((1, 16, 1, 16)));
        assert_eq!(lexer.nth(6).is_seperator(SeperatorKind::RightBracket), true);

        assert_eq!(lexer.nth(7), V4Token{ value: TokenValue::EndofFile, pos: StringPosition::from((1, 17, 1, 17)) });
        assert_eq!(lexer.pos(7), StringPosition::from((1, 17, 1, 17)));
        assert_eq!(lexer.nth(7).is_eof(), true);

        assert_eq!(lexer.nth(8), V4Token{ value: TokenValue::EndofFile, pos: StringPosition::from((1, 17, 1, 17)) });
        assert_eq!(lexer.pos(8), StringPosition::from((1, 17, 1, 17)));
        assert_eq!(lexer.nth(8).is_eof(), true);
    }

    #[test]
    fn v4_push() {
        use super::V4Lexer as Lexer;

        let lexer = &mut Lexer::new("abcdef".to_owned());
        let _ = lexer.push_expect::<i32>("123", 0, 0);
        let _ = lexer.push_expects::<i32>(vec!["456", "789"], 0, 0);
        perrorln!("Messages: {:?}", lexer.messages());
    }
}