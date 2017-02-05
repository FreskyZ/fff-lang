
// v4 lexer, input v3, act as lexer interface

use std::fmt;
use codemap::StringPosition;
use message::SyntaxMessage;
use message::Message;
use message::MessageEmitter;

use lexical::symbol_type::string_literal::StringLiteral;
use lexical::symbol_type::char_literal::CharLiteral;
use lexical::symbol_type::numeric_literal::NumericLiteral;
use lexical::KeywordKind;
use lexical::SeperatorKind;
use lexical::LitValue;

use lexical::v3lexer::V3Lexer;
use lexical::v3lexer::V3Token;

use lexical::IToken;

// Token
test_only_attr!{
    [derive(Clone, Eq, PartialEq)]
    ![derive(Clone)]
    pub enum TokenValue {
        Literal(LitValue),
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
            V3Token::StringLiteral(StringLiteral{ value, pos, is_raw: _is_raw }) => 
                V4Token{ value: TokenValue::Literal(LitValue::Str(value)), pos: pos },
            V3Token::NumericLiteral(NumericLiteral{ value, pos }) => 
                V4Token{ value: TokenValue::Literal(LitValue::Num(value)), pos: pos },
            V3Token::CharLiteral(CharLiteral{ value, pos }) => 
                V4Token{ value: TokenValue::Literal(LitValue::Char(value)), pos: pos },
            V3Token::BooleanLiteral(value, pos) => 
                V4Token{ value: TokenValue::Literal(LitValue::Bool(value)), pos: pos },
            V3Token::Identifier(name, pos) => V4Token{ value: TokenValue::Identifier(name), pos: pos },
            V3Token::Keyword(kind, pos) => V4Token{ value: TokenValue::Keyword(kind), pos: pos },
            V3Token::Seperator(kind, pos) => V4Token{ value: TokenValue::Seperator(kind), pos: pos },
        }
    }
}

impl fmt::Debug for V4Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(match self.value {
            TokenValue::Literal(ref lit) => write!(f, "{:?} at ", lit),
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
    fn is_spec_ident(&self, name: &str) -> bool {
        match self.value {
            TokenValue::Identifier(ref self_name) => self_name == name,
            _ => false,
        }
    }
    fn is_ident(&self) -> bool {
        match self.value {
            TokenValue::Identifier(_) => true,
            _ => false,
        }
    }
    fn is_eof(&self) -> bool {
        match self.value {
            TokenValue::EndofFile => true, 
            _ => false,
        }
    }

    fn is_lit(&self) -> bool {
        match self.value {
            TokenValue::Literal(_) => true,
            _ => false,
        }
    }
    fn is_str_lit(&self) -> bool {
        match self.value {
            TokenValue::Literal(LitValue::Str(_)) => true,
            _ => false,
        }
    }
    fn is_num_lit(&self) -> bool {
        match self.value {
            TokenValue::Literal(LitValue::Num(_)) => true,
            _ => false,
        }
    }
    fn is_char_lit(&self) -> bool {
        match self.value {
            TokenValue::Literal(LitValue::Char(_)) => true,
            _ => false,
        }
    }
    fn is_bool_lit(&self) -> bool {
        match self.value {
            TokenValue::Literal(LitValue::Bool(_)) => true,
            _ => false,
        }
    }

    fn get_keyword(&self) -> Option<KeywordKind> {
        match self.value {
            TokenValue::Keyword(ref kind) => Some(kind.clone()),
            _ => None
        }
    }
    fn get_seperator(&self) -> Option<SeperatorKind> {
        match self.value {
            TokenValue::Seperator(ref kind) => Some(kind.clone()),
            _ => None
        }
    }
    fn get_identifier(&self) -> Option<String> {
        match self.value {
            TokenValue::Identifier(ref name) => Some(name.clone()),
            _ => None
        }
    }
    fn get_lit_val(&self) -> Option<LitValue> {
        match self.value {
            TokenValue::Literal(ref val) => Some(val.clone()),
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
    
    pub fn new(content: &str) -> V4Lexer {
        use lexical::buf_lexer::IDetailLexer;

        let mut messages = MessageEmitter::new();
        let mut v3lexer = V3Lexer::from(content.chars());
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

    // But after syntax, this method is not used.... no one cares about length, they only knows it is eof and report unexpected error
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
    pub fn nth(&self, idx: usize) -> &IToken {
        if idx >= self.buf.len() { 
            &self.eof_token
        } else { 
            &self.buf[idx]
        }
    }

    // MessageEmitter
    pub fn push<T: Into<Message>>(&mut self, message: T) {
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

        let actual_token = if index >= self.buf.len() { 
            &self.eof_token 
        } else { 
            &self.buf[index]
        };

        self.messages.push(SyntaxMessage::ExpectSymbol{ 
            expect: desc,
            actual: format!("{:?}", actual_token), 
            pos: actual_token.get_position().start_pos,
        });
        return (None, sym_size);
    }
    pub fn messages(&self) -> &MessageEmitter {
        &self.messages
    }
    pub fn into_messages(self) -> MessageEmitter {
        self.messages
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn v4_base() {
        use codemap::StringPosition;
        use super::V4Lexer;
        use lexical::NumLitValue;
        use lexical::SeperatorKind;

        // numeric, 123, 1:1-1:3
        // identifier, abc, 1:5-1:7
        // char, 'd', 1:9-1:11
        // seperator, comma, 1:12-1:12
        // seperator, leftbracket, 1:14-1:14
        // numeric, 1, 1:15-1:15
        // seperator, rightbracket, 1:16-1:16
        let lexer = V4Lexer::new("123 abc 'd', [1]");

        assert_eq!(lexer.nth(0).is_num_lit(), true);
        assert_eq!(lexer.nth(0).get_lit_val().unwrap().get_num().unwrap(), &Some(NumLitValue::I32(123)));
        assert_eq!(lexer.nth(0).get_position(), make_str_pos!(1, 1, 1, 3));
        assert_eq!(lexer.pos(0), make_str_pos!(1, 1, 1, 3));

        assert_eq!(lexer.nth(1).is_spec_ident("abc"), true);
        assert_eq!(lexer.nth(1).get_identifier().unwrap(), format!("abc"));
        assert_eq!(lexer.nth(1).get_position(), make_str_pos!(1, 5, 1, 7));
        assert_eq!(lexer.pos(1), make_str_pos!(1, 5, 1, 7));

        assert_eq!(lexer.nth(2).is_char_lit(), true);
        assert_eq!(lexer.nth(2).get_lit_val().unwrap().get_char().unwrap(), &Some('d'));
        assert_eq!(lexer.nth(2).get_position(), make_str_pos!(1, 9, 1, 11));
        assert_eq!(lexer.pos(2), make_str_pos!(1, 9, 1, 11));

        assert_eq!(lexer.nth(3).is_seperator(SeperatorKind::Comma), true);
        assert_eq!(lexer.nth(3).get_seperator().unwrap(), SeperatorKind::Comma);
        assert_eq!(lexer.nth(3).get_position(), lexer.pos(3));
        assert_eq!(lexer.pos(3), make_str_pos!(1, 12, 1, 12));

        assert_eq!(lexer.nth(4).is_seperator(SeperatorKind::LeftBracket), true);
        assert_eq!(lexer.nth(4).get_seperator().unwrap(), SeperatorKind::LeftBracket);
        assert_eq!(lexer.nth(4).get_position(), lexer.pos(4));
        assert_eq!(lexer.pos(4), make_str_pos!(1, 14, 1, 14));

        assert_eq!(lexer.nth(5).is_num_lit(), true);
        assert_eq!(lexer.nth(5).get_lit_val().unwrap().get_num().unwrap(), &Some(NumLitValue::I32(1)));
        assert_eq!(lexer.nth(5).get_position(), lexer.pos(5));
        assert_eq!(lexer.pos(5), make_str_pos!(1, 15, 1, 15));

        assert_eq!(lexer.nth(6).is_seperator(SeperatorKind::RightBracket), true);
        assert_eq!(lexer.nth(6).get_seperator().unwrap(), SeperatorKind::RightBracket);
        assert_eq!(lexer.nth(6).get_position(), lexer.pos(6));
        assert_eq!(lexer.pos(6), make_str_pos!(1, 16, 1, 16));

        assert_eq!(lexer.nth(7).is_eof(), true);
        assert_eq!(lexer.pos(7), make_str_pos!(1, 17, 1, 17));

        assert_eq!(lexer.nth(8).is_eof(), true);
        assert_eq!(lexer.nth(8).get_position(), lexer.pos(8));
        assert_eq!(lexer.pos(8), make_str_pos!(1, 17, 1, 17));

        assert_eq!(lexer.nth(42).is_eof(), true);
        assert_eq!(lexer.nth(42).get_position(), lexer.pos(8));  // this 8 here is not forgetten
        assert_eq!(lexer.pos(42), make_str_pos!(1, 17, 1, 17));
    }

    #[test]
    fn v4_push() {
        use super::V4Lexer as Lexer;

        let lexer = &mut Lexer::new("abcdef");
        let _ = lexer.push_expect::<i32>("123", 0, 0);
        let _ = lexer.push_expects::<i32>(vec!["456", "789"], 0, 0);
        perrorln!("Messages: {:?}", lexer.messages());
    }
}