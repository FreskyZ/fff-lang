
// v4 lexer, input v3, act as lexer interface

use std::fmt;
use codepos::StringPosition;
use message::SyntaxMessage;
use message::LegacyMessage as Message;
use message::MessageCollection;
use codemap::CodeChars;

use super::KeywordKind;
use super::SeperatorKind;
use super::LitValue;

use super::v2lexer::V2Lexer;
use super::v2lexer::V2Token;

use super::IToken;

#[cfg(test)]
#[derive(Eq, PartialEq)]  
struct V4Token {
    value: V2Token,
    pos: StringPosition,
}
#[cfg(not(test))]
struct V4Token {
    value: V2Token, 
    pos: StringPosition,
}
impl V4Token {
    fn from(v2_and_pos: (V2Token, StringPosition)) -> V4Token {
        V4Token{ value: v2_and_pos.0, pos: v2_and_pos.1 }
    }
}
impl fmt::Debug for V4Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(match self.value {
            V2Token::Literal(ref lit) => write!(f, "{:?} at ", lit),
            V2Token::Identifier(ref name) => write!(f, "Identifier `{}` at ", name),
            V2Token::Keyword(ref kind) => write!(f, "Keyword {:?} at ", kind),
            V2Token::Seperator(ref kind) => write!(f, "Seperator {:?} at ", kind),
            V2Token::EOF => write!(f, "<EOF> at "), 
            V2Token::EOFs => write!(f, "<EOFs> at "),
        });
        write!(f, "{:?}", self.pos)
    }
}
impl IToken for V4Token {

    fn is_keyword(&self, kind: KeywordKind) -> bool {
        match self.value {
            V2Token::Keyword(ref self_kind) => *self_kind == kind,
            _ => false,
        }
    }
    fn is_seperator(&self, kind: SeperatorKind) -> bool {
        match self.value {
            V2Token::Seperator(ref self_kind) => *self_kind == kind,
            _ => false,
        }
    }
    fn is_spec_ident(&self, name: &str) -> bool {
        match self.value {
            V2Token::Identifier(ref self_name) => self_name == name,
            _ => false,
        }
    }
    fn is_ident(&self) -> bool {
        match self.value {
            V2Token::Identifier(_) => true,
            _ => false,
        }
    }
    fn is_eof(&self) -> bool {
        match self.value {
            V2Token::EOF => true, 
            _ => false,
        }
    }

    fn is_lit(&self) -> bool {
        match self.value {
            V2Token::Literal(_) => true,
            _ => false,
        }
    }
    fn is_str_lit(&self) -> bool {
        match self.value {
            V2Token::Literal(LitValue::Str(_)) => true,
            _ => false,
        }
    }
    fn is_num_lit(&self) -> bool {
        match self.value {
            V2Token::Literal(LitValue::Num(_)) => true,
            _ => false,
        }
    }
    fn is_char_lit(&self) -> bool {
        match self.value {
            V2Token::Literal(LitValue::Char(_)) => true,
            _ => false,
        }
    }
    fn is_bool_lit(&self) -> bool {
        match self.value {
            V2Token::Literal(LitValue::Bool(_)) => true,
            _ => false,
        }
    }

    fn get_keyword(&self) -> Option<KeywordKind> {
        match self.value {
            V2Token::Keyword(ref kind) => Some(kind.clone()),
            _ => None
        }
    }
    fn get_seperator(&self) -> Option<SeperatorKind> {
        match self.value {
            V2Token::Seperator(ref kind) => Some(kind.clone()),
            _ => None
        }
    }
    fn get_identifier(&self) -> Option<String> {
        match self.value {
            V2Token::Identifier(ref name) => Some(name.clone()),
            _ => None
        }
    }
    fn get_lit_val(&self) -> Option<LitValue> {
        match self.value {
            V2Token::Literal(ref val) => Some(val.clone()),
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
    messages: MessageCollection,
}
impl V4Lexer {
    
    #[allow(unused_assignments)]
    pub fn new<'a>(chars: CodeChars<'a>) -> V4Lexer {
        use super::buf_lexer::ILexer;

        let mut messages = MessageCollection::new();
        let mut v2lexer = V2Lexer::new(chars, &mut messages);
        let mut buf = Vec::new();
        let mut eof_pos = StringPosition::new();
        loop {
            match v2lexer.next(&mut messages) {
                (V2Token::EOFs, pos) => {
                    eof_pos = pos;
                    break;
                }
                v2 => buf.push(V4Token::from(v2)),
            }
        }

        V4Lexer{ 
            buf: buf, 
            messages: messages,
            eof_token: V4Token{ value: V2Token::EOF, pos: eof_pos },
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

    // MessageCollection
    pub fn push<T: Into<Message>>(&mut self, message: T) {
        self.messages.push(message);
    }
    pub fn messages(&self) -> &MessageCollection {
        &self.messages
    }
    pub fn into_messages(self) -> MessageCollection {
        self.messages
    }
}

#[cfg(test)]
#[test]
fn v4_base() {
    use codemap::CodeMap;
    use super::NumLitValue;

    // numeric, 123, 1:1-1:3
    // identifier, abc, 1:5-1:7
    // char, 'd', 1:9-1:11
    // seperator, comma, 1:12-1:12
    // seperator, leftbracket, 1:14-1:14
    // numeric, 1, 1:15-1:15
    // seperator, rightbracket, 1:16-1:16
    let lexer = V4Lexer::new(CodeMap::with_test_str("123 abc 'd', [1]").iter());

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

#[cfg(test)]
#[test]
fn v4_push() {
    use codepos::Position;
    use codemap::CodeMap;

    let lexer = &mut V4Lexer::new(CodeMap::with_test_str("abcdef").iter());
    assert_eq!(lexer.push_expect::<i32>("123", 0, 0), (None, 0));
    assert_eq!(lexer.push_expects::<i32>(vec!["456", "789"], 0, 0xABCD), (None, 0xABCD));
    
    let expect_messages = &mut MessageCollection::new();
    expect_messages.push(SyntaxMessage::ExpectSymbol{
        expect: "123".to_owned(),
        actual: format!("{:?}", V4Token{ value: V2Token::Identifier("abcdef".to_owned()), pos: make_str_pos!(1, 1, 1, 6) }),
        pos: make_pos!(1, 1),
    });
    expect_messages.push(SyntaxMessage::ExpectSymbol{
        expect: "456, 789".to_owned(),
        actual: format!("{:?}", V4Token{ value: V2Token::Identifier("abcdef".to_owned()), pos: make_str_pos!(1, 1, 1, 6) }),
        pos: make_pos!(1, 1),
    });
    assert_eq!(lexer.messages(), expect_messages);
}