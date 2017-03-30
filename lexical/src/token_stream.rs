///! fff-lang
///!
///! v4 lexer, input v2, act as lexer interface

use std::fmt;
use codepos::StringPosition;
use message::MessageCollection;
use codemap::CodeChars;

use super::LitValue;
use super::KeywordKind;
use super::SeperatorKind;
use super::SeperatorCategory;

use super::v2lexer::V2Lexer;
use super::v2lexer::V2Token;

use super::IToken;

#[cfg_attr(test, derive(Eq, PartialEq))]  
struct V4Token(V2Token, StringPosition);
impl fmt::Debug for V4Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(match self.0 {
            V2Token::Literal(ref lit) => write!(f, "{:?} at ", lit),
            V2Token::Identifier(ref name) => write!(f, "Identifier `{}` at ", name),
            V2Token::Keyword(ref kind) => write!(f, "Keyword {:?} at ", kind),
            V2Token::Seperator(ref kind) => write!(f, "Seperator {:?} at ", kind),
            V2Token::EOF => write!(f, "<EOF> at "), 
            V2Token::EOFs => write!(f, "<EOFs> at "),
        });
        write!(f, "{:?}", self.1)
    }
}
impl IToken for V4Token {

    fn is_keyword(&self, kind: KeywordKind) -> bool { match self.0 { V2Token::Keyword(ref self_kind) => *self_kind == kind, _ => false } }
    fn is_seperator(&self, kind: SeperatorKind) -> bool { match self.0 { V2Token::Seperator(ref self_kind) => *self_kind == kind, _ => false } }
    fn is_spec_ident(&self, name: &str) -> bool { match self.0 { V2Token::Identifier(ref self_name) => self_name == name, _ => false } }
    fn is_ident(&self) -> bool { match self.0 { V2Token::Identifier(_) => true, _ => false } }
    fn is_eof(&self) -> bool { match self.0 { V2Token::EOF => true, _ => false } }
    fn is_eofs(&self) -> bool { match self.0 { V2Token::EOFs => true, _ => false } }

    fn is_lit(&self) -> bool { match self.0 { V2Token::Literal(_) => true, _ => false } }
    fn is_str_lit(&self) -> bool { match self.0 { V2Token::Literal(LitValue::Str(_)) => true, _ => false } }
    fn is_num_lit(&self) -> bool { match self.0 { V2Token::Literal(LitValue::Num(_)) => true, _ => false } }
    fn is_char_lit(&self) -> bool { match self.0 { V2Token::Literal(LitValue::Char(_)) => true, _ => false } }
    fn is_bool_lit(&self) -> bool { match self.0 { V2Token::Literal(LitValue::Bool(_)) => true, _ => false } }

    fn is_seperator_category(&self, category: SeperatorCategory) -> bool { 
        match self.0 { V2Token::Seperator(ref seperator) => seperator.is_category(category), _ => false } 
    }

    fn get_keyword(&self) -> Option<KeywordKind> { match self.0 { V2Token::Keyword(ref kind) => Some(kind.clone()), _ => None } }
    fn get_seperator(&self) -> Option<SeperatorKind> { match self.0 { V2Token::Seperator(ref kind) => Some(kind.clone()), _ => None } }
    fn get_identifier(&self) -> Option<String> { match self.0 { V2Token::Identifier(ref name) => Some(name.clone()), _ => None } }
    fn get_lit_val(&self) -> Option<LitValue> { match self.0 { V2Token::Literal(ref val) => Some(val.clone()), _ => None } }

    fn get_position(&self) -> StringPosition { self.1 }
}

pub struct TokenStream {
    tokens: Vec<V4Token>,
    eofs_token: V4Token,
}
impl TokenStream {
    
    #[allow(unused_assignments)] // value assigned to eofs_pos is never used
    pub fn new<'a>(chars: CodeChars<'a>, messages: &mut MessageCollection) -> TokenStream {
        use super::buf_lexer::ILexer;

        let mut v2lexer = V2Lexer::new(chars, messages);
        let mut tokens = Vec::new();
        let mut eofs_pos = StringPosition::new();
        loop {
            match v2lexer.next(messages) {
                (V2Token::EOFs, pos) => { eofs_pos = pos; break; }
                v2 => tokens.push(V4Token(v2.0, v2.1)),
            }
        }

        TokenStream { 
            tokens: tokens, 
            eofs_token: V4Token(V2Token::EOFs, eofs_pos),
        }
    }
    pub fn with_test_str(program: &str) -> TokenStream {
        use codemap::CodeMap;
        
        let mut codemap = CodeMap::with_test_str(program);
        let mut messages = MessageCollection::new();
        let ret_val = TokenStream::new(codemap.iter(), &mut messages);
        check_messages_continuable!(messages);
        return ret_val;
    }

    // But after syntax, this method is not used.... no one cares about length, they only knows it is eof and report unexpected error
    pub fn len(&self) -> usize {
        self.tokens.len()
    }
    pub fn pos(&self, idx: usize) -> StringPosition { 
        if idx >= self.tokens.len() { 
            self.eofs_token.get_position()
        } else {
            self.tokens[idx].get_position()
        }
    }
    pub fn nth(&self, idx: usize) -> &IToken {
        if idx >= self.tokens.len() { 
            &self.eofs_token
        } else { 
            &self.tokens[idx]
        }
    }
}

#[cfg(test)] #[test]
fn v4_base() { // remain the name of v4 here for memory
    use codemap::CodeMap;
    use super::NumLitValue;

    // numeric, 123, 1:1-1:3
    // identifier, abc, 1:5-1:7
    // char, 'd', 1:9-1:11
    // seperator, comma, 1:12-1:12
    // seperator, leftbracket, 1:14-1:14
    // numeric, 1, 1:15-1:15
    // seperator, rightbracket, 1:16-1:16
    // EOF, 1:17-1:17
    // EOFs, 1:17-1:17
    let messages = &mut MessageCollection::new();
    let lexer = TokenStream::new(CodeMap::with_test_str("123 abc 'd', [1]").iter(), messages);
    assert!(!messages.is_uncontinuable());

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

    assert_eq!(lexer.nth(8).is_eofs(), true);
    assert_eq!(lexer.nth(8).get_position(), lexer.pos(8));
    assert_eq!(lexer.pos(8), make_str_pos!(1, 17, 1, 17));

    assert_eq!(lexer.nth(9).is_eofs(), true);
    assert_eq!(lexer.nth(9).get_position(), lexer.pos(9));
    assert_eq!(lexer.pos(9), make_str_pos!(1, 17, 1, 17));

    assert_eq!(lexer.nth(42).is_eofs(), true);
    assert_eq!(lexer.nth(42).get_position(), lexer.pos(8));  // this 8 here is not forgetten
    assert_eq!(lexer.pos(42), make_str_pos!(1, 17, 1, 17));
}
