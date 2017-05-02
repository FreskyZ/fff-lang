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
            V2Token::Literal(ref lit) => write!(f, "{:?} ", lit),
            V2Token::Identifier(ref name) => write!(f, "Ident '{}' ", name),
            V2Token::Label(ref name) => write!(f, "Lable '@{}'", name),
            V2Token::Keyword(ref kind) => write!(f, "Keyword {:?} ", kind),
            V2Token::Seperator(ref kind) => write!(f, "Seperator {:?} ", kind),
            V2Token::EOF => write!(f, "<EOF> "), 
            V2Token::EOFs => write!(f, "<EOFs> "),
        });
        write!(f, "{:?}", self.1)
    }
}
impl IToken for V4Token {

    fn is_eof(&self) -> bool { match self.0 { V2Token::EOF => true, _ => false } }
    fn is_eofs(&self) -> bool { match self.0 { V2Token::EOFs => true, _ => false } }
    fn is_label(&self) -> bool { match self.0 { V2Token::Label(_) => true, _ => false } }
    fn is_lit(&self) -> bool { match self.0 { V2Token::Literal(_) => true, _ => false } }
    fn is_identifier(&self) -> bool { match self.0 { V2Token::Identifier(_) => true, _ => false } }
    fn is_keyword(&self, kind: KeywordKind) -> bool { match self.0 { V2Token::Keyword(ref self_kind) => *self_kind == kind, _ => false } }
    fn is_seperator(&self, kind: SeperatorKind) -> bool { match self.0 { V2Token::Seperator(ref self_kind) => *self_kind == kind, _ => false } }
    fn is_seperator_category(&self, category: SeperatorCategory) -> bool { 
        match self.0 { V2Token::Seperator(ref self_kind) => self_kind.is_category(category), _ => false } 
    }

    fn get_lit(&self) -> Option<LitValue> { match self.0 { V2Token::Literal(ref val) => Some(val.clone()), _ => None } }
    fn get_label(&self) -> Option<String> { match self.0 { V2Token::Label(ref name) => Some(name.clone()), _ => None } }
    fn get_keyword(&self) -> Option<KeywordKind> { match self.0 { V2Token::Keyword(ref kind) => Some(kind.clone()), _ => None } }
    fn get_identifier(&self) -> Option<String> { match self.0 { V2Token::Identifier(ref name) => Some(name.clone()), _ => None } }
    fn get_seperator(&self) -> Option<SeperatorKind> { match self.0 { V2Token::Seperator(ref kind) => Some(kind.clone()), _ => None } }

    fn get_strpos(&self) -> StringPosition { self.1 }
}

pub struct TokenStream {
    tokens: Vec<V4Token>,
    eofs_token: V4Token,
}
impl TokenStream {
    
    pub fn new<'a>(chars: CodeChars<'a>, messages: &mut MessageCollection) -> TokenStream {
        use super::buf_lexer::ILexer;

        let mut v2lexer = V2Lexer::new(chars, messages);
        let mut tokens = Vec::new();
        let eofs_pos: StringPosition;
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
            self.eofs_token.get_strpos()
        } else {
            self.tokens[idx].get_strpos()
        }
    }
    pub fn nth(&self, idx: usize) -> &IToken {
        if idx >= self.tokens.len() { 
            &self.eofs_token
        } else { 
            &self.tokens[idx]
        }
    }

    pub fn iter<'a>(&'a self) -> TokenStreamIter<'a> {
        TokenStreamIter{ stream: self, index: 0 }
    }
}

pub struct TokenStreamIter<'a> {
    stream: &'a TokenStream,
    index: usize,
}
impl<'a> Iterator for TokenStreamIter<'a> {
    type Item = &'a IToken;

    fn next(&mut self) -> Option<&'a IToken> {
        if self.index >= self.stream.len() {
            return None;
        } else {
            let retval = Some(self.stream.nth(self.index));
            self.index += 1;
            return retval;
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
    let tokens = TokenStream::new(CodeMap::with_test_str("123 abc 'd', [1]").iter(), messages);
    assert!(!messages.is_uncontinuable());

    assert_eq!(tokens.nth(0).get_lit().unwrap().is_num(), true);
    assert_eq!(tokens.nth(0).get_lit().unwrap().get_num(), &NumLitValue::I32(123));
    assert_eq!(tokens.nth(0).get_strpos(), make_str_pos!(1, 1, 1, 3));
    assert_eq!(tokens.pos(0), make_str_pos!(1, 1, 1, 3));

    assert_eq!(tokens.nth(1).get_identifier().unwrap(), "abc".to_owned());
    assert_eq!(tokens.nth(1).get_strpos(), make_str_pos!(1, 5, 1, 7));
    assert_eq!(tokens.pos(1), make_str_pos!(1, 5, 1, 7));

    assert_eq!(tokens.nth(2).get_lit().unwrap().is_char(), true);
    assert_eq!(tokens.nth(2).get_lit().unwrap().get_char(), 'd');
    assert_eq!(tokens.nth(2).get_strpos(), make_str_pos!(1, 9, 1, 11));
    assert_eq!(tokens.pos(2), make_str_pos!(1, 9, 1, 11));

    assert_eq!(tokens.nth(3).is_seperator(SeperatorKind::Comma), true);
    assert_eq!(tokens.nth(3).get_seperator().unwrap(), SeperatorKind::Comma);
    assert_eq!(tokens.nth(3).get_strpos(), tokens.pos(3));
    assert_eq!(tokens.pos(3), make_str_pos!(1, 12, 1, 12));

    assert_eq!(tokens.nth(4).is_seperator(SeperatorKind::LeftBracket), true);
    assert_eq!(tokens.nth(4).get_seperator().unwrap(), SeperatorKind::LeftBracket);
    assert_eq!(tokens.nth(4).get_strpos(), tokens.pos(4));
    assert_eq!(tokens.pos(4), make_str_pos!(1, 14, 1, 14));

    assert_eq!(tokens.nth(5).get_lit().unwrap().is_num(), true);
    assert_eq!(tokens.nth(5).get_lit().unwrap().get_num(), &NumLitValue::I32(1));
    assert_eq!(tokens.nth(5).get_strpos(), tokens.pos(5));
    assert_eq!(tokens.pos(5), make_str_pos!(1, 15, 1, 15));

    assert_eq!(tokens.nth(6).is_seperator(SeperatorKind::RightBracket), true);
    assert_eq!(tokens.nth(6).get_seperator().unwrap(), SeperatorKind::RightBracket);
    assert_eq!(tokens.nth(6).get_strpos(), tokens.pos(6));
    assert_eq!(tokens.pos(6), make_str_pos!(1, 16, 1, 16));

    assert_eq!(tokens.nth(7).is_eof(), true);
    assert_eq!(tokens.pos(7), make_str_pos!(1, 17, 1, 17));

    assert_eq!(tokens.nth(8).is_eofs(), true);
    assert_eq!(tokens.nth(8).get_strpos(), tokens.pos(8));
    assert_eq!(tokens.pos(8), make_str_pos!(1, 17, 1, 17));

    assert_eq!(tokens.nth(9).is_eofs(), true);
    assert_eq!(tokens.nth(9).get_strpos(), tokens.pos(9));
    assert_eq!(tokens.pos(9), make_str_pos!(1, 17, 1, 17));

    assert_eq!(tokens.nth(42).is_eofs(), true);
    assert_eq!(tokens.nth(42).get_strpos(), tokens.pos(8));  // this 8 here is not forgetten
    assert_eq!(tokens.pos(42), make_str_pos!(1, 17, 1, 17));
}
