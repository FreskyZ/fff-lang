///! fff-lang
///!
///! token stream, vec<token> wrapper

use codemap::Span;
use message::MessageCollection;
use codemap::CodeChars;

use super::v2lexer::V2Lexer;
use super::v2lexer::V2Token;
use super::Token;

struct TokenAndPos(Token, Span);

impl TokenAndPos {
    fn new(v2: (V2Token, Span)) -> TokenAndPos {
        TokenAndPos(
            match v2.0 {
                V2Token::EOF | V2Token::EOFs => Token::EOF,
                V2Token::Label(label) => Token::Label(label),
                V2Token::Literal(lit) => Token::Lit(lit),
                V2Token::Identifier(ident) => Token::Ident(ident),
                V2Token::Seperator(sep) => Token::Sep(sep),
                V2Token::Keyword(kw) => Token::Keyword(kw),
            },
            v2.1
        )
    }
}

pub struct TokenStream {
    tokens: Vec<TokenAndPos>,
    eofs_token: TokenAndPos,
}
impl TokenStream {
    
    pub fn new<'a>(chars: CodeChars<'a>, messages: &mut MessageCollection) -> TokenStream {
        use super::buf_lexer::ILexer;

        let mut v2lexer = V2Lexer::new(chars, messages);
        let mut tokens = Vec::new();
        let eofs_pos: Span;
        loop {
            match v2lexer.next(messages) {
                (V2Token::EOFs, pos) => { eofs_pos = pos; break; }
                v2_and_pos => tokens.push(TokenAndPos::new(v2_and_pos)),
            }
        }

        TokenStream { 
            tokens: tokens, 
            eofs_token: TokenAndPos::new((V2Token::EOFs, eofs_pos)),
        }
    }
    pub fn with_test_str(program: &str) -> TokenStream {
        use codemap::CodeMap;
        
        let codemap = CodeMap::with_test_str(program);
        let mut messages = MessageCollection::new();
        let ret_val = TokenStream::new(codemap.iter(), &mut messages);
        check_messages_continuable!(messages);
        return ret_val;
    }

    // But after syntax, this method is not used.... no one cares about length, they only knows it is eof and report unexpected error
    pub fn len(&self) -> usize { self.tokens.len() }

    pub fn nth(&self, idx: usize) -> &Token {
        if idx >= self.tokens.len() { &self.eofs_token.0 } else { &self.tokens[idx].0 }
    }
    pub fn pos(&self, idx: usize) -> Span { 
        if idx >= self.tokens.len() { self.eofs_token.1 } else { self.tokens[idx].1 }
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
    type Item = &'a Token;

    fn next(&mut self) -> Option<&'a Token> {
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
    use super::LitValue;
    use super::SeperatorKind;

    // numeric, 123, 1:1-1:3
    // identifier, abc, 1:5-1:7
    // char, 'd', 1:9-1:11
    // seperator, comma, 1:12-1:12
    // seperator, leftbracket, 1:14-1:14
    // numeric, 1, 1:15-1:15
    // seperator, rightbracket, 1:16-1:16
    // EOF, 1:17-1:17
    // EOFs, 1:17-1:17
    let tokens = TokenStream::with_test_str("123 abc 'd', [1]");

    assert_eq!(tokens.nth(0), &Token::Lit(LitValue::from(123)));
    assert_eq!(tokens.pos(0), make_span!(0, 2));

    assert_eq!(tokens.nth(1), &Token::Ident("abc".to_owned()));
    assert_eq!(tokens.pos(1), make_span!(4, 6));

    assert_eq!(tokens.nth(2), &Token::Lit(LitValue::from('d')));
    assert_eq!(tokens.pos(2), make_span!(8, 10));

    assert_eq!(tokens.nth(3), &Token::Sep(SeperatorKind::Comma));
    assert_eq!(tokens.pos(3), make_span!(11, 11));

    assert_eq!(tokens.nth(4), &Token::Sep(SeperatorKind::LeftBracket));
    assert_eq!(tokens.pos(4), make_span!(13, 13));

    assert_eq!(tokens.nth(5), &Token::Lit(LitValue::from(1)));
    assert_eq!(tokens.pos(5), make_span!(14, 14));

    assert_eq!(tokens.nth(6), &Token::Sep(SeperatorKind::RightBracket));
    assert_eq!(tokens.pos(6), make_span!(15, 15));

    assert_eq!(tokens.nth(7), &Token::EOF);
    assert_eq!(tokens.pos(7), make_span!(16, 16));

    assert_eq!(tokens.nth(8), &Token::EOF);
    assert_eq!(tokens.pos(8), make_span!(16, 16));

    assert_eq!(tokens.nth(9), &Token::EOF);
    assert_eq!(tokens.pos(9), make_span!(16, 16));

    assert_eq!(tokens.nth(42), &Token::EOF);
    assert_eq!(tokens.pos(42), make_span!(16, 16));
}
