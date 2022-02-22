///! fff-lang
///!
///! token stream, vec<token> wrapper

use std::rc::Rc;
use crate::codemap::{Span, SourceCode, SymbolCollection};
use crate::message::MessageCollection;
use super::{Token, ILexer, ParseSession};
use super::v2lexer::{V2Lexer, V2Token};

impl From<V2Token> for Token {
    fn from(v2: V2Token) -> Token {
        match v2 {
            V2Token::EOF => Token::EOF,
            V2Token::Label(label) => Token::Label(label),
            V2Token::Literal(lit) => Token::Lit(lit),
            V2Token::Identifier(ident) => Token::Ident(ident),
            V2Token::Seperator(sep) => Token::Sep(sep),
            V2Token::Keyword(kw) => Token::Keyword(kw),
        }
    }
}

pub struct TokenStream {
    items: Vec<(Token, Span)>,
    eof_token: (Token, Span),
}
impl TokenStream {
    
    /// main module driver
    pub fn new(source: &SourceCode, messages: &mut MessageCollection, symbols: &mut SymbolCollection) -> TokenStream {

        let mut sess = ParseSession::new(messages, symbols);
        let mut v2lexer = V2Lexer::new(source.iter());
        let mut items = Vec::new();
        let eof_span: Span;
        loop {
            match v2lexer.next(&mut sess) {
                (V2Token::EOF, span) => { eof_span = span; break; }
                (v2, span) => items.push((v2.into(), span)),
            }
        }

        TokenStream { items, eof_token: (Token::EOF, eof_span) }
    }
    pub fn nth_token(&self, idx: usize) -> &Token {
        if idx >= self.items.len() { &self.eof_token.0 } else { &self.items[idx].0 }
    }
    pub fn nth_span(&self, idx: usize) -> Span { 
        if idx >= self.items.len() { self.eof_token.1 } else { self.items[idx].1 }
    }

    pub fn with_test_str(src: &str) -> TokenStream { TokenStream::with_test_input(src, None).0 }
    pub fn with_test_input(src: &str, syms: Option<SymbolCollection>) -> (TokenStream, Rc<SourceCode>, MessageCollection, SymbolCollection) {
        let mut msgs = MessageCollection::new();
        let mut syms = syms.unwrap_or_default();
        let source = Rc::new(SourceCode::with_test_str(0, src));
        let retval = TokenStream::new(source.as_ref(), &mut msgs, &mut syms);
        return (retval, source, msgs, syms);
    }
}

#[cfg(test)] #[test]
fn v4_base() { // remain the name of v4 here for memory
    use super::LitValue;
    use super::Seperator;

    // numeric, 123, 1:1-1:3
    // identifier, abc, 1:5-1:7
    // char, 'd', 1:9-1:11
    // seperator, comma, 1:12-1:12
    // seperator, leftbracket, 1:14-1:14
    // numeric, 1, 1:15-1:15
    // seperator, rightbracket, 1:16-1:16
    // EOF, 1:17-1:17
    // EOFs, 1:17-1:17
    let tokens = TokenStream::with_test_input("123 abc 'd', [1]", None).0;

    assert_eq!(tokens.nth_token(0), &Token::Lit(LitValue::from(123)));
    assert_eq!(tokens.nth_span(0), make_span!(0, 2));

    assert_eq!(tokens.nth_token(1), &Token::Ident(make_id!(1)));
    assert_eq!(tokens.nth_span(1), make_span!(4, 6));

    assert_eq!(tokens.nth_token(2), &Token::Lit(LitValue::from('d')));
    assert_eq!(tokens.nth_span(2), make_span!(8, 10));

    assert_eq!(tokens.nth_token(3), &Token::Sep(Seperator::Comma));
    assert_eq!(tokens.nth_span(3), make_span!(11, 11));

    assert_eq!(tokens.nth_token(4), &Token::Sep(Seperator::LeftBracket));
    assert_eq!(tokens.nth_span(4), make_span!(13, 13));

    assert_eq!(tokens.nth_token(5), &Token::Lit(LitValue::from(1)));
    assert_eq!(tokens.nth_span(5), make_span!(14, 14));

    assert_eq!(tokens.nth_token(6), &Token::Sep(Seperator::RightBracket));
    assert_eq!(tokens.nth_span(6), make_span!(15, 15));

    assert_eq!(tokens.nth_token(7), &Token::EOF);
    assert_eq!(tokens.nth_span(7), make_span!(16, 16));

    assert_eq!(tokens.nth_token(8), &Token::EOF);
    assert_eq!(tokens.nth_span(8), make_span!(16, 16));

    assert_eq!(tokens.nth_token(9), &Token::EOF);
    assert_eq!(tokens.nth_span(9), make_span!(16, 16));

    assert_eq!(tokens.nth_token(42), &Token::EOF);
    assert_eq!(tokens.nth_span(42), make_span!(16, 16));
}
