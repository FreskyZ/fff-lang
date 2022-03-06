///! fff-lang
///!
///! token stream, vec<token> wrapper

use crate::source::{Span, SourceChars, FileSystem, IsId};
use crate::diagnostics::MessageCollection;
use super::{Token, ILexer, ParseSession, LitValue, NumLitValue, Numeric, StringLiteralType};
use super::v2lexer::{V2Lexer, V2Token};

impl From<V2Token> for Token {
    fn from(v2: V2Token) -> Token {
        match v2 {
            V2Token::EOF => Token::EOF,
            V2Token::Label(label) => Token::Label(label),
            V2Token::Literal(LitValue::Bool(v)) => Token::Bool(v),
            V2Token::Literal(LitValue::Char(Some(v))) => Token::Char(v),
            V2Token::Literal(LitValue::Char(None)) => Token::Char('\0'),
            V2Token::Literal(LitValue::Str(Some(id))) => Token::Str(id, StringLiteralType::Normal),
            V2Token::Literal(LitValue::Str(None)) => Token::Str(IsId::new(1), StringLiteralType::Normal),
            V2Token::Literal(LitValue::Num(Some(NumLitValue::I8(v)))) => Token::Num(Numeric::I8(v)),
            V2Token::Literal(LitValue::Num(Some(NumLitValue::U8(v)))) => Token::Num(Numeric::U8(v)),
            V2Token::Literal(LitValue::Num(Some(NumLitValue::I16(v)))) => Token::Num(Numeric::I16(v)),
            V2Token::Literal(LitValue::Num(Some(NumLitValue::U16(v)))) => Token::Num(Numeric::U16(v)),
            V2Token::Literal(LitValue::Num(Some(NumLitValue::I32(v)))) => Token::Num(Numeric::I32(v)),
            V2Token::Literal(LitValue::Num(Some(NumLitValue::U32(v)))) => Token::Num(Numeric::U32(v)),
            V2Token::Literal(LitValue::Num(Some(NumLitValue::I64(v)))) => Token::Num(Numeric::I64(v)),
            V2Token::Literal(LitValue::Num(Some(NumLitValue::U64(v)))) => Token::Num(Numeric::U64(v)),
            V2Token::Literal(LitValue::Num(Some(NumLitValue::R32(v)))) => Token::Num(Numeric::R32(v)),
            V2Token::Literal(LitValue::Num(Some(NumLitValue::R64(v)))) => Token::Num(Numeric::R64(v)),
            V2Token::Literal(LitValue::Num(None)) => Token::Num(Numeric::I32(0)),
            V2Token::Identifier(ident) => Token::Ident(ident),
            V2Token::Separator(sep) => Token::Sep(sep),
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
    pub fn new<F>(chars: SourceChars<F>, messages: &mut MessageCollection) -> TokenStream where F: FileSystem {

        let mut sess = ParseSession::new(messages);
        let mut v2lexer = V2Lexer::new(chars);
        let mut items = Vec::new();
        let eof_span: Span;
        loop {
            match v2lexer.next(&mut sess) {
                (V2Token::EOF, span) => { eof_span = span; break; }
                (v2, span) => items.push((v2.into(), span)),
            }
        }

        v2lexer.v1.lexer.v0.lexer.0.finish();
        TokenStream { items, eof_token: (Token::EOF, eof_span) }
    }
    pub fn nth_token(&self, idx: usize) -> &Token {
        if idx >= self.items.len() { &self.eof_token.0 } else { &self.items[idx].0 }
    }
    pub fn nth_span(&self, idx: usize) -> Span { 
        if idx >= self.items.len() { self.eof_token.1 } else { self.items[idx].1 }
    }

    // pub fn with_test_str(src: &str) -> TokenStream { TokenStream::with_test_input(src, None).0 }
    // pub fn with_test_input(src: &str, syms: Option<SymbolCollection>) -> (TokenStream, Rc<SourceCode>, MessageCollection, SymbolCollection) {
    //     let mut msgs = MessageCollection::new();
    //     let mut syms = syms.unwrap_or_default();
    //     let source = Rc::new(make_node!(0, src));
    //     let retval = TokenStream::new(source.as_ref(), &mut msgs, &mut syms);
    //     return (retval, source, msgs, syms);
    // }
}

#[cfg(test)] #[test]
fn v4_base() { // remain the name of v4 here for memory
    use crate::source::{IsId, make_source};
    use super::*;

    // numeric, 123, 1:1-1:3
    // identifier, abc, 1:5-1:7
    // char, 'd', 1:9-1:11
    // seperator, comma, 1:12-1:12
    // seperator, leftbracket, 1:14-1:14
    // numeric, 1, 1:15-1:15
    // seperator, rightbracket, 1:16-1:16
    // EOF, 1:17-1:17
    // EOFs, 1:17-1:17
    let mut scx = make_source!("123 abc 'd', [1]");
    let chars = scx.entry("1");
    let mut messages = MessageCollection::new();
    let tokens = TokenStream::new(chars, &mut messages);

    assert_eq!(tokens.nth_token(0), &Token::Num(Numeric::I32(123)));
    assert_eq!(tokens.nth_span(0), Span::new(0, 2));

    assert_eq!(tokens.nth_token(1), &Token::Ident(IsId::new(2)));
    assert_eq!(tokens.nth_span(1), Span::new(4, 6));

    assert_eq!(tokens.nth_token(2), &Token::Char('d'));
    assert_eq!(tokens.nth_span(2), Span::new(8, 10));

    assert_eq!(tokens.nth_token(3), &Token::Sep(Separator::Comma));
    assert_eq!(tokens.nth_span(3), Span::new(11, 11));

    assert_eq!(tokens.nth_token(4), &Token::Sep(Separator::LeftBracket));
    assert_eq!(tokens.nth_span(4), Span::new(13, 13));

    assert_eq!(tokens.nth_token(5), &Token::Num(Numeric::I32(1)));
    assert_eq!(tokens.nth_span(5), Span::new(14, 14));

    assert_eq!(tokens.nth_token(6), &Token::Sep(Separator::RightBracket));
    assert_eq!(tokens.nth_span(6), Span::new(15, 15));

    assert_eq!(tokens.nth_token(7), &Token::EOF);
    assert_eq!(tokens.nth_span(7), Span::new(16, 16));

    assert_eq!(tokens.nth_token(8), &Token::EOF);
    assert_eq!(tokens.nth_span(8), Span::new(16, 16));

    assert_eq!(tokens.nth_token(9), &Token::EOF);
    assert_eq!(tokens.nth_span(9), Span::new(16, 16));

    assert_eq!(tokens.nth_token(42), &Token::EOF);
    assert_eq!(tokens.nth_span(42), Span::new(16, 16));
}
