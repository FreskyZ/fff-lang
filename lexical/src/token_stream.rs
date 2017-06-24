///! fff-lang
///!
///! token stream, vec<token> wrapper

use codemap::Span;
use codemap::CodeChars;
use codemap::SymbolCollection;
use message::MessageCollection;

use super::Token;
use super::ParseSession;
use super::v2lexer::V2Lexer;
use super::v2lexer::V2Token;

impl From<V2Token> for Token {
    fn from(v2: V2Token) -> Token {
        match v2 {
            V2Token::EOF | V2Token::EOFs => Token::EOF,
            V2Token::Label(label) => Token::Label(label),
            V2Token::Literal(lit) => Token::Lit(lit),
            V2Token::Identifier(ident) => Token::Ident(ident),
            V2Token::Seperator(sep) => Token::Sep(sep),
            V2Token::Keyword(kw) => Token::Keyword(kw),
        }
    }
}

pub struct TokenStream {
    tokens: Vec<(Token, Span)>,
    eofs_token: (Token, Span),
}
impl TokenStream {
    
    pub fn new<'a>(chars: CodeChars<'a>, messages: &mut MessageCollection, symbols: &mut SymbolCollection) -> TokenStream {
        use super::ILexer;

        let mut sess = ParseSession::new(messages, symbols);
        let mut v2lexer = V2Lexer::new(chars);
        let mut tokens = Vec::new();
        let eofs_span: Span;
        loop {
            match v2lexer.next(&mut sess) {
                (V2Token::EOFs, span) => { eofs_span = span; break; }
                (v2, span) => tokens.push((v2.into(), span)),
            }
        }

        TokenStream { 
            tokens: tokens, 
            eofs_token: (Token::EOF, eofs_span),
        }
    }
    pub fn with_test_str(program: &str) -> TokenStream {
        use codemap::CodeMap;
        
        let codemap = CodeMap::with_test_str(program);
        let mut messages = MessageCollection::new();
        let mut symbols = SymbolCollection::new();
        let ret_val = TokenStream::new(codemap.iter(), &mut messages, &mut symbols);
        check_messages_continuable!(messages);
        return ret_val;
    }
    pub fn with_test_input(program: &str, symbols: &mut SymbolCollection) -> TokenStream {
        use codemap::CodeMap;
        
        let codemap = CodeMap::with_test_str(program);
        let mut messages = MessageCollection::new();
        let mut symbols = symbols;
        let ret_val = TokenStream::new(codemap.iter(), &mut messages, symbols);
        check_messages_continuable!(messages);
        return ret_val;
    }

    // But after syntax, this method is not used.... no one cares about length, they only knows it is eof and report unexpected error
    pub fn len(&self) -> usize { self.tokens.len() }

    pub fn nth_token(&self, idx: usize) -> &Token {
        if idx >= self.tokens.len() { &self.eofs_token.0 } else { &self.tokens[idx].0 }
    }
    pub fn nth_span(&self, idx: usize) -> Span { 
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
            let retval = Some(self.stream.nth_token(self.index));
            self.index += 1;
            return retval;
        }
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
    let tokens = TokenStream::with_test_input("123 abc 'd', [1]", &mut make_symbols!["abc"]);

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
