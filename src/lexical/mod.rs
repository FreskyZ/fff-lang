///! lexical: the lexical parser

#[cfg(test)]
mod tests;
mod token;
mod token_buf;
mod v1lexer;
mod v2lexer;
mod unicode;

mod literal {
    pub mod char;
    pub mod escape;
    pub mod numeric;
    pub mod string;
    pub mod raw_string;
}

use crate::source::{Span, SourceChars, FileSystem};
use crate::diagnostics::MessageCollection;
use v2lexer::{V2Lexer};
use token_buf::{ILexer, BufLexer, ParseSession};
pub use token::{Separator, SeparatorKind, Keyword, KeywordKind};
pub use token::{Numeric, StringLiteralType, Token, TokenFormat};

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
                (Token::EOF, span) => { eof_span = span; break; }
                (v2, span) => items.push((v2, span)),
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
