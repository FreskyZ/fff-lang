///! fff-lang
///!
///! syntax/root
///! root = { item }

// use std::fmt;

use codemap::SourceMap;
use codemap::SourceCode;
use codemap::SymbolCollection;
use message::MessageCollection;
use lexical::TokenStream;

use super::Module;
use super::ParseResult;
use super::ParseSession;
use super::ISyntaxParse;

#[cfg_attr(test, derive(Debug, Eq, PartialEq))]
pub struct SyntaxTree {
    pub modules: Vec<Module>
}
impl SyntaxTree {

    fn _new_module(source: &SourceCode,  messages: &mut MessageCollection, symbols: &mut SymbolCollection) -> ParseResult<Module> {
        let tokens = TokenStream::new(source, messages, symbols);
        let mut sess = ParseSession::new(&tokens, messages, symbols);
        return Module::parse(&mut sess);
    }

    pub fn new(_sources: &mut SourceMap, _messages: &mut MessageCollection, _symbols: &mut SymbolCollection) -> Result<SyntaxTree, ()> {
        Err(())
    }
}

