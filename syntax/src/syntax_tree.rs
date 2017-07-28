///! fff-lang
///!
///! syntax/root
///! root = { item }

use std::fmt;

use codemap::SymbolCollection;
use message::MessageCollection;
use lexical::Token;
use lexical::TokenStream;

use super::Item;
use super::Formatter;
use super::ParseResult;
use super::ParseSession;
use super::ISyntaxParse;
use super::ISyntaxFormat;
use super::ISyntaxGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct SyntaxTree {
    pub items: Vec<Item>,
}
impl ISyntaxFormat for SyntaxTree {
    fn format(&self, f: Formatter) -> String {
        let mut f = f.indent().header_text_or("syntax-tree");
        for item in &self.items {
            f = f.endl().apply1(item);
        }
        f.finish()
    }
}
impl fmt::Debug for SyntaxTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(Formatter::empty())) }
}
impl SyntaxTree {
    pub fn new_items(items: Vec<Item>) -> SyntaxTree { SyntaxTree{ items } }
}
impl ISyntaxParse for SyntaxTree {
    type Output = SyntaxTree;

    fn parse(sess: &mut ParseSession) -> ParseResult<SyntaxTree> {
        let mut items = Vec::new();
        loop {
            if Item::matches_first(sess.current_tokens()) {
                items.push(Item::parse(sess)?);
            } else if sess.current_tokens()[0] == &Token::EOF {
                break;
            } else {
                return sess.push_unexpect("if, while, for, var, const, expr");
            }
        }
        return Ok(SyntaxTree::new_items(items));
    }
}
impl SyntaxTree {
    pub fn new(tokens: &TokenStream, messages: &mut MessageCollection, symbols: &mut SymbolCollection) -> SyntaxTree {
        let mut sess = ParseSession::new(tokens, messages, symbols);
        match SyntaxTree::parse(&mut sess) {
            Ok(tree) => tree,
            Err(_) => SyntaxTree::new_items(Vec::new()),
        }
    }
}

// TODO: case `fn main() { println("hello") }
// current message: Unexpect symbol, meet }, expect assignment operator, semicolon
// expect message: unexpect symbol, expect `;`, `.`, `(`, `+`, etc., meet `}`
// and recover this case
