///! fff-lang
///!
///! syntax/root
///! root = { fndef | typedef }

use std::fmt;

use codemap::SymbolCollection;
use message::MessageCollection;
use lexical::TokenStream;
use lexical::Token;

use super::FnDef;
use super::TypeDef;
use super::ParseSession;
use super::ParseResult;
use super::ISyntaxItemParse;
use super::ISyntaxItemFormat;
use super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct SyntaxTree {
    pub types: Vec<TypeDef>,
    pub fns: Vec<FnDef>,
}
impl ISyntaxItemFormat for SyntaxTree {
    fn format(&self, indent: u32) -> String {
        format!("{}SyntaxTree{}{}", 
            SyntaxTree::indent_str(indent),
            self.types.iter().fold(String::new(), |mut buf, item| { buf.push_str("\n"); buf.push_str(&item.format(indent + 1)); buf }),
            self.fns.iter().fold(String::new(), |mut buf, item| { buf.push_str("\n"); buf.push_str(&item.format(indent + 1)); buf }))
    }
}
impl fmt::Debug for SyntaxTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(0)) }
}
impl SyntaxTree {
    pub fn new_items(types: Vec<TypeDef>, fns: Vec<FnDef>) -> SyntaxTree { SyntaxTree{ types, fns } }
}
impl ISyntaxItemParse for SyntaxTree {
    type Target = SyntaxTree;

    fn parse(sess: &mut ParseSession) -> ParseResult<SyntaxTree> {

        let mut types = Vec::new();
        let mut fns = Vec::new();
        loop {
            if TypeDef::is_first_final(sess) {
                types.push(TypeDef::parse(sess)?);
            } else if FnDef::is_first_final(sess) {
                fns.push(FnDef::parse(sess)?);
            } else if sess.tk == &Token::EOF {
                break;
            } // else if sess.tk == &Token::EOFs { break; }
        }
        return Ok(SyntaxTree::new_items(types, fns));
    }
}
impl SyntaxTree {
    pub fn new(tokens: &mut TokenStream, messages: &mut MessageCollection, symbols: &mut SymbolCollection) -> SyntaxTree {
        let mut sess = ParseSession::new(tokens, messages, symbols);
        match SyntaxTree::parse(&mut sess) {
            Ok(tree) => tree,
            Err(_) => SyntaxTree::new_items(Vec::new(), Vec::new()),
        }
    }
}

#[cfg(test)] #[test]
fn syntax_tree_parse() {
    // test_case!("../tests/syntax/hello.sm");
    // test_case!("../tests/syntax/list.sm");
    // test_case!("../tests/syntax/prime.sm");
    // test_case!("../tests/syntax/string.sm");
}