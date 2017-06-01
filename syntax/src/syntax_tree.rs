///! fff-lang
///!
///! syntax/root
///! Root = [FnDef]*

use std::fmt;

use codemap::StringPosition;
use message::MessageCollection;
use lexical::TokenStream;
use lexical::Token;

use super::ParseSession;
use super::ParseResult;
use super::ISyntaxItemParse;
use super::ISyntaxItemFormat;
use super::FnDef;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct SyntaxTree {
    items: Vec<FnDef>,
    all_strpos: StringPosition,
}
impl ISyntaxItemFormat for SyntaxTree {
    fn format(&self, indent: u32) -> String {
        format!("{}SyntaxTree {}<{:?}>{}", 
            SyntaxTree::indent_str(indent), if self.items.is_empty() { "(empty) " } else { "" }, self.all_strpos, 
            self.items.iter().fold(String::new(), |mut buf, item| { buf.push_str("\n"); buf.push_str(&item.format(indent + 1)); buf }))
    }
}
impl fmt::Debug for SyntaxTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(0)) }
}
impl SyntaxTree {
    
    pub fn new_items(items: Vec<FnDef>) -> SyntaxTree {
        SyntaxTree{ 
            all_strpos: match items.len() {
                0 => StringPosition::new(),
                1 => items[0].get_all_strpos(),
                n => StringPosition::merge(items[0].get_all_strpos(), items[n - 1].get_all_strpos()),
            },
            items: items,
        }
    }
    pub fn into_items(self) -> Vec<FnDef> { self.items }
    pub fn get_all_strpos(&self) -> StringPosition { self.all_strpos }
}
impl ISyntaxItemParse for SyntaxTree {

    fn parse(sess: &mut ParseSession) -> ParseResult<SyntaxTree> {
        // TODO: meet EOF not break, meet EOFs to break
        // maybe recover none function by find next paired '}' and expecting `fn` again
        
        let mut items = Vec::new();
        loop {
            if sess.tk == &Token::EOF { break; }
            items.push(FnDef::parse(sess)?);
        }
        return Ok(SyntaxTree::new_items(items));
    }
}
impl SyntaxTree {
    pub fn new(tokens: &mut TokenStream, messages: &mut MessageCollection) -> SyntaxTree {
        let mut sess = ParseSession::new(tokens, messages);
        match SyntaxTree::parse(&mut sess) {
            Ok(tree) => tree,
            Err(_) => SyntaxTree::new_items(Vec::new()),
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