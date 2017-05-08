///! fff-lang
///!
///! syntax/root
///! Root = [FnDef]*

use std::fmt;

use codepos::StringPosition;
use message::MessageCollection;
use lexical::TokenStream;
use lexical::Token;

#[cfg(feature = "parse_sess")] use super::ParseSession;
#[cfg(feature = "parse_sess")] use super::ParseResult;
#[cfg(feature = "parse_sess")] use super::ISyntaxItemParseX;
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

    pub fn get_all_strpos(&self) -> StringPosition { self.all_strpos }
}
impl ISyntaxItemParse for SyntaxTree {

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<SyntaxTree>, usize) {
        // meet EOF and break, 
        // meet function get None break actually is an unrecoverable and return none
        // recover none function by find next paired '}' and expecting `fn` again
        
        let mut funcs = Vec::new();
        let mut current_len = 0_usize;
        loop {
            if tokens.nth(index + current_len) == &Token::EOF {
                break;
            }
            match FnDef::parse(tokens, messages, index + current_len) {
                (Some(func), length) => { 
                    current_len += length;
                    funcs.push(func);
                }
                (None, length) => return (None, current_len + length),
            }
        }

        (Some(SyntaxTree::new_items(funcs)), current_len)
    }
}
#[cfg(feature = "parse_sess")]
impl ISyntaxItemParseX for SyntaxTree {

    fn parsex(sess: &mut ParseSession) -> ParseResult<SyntaxTree> {

        let mut items = Vec::new();
        loop {
            if sess.tk == &Token::EOF { break; }
            items.push(FnDef::parsex(sess)?);
        }
        return Ok(SyntaxTree::new_items(items));
    }
}

#[cfg(not(feature = "parse_sess"))]
impl SyntaxTree {
    pub fn new(tokens: &mut TokenStream, messages: &mut MessageCollection) -> SyntaxTree {
        match SyntaxTree::parse(tokens, messages, 0).0 {
            Some(tree) => tree,
            None => SyntaxTree::new_items(Vec::new()),
        }
    }
}
#[cfg(feature = "parse_sess")]
impl SyntaxTree {
    pub fn new(tokens: &mut TokenStream, messages: &mut MessageCollection) -> SyntaxTree {
        let mut sess = ParseSession::new(tokens, messages);
        match SyntaxTree::parsex(&mut sess) {
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