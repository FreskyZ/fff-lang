
// Program = [FnDef]*

use std::fmt;

use codepos::StringPosition;
use util::format_vector_debug;

use message::MessageCollection;
use lexical::TokenStream;

use super::ISyntaxItemParse;
use super::FnDef;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct SyntaxTree {
    pub functions: Vec<FnDef>,
}
impl fmt::Debug for SyntaxTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format_vector_debug(&self.functions, "\n\n"))
    }
}
impl SyntaxTree {
    pub fn get_all_strpos(&self) -> StringPosition {
        match self.functions.len() {
            0 => StringPosition::new(),
            1 => self.functions[0].get_all_strpos(),
            _ => StringPosition::merge(self.functions[0].get_all_strpos(), self.functions.iter().last().unwrap().get_all_strpos()),
        }
    }
}
impl ISyntaxItemParse for SyntaxTree {

    fn parse(lexer: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<SyntaxTree>, usize) {
        // meet EOF and break, 
        // meet function get None break actually is an unrecoverable and return none
        // recover none function by find next paired '}' and expecting `fn` again
        
        let mut funcs = Vec::new();
        let mut current_len = 0_usize;
        loop {
            if lexer.nth(index + current_len).is_eof() {
                break;
            }
            match FnDef::parse(lexer, messages, index + current_len) {
                (Some(func), length) => { 
                    current_len += length;
                    funcs.push(func);
                }
                (None, length) => return (None, current_len + length),
            }
        }

        (Some(SyntaxTree{ functions: funcs }), current_len)
    }
}
impl SyntaxTree {
    pub fn new(tokens: &mut TokenStream, messages: &mut MessageCollection) -> SyntaxTree {
        match SyntaxTree::parse(tokens, messages, 0).0 {
            Some(tree) => tree,
            None => SyntaxTree{ functions: Vec::new() },
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