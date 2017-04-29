
// Program = [FunctionDef]*

use std::fmt;

use codepos::StringPosition;
use util::format_vector_debug;

use message::MessageCollection;
use lexical::TokenStream;

use super::ISyntaxItem;
use super::FunctionDef;

#[derive(Eq, PartialEq)]
pub struct SyntaxTree {
    pub functions: Vec<FunctionDef>,
}
impl fmt::Debug for SyntaxTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format_vector_debug(&self.functions, "\n\n"))
    }
}
impl ISyntaxItem for SyntaxTree {

    fn pos_all(&self) -> StringPosition {

        match self.functions.len() {
            0 => StringPosition::new(),
            1 => self.functions[0].pos_all(),
            _ => StringPosition::merge(self.functions[0].pos_all(), self.functions.iter().last().unwrap().pos_all()),
        }
    }

    fn is_first_final(lexer: &mut TokenStream, index: usize) -> bool {
        FunctionDef::is_first_final(lexer, index) 
    }

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
            match FunctionDef::parse(lexer, messages, index + current_len) {
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

#[cfg(test)]
mod tests {
    // use file_map::InputReader;
    // use lexical::Lexer;
    // use super::super::ast_item::ISyntaxItem;
    // use super::Program;

    #[test]
    fn syntax_tree_parse() {
        
        // macro_rules! test_case {
        //     ($file_name: expr) => (
        //         let mut reader = InputReader::new();
        //         reader.read_inputs(vec![$file_name]);

        //         if !reader.get_errors().is_empty() {
        //             panic!("errors: {:?}", reader.get_errors());
        //         }

        //         let lexer = &mut Lexer::new(&reader.into_result());
        //         let (result, length) = Program::parse(lexer, 0);

        //         perrorln!("Debug: {:?}", result);
        //         perrorln!("errors: {:?}", lexer.messages());
        //         perrorln!("Display: {}, {}", result.unwrap(), length);
        //     )
        // }

        // test_case!("../tests/syntax/hello.sm");
        // test_case!("../tests/syntax/list.sm");
        // test_case!("../tests/syntax/prime.sm");
        // test_case!("../tests/syntax/string.sm");
    }
}