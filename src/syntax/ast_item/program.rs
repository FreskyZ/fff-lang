
// Program = [FunctionDef]*

use common::StringPosition;

use lexical::Lexer;

use syntax::ast_item::IASTItem;
use syntax::FunctionDef;
use syntax::SMType;
use syntax::SMTypeBase;

#[derive(Debug, Eq, PartialEq)]
pub struct Program {
    pub functions: Vec<FunctionDef>,
}

impl Program {

    pub fn get_main(&self) -> Option<&FunctionDef> {

        for func in &self.functions {
            if func.name == "main" && func.ret_type.0 == SMTypeBase::Unit {
                return Some(&func);
            }
        }
        None
    } 
}

impl IASTItem for Program {

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<Program>, usize) {
        
        let mut funcs = Vec::new();
        let mut funcs_len = 0_usize;
        loop {
            match FunctionDef::parse(lexer, index + funcs_len) {
                (Some(func), length) => {
                    funcs_len += length;
                    funcs.push(func);
                }
                (None, _) => break,
            }
        }
        // recover none function by find next paired '}' and expecting `fn` again

        (Some(Program{ functions: funcs }), funcs_len)
    }
}