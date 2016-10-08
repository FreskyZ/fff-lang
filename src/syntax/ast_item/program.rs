
// <Program> -> <FunctionDef>*

use lexical::Lexer;
use syntax::ast_item::IASTItem;
use syntax::FunctionDef;
use syntax::PrimitiveType;
use syntax::Type;

#[derive(Debug, Eq, PartialEq)]
pub struct Program {
    pub functions: Vec<FunctionDef>,
}

impl Program {

    pub fn get_main(&self) -> Option<&FunctionDef> {

        for func in &self.functions {
            if func.name == "main" && func.return_type == Type::Primitive(PrimitiveType::Unit) {
                return Some(&func);
            }
        }
        None
    } 
}

impl IASTItem for Program {

    fn symbol_len(&self) -> usize {
        self.functions.iter().fold(0, |counter, ref func| counter + func.symbol_len())
    }

    fn parse(lexer: &mut Lexer, index: usize) -> Option<Program> {
        
        let mut funcs = Vec::new();
        let mut funcs_sym_len = 0_usize;
        loop {
            match FunctionDef::parse(lexer, index + funcs_sym_len) {
                Some(func) => {
                    funcs_sym_len += func.symbol_len();
                    funcs.push(func);
                }
                None => break,
            }
        }

        Some(Program{ functions: funcs })
    }
}