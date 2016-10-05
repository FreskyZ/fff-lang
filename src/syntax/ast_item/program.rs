
// <Program> -> <FunctionDef>*

use message::MessageEmitter;
use lexical::BufLexer as Lexer;
use syntax::ast_item::ASTParser;
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

impl ASTParser for Program {

    fn parse(lexer: &mut Lexer, messages: &mut MessageEmitter) -> Option<Program> {
        None
    }
}