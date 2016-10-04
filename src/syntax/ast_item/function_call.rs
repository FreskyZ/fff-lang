
// FunctionCall -> Identifier LeftParen [ Expression [, Expression]* ] RightParen

use message::MessageEmitter;
use lexical::Lexer;
use syntax::ast_item::ASTParser;
use syntax::FunctionDef;
use syntax::Expression;

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionCall {
    pub function_name: String,
    pub function: Option<FunctionDef>, // delay bind
    pub parameter: Vec<Expression>,
}

impl ASTParser for FunctionCall {
    
    fn parse(lexer: &mut Lexer, messages: &mut MessageEmitter) -> Option<FunctionCall> {
        None
    }
}