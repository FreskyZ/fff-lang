
// FunctionCall -> Identifier LeftParen [ Expression [, Expression]* ] RightParen

use lexical::Lexer;
use syntax::ast_item::IASTItem;
use syntax::FunctionDef;
use syntax::Expression;

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionCall {
    pub function_name: String,
    pub function: Option<FunctionDef>, // delay bind
    pub parameter: Vec<Expression>,
}

impl IASTItem for FunctionCall {

    fn symbol_len(&self) -> usize {
        0
    }
    
    fn parse(lexer: &mut Lexer, index: usize) -> Option<FunctionCall> {
        None
    }
}