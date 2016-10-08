
// Expression -> Identifier LeftParen [ Expression [Comma Expression]* ] RightParen

use lexical::Lexer;
use syntax::ast_item::IASTItem;
use syntax::FunctionCall;

#[derive(Debug, Eq, PartialEq)]
pub enum Expression {
    FunctionCall(FunctionCall),
    Identifier(String),
}

impl IASTItem for Expression {

    fn symbol_len(&self) -> usize {
        0
    }
    
    fn parse(lexer: &mut Lexer, index: usize) -> Option<Expression> {
        None
    }
}