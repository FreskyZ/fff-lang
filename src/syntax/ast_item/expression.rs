
// Expression -> Identifier LeftParen [ Expression [Comma Expression]* ] RightParen

use message::MessageEmitter;
use lexical::Lexer;
use syntax::ast_item::ASTParser;
use syntax::FunctionCall;
use syntax::Variable;

#[derive(Debug, Eq, PartialEq)]
pub enum Expression {
    FunctionCall(FunctionCall),
    Variable(Variable),
}

impl ASTParser for Expression {
    
    fn parse(lexer: &mut Lexer, messages: &mut MessageEmitter) -> Option<Expression> {
        None
    }
}