
// FunctionDef -> 
//     FnDef Identifier LeftParen [Argument [, Argument]*] RightParen [ NarrowRightArrow Type]{0, 1} Statement

use message::MessageEmitter;
use lexical::BufLexer as Lexer;
use syntax::ast_item::ASTParser;
use syntax::Argument;
use syntax::Type;
use syntax::Statement;

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionDef {
    pub name: String,
    pub arguments: Vec<Argument>,
    pub return_type: Type,
    pub body: Statement,
}

impl ASTParser for FunctionDef {
    
    fn parse(lexer: &mut Lexer, messages: &mut MessageEmitter) -> Option<FunctionDef> {
        None
    }
}