
pub mod program;
pub mod function_def;
pub mod argument;
pub mod smtype;
pub mod statement;
pub mod function_call;
pub mod variable;
pub mod expression;

use message::MessageEmitter;
use lexical::Lexer;

pub trait ASTItem {
    fn symbol_len(&self) -> usize;
}

pub trait ASTParser {
    // some for valid ones, none for invalid and can not recover
    fn parse(lexer: &mut Lexer, messages: &mut MessageEmitter) -> Option<Self> where Self: Sized;
}
