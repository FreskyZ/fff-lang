
pub mod program;
pub mod function_def;
pub mod smtype;
pub mod statement;
pub mod block;
pub mod expression;

use lexical::Lexer;

pub trait IASTItem {

    // some for valid ones, none for invalid and can not recover
    // and consumed symbol length
    fn parse(lexer: &mut Lexer, index: usize) -> (Option<Self>, usize) where Self: Sized;
}
