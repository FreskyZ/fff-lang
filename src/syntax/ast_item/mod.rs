
pub mod program;
pub mod function_def;
pub mod smtype;
pub mod statement;
pub mod block;
pub mod expression;

use lexical::Lexer;

pub trait IASTItem {

    // cusomed symbol length
    fn symbol_len(&self) -> usize;

    // some for valid ones, none for invalid and can not recover
    // if try, not emit error
    fn parse(lexer: &mut Lexer, index: usize) -> Option<Self> where Self: Sized;
}
