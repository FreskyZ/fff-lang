
pub mod program;
pub mod function_def;
pub mod smtype;
pub mod statement;
pub mod block;
pub mod expression;

use common::StringPosition;
use lexical::Lexer;

pub trait IASTItem {

    // Start of start token and end of end token
    fn pos_all(&self) -> StringPosition;

    // some for valid ones, none for invalid and can not recover
    // and consumed symbol length
    fn parse(lexer: &mut Lexer, index: usize) -> (Option<Self>, usize) where Self: Sized;
}
