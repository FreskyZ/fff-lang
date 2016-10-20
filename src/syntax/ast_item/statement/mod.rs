
// Statement -> LeftBrace [Expression]* RightBrace

use lexical::Lexer;
// use lexical::IToken;
// use lexical::SeperatorKind;

use syntax::ast_item::IASTItem;
use syntax::Expression;

#[derive(Debug, Eq, PartialEq)]
pub struct Statement {
    pub exprs: Vec<Expression>,
}

impl IASTItem for Statement {
    
    fn parse(lexer: &mut Lexer, index: usize) -> (Option<Statement>, usize) {

        (None, 0)
    }
}