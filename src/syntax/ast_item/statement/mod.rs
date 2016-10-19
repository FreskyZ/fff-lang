
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

    fn symbol_len(&self) -> usize {
        self.exprs.iter().fold(0, |counter, ref expr| counter + expr.symbol_len())
    }
    
    fn parse(lexer: &mut Lexer, index: usize) -> Option<Statement> {

        None
    }
}