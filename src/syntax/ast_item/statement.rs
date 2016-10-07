
// Statement -> LeftBrace [Expression]* RightBrace

use lexical::Lexer;
use lexical::IToken;
use lexical::SeperatorKind;

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

        if !lexer.nth(index).is_seperator(SeperatorKind::LeftBrace) {
            return lexer.push_expect_symbol("left brace", index);
        }

        let mut exprs = Vec::<Expression>::new();
        let mut exprs_sym_len = 0_usize;
        loop {
            match Expression::parse(lexer, index + exprs_sym_len) {
                Some(expr) => {
                    exprs_sym_len += expr.symbol_len();
                    exprs.push(expr);
                }
                None => break,
            }
        }

        if !lexer.nth(index + exprs_sym_len + 1).is_seperator(SeperatorKind::RightBrace) {
            lexer.push_expect_symbol("right brace", index + exprs_sym_len + 1)
        } else {
            Some(Statement{ exprs: exprs })
        }
    }
}