

// UnaryExpression = PostfixExpression | UnaryOperator UnaryExpression
// MultiplicativeExpression = UnaryExpression | MultiplicativeExpression MultiplicativeOperator UnaryExpression
// AdditiveExpression = MultiplicativeExpression | AdditiveExpression AdditiveOperator MultiplicativeExpression
// ShiftExpression = AdditiveExpression | ShiftExpression ShiftOperator AdditiveExpression
// RelationalExpression = ShiftExpression | RelationalExpression RelationalOperator ShiftExpression
// BitAndExpression = RelationalExpression | BitAndExpression BitAndOperator RelationalExpression
// BitXorExpression = BitAndExpression | BitXorExpression BitXorOperator BitAndExpression
// BitOrExpression = BitXorExpression | BitOrExpression BitOrOperator BitXorExpression
// EqualityExpression = BitOrExpression | EqualityExpression EqualityOperator BitOrExpression  // `==` and `!=` lower than `|` for `if (enum_var & enum_mem1 == enum_mem1)` 
// LogicalAndExpression = EqualityExpression | LogicalAndExpression LogicalAndOperator EqualityExpression 
// LogicalOrExpression = LogicalAndExpression | LogicalOrExpression LogicalOrOperator LogicalAndExpression
// Expression = LogicalOrExpression

use lexical::Lexer;
use syntax::ast_item::IASTItem;

mod primary;
mod postfix;
use self::postfix::PostfixExpression;

#[derive(Debug, Eq, PartialEq)]
pub enum Expression {
    Postfix(PostfixExpression),
}

impl IASTItem for Expression {

    fn symbol_len(&self) -> usize {
        match *self {
            Expression::Postfix(ref postfix) => postfix.symbol_len(),
        }
    }
    
    fn parse(lexer: &mut Lexer, index: usize) -> Option<Expression> {
        perrorln!("Parsing expression at index {}", index);

        match PostfixExpression::parse(lexer, index) {
            Some(post) => test_perrorln_and_val!("get post expr in expr parse"; Some(Expression::Postfix(post))),
            None => test_perrorln_and_val!("not get post expr in expr parse"; lexer.push_expect_symbol("Some expression", index)),
        }
    }
}