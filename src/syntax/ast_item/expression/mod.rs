

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

// TODO
// IASTItem to position() -> &[StringPosition] 
// and parser(...) -> (Option<Self>, usize), usize for parsed length because incorrect syntax item may not be standard
// Postion is actually an array, different syntax item have different amount of special position to record for error reporting
// for messages, only when None is created then a message is emitted, receiving None do not emit same message, but recovery can emit different message
// message divided into lexical message and syntax message, having any message after syntax parse stops the compilation process

use lexical::Lexer;
use syntax::ast_item::IASTItem;

mod primary;
mod postfix;
use self::postfix::PostfixExpression;

#[derive(Debug, Eq, PartialEq)]
pub struct Expression(PostfixExpression);

impl IASTItem for Expression {
    
    fn parse(lexer: &mut Lexer, index: usize) -> (Option<Expression>, usize) {
        perrorln!("Parsing expression at index {}", index);

        match PostfixExpression::parse(lexer, index) {
            (Some(post), length) => test_perrorln_and_val!("get post expr in expr parse"; (Some(Expression(post)), length)),
            (None, length) => test_perrorln_and_val!("not get post expr in expr parse"; (None, length)), // no recoverable here, pass through
        }
    }
}

#[cfg(test)]
mod tests {

}