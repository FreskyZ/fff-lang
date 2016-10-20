
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
// for messages, only when None is created then a message is emitted, receiving None do not emit same message, but recovery can emit different message
// message divided into lexical message and syntax message, having any message after syntax parse stops the compilation process
// precise display and debug formats for expressions for convenience debug
// pass tests

use std::fmt;

use common::StringPosition;

use lexical::Lexer;
use syntax::ast_item::IASTItem;

mod primary;
mod postfix;
use self::postfix::PostfixExpression;

#[derive(Eq, PartialEq)]
pub struct Expression(PostfixExpression);

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({:?} @ {:?})", self.0, self.0.pos_all())
    }
}
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl IASTItem for Expression {
    
    fn pos_all(&self) -> StringPosition { self.0.pos_all() }

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<Expression>, usize) {

        match PostfixExpression::parse(lexer, index) {
            (Some(post), length) => (Some(Expression(post)), length),
            (None, length) => (None, length), // no recoverable here, pass through
        }
    }
}

// Helper macros
// primary expression
macro_rules! expr_to_primary {
    ($inner: expr) => (Expression(PostfixExpression{ prim: $inner, postfixs: Vec::new() }));
}
macro_rules! expr_ident { 
    ($name: expr) => (expr_to_primary!(PrimaryExpression::make_ident($name.to_owned(), StringPosition::new()))) 
}
macro_rules! expr_str_lit { 
    ($val: expr) => (expr_to_primary!(PrimaryExpression::make_str_lit($val.to_owned(), StringPosition::new()))) 
}
macro_rules! expr_char_lit { 
    ($val: expr) => (expr_to_primary!(PrimaryExpression::make_char_lit($val, StringPosition::new()))) 
}
macro_rules! expr_num_lit { 
    ($val: expr) => (expr_to_primary!(PrimaryExpression::make_num_lit($val, StringPosition::new()))) 
}
macro_rules! expr_bool_lit { 
    ($val: expr) => (expr_to_primary!(PrimaryExpression::make_bool_lit($val, StringPosition::new()))) 
}
macro_rules! expr_paren_expr { 
    ($expr: expr) => (expr_to_primary!(PrimaryExpression::make_paren($expr, StringPosition::new()))) 
}
macro_rules! expr_array_def { 
    ($($exprs: expr, )*) => (expr_to_primary!(PrimaryExpression::make_array_def(vec![$($exprs, )*], StringPosition::new()))) 
}
macro_rules! expr_array_dup_def { 
    ($expr1: expr, $expr2: expr) => (expr_to_primary!(PrimaryExpression::make_array_dup_def($expr1, $expr2, StringPosition::new()))); 
}

#[cfg(test)]
mod tests {

    #[test]
    fn ast_expr_prim_parse() {
        use syntax::ast_item::IASTItem;
        use common::StringPosition;
        use message::MessageEmitter;
        use lexical::Lexer;
        use syntax::Expression;
        use lexical::NumericLiteralValue;
        use syntax::ast_item::expression::postfix::PostfixExpression;
        use syntax::ast_item::expression::primary::PrimaryExpression;
        use syntax::ast_item::expression::primary::PrimaryExpressionBase;

        macro_rules! parse {
            ($program: expr) => (PrimaryExpression::parse(&mut Lexer::new_test($program, MessageEmitter::new()), 0))
        }

        // Case 1
        assert_eq!(
            expr_to_primary!(parse!("[1, 2, 3f128, 0u64]").0.unwrap()), 
            expr_array_def!{
                expr_num_lit!(NumericLiteralValue::I32(1)),
                expr_num_lit!(NumericLiteralValue::I32(2)), 
                expr_num_lit!(NumericLiteralValue::I32(0)),
                expr_num_lit!(NumericLiteralValue::U64(0)),
            }
        );

        // Case 2
                       // 0123456 78  9 ABCDE FGH I    JKLMN
        let res = parse!("[[(1)], [abc, (3)], [4, defg, [6]]]");
        match res.0.unwrap() {
            PrimaryExpression(PrimaryExpressionBase::ArrayDef(exprs), _pos) => {
                assert_eq!(exprs[0], expr_array_def!{ expr_paren_expr!(expr_num_lit!(NumericLiteralValue::I32(1))), });
                assert_eq!(exprs[1], 
                    expr_array_def!{
                        expr_ident!("abc"),
                        expr_paren_expr!(expr_num_lit!(NumericLiteralValue::I32(3))),
                    }
                );
                assert_eq!(exprs[2], 
                    expr_array_def!{
                        expr_num_lit!(NumericLiteralValue::I32(4)),
                        expr_ident!("defg"),
                        expr_array_def!{
                            expr_num_lit!(NumericLiteralValue::I32(6)),
                        },
                    }
                );
            }
            other => panic!("Not expected, but {:?}", other),
        }

        // Case 3
        assert_eq!(
            expr_to_primary!(parse!("[abc, 123, \"456\", '\\u0065', false, (a)]").0.unwrap()),
            expr_array_def!{
                expr_ident!("abc"),
                expr_num_lit!(NumericLiteralValue::I32(123)),
                expr_str_lit!("456"),
                expr_char_lit!('\u{0065}'),
                expr_bool_lit!(false),
                expr_paren_expr!(expr_ident!("a")),
            }
        );        
        
        // Case 4
        assert_eq!(
            expr_to_primary!(parse!("[abc, 123f, \"456\\u\", '\\u00', false, (a)]").0.unwrap()),
            expr_array_def!{
                expr_ident!("abc"),
                expr_num_lit!(NumericLiteralValue::I32(0)),
                expr_str_lit!("<invalid>"),
                expr_char_lit!('\u{FFFE}'),
                expr_bool_lit!(false),
                expr_paren_expr!(expr_ident!("a")),
            }
        );
    }
}