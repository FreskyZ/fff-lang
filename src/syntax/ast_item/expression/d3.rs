
// Expression = LogicalOrExpression
// 3D Expression, D3Expression{ BinaryExpression { UnaryExpression { PostfixExpression { 
//     PrimaryExpression { Expression, some other }, some other }, some other }, some other }, some other }
// Remain here mainly for the old tests

use std::fmt;

use common::StringPosition;

use lexical::Lexer;
use syntax::ast_item::IASTItem;

use super::binary::BinaryExpression;

#[derive(Eq, PartialEq, Clone)]
pub struct D3Expression(pub BinaryExpression);

impl fmt::Debug for D3Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}
impl fmt::Display for D3Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl IASTItem for D3Expression {
    
    fn pos_all(&self) -> StringPosition { self.0.pos_all() }

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<D3Expression>, usize) {

        match BinaryExpression::parse(lexer, index) {
            (None, length) => (None, length), // no recoverable here, pass through
            (Some(binary), length) => (Some(D3Expression(binary)), length),
        }
    }
}

// Helper macros
// primary expression
macro_rules! expr_to_primary {
    ($inner: expr) => (D3Expression(BinaryExpression{ unary: UnaryExpression{ post: PostfixExpression{ prim: $inner, postfixs: Vec::new() }, unaries: Vec::new() }, ops: Vec::new() }));
}
macro_rules! expr_ident { 
    ($name: expr, $pos: expr) => (expr_to_primary!(PrimaryExpression::make_ident($name.to_owned(), $pos))) 
}
macro_rules! expr_str_lit { 
    ($val: expr, $pos: expr) => (expr_to_primary!(PrimaryExpression::make_str_lit($val.to_owned(), $pos))) 
}
macro_rules! expr_char_lit { 
    ($val: expr, $pos: expr) => (expr_to_primary!(PrimaryExpression::make_char_lit($val, $pos))) 
}
macro_rules! expr_num_lit { 
    ($val: expr, $pos: expr) => (expr_to_primary!(PrimaryExpression::make_num_lit($val, $pos))) 
}
macro_rules! expr_bool_lit { 
    ($val: expr, $pos: expr) => (expr_to_primary!(PrimaryExpression::make_bool_lit($val, $pos))) 
}
macro_rules! expr_paren_expr { 
    ($expr: expr, $pos: expr) => (expr_to_primary!(PrimaryExpression::make_paren($expr, $pos))) 
}
macro_rules! expr_array_def { 
    ([$($exprs: expr, )*] $pos: expr) => (expr_to_primary!(PrimaryExpression::make_array_def(vec![$($exprs, )*], $pos))) 
}
macro_rules! expr_array_dup_def { 
    ($expr1: expr, $expr2: expr, $pos: expr) => (expr_to_primary!(PrimaryExpression::make_array_dup_def($expr1, $expr2, $pos))); 
}

// postfix expression
macro_rules! expr_to_postfix {
    ($prim: expr) => (D3Expression(BinaryExpression{ unary: UnaryExpression{ post: PostfixExpression{ prim: $prim, postfixs: Vec::new() }, unaries: Vec::new() }, ops: Vec::new() }));
    ($prim: expr, $($posts: expr)*) => (
        D3Expression(BinaryExpression{ unary: UnaryExpression{ post: PostfixExpression{ prim: $prim, postfixs: vec![$($posts, )*]}, unaries: Vec::new() }, ops: Vec::new() })
    )
}
macro_rules! expr_post_call { ($($exprs: expr, )*) => (Postfix::FunctionCall(vec![$($exprs, )*])) }
macro_rules! expr_post_sub { ($($exprs: expr, )*) => (Postfix::Subscription(vec![$($exprs, )*])) }
macro_rules! expr_post_member { ($name: expr, $pos: expr) => (Postfix::MemberAccess($name.to_owned(), $pos)) }
macro_rules! expr_post_cast { ($ty: expr) => (Postfix::TypeCast($ty)) }

// unary expression
macro_rules! expr_to_unary {
    ($post: expr) => (D3Expression(BinaryExpression{ unary: UnaryExpression{ post: $post, unary: Vec::new() }, ops: Vec::new() }));
    ($post: expr, $($unaries: expr)*) => 
        (D3Expression(BinaryExpression{ unary: UnaryExpression{ post: $post, unaries: vec![$($unaries, )*] }, ops: Vec::new() }));
}

#[cfg(test)]
mod tests {
    use syntax::ast_item::IASTItem;
    use common::StringPosition;
    use message::MessageEmitter;

    use lexical::Lexer;
    use lexical::SeperatorKind;
    use lexical::NumericLiteralValue;

    use syntax::SMType;
    use syntax::SMTypeBase;
    use syntax::ast_item::expression::postfix::PostfixExpression;
    use syntax::ast_item::expression::primary::PrimaryExpression;
    use syntax::ast_item::expression::postfix::Postfix;
    use syntax::ast_item::expression::unary::UnaryOperator;
    use syntax::ast_item::expression::unary::UnaryExpression;
    // use syntax::ast_item::expression::binary::BinaryOperator;
    use syntax::ast_item::expression::binary::BinaryExpression;
    use syntax::ast_item::expression::d3::D3Expression;

    #[test]
    fn ast_expr_prim_parse() {
        // use syntax::ast_item::expression::primary::PrimaryExpressionBase;

        macro_rules! parse {
            ($program: expr) => ({
                let lexer = &mut Lexer::new_test($program, MessageEmitter::new());
                let result = D3Expression::parse(lexer, 0);
                perrorln!("messages: {:?}", lexer.messages());
                result
            })
        }

        // Case 1
        assert_eq!(//12345678901234567890
            parse!( "[1, 2, 3f128, 0u64]").0.unwrap(), 
            expr_array_def!{[
                expr_num_lit!(NumericLiteralValue::I32(1), make_str_pos!(1, 2, 1, 2)),
                expr_num_lit!(NumericLiteralValue::I32(2), make_str_pos!(1, 5, 1, 5)), 
                expr_num_lit!(NumericLiteralValue::I32(0), make_str_pos!(1, 8, 1, 12)),
                expr_num_lit!(NumericLiteralValue::U64(0), make_str_pos!(1, 15, 1, 18)),]
                make_str_pos!(1, 1, 1, 19)
            }
        );

        // Case 2         0        1         2         3
        //                12345678901234567890123456789012345
        let res = parse!("[[(1)], [abc, (3)], [4, defg, [6]]]"); 
        assert_eq!(res.0.unwrap(),
            expr_array_def!{[
                expr_array_def!{[
                    expr_paren_expr!(expr_num_lit!(NumericLiteralValue::I32(1), make_str_pos!(1, 4, 1, 4)), make_str_pos!(1, 3, 1, 5)),]
                    make_str_pos!(1, 2, 1, 6) 
                },
                expr_array_def!{[
                    expr_ident!("abc", make_str_pos!(1, 10, 1, 12)),
                    expr_paren_expr!(expr_num_lit!(NumericLiteralValue::I32(3), make_str_pos!(1, 16, 1, 16)), make_str_pos!(1, 15, 1, 17)),]
                    make_str_pos!(1, 9, 1, 18)
                },
                expr_array_def!{[
                    expr_num_lit!(NumericLiteralValue::I32(4), make_str_pos!(1, 22, 1, 22)),
                    expr_ident!("defg", make_str_pos!(1, 25, 1, 28)),
                    expr_array_def!{[
                        expr_num_lit!(NumericLiteralValue::I32(6), make_str_pos!(1, 32, 1, 32)),]
                        make_str_pos!(1, 31, 1, 33)
                    },]
                    make_str_pos!(1, 21, 1, 34)
                },]
                make_str_pos!(1, 1, 1, 35)
            }
        );

        // Case 3    0        1           2          3         4
        assert_eq!(//12345678901234 5678 9012 34567890123456789012
            parse!( "[abc, 123u32, \"456\", '\\u0065', false, (a)]").0.unwrap(),
            expr_array_def!{[
                expr_ident!("abc", make_str_pos!(1, 2, 1, 4)),
                expr_num_lit!(NumericLiteralValue::U32(123), make_str_pos!(1, 7, 1, 12)),
                expr_str_lit!("456", make_str_pos!(1, 15, 1, 19)),
                expr_char_lit!('\u{0065}', make_str_pos!(1, 22, 1, 29)),
                expr_bool_lit!(false, make_str_pos!(1, 32, 1, 36)),
                expr_paren_expr!(expr_ident!("a", make_str_pos!(1, 40, 1, 40)), make_str_pos!(1, 39, 1, 41)),]
                make_str_pos!(1, 1, 1, 42)
            }
        );        
        
        // Case 4    0        1            2          3         4
        assert_eq!(//123456789012 3456 78 9012 345678901234567890
            parse!( "[abc, 123f, \"456\\u\", '\\u00', false, (a)]").0.unwrap(),
            expr_array_def!{[
                expr_ident!("abc", make_str_pos!(1, 2, 1, 4)),
                expr_num_lit!(NumericLiteralValue::I32(0), make_str_pos!(1, 7, 1, 10)),
                expr_str_lit!("<invalid>", make_str_pos!(1, 13, 1, 19)),
                expr_char_lit!('\u{FFFE}', make_str_pos!(1, 22, 1, 27)),
                expr_bool_lit!(false, make_str_pos!(1, 30, 1, 34)),
                expr_paren_expr!(expr_ident!("a", make_str_pos!(1, 38, 1, 38)), make_str_pos!(1, 37, 1, 39)),]
                make_str_pos!(1, 1, 1, 40)
            }
        );

        
        // Case 5    0        1         2
        assert_eq!(//1234567890123456789012
            parse!( "[[123u32, abc]; 4567]").0.unwrap(),
            expr_array_dup_def!{
                expr_array_def!{[
                    expr_num_lit!(NumericLiteralValue::U32(123), make_str_pos!(1, 3, 1, 8)),
                    expr_ident!("abc", make_str_pos!(1, 11, 1, 13)),]
                    make_str_pos!(1, 2, 1, 14)
                },
                expr_num_lit!(NumericLiteralValue::I32(4567), make_str_pos!(1, 17, 1, 20)),
                make_str_pos!(1, 1, 1, 21)
            }
        );   
    }
    
    #[test]
    fn ast_expr_post_parse() {

        //                                0        1         2         3         4         5         6         7         8     
        //                                1234567890123456789012345678901234567890123456789012345678901234567890123456789
        let lexer = &mut Lexer::new_test("abc.defg[[1](klm, [123, 456,], )](opq, 456.)() as [i32].rst[uvw, xyz, ABC]", MessageEmitter::new());
        let result = D3Expression::parse(lexer, 0);
        let left = D3Expression::parse(lexer, 0).0.unwrap();
        let right = expr_to_postfix!{
            PrimaryExpression::make_ident("abc".to_owned(), make_str_pos!(1, 1, 1, 3)),
            expr_post_member!("defg", make_str_pos!(1, 4, 1, 8))
            expr_post_sub!(
                expr_to_postfix!(
                    PrimaryExpression::make_array_def(
                        vec![expr_num_lit!(NumericLiteralValue::I32(1), make_str_pos!(1, 11, 1, 11))],
                        make_str_pos!(1, 10, 1, 12)
                    ), 
                    expr_post_call!(
                        expr_ident!("klm", make_str_pos!(1, 14, 1, 16)),
                        expr_array_def!([
                            expr_num_lit!(NumericLiteralValue::I32(123), make_str_pos!(1, 20, 1, 22)),
                            expr_num_lit!(NumericLiteralValue::I32(456), make_str_pos!(1, 25, 1, 27)),]
                            make_str_pos!(1, 19, 1, 29)
                        ),
                    )
                ), 
            )
            expr_post_call!(
                expr_ident!("opq", make_str_pos!(1, 35, 1, 37)),
                expr_num_lit!(NumericLiteralValue::F64(456f64), make_str_pos!(1, 40, 1, 43)), 
            )
            expr_post_call!()
            expr_post_cast!(
                SMType::make_array(SMType::make_base(SMTypeBase::I32, make_str_pos!(1, 52, 1, 54)), make_str_pos!(1, 51, 1, 55))
            )
            expr_post_member!("rst", make_str_pos!(1, 56, 1, 59))
            expr_post_sub!(
                expr_ident!("uvw", make_str_pos!(1, 61, 1, 63)),
                expr_ident!("xyz", make_str_pos!(1, 66, 1, 68)),
                expr_ident!("ABC", make_str_pos!(1, 71, 1, 73)),
            )
        };

        let left_desc = &format!("{:?}", left);
        let right_desc = &format!("{:?}", right);
        for (index, (ch1, ch2)) in left_desc.chars().into_iter().zip(right_desc.chars().into_iter()).enumerate() {
            if ch1 != ch2 {
                panic!("ch pair diff at {}: {}, {}", index, ch1, ch2);
          
            }
        }
        perrorln!("Messages: {:?}", lexer.messages());
    }

    #[test]
    fn ast_expr_post_helloworld_expr() {

        let lexer = &mut Lexer::new("writeln(\"helloworld\")".to_owned());
        perrorln!("{}", D3Expression::parse(lexer, 0).0.unwrap());
    }

    #[test]
    fn ast_expr_unary_parse() {
        //                           12345678901234
        let lexer = &mut Lexer::new("++!~[!1; ~--2]".to_owned());
        let (result, length) = D3Expression::parse(lexer, 0);

        assert_eq!(
            result.unwrap(),
            expr_to_unary!(
                PostfixExpression{ 
                    prim: PrimaryExpression::make_array_dup_def(
                        expr_to_unary!(
                            PostfixExpression{ 
                                prim: PrimaryExpression::make_num_lit(NumericLiteralValue::I32(1), make_str_pos!(1, 7, 1, 7)),
                                postfixs: Vec::new()
                            },
                            UnaryOperator::new(SeperatorKind::LogicalNot, make_str_pos!(1, 6, 1, 6))
                        ), 
                        expr_to_unary!(
                            PostfixExpression{ 
                                prim: PrimaryExpression::make_num_lit(NumericLiteralValue::I32(2), make_str_pos!(1, 13, 1, 13)),
                                postfixs: Vec::new()
                            },
                            UnaryOperator::new(SeperatorKind::BitNot, make_str_pos!(1, 10, 1, 10))
                            UnaryOperator::new(SeperatorKind::Decrease, make_str_pos!(1, 11, 1, 12))
                        ), 
                        make_str_pos!(1, 5, 1, 14)
                    ),
                    postfixs: Vec::new(),
                },
                UnaryOperator::new(SeperatorKind::Increase, make_str_pos!(1, 1, 1, 2))
                UnaryOperator::new(SeperatorKind::LogicalNot, make_str_pos!(1, 3, 1, 3))
                UnaryOperator::new(SeperatorKind::BitNot, make_str_pos!(1, 4, 1, 4))
            )
        );
    }

    #[test]
    fn ast_expr_binary() {

        let lexer = &mut Lexer::new("[1] * [2] / [3]".to_owned());
        { perrorln!("{:?}", BinaryExpression::parse(lexer, 0)); }
        { perrorln!("{}", BinaryExpression::parse(lexer, 0).0.unwrap()); }

        let lexer = &mut Lexer::new("a * b / c + d % e - f".to_owned());
        perrorln!("{:?}", BinaryExpression::parse(lexer, 0));
        perrorln!("{}", BinaryExpression::parse(lexer, 0).0.unwrap());
        
        let lexer = &mut Lexer::new("a * b << h / c + d % e - f >> g".to_owned());
        perrorln!("{:?}", BinaryExpression::parse(lexer, 0));
        perrorln!("{}", BinaryExpression::parse(lexer, 0).0.unwrap());

        let lexer = &mut Lexer::new("a * b << h / c + d % e - f >> g > h * i < j << k > m && n || o & p | q ^ r != s == t".to_owned());
        perrorln!("{:?}", BinaryExpression::parse(lexer, 0));
        perrorln!("{}", BinaryExpression::parse(lexer, 0).0.unwrap());

        let lexer = &mut Lexer::new("a & b == c".to_owned());
        perrorln!("{:?}", BinaryExpression::parse(lexer, 0));
        perrorln!("{}", BinaryExpression::parse(lexer, 0).0.unwrap());
    }
}