
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

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        BinaryExpression::is_first_final(lexer, index)
    }

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<D3Expression>, usize) {

        match BinaryExpression::parse(lexer, index) {
            (None, length) => (None, length), // no recoverable here, pass through
            (Some(binary), length) => (Some(D3Expression(binary)), length),
        }
    }
}

#[cfg(test)]
mod tests {
    use syntax::ast_item::IASTItem;
    use common::StringPosition;
    use message::MessageEmitter;

    use lexical::Lexer;
    use lexical::SeperatorKind;
    use lexical::NumLitValue;
    use lexical::LexicalLiteral;

    use syntax::SMType;
    use syntax::PrimitiveType;
    use syntax::expression::postfix::PostfixExpression;
    use syntax::expression::primary::PrimaryExpression;
    use syntax::expression::postfix::Postfix;
    use syntax::expression::unary::UnaryOperator;
    use syntax::expression::unary::UnaryExpression;
    // use syntax::expression::binary::BinaryOperator;
    use syntax::expression::binary::BinaryExpression;
    use syntax::expression::d3::D3Expression;
        
    // Helper macros
    // primary expression
    macro_rules! expr_to_primary {
        ($inner: expr) => (D3Expression(BinaryExpression{ unary: UnaryExpression{ post: PostfixExpression{ prim: $inner, postfixs: Vec::new() }, unaries: Vec::new() }, ops: Vec::new() }));
    }
    macro_rules! expr_ident { 
        ($name: expr, $pos: expr) => (expr_to_primary!(PrimaryExpression::Ident($name.to_owned(), $pos)))
    }
    macro_rules! expr_str_lit { 
        ($pos: expr) => (expr_to_primary!(PrimaryExpression::Lit(LexicalLiteral::Str(None), $pos)));
        ($val: expr, $pos: expr) => (expr_to_primary!(PrimaryExpression::Lit(LexicalLiteral::Str(Some($val.to_owned())), $pos)))
    }
    macro_rules! expr_char_lit { 
        ($pos: expr) => (expr_to_primary!(PrimaryExpression::Lit(LexicalLiteral::Char(None), $pos)));
        ($val: expr, $pos: expr) => (expr_to_primary!(PrimaryExpression::Lit(LexicalLiteral::Char(Some($val)), $pos)))
    }
    macro_rules! expr_num_lit { 
        ($pos: expr) => (expr_to_primary!(PrimaryExpression::Lit(LexicalLiteral::Num(None), $pos)));
        ($val: expr, $pos: expr) => (expr_to_primary!(PrimaryExpression::Lit(LexicalLiteral::Num(Some($val)), $pos)))
    }
    macro_rules! expr_bool_lit { 
        ($val: expr, $pos: expr) => (expr_to_primary!(PrimaryExpression::Lit(LexicalLiteral::Bool($val), $pos)))
    }
    macro_rules! expr_paren_expr { 
        ($expr: expr, $pos: expr) => (expr_to_primary!(PrimaryExpression::make_paren($expr, $pos)))
    }
    macro_rules! expr_array_def { 
        ([$($exprs: expr, )*] $pos: expr) => (expr_to_primary!(PrimaryExpression::ArrayDef(vec![$($exprs, )*], $pos)))
    }
    macro_rules! expr_array_dup_def { 
        ($expr1: expr, $expr2: expr, $pos: expr) => (expr_to_primary!(PrimaryExpression::make_array_dup_def($expr1, $expr2, $pos))); 
    }

    // postfix expression
    macro_rules! expr_to_postfix {
        ($prim: expr, $($posts: expr)*) => (
            D3Expression(BinaryExpression{ unary: UnaryExpression{ 
                post: PostfixExpression{ prim: $prim, postfixs: vec![$($posts, )*]}, unaries: Vec::new() }, ops: Vec::new() })
        )
    }

    // unary expression
    macro_rules! expr_to_unary {
        ($post: expr) => (D3Expression(BinaryExpression{ unary: UnaryExpression{ post: $post, unary: Vec::new() }, ops: Vec::new() }));
        ($post: expr, $($unaries: expr)*) => 
            (D3Expression(BinaryExpression{ unary: UnaryExpression{ post: $post, unaries: vec![$($unaries, )*] }, ops: Vec::new() }));
    }

    #[test]
    fn ast_expr_prim_parse() {

        macro_rules! parse {
            ($program: expr) => ({
                let lexer = &mut Lexer::new($program);
                let result = D3Expression::parse(lexer, 0);
                perrorln!("messages: {:?}", lexer.messages());
                result
            })
        }

        // Case 0
        assert_eq!(//12345678901234567890
            parse!( "[1, 2, 3f128, 0u64]").0.unwrap(), 
            expr_array_def!{[
                expr_num_lit!(NumLitValue::I32(1), make_str_pos!(1, 2, 1, 2)),
                expr_num_lit!(NumLitValue::I32(2), make_str_pos!(1, 5, 1, 5)), 
                expr_num_lit!(make_str_pos!(1, 8, 1, 12)),
                expr_num_lit!(NumLitValue::U64(0), make_str_pos!(1, 15, 1, 18)),]
                make_str_pos!(1, 1, 1, 19)
            }
        );

        // Case 1         0        1         2         3
        //                12345678901234567890123456789012345
        let res = parse!("[[(1)], [abc, (3)], [4, defg, [6]]]"); 
        assert_eq!(res.0.unwrap(),
            expr_array_def!{[
                expr_array_def!{[
                    expr_paren_expr!(expr_num_lit!(NumLitValue::I32(1), make_str_pos!(1, 4, 1, 4)), make_str_pos!(1, 3, 1, 5)),]
                    make_str_pos!(1, 2, 1, 6) 
                },
                expr_array_def!{[
                    expr_ident!("abc", make_str_pos!(1, 10, 1, 12)),
                    expr_paren_expr!(expr_num_lit!(NumLitValue::I32(3), make_str_pos!(1, 16, 1, 16)), make_str_pos!(1, 15, 1, 17)),]
                    make_str_pos!(1, 9, 1, 18)
                },
                expr_array_def!{[
                    expr_num_lit!(NumLitValue::I32(4), make_str_pos!(1, 22, 1, 22)),
                    expr_ident!("defg", make_str_pos!(1, 25, 1, 28)),
                    expr_array_def!{[
                        expr_num_lit!(NumLitValue::I32(6), make_str_pos!(1, 32, 1, 32)),]
                        make_str_pos!(1, 31, 1, 33)
                    },]
                    make_str_pos!(1, 21, 1, 34)
                },]
                make_str_pos!(1, 1, 1, 35)
            }
        );

        // Case 2, empty array literal
        assert_eq!(
            parse!("[]"),
            (Some(expr_array_def!{
                []
                make_str_pos!(1, 1, 1, 2)
            }), 2)
        );

        // Case 3    0        1           2          3         4         5           6
        assert_eq!(//12345678901234 5678 9012 3456789012345678901234567890123 456789 0123456
            parse!( "[abc, 123u32, \"456\", '\\u0065', false, (), (a), (abc, \"hello\", ), ]").0.unwrap(),
            expr_array_def!{[
                expr_ident!("abc", make_str_pos!(1, 2, 1, 4)),
                expr_num_lit!(NumLitValue::U32(123), make_str_pos!(1, 7, 1, 12)),
                expr_str_lit!("456", make_str_pos!(1, 15, 1, 19)),
                expr_char_lit!('\u{0065}', make_str_pos!(1, 22, 1, 29)),
                expr_bool_lit!(false, make_str_pos!(1, 32, 1, 36)),
                expr_to_primary!(PrimaryExpression::Unit(make_str_pos!(1, 39, 1, 40))),
                expr_paren_expr!(expr_ident!("a", make_str_pos!(1, 44, 1, 44)), make_str_pos!(1, 43, 1, 45)),
                expr_to_primary!(PrimaryExpression::TupleDef(
                    vec![
                        expr_ident!("abc", make_str_pos!(1, 49, 1, 51)),
                        expr_str_lit!("hello", make_str_pos!(1, 54, 1, 60)),
                    ], 
                    make_str_pos!(1, 48, 1, 63)
                )), ]
                make_str_pos!(1, 1, 1, 66)
            }
        );        
        
        // Case 4    0        1            2          3         4
        assert_eq!(//123456789012 3456 78 9012 345678901234567890123456
            parse!( "[abc, 123f, \"456\\u\", '\\u00', false, (a), (  )]").0.unwrap(),
            expr_array_def!{[
                expr_ident!("abc", make_str_pos!(1, 2, 1, 4)),
                expr_num_lit!(make_str_pos!(1, 7, 1, 10)),
                expr_str_lit!(make_str_pos!(1, 13, 1, 19)),
                expr_char_lit!( make_str_pos!(1, 22, 1, 27)),
                expr_bool_lit!(false, make_str_pos!(1, 30, 1, 34)),
                expr_paren_expr!(expr_ident!("a", make_str_pos!(1, 38, 1, 38)), make_str_pos!(1, 37, 1, 39)),
                expr_to_primary!(PrimaryExpression::Unit(make_str_pos!(1, 42, 1, 45))),]
                make_str_pos!(1, 1, 1, 46)
            }
        );

        // Case 5    0        1         2
        assert_eq!(//1234567890123456789012
            parse!( "[[123u32, abc]; 4567]").0.unwrap(),
            expr_array_dup_def!{
                expr_array_def!{[
                    expr_num_lit!(NumLitValue::U32(123), make_str_pos!(1, 3, 1, 8)),
                    expr_ident!("abc", make_str_pos!(1, 11, 1, 13)),]
                    make_str_pos!(1, 2, 1, 14)
                },
                expr_num_lit!(NumLitValue::I32(4567), make_str_pos!(1, 17, 1, 20)),
                [make_str_pos!(1, 1, 1, 21), make_str_pos!(1, 15, 1, 15)]
            }
        );   


    }
    
    #[test]
    fn ast_expr_post_parse() {

        //                                0        1         2         3         4         5         6         7         8     
        //                                1234567890123456789012345678901234567890123456789012345678901234567890123456789
        let lexer = &mut Lexer::new("abc.defg[[1](klm, [123, 456,], )](opq, 456.)() as [i32].rst[uvw, xyz, ABC]");
        let left = D3Expression::parse(lexer, 0).0.unwrap();
        let right = expr_to_postfix!{
            PrimaryExpression::Ident("abc".to_owned(), make_str_pos!(1, 1, 1, 3)),
            Postfix::MemberAccess("defg".to_owned(), make_str_pos!(1, 4, 1, 8))
            Postfix::Subscription(vec![
                expr_to_postfix!(
                    PrimaryExpression::ArrayDef(
                        vec![expr_num_lit!(NumLitValue::I32(1), make_str_pos!(1, 11, 1, 11))],
                        make_str_pos!(1, 10, 1, 12)
                    ), 
                    Postfix::FunctionCall(vec![
                        expr_ident!("klm", make_str_pos!(1, 14, 1, 16)),
                        expr_array_def!([
                            expr_num_lit!(NumLitValue::I32(123), make_str_pos!(1, 20, 1, 22)),
                            expr_num_lit!(NumLitValue::I32(456), make_str_pos!(1, 25, 1, 27)), ]
                            make_str_pos!(1, 19, 1, 29)
                        )], 
                        make_str_pos!(1, 13, 1, 32)
                    )
                )], 
                make_str_pos!(1, 9, 1, 33)
            )
            Postfix::FunctionCall(vec![
                expr_ident!("opq", make_str_pos!(1, 35, 1, 37)),
                expr_num_lit!(NumLitValue::F64(456f64), make_str_pos!(1, 40, 1, 43))],
                make_str_pos!(1, 34, 1, 44) 
            )
            Postfix::FunctionCall(
                Vec::new(),
                make_str_pos!(1, 45, 1, 46)
            )
            Postfix::TypeCast(
                SMType::Array(Box::new(
                    SMType::Prim(PrimitiveType::I32, make_str_pos!(1, 52, 1, 54))
                ), make_str_pos!(1, 51, 1, 55)),
                make_str_pos!(1, 48, 1, 49)
            )
            Postfix::MemberAccess("rst".to_owned(), make_str_pos!(1, 56, 1, 59))
            Postfix::Subscription(vec![
                expr_ident!("uvw", make_str_pos!(1, 61, 1, 63)),
                expr_ident!("xyz", make_str_pos!(1, 66, 1, 68)),
                expr_ident!("ABC", make_str_pos!(1, 71, 1, 73)),],
                make_str_pos!(1, 60, 1, 74),
            )
        };

        assert_eq!(left, right);
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

        let lexer = &mut Lexer::new("writeln(\"helloworld\")");
        perrorln!("{}", D3Expression::parse(lexer, 0).0.unwrap());
    }

    #[test]
    fn ast_expr_unary_parse() {
        //                           12345678901234
        let lexer = &mut Lexer::new("++!~[!1; ~--2]");
        let (result, length) = D3Expression::parse(lexer, 0);

        assert_eq!(length, 11);
        assert_eq!(
            result.unwrap(),
            expr_to_unary!(
                PostfixExpression{ 
                    prim: PrimaryExpression::make_array_dup_def(
                        expr_to_unary!(
                            PostfixExpression{ 
                                prim: PrimaryExpression::Lit(LexicalLiteral::Num(Some(NumLitValue::I32(1))), make_str_pos!(1, 7, 1, 7)),
                                postfixs: Vec::new()
                            },
                            UnaryOperator::new(SeperatorKind::LogicalNot, make_str_pos!(1, 6, 1, 6))
                        ), 
                        expr_to_unary!(
                            PostfixExpression{ 
                                prim: PrimaryExpression::Lit(LexicalLiteral::Num(Some(NumLitValue::I32(2))), make_str_pos!(1, 13, 1, 13)),
                                postfixs: Vec::new()
                            },
                            UnaryOperator::new(SeperatorKind::BitNot, make_str_pos!(1, 10, 1, 10))
                            UnaryOperator::new(SeperatorKind::Decrease, make_str_pos!(1, 11, 1, 12))
                        ), 
                        [make_str_pos!(1, 5, 1, 14), make_str_pos!(1, 8, 1, 8)]
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

        let lexer = &mut Lexer::new("[1] * [2] / [3]");
        { perrorln!("{:?}", BinaryExpression::parse(lexer, 0)); }
        { perrorln!("{}", BinaryExpression::parse(lexer, 0).0.unwrap()); }

        let lexer = &mut Lexer::new("a * b / c + d % e - f");
        perrorln!("{:?}", BinaryExpression::parse(lexer, 0));
        perrorln!("{}", BinaryExpression::parse(lexer, 0).0.unwrap());
        
        let lexer = &mut Lexer::new("a * b << h / c + d % e - f >> g");
        perrorln!("{:?}", BinaryExpression::parse(lexer, 0));
        perrorln!("{}", BinaryExpression::parse(lexer, 0).0.unwrap());

        let lexer = &mut Lexer::new("a * b << h / c + d % e - f >> g > h * i < j << k > m && n || o & p | q ^ r != s == t");
        perrorln!("{:?}", BinaryExpression::parse(lexer, 0));
        perrorln!("{}", BinaryExpression::parse(lexer, 0).0.unwrap());

        let lexer = &mut Lexer::new("a & b == c");
        perrorln!("{:?}", BinaryExpression::parse(lexer, 0));
        perrorln!("{}", BinaryExpression::parse(lexer, 0).0.unwrap());
    }
}