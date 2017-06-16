///! fff-lang
///!
///! syntax/expr

use std::fmt;

use codemap::Span;
use lexical::Token;
use lexical::KeywordKind;

mod lit_expr;
mod ident_expr;
mod expr_list;
mod tuple_def;
mod array_def;
mod primary;
mod postfix;
mod unary_expr;
mod binary_expr;

pub use self::lit_expr::LitExpr;
pub use self::ident_expr::IdentExpr;
pub use self::expr_list::ExprList;
pub use self::expr_list::ExprListParseResult;
pub use self::tuple_def::ParenExpr;
pub use self::tuple_def::TupleDef;
pub use self::array_def::ArrayDef;
pub use self::binary_expr::BinaryExpr;
pub use self::unary_expr::UnaryExpr;
pub use self::postfix::PostfixExpr;
pub use self::primary::PrimaryExpr;

use super::ParseSession;
use super::ParseResult;
use super::ISyntaxItemParse;
use super::ISyntaxItemFormat;
use super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub enum Expr {
    Postfix(PostfixExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
}
impl ISyntaxItemFormat for Expr {
    fn format(&self, indent: u32) -> String {
        match self {
            &Expr::Postfix(ref postfix_expr) => postfix_expr.format(indent),
            &Expr::Unary(ref unary_expr) => unary_expr.format(indent),
            &Expr::Binary(ref binary_expr) => binary_expr.format(indent),
        }
    }
}
impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl Expr {

    pub fn get_all_span(&self) -> Span {
        match self {
            &Expr::Postfix(ref postfix_expr) => postfix_expr.get_all_span(),
            &Expr::Unary(ref unary_expr) => unary_expr.all_span,
            &Expr::Binary(ref binary_expr) => binary_expr.all_span,
        }
    }
    
    pub fn new_primary(primary_expr: PrimaryExpr) -> Expr {
        Expr::Postfix(PostfixExpr::new_primary(primary_expr))
    }

    pub fn new_lit(lit_expr: LitExpr) -> Expr {
        Expr::new_primary(PrimaryExpr::Lit(lit_expr))
    }
    pub fn new_ident(ident_expr: IdentExpr) -> Expr {
        Expr::new_primary(PrimaryExpr::Ident(ident_expr))
    }
    pub fn new_paren(paren_expr: ParenExpr) -> Expr {
        Expr::new_primary(PrimaryExpr::Paren(paren_expr))
    }
    pub fn new_tuple(tuple_def: TupleDef) -> Expr {
        Expr::new_primary(PrimaryExpr::Tuple(tuple_def))
    }
    pub fn new_array(array_def: ArrayDef) -> Expr {
        Expr::new_primary(PrimaryExpr::Array(array_def))
    }
}
impl ISyntaxItemGrammar for Expr {
    fn is_first_final(sess: &ParseSession) -> bool { PostfixExpr::is_first_final(sess)
        || UnaryExpr::is_first_final(sess) 
        || sess.tk == &Token::Keyword(KeywordKind::This)
    }
}
impl ISyntaxItemParse for Expr {
    type Target = Expr;

    fn parse(sess: &mut ParseSession) -> ParseResult<Expr> {

        return BinaryExpr::parse(sess);
    }
}

// #[cfg(test)] #[test]
// fn expr_usage() {
//     use codemap::Span;
//     use lexical::LitValue;
//     use super::ISyntaxItemFormat;

//     // Quickly get lit and ident
//     let actual_lit = Expr::new_lit(LitExpr::new(LitValue::from(42), make_span!(2, 3)));

//     println!("{:?}", 
//         Some(&actual_lit)
//             .and_then(Expr::get_unary)
//             .and_then(UnaryExpr::get_postfix)
//             .and_then(PostfixExpr::get_primary)
//             .map(|primary| primary.format(0))
//     );
// }

#[cfg(test)] #[test]
fn expr_parse() {
    use codemap::Span;
    use codemap::SymbolCollection;
    use lexical::LitValue;
    use super::ISyntaxItemWithStr;

    assert_eq!{ Expr::with_test_str("\"abc\""),
        Expr::new_lit(LitExpr::new(LitValue::new_str_lit(make_id!(1)), make_span!(0, 4)))
    }
    assert_eq!{ Expr::with_test_str("0xfffu64"), 
        Expr::new_lit(LitExpr::new(LitValue::from(0xFFFu64), make_span!(0, 7)))
    }
    assert_eq!{ Expr::with_test_str("'f'"), 
        Expr::new_lit(LitExpr::new(LitValue::from('f'), make_span!(0, 2)))
    }
    assert_eq!{ Expr::with_test_str("true"),
        Expr::new_lit(LitExpr::new(LitValue::from(true), make_span!(0, 3)))
    }

    assert_eq!{ Expr::with_test_str("binary_expr"),
        Expr::new_ident(IdentExpr::new(make_id!(1), make_span!(0, 10)))
    }

    assert_eq!{ Expr::with_test_str("(  )"),
        Expr::new_lit(LitExpr::new(LitValue::Unit, make_span!(0, 3)))
    }
    
    // Case from fn_def_parse
    assert_eq!{ Expr::with_test_input("println(this)", &mut make_symbols!["println", "this"]),
        Expr::Postfix(PostfixExpr::new_function_call(
            PostfixExpr::new_primary(PrimaryExpr::Ident(IdentExpr::new(make_id!(1), make_span!(0, 6)))),
            make_span!(7, 12), vec![
                Expr::new_ident(IdentExpr::new(make_id!(2), make_span!(8, 11)))
            ]
        ))
    }
}

//     // Paren expr
//     //           1234567
//     ast_test_case!{ "(1)", 3, make_span!(0, 2),
//         Expression::new(
//             ExpressionBase::Paren(
//                 Expression::new(
//                     ExpressionBase::Lit(LitValue::from(1), make_span!(1, 1)),
//                     Vec::new(),
//                     make_span!(1, 1),
//                 ),
//                 make_span!(0, 2),
//             ),
//             Vec::new(),
//             make_span!(0, 2)
//         )
//     }
//     // I can see future of Ok(())! 
//     ast_test_case!{ "(())", 4, make_span!(0, 3),
//         Expression::new(
//             ExpressionBase::Paren(
//                 Expression::new(
//                     ExpressionBase::Lit(LitValue::Unit, make_span!(1, 2)),
//                     Vec::new(),
//                     make_span!(1, 2),
//                 ),
//                 make_span!(0, 3),
//             ),
//             Vec::new(),
//             make_span!(0, 3)
//         )
//     }

//     // Tuple def
//     //           123456
//     ast_test_case!{ "(a, b)", 5, make_span!(0, 5),
//         Expression::new(
//             ExpressionBase::TupleDef(
//                 vec![
//                     Expression::new(
//                         ExpressionBase::Ident("a".to_owned(), make_span!(1, 1)),
//                         Vec::new(),
//                         make_span!(1, 1),
//                     ),
//                     Expression::new(
//                         ExpressionBase::Ident("b".to_owned(), make_span!(4, 4)),
//                         Vec::new(),
//                         make_span!(4, 4),
//                     )
//                 ],
//                 make_span!(0, 5)
//             ),
//             Vec::new(),
//             make_span!(0, 5),
//         ) 
//     }        //  12345678901
//     ast_test_case!{ "(1, 2, 3, )", 8, make_span!(0, 10),
//         Expression::new(
//             ExpressionBase::TupleDef(
//                 vec![
//                     Expression::new(
//                         ExpressionBase::Lit(LitValue::from(1), make_span!(1, 1)),
//                         Vec::new(), 
//                         make_span!(1, 1),
//                     ),
//                     Expression::new(
//                         ExpressionBase::Lit(LitValue::from(2), make_span!(4, 4)),
//                         Vec::new(),
//                         make_span!(4, 4),
//                     ),
//                     Expression::new(
//                         ExpressionBase::Lit(LitValue::from(3), make_span!(7, 7)),
//                         Vec::new(),
//                         make_span!(7, 7),
//                     )
//                 ],
//                 make_span!(0, 10),
//             ),
//             Vec::new(),
//             make_span!(0, 10),
//         )
//     }

//     // Array def
//     ast_test_case!{ "[a]", 3, make_span!(0, 2), 
//         Expression::new(
//             ExpressionBase::ArrayDef(
//                 vec![
//                     Expression::new(
//                         ExpressionBase::Ident("a".to_owned(), make_span!(1, 1)),
//                         Vec::new(),
//                         make_span!(1, 1),
//                     )
//                 ],
//                 make_span!(0, 2)
//             ),
//             Vec::new(),
//             make_span!(0, 2),
//         )
//     }        //  12345678
//     ast_test_case!{ "[1, 2, ]", 6, make_span!(0, 7),
//         Expression::new(
//             ExpressionBase::ArrayDef(
//                 vec![
//                     Expression::new(
//                         ExpressionBase::Lit(LitValue::from(1), make_span!(1, 1)),
//                         Vec::new(), 
//                         make_span!(1, 1)
//                     ),
//                     Expression::new(
//                         ExpressionBase::Lit(LitValue::from(2), make_span!(4, 4)),
//                         Vec::new(),
//                         make_span!(4, 4)
//                     )
//                 ],
//                 make_span!(0, 7)
//             ),
//             Vec::new(),
//             make_span!(0, 7)
//         )
//     }
//     ast_test_case!{ "[]", 2, make_span!(0, 1),
//         Expression::new(
//             ExpressionBase::ArrayDef(Vec::new(), make_span!(0, 1)),
//             Vec::new(),
//             make_span!(0, 1),
//         )
//         // Temp error is not here
//     }

//     // Array dup def
//     //           123456
//     ast_test_case!{ "[1; 2]", 5, make_span!(0, 5),
//         Expression::new(
//             ExpressionBase::ArrayDupDef(
//                 Expression::new(
//                     ExpressionBase::Lit(LitValue::from(1), make_span!(1, 1)),
//                     Vec::new(), 
//                     make_span!(1, 1)
//                 ),
//                 Expression::new(
//                     ExpressionBase::Lit(LitValue::from(2), make_span!(4, 4)),
//                     Vec::new(),
//                     make_span!(4, 4)
//                 ),
//                 [
//                     make_span!(0, 5),
//                     make_span!(2, 2),
//                 ]
//             ),
//             Vec::new(),
//             make_span!(0, 5),
//         )
//     }        //  1234567890
//     ast_test_case!{ "[[1]; [2]]", 9, make_span!(0, 9), 
//         Expression::new(
//             ExpressionBase::ArrayDupDef(
//                 Expression::new(
//                     ExpressionBase::ArrayDef(
//                         vec![
//                             Expression::new(
//                                 ExpressionBase::Lit(LitValue::from(1), make_span!(2, 2)),
//                                 Vec::new(),
//                                 make_span!(2, 2)
//                             )
//                         ],
//                         make_span!(1, 3),
//                     ),
//                     Vec::new(),
//                     make_span!(1, 3)
//                 ),
//                 Expression::new(
//                     ExpressionBase::ArrayDef(
//                         vec![
//                             Expression::new(
//                                 ExpressionBase::Lit(LitValue::from(2), make_span!(7, 7)),
//                                 Vec::new(),
//                                 make_span!(7, 7),
//                             )
//                         ],
//                         make_span!(6, 8)
//                     ),
//                     Vec::new(),
//                     make_span!(6, 8),
//                 ),
//                 [
//                     make_span!(0, 9),
//                     make_span!(4, 4),
//                 ]
//             ), 
//             Vec::new(),
//             make_span!(0, 9),
//         )
//     }

//     // Member access
//     ast_test_case!{ "a.b", 3, make_span!(0, 2),
//         Expression::new(
//             ExpressionBase::Ident("a".to_owned(), make_span!(0, 0)),
//             vec![
//                 ExpressionOperator::MemberAccess("b".to_owned(), make_span!(1, 2)),
//             ],
//             make_span!(0, 2)
//         )
//     }

//     // function call
//     ast_test_case!{ "defg()", 3, make_span!(0, 5),
//         Expression::new(
//             ExpressionBase::Ident("defg".to_owned(), make_span!(0, 3)),
//             vec![
//                 ExpressionOperator::FunctionCall(Vec::new(), make_span!(4, 5)),
//             ],
//             make_span!(0, 5),
//         )
//     }        //  123456
//     ast_test_case!{ "deg(a)", 4, make_span!(0, 5),
//         Expression::new(
//             ExpressionBase::Ident("deg".to_owned(), make_span!(0, 2)),
//             vec![
//                 ExpressionOperator::FunctionCall(
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Ident("a".to_owned(), make_span!(4, 4)),
//                             Vec::new(),
//                             make_span!(4, 4)
//                         ),
//                     ],
//                     make_span!(3, 5),
//                 )
//             ],
//             make_span!(0, 5),
//         )
//     }        //  1234567890123
//     ast_test_case!{ "degg(a, b, )", 7, make_span!(0, 11),
//         Expression::new(
//             ExpressionBase::Ident("degg".to_owned(), make_span!(0, 3)),
//             vec![
//                 ExpressionOperator::FunctionCall(
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Ident("a".to_owned(), make_span!(5, 5)),
//                             Vec::new(),
//                             make_span!(5, 5)
//                         ),
//                         Expression::new(
//                             ExpressionBase::Ident("b".to_owned(), make_span!(8, 8)),
//                             Vec::new(),
//                             make_span!(8, 8),
//                         )
//                     ],
//                     make_span!(4, 11),
//                 )
//             ],
//             make_span!(0, 11),
//         )
//     }        //  123456
//     ast_test_case!{ "de(, )", 4, make_span!(0, 5),
//         Expression::new(
//             ExpressionBase::Ident("de".to_owned(), make_span!(0, 1)),
//             vec![
//                 ExpressionOperator::FunctionCall(
//                     Vec::new(),
//                     make_span!(2, 5)
//                 )
//             ],
//             make_span!(0, 5),
//         ),
//         [
//             // Message::Syntax(SyntaxMessage::SingleCommaInFunctionCall{ call_pos: make_span!(2, 5), comma_pos: make_pos!(1, 4) })
//         ]
//     }

//     // member function call
//     //           1234567890
//     ast_test_case!{ "abc.defg()", 5, make_span!(0, 9),
//         Expression::new(
//             ExpressionBase::Ident("abc".to_owned(), make_span!(0, 2)),
//             vec![
//                 ExpressionOperator::MemberFunctionCall(
//                     "defg".to_owned(),
//                     Vec::new(),
//                     [
//                         make_span!(3, 7),
//                         make_span!(8, 9),
//                     ]
//                 )
//             ],
//             make_span!(0, 9),
//         )
//     }        //  1234567890
//     ast_test_case!{ "abc.deg(a)", 6, make_span!(0, 9),
//         Expression::new(
//             ExpressionBase::Ident("abc".to_owned(), make_span!(0, 2)),
//             vec![
//                 ExpressionOperator::MemberFunctionCall(
//                     "deg".to_owned(),
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Ident("a".to_owned(), make_span!(8, 8)),
//                             Vec::new(),
//                             make_span!(8, 8)
//                         )
//                     ],
//                     [
//                         make_span!(3, 6),
//                         make_span!(7, 9),
//                     ]
//                 )
//             ],
//             make_span!(0, 9)
//         )
//     }        //  12345678901234
//     ast_test_case!{ "1.degg(a, b, )", 9, make_span!(0, 13),
//         Expression::new(
//             ExpressionBase::Lit(LitValue::from(1), make_span!(0, 0)),
//             vec![
//                 ExpressionOperator::MemberFunctionCall(
//                     "degg".to_owned(),
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Ident("a".to_owned(), make_span!(7, 7)),
//                             Vec::new(),
//                             make_span!(7, 7),
//                         ),
//                         Expression::new(
//                             ExpressionBase::Ident("b".to_owned(), make_span!(10, 10)),
//                             Vec::new(),
//                             make_span!(10, 10),
//                         )
//                     ],
//                     [
//                         make_span!(1, 5),
//                         make_span!(6, 13),
//                     ]
//                 )
//             ],
//             make_span!(0, 13)
//         )
//     }        //   1 23456789
//     ast_test_case!{ "\"\".de(, )", 6, make_span!(0, 8),
//         Expression::new(
//             ExpressionBase::Lit(LitValue::from(""), make_span!(0, 1)),
//             vec![
//                 ExpressionOperator::MemberFunctionCall(
//                     "de".to_owned(),
//                     Vec::new(),
//                     [
//                         make_span!(2, 4),
//                         make_span!(5, 8),
//                     ]
//                 )
//             ],
//             make_span!(0, 8)
//         ),
//         [
//             // Message::Syntax(SyntaxMessage::SingleCommaInFunctionCall{ call_pos: make_span!(5, 8), comma_pos: make_pos!(1, 7) })
//         ]
//     }

//     // get index
//     ast_test_case!{ "defg[]", 3, make_span!(0, 5),
//         Expression::new(
//             ExpressionBase::Ident("defg".to_owned(), make_span!(0, 3)),
//             vec![
//                 ExpressionOperator::GetIndex(Vec::new(), make_span!(4, 5)),
//             ],
//             make_span!(0, 5),
//         )
//     }        //  123456
//     ast_test_case!{ "deg[a]", 4, make_span!(0, 5),
//         Expression::new(
//             ExpressionBase::Ident("deg".to_owned(), make_span!(0, 2)),
//             vec![
//                 ExpressionOperator::GetIndex(
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Ident("a".to_owned(), make_span!(4, 4)),
//                             Vec::new(),
//                             make_span!(4, 4)
//                         ),
//                     ],
//                     make_span!(3, 5),
//                 )
//             ],
//             make_span!(0, 5),
//         )
//     }        //  123456789012
//     ast_test_case!{ "degg[a, b, ]", 7, make_span!(0, 11),
//         Expression::new(
//             ExpressionBase::Ident("degg".to_owned(), make_span!(0, 3)),
//             vec![
//                 ExpressionOperator::GetIndex(
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Ident("a".to_owned(), make_span!(5, 5)),
//                             Vec::new(),
//                             make_span!(5, 5)
//                         ),
//                         Expression::new(
//                             ExpressionBase::Ident("b".to_owned(), make_span!(8, 8)),
//                             Vec::new(),
//                             make_span!(8, 8),
//                         )
//                     ],
//                     make_span!(4, 11),
//                 )
//             ],
//             make_span!(0, 11),
//         )
//     }        //  123456
//     ast_test_case!{ "de[, ]", 4, make_span!(0, 5),
//         Expression::new(
//             ExpressionBase::Ident("de".to_owned(), make_span!(0, 1)),
//             vec![
//                 ExpressionOperator::GetIndex(
//                     Vec::new(),
//                     make_span!(2, 5)
//                 )
//             ],
//             make_span!(0, 5),
//         ),
//         [
//             // Message::Syntax(SyntaxMessage::SingleCommaInSubscription{ sub_pos: make_span!(2, 5), comma_pos: make_pos!(1, 4) })
//         ]
//     }

//     // explicit type cast
//     //           12345678
//     ast_test_case!{ "1 as u32", 3, make_span!(0, 7),
//         Expression::new(
//             ExpressionBase::Lit(LitValue::from(1), make_span!(0, 0)),
//             vec![
//                 ExpressionOperator::TypeCast(
//                     TypeUseF::new_simple("u32".to_owned(), make_span!(5, 7)),
//                     make_span!(2, 3),
//                 )
//             ],
//             make_span!(0, 7),
//         )
//     }        //  123456789012
//     ast_test_case!{ "[1] as [f32]", 7, make_span!(0, 11),
//         Expression::new(
//             ExpressionBase::ArrayDef(
//                 vec![
//                     Expression::new(
//                         ExpressionBase::Lit(LitValue::from(1), make_span!(1, 1)),
//                         Vec::new(), 
//                         make_span!(1, 1),
//                     )
//                 ],
//                 make_span!(0, 2),
//             ),
//             vec![
//                 ExpressionOperator::TypeCast(
//                     // TypeUse::Array(Box::new(
//                         TypeUseF::new_simple("f32".to_owned(), make_span!(8, 10)),
//                     // ), make_span!(7, 11)),
//                     make_span!(4, 5),
//                 )
//             ],
//             make_span!(0, 11),
//         )
//     }

//     // Multi postfixes
//     // member at prev, check 
//     // sub at next, check
//     // call at next, check
//     // sub at prev, member at next, check
//     // call at prev, cast at next, check
//     // cast at prev, member at next check
//     //           123456
//     ast_test_case!{ "2[3].a", 6, make_span!(0, 5),
//         Expression::new(
//             ExpressionBase::Lit(LitValue::from(2), make_span!(0, 0)),
//             vec![
//                 ExpressionOperator::GetIndex(
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Lit(LitValue::from(3), make_span!(2, 2)),
//                             Vec::new(),
//                             make_span!(2, 2),
//                         )
//                     ], 
//                     make_span!(1, 3),
//                 ),
//                 ExpressionOperator::MemberAccess(
//                     "a".to_owned(),
//                     make_span!(4, 5),
//                 ),
//             ],
//             make_span!(0, 5),
//         )    //  0         1          2         
//     }        //  123456 789012 345678901
//     ast_test_case!{ "write(\"hello\") as i32", 6, make_span!(0, 20), 
//         Expression::new(
//             ExpressionBase::Ident("write".to_owned(), make_span!(0, 4)),
//             vec![
//                 ExpressionOperator::FunctionCall(
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Lit(LitValue::from("hello"), make_span!(6, 12)),
//                             Vec::new(),
//                             make_span!(6, 12),
//                         )
//                     ],
//                     make_span!(5, 13),
//                 ),
//                 ExpressionOperator::TypeCast(
//                     TypeUseF::new_simple("i32".to_owned(), make_span!(18, 20)),
//                     make_span!(15, 16),
//                 )
//             ],
//             make_span!(0, 20),
//         )
//     }        //  1234567890123456
//     ast_test_case!{ "print(233, ).bit", 7, make_span!(0, 15),
//         Expression::new(
//             ExpressionBase::Ident("print".to_owned(), make_span!(0, 4)),
//             vec![
//                 ExpressionOperator::FunctionCall(
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Lit(LitValue::from(233), make_span!(6, 8)),
//                             Vec::new(),
//                             make_span!(6, 8)
//                         )
//                     ],
//                     make_span!(5, 11),
//                 ),
//                 ExpressionOperator::MemberAccess(
//                     "bit".to_owned(),
//                     make_span!(12, 15),
//                 )
//             ],
//             make_span!(0, 15),
//         )
//     }            //  12345678901234
//     ast_test_case!{ "1.degg[a, b, ]", 9, make_span!(0, 13),
//         Expression::new(
//             ExpressionBase::Lit(LitValue::from(1), make_span!(0, 0)),
//             vec![
//                 ExpressionOperator::MemberAccess(
//                     "degg".to_owned(),
//                     make_span!(1, 5),
//                 ),
//                 ExpressionOperator::GetIndex(
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Ident("a".to_owned(), make_span!(7, 7)),
//                             Vec::new(),
//                             make_span!(7, 7),
//                         ),
//                         Expression::new(
//                             ExpressionBase::Ident("b".to_owned(), make_span!(10, 10)),
//                             Vec::new(),
//                             make_span!(10, 10),
//                         )
//                     ],
//                     make_span!(6, 13),
//                 )
//             ],
//             make_span!(0, 13)
//         )
//     }        

//     // logical not and bit not
//     //           1234567
//     ast_test_case!{ "!~!1[1]", 7, make_span!(0, 6),
//         Expression::new(
//             ExpressionBase::Lit(LitValue::from(1), make_span!(3, 3)),
//             vec![
//                 ExpressionOperator::GetIndex(
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Lit(LitValue::from(1), make_span!(5, 5)),
//                             Vec::new(),
//                             make_span!(5, 5),
//                         )
//                     ],
//                     make_span!(4, 6),
//                 ),
//                 ExpressionOperator::Unary(
//                     SeperatorKind::LogicalNot,
//                     make_span!(2, 2),
//                 ),
//                 ExpressionOperator::Unary(
//                     SeperatorKind::BitNot,
//                     make_span!(1, 1),
//                 ),
//                 ExpressionOperator::Unary(
//                     SeperatorKind::LogicalNot,
//                     make_span!(0, 0),
//                 )
//             ],
//             make_span!(0, 6),
//         )
//     }

//     // increase and decrease
//     //           1234567
//     ast_test_case!{ "!++--!1", 5, make_span!(0, 6),
//         Expression::new(
//             ExpressionBase::Lit(LitValue::from(1), make_span!(6, 6)),
//             vec![
//                 ExpressionOperator::Unary(SeperatorKind::LogicalNot, make_span!(5, 5)),
//                 ExpressionOperator::Unary(SeperatorKind::BitNot, make_span!(3, 4)),
//                 ExpressionOperator::Unary(SeperatorKind::LogicalNot, make_span!(1, 2)),
//                 ExpressionOperator::Unary(SeperatorKind::LogicalNot, make_span!(0, 0)),
//             ],
//             make_span!(0, 6),
//         )
//     }

//     // binary operators priority
//     //           0        1         2         3         4     
//     //           123456789012345678901234567890123456789012345678
//     ast_test_case!{ "1 || 2 && 3 == 4 ^ 5 | 6 & 7 >= 8 >> 9 + 10 * 11", 21,  make_span!(0, 47),
//         Expression::new(
//             ExpressionBase::Lit(LitValue::from(1), make_span!(0, 0)),
//             vec![
//                 ExpressionOperator::Binary(
//                     SeperatorKind::LogicalOr,
//                     make_span!(2, 3),
//                     Expression::new(
//                         ExpressionBase::Lit(LitValue::from(2), make_span!(5, 5)),
//                         vec![
//                             ExpressionOperator::Binary(
//                                 SeperatorKind::LogicalAnd,
//                                 make_span!(7, 8),
//                                 Expression::new(
//                                     ExpressionBase::Lit(LitValue::from(3), make_span!(10, 10)),
//                                     vec![
//                                         ExpressionOperator::Binary(
//                                             SeperatorKind::Equal,
//                                             make_span!(12, 13),
//                                             Expression::new(
//                                                 ExpressionBase::Lit(LitValue::from(4), make_span!(15, 15)),
//                                                 vec![
//                                                     ExpressionOperator::Binary(
//                                                         SeperatorKind::BitXor,
//                                                         make_span!(17, 17),
//                                                         Expression::new(
//                                                             ExpressionBase::Lit(LitValue::from(5), make_span!(19, 19)),
//                                                             vec![
//                                                                 ExpressionOperator::Binary(
//                                                                     SeperatorKind::BitOr,
//                                                                     make_span!(21, 21),
//                                                                     Expression::new(
//                                                                         ExpressionBase::Lit(LitValue::from(6), make_span!(23, 23)),
//                                                                         vec![
//                                                                             ExpressionOperator::Binary(
//                                                                                 SeperatorKind::BitAnd,
//                                                                                 make_span!(25, 25),
//                                                                                 Expression::new(
//                                                                                     ExpressionBase::Lit(LitValue::from(7), make_span!(27, 27)),
//                                                                                     vec![
//                                                                                         ExpressionOperator::Binary(
//                                                                                             SeperatorKind::GreatEqual,
//                                                                                             make_span!(29, 30),
//                                                                                             Expression::new(
//                                                                                                 ExpressionBase::Lit(LitValue::from(8), make_span!(32, 32)),
//                                                                                                 vec![
//                                                                                                     ExpressionOperator::Binary(
//                                                                                                         SeperatorKind::ShiftRight,
//                                                                                                         make_span!(34, 35),
//                                                                                                         Expression::new(
//                                                                                                             ExpressionBase::Lit(LitValue::from(9), make_span!(37, 37)),
//                                                                                                             vec![
//                                                                                                                 ExpressionOperator::Binary(
//                                                                                                                     SeperatorKind::Add,
//                                                                                                                     make_span!(39, 39),
//                                                                                                                     Expression::new(
//                                                                                                                         ExpressionBase::Lit(LitValue::from(10), make_span!(41, 42)),
//                                                                                                                         vec![
//                                                                                                                             ExpressionOperator::Binary(
//                                                                                                                                 SeperatorKind::Mul,
//                                                                                                                                 make_span!(44, 44),
//                                                                                                                                 Expression::new(
//                                                                                                                                     ExpressionBase::Lit(LitValue::from(11), make_span!(46, 47)),
//                                                                                                                                     Vec::new(),
//                                                                                                                                     make_span!(46, 47),
//                                                                                                                                 ),
//                                                                                                                             ),
//                                                                                                                         ],
//                                                                                                                         make_span!(41, 47),
//                                                                                                                     ),
//                                                                                                                 ),
//                                                                                                             ],
//                                                                                                             make_span!(37, 47),
//                                                                                                         ),
//                                                                                                     ),
//                                                                                                 ],
//                                                                                                 make_span!(32, 47),
//                                                                                             ),
//                                                                                         ),
//                                                                                     ],
//                                                                                     make_span!(27, 47),
//                                                                                 ),
//                                                                             ),
//                                                                         ],
//                                                                         make_span!(23, 47),
//                                                                     ),
//                                                                 ),
//                                                             ],
//                                                             make_span!(19, 47),
//                                                         ),
//                                                     ),
//                                                 ],
//                                                 make_span!(15, 47),
//                                             ),
//                                         ),
//                                     ],
//                                     make_span!(10, 47),
//                                 ),
//                             ),
//                         ],
//                         make_span!(5, 47),
//                     ),
//                 ),
//             ],
//             make_span!(0, 47),
//         )
//     }
// }


#[cfg(remove_this_after_expr_refactor)]
#[cfg(test)] #[test]
fn expr_parse_2() {
    use super::super::ISyntaxItemWithStr;

    // this is the loop of tokens.nth(current) is left bracket does not cover everything and infinite loop is here
    assert_eq!{ PrimaryExpr::with_test_str("[a]"),   
        PrimaryExpr::new_array(
            make_span!(0, 2), vec![
                Expr::new_primary(PrimaryExpr::new_ident(make_id!(1), make_span!(1, 1)))
            ]
        )
    }

    //                                        0        1         2         3         4
    //                                        01234567890123456789012345678901234567890123456
    assert_eq!{ PrimaryExpr::with_test_input("(463857, IEfN, atau8M, (fNAE, ((cAeJN4)), nHg))", 
        //                  1       2         3       4         5
        &mut make_symbols!["IEfN", "atau8M", "fNAE", "cAeJN4", "nHG"]), 
        PrimaryExpr::new_tuple(make_span!(0, 46), vec![
            Expr::new_primary(PrimaryExpr::new_lit(LitValue::from(463857), make_span!(1, 6))),
            Expr::new_primary(PrimaryExpr::new_ident(make_id!(1), make_span!(9, 12))),
            Expr::new_primary(PrimaryExpr::new_ident(make_id!(2), make_span!(15, 20))),
            Expr::new_primary(PrimaryExpr::new_tuple(make_span!(23, 45), vec![
                Expr::new_primary(PrimaryExpr::new_ident(make_id!(3), make_span!(24, 27))),
                Expr::new_primary(PrimaryExpr::new_paren(make_span!(30, 39), 
                    Expr::new_primary(PrimaryExpr::new_paren(make_span!(31, 38), 
                        Expr::new_primary(PrimaryExpr::new_ident(make_id!(4), make_span!(32, 37)))
                    ))
                )),
                Expr::new_primary(PrimaryExpr::new_ident(make_id!(5), make_span!(42, 44)))
            ]))
        ])
    }

    assert_eq!{ PrimaryExpr::with_test_str("10363"), PrimaryExpr::new_lit(LitValue::from(10363), make_span!(0, 4)) }

    assert_eq!{ PrimaryExpr::with_test_str(
    //   0         1         2         3         4         5         6         7         8         9         0         1         2         3       
    //   01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567 8901234567890123456789012345678901234
        "[(0x7E), FFGqfJe, I4, [(m7A, (41, ([(jL, rAn, K0FgLc7h, true), C, w]), (J3cEFDG, d, (j8h))), (), \neIuArjF), 400, 0o535147505, 0xDB747]]"),
        PrimaryExpr::new_array(make_span!(0, 134), vec![
            Expr::new_paren(make_span!(1, 6), 
                Expr::new_lit(LitValue::from(0x7E), make_span!(2, 5))
            ),
            Expr::new_ident("FFGqfJe".to_owned(), make_span!(9, 15)),
            Expr::new_ident("I4".to_owned(), make_span!(18, 19)), 
            Expr::new_array(make_span!(22, 133), vec![
                Expr::new_tuple(make_span!(23, 105), vec![
                    Expr::new_ident("m7A".to_owned(), make_span!(24, 26)),
                    Expr::new_tuple(make_span!(29, 90), vec![
                        Expr::new_lit(LitValue::from(41), make_span!(30, 31)),
                        Expr::new_paren(make_span!(34, 68), 
                            Expr::new_array(make_span!(35, 67), vec![
                                Expr::new_tuple(make_span!(36, 60), vec![
                                    Expr::new_ident("jL".to_owned(), make_span!(37, 38)), 
                                    Expr::new_ident("rAn".to_owned(), make_span!(41, 43)),
                                    Expr::new_ident("K0FgLc7h".to_owned(), make_span!(46, 53)),
                                    Expr::new_lit(LitValue::from(true), make_span!(56, 59))
                                ]),
                                Expr::new_ident("C".to_owned(), make_span!(63, 63)),
                                Expr::new_ident("w".to_owned(), make_span!(66, 66)),
                            ])
                        ),
                        Expr::new_tuple(make_span!(71, 89), vec![
                            Expr::new_ident("J3cEFDG".to_owned(), make_span!(72, 78)),
                            Expr::new_ident("d".to_owned(), make_span!(81, 81)),
                            Expr::new_paren(make_span!(84, 88), 
                                Expr::new_ident("j8h".to_owned(), make_span!(85, 87))
                            )
                        ])
                    ]),
                    Expr::new_unit(make_span!(93, 94)),
                    Expr::new_ident("eIuArjF".to_owned(), make_span!(98, 104))
                ]),
                Expr::new_lit(LitValue::from(400), make_span!(108, 110)),
                Expr::new_lit(LitValue::from(0o535147505), make_span!(113, 123)),
                Expr::new_lit(LitValue::from(0xDB747), make_span!(126, 132))
            ])
        ])
    }

    assert_eq!{ PrimaryExpr::with_test_str("CMDoF"), PrimaryExpr::new_ident(make_id!(1), make_span!(0, 4)) }
    assert_eq!{ PrimaryExpr::with_test_str("false"), PrimaryExpr::new_lit(LitValue::from(false), make_span!(0, 4)) }

    
    //                                      0        1         2         3         4         5         6          7          8         9         A
    //                                      12345678901234567890123456789012345678901234567890123456789012345678901 234 5678901234567890123456789012
    assert_eq!{ PrimaryExpr::with_test_str("[uy6, 4373577, [(q, AJBN0n, MDEgKh5,), KG, (NsL, ((), D, false, d, ), \"H=\"), true, ((vvB3, true, 5))]]"), 
        PrimaryExpr::new_array(make_span!(0, 101), vec![
            Expr::new_ident("uy6".to_owned(), make_span!(1, 3)),
            Expr::new_lit(LitValue::from(4373577), make_span!(6, 12)),
            Expr::new_array(make_span!(15, 100), vec![
                Expr::new_tuple(make_span!(16, 36), vec![
                    Expr::new_ident("q".to_owned(), make_span!(17, 17)),
                    Expr::new_ident("AJBN0n".to_owned(), make_span!(20, 25)), 
                    Expr::new_ident("MDEgKh5".to_owned(), make_span!(28, 34))
                ]), 
                Expr::new_ident("KG".to_owned(), make_span!(39, 40)),
                Expr::new_tuple(make_span!(43, 74), vec![
                    Expr::new_ident("NsL".to_owned(), make_span!(44, 46)),
                    Expr::new_tuple(make_span!(49, 67), vec![
                        Expr::new_unit(make_span!(50, 51)),
                        Expr::new_ident("D".to_owned(), make_span!(54, 54)),
                        Expr::new_lit(LitValue::from(false), make_span!(57, 61)),
                        Expr::new_ident("d".to_owned(), make_span!(64, 64)),
                    ]),
                    Expr::new_lit(LitValue::from("H="), make_span!(70, 73))
                ]),
                Expr::new_lit(LitValue::from(true), make_span!(77, 80)),
                Expr::new_paren(make_span!(83, 99), 
                    Expr::new_tuple(make_span!(84, 98), vec![
                        Expr::new_ident("vvB3".to_owned(), make_span!(85, 88)),
                        Expr::new_lit(LitValue::from(true), make_span!(91, 94)),
                        Expr::new_lit(LitValue::from(5), make_span!(97, 97))
                    ])
                )
            ])
        ])
    }

    assert_eq!{ PrimaryExpr::with_test_str("(() )"), PrimaryExpr::new_paren(make_span!(0, 4), Expr::new_unit(make_span!(1, 2))) }
    assert_eq!{ PrimaryExpr::with_test_str("((),)"), PrimaryExpr::new_tuple(make_span!(0, 4), vec![Expr::new_unit(make_span!(1, 2))]) }

    //                                      1234567890123
    assert_eq!{ PrimaryExpr::with_test_str("[Fhi;vjIj0Dt]"), 
        PrimaryExpr::new_array_dup(make_span!(0, 12), 
            Expr::new_ident("Fhi".to_owned(), make_span!(1, 3)),
            Expr::new_ident("vjIj0Dt".to_owned(), make_span!(5, 11))
        )
    }

    assert_eq!{ PrimaryExpr::with_test_str("(\"o5\")"), 
        PrimaryExpr::new_paren(make_span!(0, 5), 
            Expr::new_lit(LitValue::from("o5"), make_span!(1, 4))
        )
    }

    //                                      0        1         2        
    //                                      1234567890123456789012345678
    assert_eq!{ PrimaryExpr::with_test_str("(nn, ([false;true]), 183455)"),
        PrimaryExpr::new_tuple(make_span!(0, 27), vec![
            Expr::new_ident("nn".to_owned(), make_span!(1, 2)),
            Expr::new_paren(make_span!(5, 18), 
                Expr::new_array_dup(make_span!(6, 17), 
                    Expr::new_lit(LitValue::from(false), make_span!(7, 11)),
                    Expr::new_lit(LitValue::from(true), make_span!(13, 16))
                )
            ),
            Expr::new_lit(LitValue::from(183455), make_span!(21, 26))
        ])
    }
    
    //                                      0        1         2         3         4         5         6         7       
    //                                      123456789012345678901234567890123456789012345678901234567890123456789012345678
    assert_eq!{ PrimaryExpr::with_test_str("((true, (mO, [(q5k);a], (((KttG))), (K5DJ, r, ())), (McsaEdfdfalse,)), rIOKt,)"),
        PrimaryExpr::new_tuple(make_span!(0, 77), vec![
            Expr::new_tuple(make_span!(1, 68), vec![
                Expr::new_lit(LitValue::from(true), make_span!(2, 5)),
                Expr::new_tuple(make_span!(8, 49), vec![
                    Expr::new_ident("mO".to_owned(), make_span!(9, 10)), 
                    Expr::new_array_dup(make_span!(13, 21), 
                        Expr::new_paren(make_span!(14, 18), 
                            Expr::new_ident("q5k".to_owned(), make_span!(15, 17))
                        ),
                        Expr::new_ident("a".to_owned(), make_span!(20, 20))
                    ),
                    Expr::new_paren(make_span!(24, 33), 
                        Expr::new_paren(make_span!(25, 32), 
                            Expr::new_paren(make_span!(26, 31), 
                                Expr::new_ident("KttG".to_owned(), make_span!(27, 30))
                            )
                        )
                    ),
                    Expr::new_tuple(make_span!(36, 48), vec![
                        Expr::new_ident("K5DJ".to_owned(), make_span!(37, 40)), 
                        Expr::new_ident("r".to_owned(), make_span!(43, 43)),
                        Expr::new_unit(make_span!(46, 47))
                    ]),
                ]),
                Expr::new_tuple(make_span!(52, 67), vec![
                    Expr::new_ident("McsaEdfdfalse".to_owned(), make_span!(53, 65))
                ])
            ]),
            Expr::new_ident("rIOKt".to_owned(), make_span!(71, 75))
        ])
    }

    //                                      0          1         2      
    //                                      12 345 67890123456789012
    assert_eq!{ PrimaryExpr::with_test_str("[\"il\", 0o52u32, sO04n]"),
        PrimaryExpr::new_array(make_span!(0, 21), vec![
            Expr::new_lit(LitValue::from("il"), make_span!(1, 4)),
            Expr::new_lit(LitValue::from(0o52u32), make_span!(7, 13)), 
            Expr::new_ident("sO04n".to_owned(), make_span!(16, 20))
        ])
    }
    //                                      12345678
    assert_eq!{ PrimaryExpr::with_test_str("['f';()]"), 
        PrimaryExpr::new_array_dup(make_span!(0, 7), 
            Expr::new_lit(LitValue::from('f'), make_span!(1, 3)),
            Expr::new_unit(make_span!(5, 6))
        )
    }
    assert_eq!{ PrimaryExpr::with_test_str("[]"), PrimaryExpr::new_array(make_span!(0, 1), vec![]) }

    //                                      0        1           2         3         4      
    //                                      12345 678901 234567890123456789012345678901234
    assert_eq!{ PrimaryExpr::with_test_str("[8, \"@=?GF\", 87f32, 1340323.74f64, FKOxAvx5]"),
        PrimaryExpr::new_array(make_span!(0, 43), vec![
            Expr::new_lit(LitValue::from(8), make_span!(1, 1)),
            Expr::new_lit(LitValue::from("@=?GF"), make_span!(4, 10)), 
            Expr::new_lit(LitValue::from(87f32), make_span!(13, 17)),
            Expr::new_lit(LitValue::from(1340323.74f64), make_span!(20, 32)),
            Expr::new_ident("FKOxAvx5".to_owned(), make_span!(35, 42))
        ])
    }

    //                                        0        1         2         3         4         5         6     
    //                                        123456789012345678901234567890123456789012345678901234567890123
    assert_eq!{ PrimaryExpr::with_test_str(r#"  [[dnr4, lGFd3yL, tJ], ['\\', p, (xGaBwiL,), DE], true, aB8aE]"#),
        PrimaryExpr::new_array(make_span!(2, 62), vec![
            Expr::new_array(make_span!(3, 21), vec![
                Expr::new_ident("dnr4".to_owned(), make_span!(4, 7)),
                Expr::new_ident("lGFd3yL".to_owned(), make_span!(10, 16)),
                Expr::new_ident("tJ".to_owned(), make_span!(19, 20))
            ]),
            Expr::new_array(make_span!(24, 48), vec![
                Expr::new_lit(LitValue::from('\\'), make_span!(25, 28)), 
                Expr::new_ident("p".to_owned(), make_span!(31, 31)),
                Expr::new_tuple(make_span!(34, 43), vec![
                    Expr::new_ident("xGaBwiL".to_owned(), make_span!(35, 41))
                ]),
                Expr::new_ident("DE".to_owned(), make_span!(46, 47))
            ]),
            Expr::new_lit(LitValue::from(true), make_span!(51, 54)),
            Expr::new_ident("aB8aE".to_owned(), make_span!(57, 61))
        ])
    } 

    // Previous manual tests
    //                                      0        1           2          3         4         5           6
    //                                      12345678901234 5678 9012 3456789012345678901234567890123 456789 0123456
    assert_eq!{ PrimaryExpr::with_test_str("[abc, 123u32, \"456\", '\\u0065', false, (), (a), (abc, \"hello\", ), ]"),
        PrimaryExpr::new_array(make_span!(0, 65), vec![
            Expr::new_ident("abc".to_owned(), make_span!(1, 3)),
            Expr::new_lit(LitValue::from(123u32), make_span!(6, 11)),
            Expr::new_lit(LitValue::from("456"), make_span!(14, 18)),
            Expr::new_lit(LitValue::from('\u{0065}'), make_span!(21, 28)),
            Expr::new_lit(LitValue::from(false), make_span!(31, 35)),
            Expr::new_unit(make_span!(38, 39)),
            Expr::new_paren(make_span!(42, 44), 
                Expr::new_ident("a".to_owned(), make_span!(43, 43))
            ),
            Expr::new_tuple(make_span!(47, 62), vec![
                Expr::new_ident("abc".to_owned(), make_span!(48, 50)),
                Expr::new_lit(LitValue::from("hello"), make_span!(53, 59)),
            ])
        ])
    }       

    assert_eq!{ PrimaryExpr::with_test_str("(                             )"), PrimaryExpr::new_unit(make_span!(0, 30)) }

    //                                      0        1         2
    //                                      123456789012345678901
    assert_eq!{ PrimaryExpr::with_test_str("[[123u32, abc]; 4567]"), 
        PrimaryExpr::new_array_dup(make_span!(0, 20), 
            Expr::new_array(make_span!(1, 13), vec![
                Expr::new_lit(LitValue::from(123u32), make_span!(2, 7)), 
                Expr::new_ident("abc".to_owned(), make_span!(10, 12))
            ]),
            Expr::new_lit(LitValue::from(4567), make_span!(16, 19))
        )
    }
}

#[cfg(test)] #[test] #[ignore]
fn expr_errors() {
    use message::Message;
    use message::MessageCollection;
    use super::ISyntaxItemWithStr;

    const UNEXPECTED_SYMBOL: &'static str = "Unexpected symbol";

    assert_eq!{ PrimaryExpr::with_test_str_ret_size_messages("(1, ]"), (
        None, 
        3,
        make_messages![
            Message::with_help_by_str(UNEXPECTED_SYMBOL, 
                vec![(make_span!(4, 4), "Meet Seperator `]`(RightBracket) <0>1:5-1:5")],
                vec![&("Expect ".to_owned() + "error_strings::ExpectExpression")]
            )
        ], 
    )}

    assert_eq!{ PrimaryExpr::with_test_str_ret_size_messages("(,)"), (
        None, 
        1,
        make_messages![],
    )}

    assert_eq!{ PrimaryExpr::with_test_str_ret_size_messages("[1, )"), (
        None,
        1,
        make_messages![],
    )}
}