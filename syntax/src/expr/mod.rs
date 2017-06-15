///! fff-lang
///!
///! syntax/expr

mod lit_expr;
mod ident_expr;
mod primary;
mod postfix;
mod unary;
mod binary;

pub use self::lit_expr::LitExpr;
pub use self::ident_expr::IdentExpr;
pub use self::binary::BinaryExpr;
pub use self::unary::UnaryExpr;
pub use self::postfix::PostfixExpr;
pub use self::primary::PrimaryExpr;

// #[cfg(test)] #[test]
// fn expr_usage() {
//     use codemap::Span;
//     use lexical::LitValue;
//     use super::ISyntaxItemFormat;

//     // Quickly get lit and ident
//     let actual_lit = BinaryExpr::new_lit(LitExpr::new(LitValue::from(42), make_span!(2, 3)));

//     println!("{:?}", 
//         Some(&actual_lit)
//             .and_then(BinaryExpr::get_unary)
//             .and_then(UnaryExpr::get_postfix)
//             .and_then(PostfixExpr::get_primary)
//             .map(|primary| primary.format(0))
//     );
// }

#[cfg(test)] #[test]
fn expr_parse() {
    use codemap::Span;
    // use codemap::SymbolID;
    use lexical::LitValue;
    use super::ISyntaxItemWithStr;

    assert_eq!{ BinaryExpr::with_test_str("\"abc\""),
        BinaryExpr::new_lit(LitExpr::new(LitValue::new_str_lit(make_id!(1)), make_span!(0, 4)))
    }
    assert_eq!{ BinaryExpr::with_test_str("0xfffu64"), 
        BinaryExpr::new_lit(LitExpr::new(LitValue::from(0xFFFu64), make_span!(0, 7)))
    }
    assert_eq!{ BinaryExpr::with_test_str("'f'"), 
        BinaryExpr::new_lit(LitExpr::new(LitValue::from('f'), make_span!(0, 2)))
    }
    assert_eq!{ BinaryExpr::with_test_str("true"),
        BinaryExpr::new_lit(LitExpr::new(LitValue::from(true), make_span!(0, 3)))
    }

    assert_eq!{ BinaryExpr::with_test_str("binary_expr"),
        BinaryExpr::new_ident(IdentExpr::new(make_id!(1), make_span!(0, 10)))
    }

    assert_eq!{ BinaryExpr::with_test_str("(  )"),
        BinaryExpr::new_lit(LitExpr::new(LitValue::Unit, make_span!(0, 3)))
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