///! fff-lang
///!
///! syntax/expr

mod primary;
mod postfix;
mod unary;
mod binary;

pub use self::binary::BinaryExpr;
pub use self::unary::UnaryExpr;
pub use self::postfix::PostfixExpr;
pub use self::primary::PrimaryExpr;

#[cfg(test)] #[test]
fn expr_usage() {
    use codemap::StringPosition;
    use lexical::LitValue;
    use super::ISyntaxItemFormat;

    // Quickly get lit and ident
    let actual_lit = BinaryExpr::new_lit(LitValue::from(42), make_strpos!(1, 3, 1, 4));

    println!("{:?}", 
        Some(&actual_lit)
            .and_then(BinaryExpr::get_unary)
            .and_then(UnaryExpr::get_postfix)
            .and_then(PostfixExpr::get_primary)
            .map(|primary| primary.format(0))
    );
}

#[cfg(test)] #[test]
fn expr_parse() {
    use codemap::StringPosition;    
    // use lexical::SeperatorKind;
    use lexical::LitValue;
    use super::ISyntaxItemWithStr;

    assert_eq!{ BinaryExpr::with_test_str("\"abc\""),
        BinaryExpr::new_lit(LitValue::from("abc"), make_strpos!(1, 1, 1, 5))
    }
    assert_eq!{ BinaryExpr::with_test_str("0xfffu64"), 
        BinaryExpr::new_lit(LitValue::from(0xFFFu64), make_strpos!(1, 1, 1, 8))
    }
    assert_eq!{ BinaryExpr::with_test_str("'f'"), 
        BinaryExpr::new_lit(LitValue::from('f'), make_strpos!(1, 1, 1, 3))
    }
    assert_eq!{ BinaryExpr::with_test_str("true"),
        BinaryExpr::new_lit(LitValue::from(true), make_str_pos!(1, 1, 1, 4))
    }

    assert_eq!{ BinaryExpr::with_test_str("binary_expr"),
        BinaryExpr::new_ident("binary_expr".to_owned(), make_strpos!(1, 1, 1, 11))
    }

    assert_eq!{ BinaryExpr::with_test_str("(  )"),
        BinaryExpr::new_unit(make_strpos!(1, 1, 1, 4))
    }
}

//     // Paren expr
//     //           1234567
//     ast_test_case!{ "(1)", 3, make_str_pos!(1, 1, 1, 3),
//         Expression::new(
//             ExpressionBase::Paren(
//                 Expression::new(
//                     ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 2, 1, 2)),
//                     Vec::new(),
//                     make_str_pos!(1, 2, 1, 2),
//                 ),
//                 make_str_pos!(1, 1, 1, 3),
//             ),
//             Vec::new(),
//             make_str_pos!(1, 1, 1, 3)
//         )
//     }
//     // I can see future of Ok(())! 
//     ast_test_case!{ "(())", 4, make_str_pos!(1, 1, 1, 4),
//         Expression::new(
//             ExpressionBase::Paren(
//                 Expression::new(
//                     ExpressionBase::Lit(LitValue::Unit, make_str_pos!(1, 2, 1, 3)),
//                     Vec::new(),
//                     make_str_pos!(1, 2, 1, 3),
//                 ),
//                 make_str_pos!(1, 1, 1, 4),
//             ),
//             Vec::new(),
//             make_str_pos!(1, 1, 1, 4)
//         )
//     }

//     // Tuple def
//     //           123456
//     ast_test_case!{ "(a, b)", 5, make_str_pos!(1, 1, 1, 6),
//         Expression::new(
//             ExpressionBase::TupleDef(
//                 vec![
//                     Expression::new(
//                         ExpressionBase::Ident("a".to_owned(), make_str_pos!(1, 2, 1, 2)),
//                         Vec::new(),
//                         make_str_pos!(1, 2, 1, 2),
//                     ),
//                     Expression::new(
//                         ExpressionBase::Ident("b".to_owned(), make_str_pos!(1, 5, 1, 5)),
//                         Vec::new(),
//                         make_str_pos!(1, 5, 1, 5),
//                     )
//                 ],
//                 make_str_pos!(1, 1, 1, 6)
//             ),
//             Vec::new(),
//             make_str_pos!(1, 1, 1, 6),
//         ) 
//     }        //  12345678901
//     ast_test_case!{ "(1, 2, 3, )", 8, make_str_pos!(1, 1, 1, 11),
//         Expression::new(
//             ExpressionBase::TupleDef(
//                 vec![
//                     Expression::new(
//                         ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 2, 1, 2)),
//                         Vec::new(), 
//                         make_str_pos!(1, 2, 1, 2),
//                     ),
//                     Expression::new(
//                         ExpressionBase::Lit(LitValue::from(2), make_str_pos!(1, 5, 1, 5)),
//                         Vec::new(),
//                         make_str_pos!(1, 5, 1, 5),
//                     ),
//                     Expression::new(
//                         ExpressionBase::Lit(LitValue::from(3), make_str_pos!(1, 8, 1, 8)),
//                         Vec::new(),
//                         make_str_pos!(1, 8, 1, 8),
//                     )
//                 ],
//                 make_str_pos!(1, 1, 1, 11),
//             ),
//             Vec::new(),
//             make_str_pos!(1, 1, 1, 11),
//         )
//     }

//     // Array def
//     ast_test_case!{ "[a]", 3, make_str_pos!(1, 1, 1, 3), 
//         Expression::new(
//             ExpressionBase::ArrayDef(
//                 vec![
//                     Expression::new(
//                         ExpressionBase::Ident("a".to_owned(), make_str_pos!(1, 2, 1, 2)),
//                         Vec::new(),
//                         make_str_pos!(1, 2, 1, 2),
//                     )
//                 ],
//                 make_str_pos!(1, 1, 1, 3)
//             ),
//             Vec::new(),
//             make_str_pos!(1, 1, 1, 3),
//         )
//     }        //  12345678
//     ast_test_case!{ "[1, 2, ]", 6, make_str_pos!(1, 1, 1, 8),
//         Expression::new(
//             ExpressionBase::ArrayDef(
//                 vec![
//                     Expression::new(
//                         ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 2, 1, 2)),
//                         Vec::new(), 
//                         make_str_pos!(1, 2, 1, 2)
//                     ),
//                     Expression::new(
//                         ExpressionBase::Lit(LitValue::from(2), make_str_pos!(1, 5, 1, 5)),
//                         Vec::new(),
//                         make_str_pos!(1, 5, 1, 5)
//                     )
//                 ],
//                 make_str_pos!(1, 1, 1, 8)
//             ),
//             Vec::new(),
//             make_str_pos!(1, 1, 1, 8)
//         )
//     }
//     ast_test_case!{ "[]", 2, make_str_pos!(1, 1, 1, 2),
//         Expression::new(
//             ExpressionBase::ArrayDef(Vec::new(), make_str_pos!(1, 1, 1, 2)),
//             Vec::new(),
//             make_str_pos!(1, 1, 1, 2),
//         )
//         // Temp error is not here
//     }

//     // Array dup def
//     //           123456
//     ast_test_case!{ "[1; 2]", 5, make_str_pos!(1, 1, 1, 6),
//         Expression::new(
//             ExpressionBase::ArrayDupDef(
//                 Expression::new(
//                     ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 2, 1, 2)),
//                     Vec::new(), 
//                     make_str_pos!(1, 2, 1, 2)
//                 ),
//                 Expression::new(
//                     ExpressionBase::Lit(LitValue::from(2), make_str_pos!(1, 5, 1, 5)),
//                     Vec::new(),
//                     make_str_pos!(1, 5, 1, 5)
//                 ),
//                 [
//                     make_str_pos!(1, 1, 1, 6),
//                     make_str_pos!(1, 3, 1, 3),
//                 ]
//             ),
//             Vec::new(),
//             make_str_pos!(1, 1, 1, 6),
//         )
//     }        //  1234567890
//     ast_test_case!{ "[[1]; [2]]", 9, make_str_pos!(1, 1, 1, 10), 
//         Expression::new(
//             ExpressionBase::ArrayDupDef(
//                 Expression::new(
//                     ExpressionBase::ArrayDef(
//                         vec![
//                             Expression::new(
//                                 ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 3, 1, 3)),
//                                 Vec::new(),
//                                 make_str_pos!(1, 3, 1, 3)
//                             )
//                         ],
//                         make_str_pos!(1, 2, 1, 4),
//                     ),
//                     Vec::new(),
//                     make_str_pos!(1, 2, 1, 4)
//                 ),
//                 Expression::new(
//                     ExpressionBase::ArrayDef(
//                         vec![
//                             Expression::new(
//                                 ExpressionBase::Lit(LitValue::from(2), make_str_pos!(1, 8, 1, 8)),
//                                 Vec::new(),
//                                 make_str_pos!(1, 8, 1, 8),
//                             )
//                         ],
//                         make_str_pos!(1, 7, 1, 9)
//                     ),
//                     Vec::new(),
//                     make_str_pos!(1, 7, 1, 9),
//                 ),
//                 [
//                     make_str_pos!(1, 1, 1, 10),
//                     make_str_pos!(1, 5, 1, 5),
//                 ]
//             ), 
//             Vec::new(),
//             make_str_pos!(1, 1, 1, 10),
//         )
//     }

//     // Member access
//     ast_test_case!{ "a.b", 3, make_str_pos!(1, 1, 1, 3),
//         Expression::new(
//             ExpressionBase::Ident("a".to_owned(), make_str_pos!(1, 1, 1, 1)),
//             vec![
//                 ExpressionOperator::MemberAccess("b".to_owned(), make_str_pos!(1, 2, 1, 3)),
//             ],
//             make_str_pos!(1, 1, 1, 3)
//         )
//     }

//     // function call
//     ast_test_case!{ "defg()", 3, make_str_pos!(1, 1, 1, 6),
//         Expression::new(
//             ExpressionBase::Ident("defg".to_owned(), make_str_pos!(1, 1, 1, 4)),
//             vec![
//                 ExpressionOperator::FunctionCall(Vec::new(), make_str_pos!(1, 5, 1, 6)),
//             ],
//             make_str_pos!(1, 1, 1, 6),
//         )
//     }        //  123456
//     ast_test_case!{ "deg(a)", 4, make_str_pos!(1, 1, 1, 6),
//         Expression::new(
//             ExpressionBase::Ident("deg".to_owned(), make_str_pos!(1, 1, 1, 3)),
//             vec![
//                 ExpressionOperator::FunctionCall(
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Ident("a".to_owned(), make_str_pos!(1, 5, 1, 5)),
//                             Vec::new(),
//                             make_str_pos!(1, 5, 1, 5)
//                         ),
//                     ],
//                     make_str_pos!(1, 4, 1, 6),
//                 )
//             ],
//             make_str_pos!(1, 1, 1, 6),
//         )
//     }        //  1234567890123
//     ast_test_case!{ "degg(a, b, )", 7, make_str_pos!(1, 1, 1, 12),
//         Expression::new(
//             ExpressionBase::Ident("degg".to_owned(), make_str_pos!(1, 1, 1, 4)),
//             vec![
//                 ExpressionOperator::FunctionCall(
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Ident("a".to_owned(), make_str_pos!(1, 6, 1, 6)),
//                             Vec::new(),
//                             make_str_pos!(1, 6, 1, 6)
//                         ),
//                         Expression::new(
//                             ExpressionBase::Ident("b".to_owned(), make_str_pos!(1, 9, 1, 9)),
//                             Vec::new(),
//                             make_str_pos!(1, 9, 1, 9),
//                         )
//                     ],
//                     make_str_pos!(1, 5, 1, 12),
//                 )
//             ],
//             make_str_pos!(1, 1, 1, 12),
//         )
//     }        //  123456
//     ast_test_case!{ "de(, )", 4, make_str_pos!(1, 1, 1, 6),
//         Expression::new(
//             ExpressionBase::Ident("de".to_owned(), make_str_pos!(1, 1, 1, 2)),
//             vec![
//                 ExpressionOperator::FunctionCall(
//                     Vec::new(),
//                     make_str_pos!(1, 3, 1, 6)
//                 )
//             ],
//             make_str_pos!(1, 1, 1, 6),
//         ),
//         [
//             // Message::Syntax(SyntaxMessage::SingleCommaInFunctionCall{ call_pos: make_str_pos!(1, 3, 1, 6), comma_pos: make_pos!(1, 4) })
//         ]
//     }

//     // member function call
//     //           1234567890
//     ast_test_case!{ "abc.defg()", 5, make_str_pos!(1, 1, 1, 10),
//         Expression::new(
//             ExpressionBase::Ident("abc".to_owned(), make_str_pos!(1, 1, 1, 3)),
//             vec![
//                 ExpressionOperator::MemberFunctionCall(
//                     "defg".to_owned(),
//                     Vec::new(),
//                     [
//                         make_str_pos!(1, 4, 1, 8),
//                         make_str_pos!(1, 9, 1, 10),
//                     ]
//                 )
//             ],
//             make_str_pos!(1, 1, 1, 10),
//         )
//     }        //  1234567890
//     ast_test_case!{ "abc.deg(a)", 6, make_str_pos!(1, 1, 1, 10),
//         Expression::new(
//             ExpressionBase::Ident("abc".to_owned(), make_str_pos!(1, 1, 1, 3)),
//             vec![
//                 ExpressionOperator::MemberFunctionCall(
//                     "deg".to_owned(),
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Ident("a".to_owned(), make_str_pos!(1, 9, 1, 9)),
//                             Vec::new(),
//                             make_str_pos!(1, 9, 1, 9)
//                         )
//                     ],
//                     [
//                         make_str_pos!(1, 4, 1, 7),
//                         make_str_pos!(1, 8, 1, 10),
//                     ]
//                 )
//             ],
//             make_str_pos!(1, 1, 1, 10)
//         )
//     }        //  12345678901234
//     ast_test_case!{ "1.degg(a, b, )", 9, make_str_pos!(1, 1, 1, 14),
//         Expression::new(
//             ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 1, 1, 1)),
//             vec![
//                 ExpressionOperator::MemberFunctionCall(
//                     "degg".to_owned(),
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Ident("a".to_owned(), make_str_pos!(1, 8, 1, 8)),
//                             Vec::new(),
//                             make_str_pos!(1, 8, 1, 8),
//                         ),
//                         Expression::new(
//                             ExpressionBase::Ident("b".to_owned(), make_str_pos!(1, 11, 1, 11)),
//                             Vec::new(),
//                             make_str_pos!(1, 11, 1, 11),
//                         )
//                     ],
//                     [
//                         make_str_pos!(1, 2, 1, 6),
//                         make_str_pos!(1, 7, 1, 14),
//                     ]
//                 )
//             ],
//             make_str_pos!(1, 1, 1, 14)
//         )
//     }        //   1 23456789
//     ast_test_case!{ "\"\".de(, )", 6, make_str_pos!(1, 1, 1, 9),
//         Expression::new(
//             ExpressionBase::Lit(LitValue::from(""), make_str_pos!(1, 1, 1, 2)),
//             vec![
//                 ExpressionOperator::MemberFunctionCall(
//                     "de".to_owned(),
//                     Vec::new(),
//                     [
//                         make_str_pos!(1, 3, 1, 5),
//                         make_str_pos!(1, 6, 1, 9),
//                     ]
//                 )
//             ],
//             make_str_pos!(1, 1, 1, 9)
//         ),
//         [
//             // Message::Syntax(SyntaxMessage::SingleCommaInFunctionCall{ call_pos: make_str_pos!(1, 6, 1, 9), comma_pos: make_pos!(1, 7) })
//         ]
//     }

//     // get index
//     ast_test_case!{ "defg[]", 3, make_str_pos!(1, 1, 1, 6),
//         Expression::new(
//             ExpressionBase::Ident("defg".to_owned(), make_str_pos!(1, 1, 1, 4)),
//             vec![
//                 ExpressionOperator::GetIndex(Vec::new(), make_str_pos!(1, 5, 1, 6)),
//             ],
//             make_str_pos!(1, 1, 1, 6),
//         )
//     }        //  123456
//     ast_test_case!{ "deg[a]", 4, make_str_pos!(1, 1, 1, 6),
//         Expression::new(
//             ExpressionBase::Ident("deg".to_owned(), make_str_pos!(1, 1, 1, 3)),
//             vec![
//                 ExpressionOperator::GetIndex(
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Ident("a".to_owned(), make_str_pos!(1, 5, 1, 5)),
//                             Vec::new(),
//                             make_str_pos!(1, 5, 1, 5)
//                         ),
//                     ],
//                     make_str_pos!(1, 4, 1, 6),
//                 )
//             ],
//             make_str_pos!(1, 1, 1, 6),
//         )
//     }        //  123456789012
//     ast_test_case!{ "degg[a, b, ]", 7, make_str_pos!(1, 1, 1, 12),
//         Expression::new(
//             ExpressionBase::Ident("degg".to_owned(), make_str_pos!(1, 1, 1, 4)),
//             vec![
//                 ExpressionOperator::GetIndex(
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Ident("a".to_owned(), make_str_pos!(1, 6, 1, 6)),
//                             Vec::new(),
//                             make_str_pos!(1, 6, 1, 6)
//                         ),
//                         Expression::new(
//                             ExpressionBase::Ident("b".to_owned(), make_str_pos!(1, 9, 1, 9)),
//                             Vec::new(),
//                             make_str_pos!(1, 9, 1, 9),
//                         )
//                     ],
//                     make_str_pos!(1, 5, 1, 12),
//                 )
//             ],
//             make_str_pos!(1, 1, 1, 12),
//         )
//     }        //  123456
//     ast_test_case!{ "de[, ]", 4, make_str_pos!(1, 1, 1, 6),
//         Expression::new(
//             ExpressionBase::Ident("de".to_owned(), make_str_pos!(1, 1, 1, 2)),
//             vec![
//                 ExpressionOperator::GetIndex(
//                     Vec::new(),
//                     make_str_pos!(1, 3, 1, 6)
//                 )
//             ],
//             make_str_pos!(1, 1, 1, 6),
//         ),
//         [
//             // Message::Syntax(SyntaxMessage::SingleCommaInSubscription{ sub_pos: make_str_pos!(1, 3, 1, 6), comma_pos: make_pos!(1, 4) })
//         ]
//     }

//     // explicit type cast
//     //           12345678
//     ast_test_case!{ "1 as u32", 3, make_str_pos!(1, 1, 1, 8),
//         Expression::new(
//             ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 1, 1, 1)),
//             vec![
//                 ExpressionOperator::TypeCast(
//                     TypeUseF::new_simple("u32".to_owned(), make_str_pos!(1, 6, 1, 8)),
//                     make_str_pos!(1, 3, 1, 4),
//                 )
//             ],
//             make_str_pos!(1, 1, 1, 8),
//         )
//     }        //  123456789012
//     ast_test_case!{ "[1] as [f32]", 7, make_str_pos!(1, 1, 1, 12),
//         Expression::new(
//             ExpressionBase::ArrayDef(
//                 vec![
//                     Expression::new(
//                         ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 2, 1, 2)),
//                         Vec::new(), 
//                         make_str_pos!(1, 2, 1, 2),
//                     )
//                 ],
//                 make_str_pos!(1, 1, 1, 3),
//             ),
//             vec![
//                 ExpressionOperator::TypeCast(
//                     // TypeUse::Array(Box::new(
//                         TypeUseF::new_simple("f32".to_owned(), make_str_pos!(1, 9, 1, 11)),
//                     // ), make_str_pos!(1, 8, 1, 12)),
//                     make_str_pos!(1, 5, 1, 6),
//                 )
//             ],
//             make_str_pos!(1, 1, 1, 12),
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
//     ast_test_case!{ "2[3].a", 6, make_str_pos!(1, 1, 1, 6),
//         Expression::new(
//             ExpressionBase::Lit(LitValue::from(2), make_str_pos!(1, 1, 1, 1)),
//             vec![
//                 ExpressionOperator::GetIndex(
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Lit(LitValue::from(3), make_str_pos!(1, 3, 1, 3)),
//                             Vec::new(),
//                             make_str_pos!(1, 3, 1, 3),
//                         )
//                     ], 
//                     make_str_pos!(1, 2, 1, 4),
//                 ),
//                 ExpressionOperator::MemberAccess(
//                     "a".to_owned(),
//                     make_str_pos!(1, 5, 1, 6),
//                 ),
//             ],
//             make_str_pos!(1, 1, 1, 6),
//         )    //  0         1          2         
//     }        //  123456 789012 345678901
//     ast_test_case!{ "write(\"hello\") as i32", 6, make_str_pos!(1, 1, 1, 21), 
//         Expression::new(
//             ExpressionBase::Ident("write".to_owned(), make_str_pos!(1, 1, 1, 5)),
//             vec![
//                 ExpressionOperator::FunctionCall(
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Lit(LitValue::from("hello"), make_str_pos!(1, 7, 1, 13)),
//                             Vec::new(),
//                             make_str_pos!(1, 7, 1, 13),
//                         )
//                     ],
//                     make_str_pos!(1, 6, 1, 14),
//                 ),
//                 ExpressionOperator::TypeCast(
//                     TypeUseF::new_simple("i32".to_owned(), make_str_pos!(1, 19, 1, 21)),
//                     make_str_pos!(1, 16, 1, 17),
//                 )
//             ],
//             make_str_pos!(1, 1, 1, 21),
//         )
//     }        //  1234567890123456
//     ast_test_case!{ "print(233, ).bit", 7, make_str_pos!(1, 1, 1, 16),
//         Expression::new(
//             ExpressionBase::Ident("print".to_owned(), make_str_pos!(1, 1, 1, 5)),
//             vec![
//                 ExpressionOperator::FunctionCall(
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Lit(LitValue::from(233), make_str_pos!(1, 7, 1, 9)),
//                             Vec::new(),
//                             make_str_pos!(1, 7, 1, 9)
//                         )
//                     ],
//                     make_str_pos!(1, 6, 1, 12),
//                 ),
//                 ExpressionOperator::MemberAccess(
//                     "bit".to_owned(),
//                     make_str_pos!(1, 13, 1, 16),
//                 )
//             ],
//             make_str_pos!(1, 1, 1, 16),
//         )
//     }            //  12345678901234
//     ast_test_case!{ "1.degg[a, b, ]", 9, make_str_pos!(1, 1, 1, 14),
//         Expression::new(
//             ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 1, 1, 1)),
//             vec![
//                 ExpressionOperator::MemberAccess(
//                     "degg".to_owned(),
//                     make_str_pos!(1, 2, 1, 6),
//                 ),
//                 ExpressionOperator::GetIndex(
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Ident("a".to_owned(), make_str_pos!(1, 8, 1, 8)),
//                             Vec::new(),
//                             make_str_pos!(1, 8, 1, 8),
//                         ),
//                         Expression::new(
//                             ExpressionBase::Ident("b".to_owned(), make_str_pos!(1, 11, 1, 11)),
//                             Vec::new(),
//                             make_str_pos!(1, 11, 1, 11),
//                         )
//                     ],
//                     make_str_pos!(1, 7, 1, 14),
//                 )
//             ],
//             make_str_pos!(1, 1, 1, 14)
//         )
//     }        

//     // logical not and bit not
//     //           1234567
//     ast_test_case!{ "!~!1[1]", 7, make_str_pos!(1, 1, 1, 7),
//         Expression::new(
//             ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 4, 1, 4)),
//             vec![
//                 ExpressionOperator::GetIndex(
//                     vec![
//                         Expression::new(
//                             ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 6, 1, 6)),
//                             Vec::new(),
//                             make_str_pos!(1, 6, 1, 6),
//                         )
//                     ],
//                     make_str_pos!(1, 5, 1, 7),
//                 ),
//                 ExpressionOperator::Unary(
//                     SeperatorKind::LogicalNot,
//                     make_str_pos!(1, 3, 1, 3),
//                 ),
//                 ExpressionOperator::Unary(
//                     SeperatorKind::BitNot,
//                     make_str_pos!(1, 2, 1, 2),
//                 ),
//                 ExpressionOperator::Unary(
//                     SeperatorKind::LogicalNot,
//                     make_str_pos!(1, 1, 1, 1),
//                 )
//             ],
//             make_str_pos!(1, 1, 1, 7),
//         )
//     }

//     // increase and decrease
//     //           1234567
//     ast_test_case!{ "!++--!1", 5, make_str_pos!(1, 1, 1, 7),
//         Expression::new(
//             ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 7, 1, 7)),
//             vec![
//                 ExpressionOperator::Unary(SeperatorKind::LogicalNot, make_str_pos!(1, 6, 1, 6)),
//                 ExpressionOperator::Unary(SeperatorKind::BitNot, make_str_pos!(1, 4, 1, 5)),
//                 ExpressionOperator::Unary(SeperatorKind::LogicalNot, make_str_pos!(1, 2, 1, 3)),
//                 ExpressionOperator::Unary(SeperatorKind::LogicalNot, make_str_pos!(1, 1, 1, 1)),
//             ],
//             make_str_pos!(1, 1, 1, 7),
//         )
//     }

//     // binary operators priority
//     //           0        1         2         3         4     
//     //           123456789012345678901234567890123456789012345678
//     ast_test_case!{ "1 || 2 && 3 == 4 ^ 5 | 6 & 7 >= 8 >> 9 + 10 * 11", 21,  make_str_pos!(1, 1, 1, 48),
//         Expression::new(
//             ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 1, 1, 1)),
//             vec![
//                 ExpressionOperator::Binary(
//                     SeperatorKind::LogicalOr,
//                     make_str_pos!(1, 3, 1, 4),
//                     Expression::new(
//                         ExpressionBase::Lit(LitValue::from(2), make_str_pos!(1, 6, 1, 6)),
//                         vec![
//                             ExpressionOperator::Binary(
//                                 SeperatorKind::LogicalAnd,
//                                 make_str_pos!(1, 8, 1, 9),
//                                 Expression::new(
//                                     ExpressionBase::Lit(LitValue::from(3), make_str_pos!(1, 11, 1, 11)),
//                                     vec![
//                                         ExpressionOperator::Binary(
//                                             SeperatorKind::Equal,
//                                             make_str_pos!(1, 13, 1, 14),
//                                             Expression::new(
//                                                 ExpressionBase::Lit(LitValue::from(4), make_str_pos!(1, 16, 1, 16)),
//                                                 vec![
//                                                     ExpressionOperator::Binary(
//                                                         SeperatorKind::BitXor,
//                                                         make_str_pos!(1, 18, 1, 18),
//                                                         Expression::new(
//                                                             ExpressionBase::Lit(LitValue::from(5), make_str_pos!(1, 20, 1, 20)),
//                                                             vec![
//                                                                 ExpressionOperator::Binary(
//                                                                     SeperatorKind::BitOr,
//                                                                     make_str_pos!(1, 22, 1, 22),
//                                                                     Expression::new(
//                                                                         ExpressionBase::Lit(LitValue::from(6), make_str_pos!(1, 24, 1, 24)),
//                                                                         vec![
//                                                                             ExpressionOperator::Binary(
//                                                                                 SeperatorKind::BitAnd,
//                                                                                 make_str_pos!(1, 26, 1, 26),
//                                                                                 Expression::new(
//                                                                                     ExpressionBase::Lit(LitValue::from(7), make_str_pos!(1, 28, 1, 28)),
//                                                                                     vec![
//                                                                                         ExpressionOperator::Binary(
//                                                                                             SeperatorKind::GreatEqual,
//                                                                                             make_str_pos!(1, 30, 1, 31),
//                                                                                             Expression::new(
//                                                                                                 ExpressionBase::Lit(LitValue::from(8), make_str_pos!(1, 33, 1, 33)),
//                                                                                                 vec![
//                                                                                                     ExpressionOperator::Binary(
//                                                                                                         SeperatorKind::ShiftRight,
//                                                                                                         make_str_pos!(1, 35, 1, 36),
//                                                                                                         Expression::new(
//                                                                                                             ExpressionBase::Lit(LitValue::from(9), make_str_pos!(1, 38, 1, 38)),
//                                                                                                             vec![
//                                                                                                                 ExpressionOperator::Binary(
//                                                                                                                     SeperatorKind::Add,
//                                                                                                                     make_str_pos!(1, 40, 1, 40),
//                                                                                                                     Expression::new(
//                                                                                                                         ExpressionBase::Lit(LitValue::from(10), make_str_pos!(1, 42, 1, 43)),
//                                                                                                                         vec![
//                                                                                                                             ExpressionOperator::Binary(
//                                                                                                                                 SeperatorKind::Mul,
//                                                                                                                                 make_str_pos!(1, 45, 1, 45),
//                                                                                                                                 Expression::new(
//                                                                                                                                     ExpressionBase::Lit(LitValue::from(11), make_str_pos!(1, 47, 1, 48)),
//                                                                                                                                     Vec::new(),
//                                                                                                                                     make_str_pos!(1, 47, 1, 48),
//                                                                                                                                 ),
//                                                                                                                             ),
//                                                                                                                         ],
//                                                                                                                         make_str_pos!(1, 42, 1, 48),
//                                                                                                                     ),
//                                                                                                                 ),
//                                                                                                             ],
//                                                                                                             make_str_pos!(1, 38, 1, 48),
//                                                                                                         ),
//                                                                                                     ),
//                                                                                                 ],
//                                                                                                 make_str_pos!(1, 33, 1, 48),
//                                                                                             ),
//                                                                                         ),
//                                                                                     ],
//                                                                                     make_str_pos!(1, 28, 1, 48),
//                                                                                 ),
//                                                                             ),
//                                                                         ],
//                                                                         make_str_pos!(1, 24, 1, 48),
//                                                                     ),
//                                                                 ),
//                                                             ],
//                                                             make_str_pos!(1, 20, 1, 48),
//                                                         ),
//                                                     ),
//                                                 ],
//                                                 make_str_pos!(1, 16, 1, 48),
//                                             ),
//                                         ),
//                                     ],
//                                     make_str_pos!(1, 11, 1, 48),
//                                 ),
//                             ),
//                         ],
//                         make_str_pos!(1, 6, 1, 48),
//                     ),
//                 ),
//             ],
//             make_str_pos!(1, 1, 1, 48),
//         )
//     }
// }