///! fff-lang
///!
///! syntax/expr

use std::fmt;

use codemap::Span;
use lexical::Token;
use lexical::LitValue;
use lexical::KeywordKind;

mod lit_expr;
mod ident_expr;
mod expr_list;
mod tuple_def;
mod array_def;
mod member_access;
mod fn_call;
mod index_call;
mod unary_expr;
mod binary_expr;
mod priority_proxy;

pub use self::lit_expr::LitExpr;
pub use self::ident_expr::IdentExpr;
pub use self::expr_list::ExprList;
pub use self::expr_list::ExprListParseResult;
pub use self::tuple_def::ParenExpr;
pub use self::tuple_def::TupleDef;
pub use self::array_def::ArrayDef;
pub use self::fn_call::FnCallExpr;
pub use self::index_call::IndexCallExpr;
pub use self::member_access::MemberAccessExpr;

pub use self::binary_expr::BinaryExpr;
pub use self::unary_expr::UnaryExpr;
pub use self::priority_proxy::PostfixExpr;
pub use self::priority_proxy::PrimaryExpr;

use super::ParseSession;
use super::ParseResult;
use super::ISyntaxItemParse;
use super::ISyntaxItemFormat;
use super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub enum Expr {
    Lit(LitExpr),
    Ident(IdentExpr),
    Paren(ParenExpr),
    Tuple(TupleDef),
    Array(ArrayDef),
    FnCall(FnCallExpr),
    IndexCall(IndexCallExpr),
    MemberAccess(MemberAccessExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
}
impl ISyntaxItemFormat for Expr {
    fn format(&self, indent: u32) -> String {
        match self {
            &Expr::Lit(ref lit_expr) => lit_expr.format(indent),
            &Expr::Ident(ref ident_expr) => ident_expr.format(indent),
            &Expr::Paren(ref paren_expr) => paren_expr.format(indent),
            &Expr::Tuple(ref tuple_def) => tuple_def.format(indent),
            &Expr::Array(ref array_def) => array_def.format(indent),
            &Expr::FnCall(ref fn_call) => fn_call.format(indent),
            &Expr::IndexCall(ref index_call) => index_call.format(indent),
            &Expr::MemberAccess(ref member_access) => member_access.format(indent),
            &Expr::Unary(ref unary_expr) => unary_expr.format(indent),
            &Expr::Binary(ref binary_expr) => binary_expr.format(indent),
        }
    }
}
impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl Default for Expr {
    fn default() -> Expr { Expr::Lit(LitExpr::new(LitValue::from(0), Span::default())) }
}
impl Expr {

    pub fn get_all_span(&self) -> Span {
        match self {
            &Expr::Lit(ref lit_expr) => lit_expr.span,
            &Expr::Ident(ref ident_expr) => ident_expr.span,
            &Expr::Paren(ref paren_expr) => paren_expr.span,
            &Expr::Tuple(ref tuple_def) => tuple_def.paren_span, 
            &Expr::Array(ref array_def) => array_def.bracket_span,
            &Expr::FnCall(ref fn_call) => fn_call.all_span,
            &Expr::IndexCall(ref index_call) => index_call.all_span,
            &Expr::MemberAccess(ref member_access) => member_access.all_span,
            &Expr::Unary(ref unary_expr) => unary_expr.all_span,
            &Expr::Binary(ref binary_expr) => binary_expr.all_span,
        }
    }
}
impl ISyntaxItemGrammar for Expr {
    fn is_first_final(sess: &ParseSession) -> bool { 
        LitExpr::is_first_final(sess)
        || IdentExpr::is_first_final(sess)
        || TupleDef::is_first_final(sess)
        || ArrayDef::is_first_final(sess)
        || sess.tk == &Token::Keyword(KeywordKind::This)
        || UnaryExpr::is_first_final(sess) 
        // || PostfixExpr::is_first_final(sess) // same as Expr
        // || BinaryExpr::is_first_final(sess)  // same as Expr
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
//     let actual_lit = Expr::Lit(LitExpr::new(LitValue::from(42), make_span!(2, 3)));

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
        Expr::Lit(LitExpr::new(LitValue::new_str_lit(make_id!(1)), make_span!(0, 4)))
    }
    assert_eq!{ Expr::with_test_str("0xfffu64"), 
        Expr::Lit(LitExpr::new(LitValue::from(0xFFFu64), make_span!(0, 7)))
    }
    assert_eq!{ Expr::with_test_str("'f'"), 
        Expr::Lit(LitExpr::new(LitValue::from('f'), make_span!(0, 2)))
    }
    assert_eq!{ Expr::with_test_str("true"),
        Expr::Lit(LitExpr::new(LitValue::from(true), make_span!(0, 3)))
    }

    assert_eq!{ Expr::with_test_str("binary_expr"),
        Expr::Ident(IdentExpr::new(make_id!(1), make_span!(0, 10)))
    }

    assert_eq!{ Expr::with_test_str("(  )"),
        Expr::Lit(LitExpr::new(LitValue::Unit, make_span!(0, 3)))
    }
    
    // Case from fn_def_parse
    assert_eq!{ Expr::with_test_input("println(this)", &mut make_symbols!["println", "this"]),
        Expr::FnCall(FnCallExpr::new(
            Expr::Ident(IdentExpr::new(make_id!(1), make_span!(0, 6))),
            make_span!(7, 12), ExprList::new(vec![
                Expr::Ident(IdentExpr::new(make_id!(2), make_span!(8, 11)))
            ])
        ))
    }
}

#[cfg(remove_this_after_expr_refactor)]
#[cfg(test)] #[test]
fn postfix_expr_format() {
    use super::super::ISyntaxItemWithStr;

    macro_rules! test_case {
        ($left: expr, $right: expr) => {
            if $left != $right {
                let left_owned = $left.to_owned();
                let left_lines = left_owned.lines();
                let right_lines = $right.lines();
                for (index, (left_line, right_line)) in left_lines.zip(right_lines).enumerate() {
                    if left_line != right_line {
                        panic!("assertion failed at index {}\nleft: {}\nright: {}", index, $left, $right);
                    }
                }
                panic!("assertion failed, but cannot detected by compare each line\nleft: {}\nright: {}", $left, $right);
            }
        }
    }

    // Attention that this source code line's LF is also the string literal (test oracle)'s LF
    //                                     0         1         2         3         4         5        
    //                                     0123456789012345678901234567890123456789012345678901234567
    test_case!(PostfixExpr::with_test_str("a.b(c, d, e).f(g, h, i,)(u,).j[k].l().m[n, o, p][r, s, t,]").format(0), r##"PostfixExpr <<0>0-57>
  PostfixExpr <<0>0-47>
    PostfixExpr <<0>0-38>
      PostfixExpr <<0>0-36>
        PostfixExpr <<0>0-32>
          PostfixExpr <<0>0-29>
            PostfixExpr <<0>0-27>
              PostfixExpr <<0>0-23>
                PostfixExpr <<0>0-11>
                  Ident #1 <<0>0-0>
                  MemberFunctionCall <<0>1-11>
                    '.' <<0>1-1>
                    Ident #2 <<0>2-2>
                    paren <<0>3-11>
                    Ident #3 <<0>4-4>
                    Ident #4 <<0>7-7>
                    Ident #5 <<0>10-10>
                MemberFunctionCall <<0>12-23>
                  '.' <<0>12-12>
                  Ident '#6 <<0>13-13>
                  paren <<0>14-23>
                  Ident #7 <<0>15-15>
                  Ident #8 <<0>18-18>
                  Ident #9 <<0>21-21>
              FunctionCall <<0>24-27>
                Ident #10 <<0>25-25>
            MemberAccess <<0>28-29>
              '.' <<0>28-28>
              Ident #11 <<0>29-29>
          Subscription <<0>30-32>
            Ident #12 <<0>31-31>
        MemberFunctionCall (empty) <<0>33-36>
          '.' <<0>33-33>
          Ident #13 <<0>34-34>
          paren <<0>35-36>
      MemberAccess <<0>37-38>
        '.' <<0>37-37>
        Ident #14 <<0>38-38>
    Subscription <<0>39-47>
      Ident #15 <<0>40-40>
      Ident #16 <<0>43-43>
      Ident #17 <<0>46-46>
  Subscription <<0>48-57>
    Ident #18 <<0>49-49>
    Ident #19 <<0>52-52>
    Ident #20 <<0>55-55>"##
    );
}

#[cfg(remove_this_after_expr_refactor)]
#[cfg(test)] #[test]
fn postfix_expr_parse() {
    use super::super::ISyntaxItemWithStr;
    use message::MessageCollection;
    use super::IdentExpr;

    macro_rules! ident {
        ($name: expr, $strpos: expr) => (Expr::Primary(PrimaryExpr::Ident(IdentExpr::new($name, $strpos))));
        (post, $name: expr, $strpos: expr) => (PostfixExpr::new_primary(PrimaryExpr::Ident(IdentExpr::new($name, $strpos))))
    }

    //                                      0        1         2         3         4         5     
    // plain                                1234567890123456789012345678901234567890123456789012345678
    assert_eq!{ PostfixExpr::with_test_str("a.b(c, d, e).f(g, h, i,)(u,).j[k].l().m[n, o, p][r, s, t,]"),
        PostfixExpr::new_subscription(
            PostfixExpr::new_subscription(
                PostfixExpr::new_member_access(
                    PostfixExpr::new_member_function_call(
                        PostfixExpr::new_subscription(
                            PostfixExpr::new_member_access(
                                PostfixExpr::new_function_call(
                                    PostfixExpr::new_member_function_call(
                                        PostfixExpr::new_member_function_call(
                                            ident!(post, make_id!(1), make_span!(0, 0)),
                                            make_span!(1, 1),
                                            make_id!(2), make_span!(2, 2),
                                            make_span!(3, 11), vec![
                                                ident!(make_id!(3), make_span!(4, 4)),
                                                ident!(make_id!(4), make_span!(7, 7)),
                                                ident!(make_id!(5), make_span!(10, 10)),
                                            ]
                                        ),
                                        make_span!(12, 12),
                                        make_id!(6), make_span!(13, 13),
                                        make_span!(14, 23), vec![
                                            ident!(make_id!(7), make_span!(15, 15)),
                                            ident!(make_id!(8), make_span!(18, 18)),
                                            ident!(make_id!(9), make_span!(21, 21)),
                                        ]
                                    ),
                                    make_span!(24, 27), vec![
                                        ident!(make_id!(10), make_span!(25, 25))
                                    ]
                                ),
                                make_span!(28, 28),
                                make_id!(11), make_span!(29, 29)
                            ),
                            make_span!(30, 32), vec![
                                ident!(make_id!(12), make_span!(31, 31))
                            ]
                        ),
                        make_span!(33, 33),
                        make_id!(13), make_span!(34, 34),
                        make_span!(35, 36),
                        vec![]
                    ),
                    make_span!(37, 37),
                    make_id!(14), make_span!(38, 38)
                ),
                make_span!(39, 47), vec![
                    ident!(make_id!(15), make_span!(40, 40)),
                    ident!(make_id!(16), make_span!(43, 43)),
                    ident!(make_id!(17), make_span!(46, 46))
                ]
            ),
            make_span!(48, 57), vec![
                ident!(make_id!(18), make_span!(49, 49)),
                ident!(make_id!(19), make_span!(52, 52)),
                ident!(make_id!(20), make_span!(55, 55))
            ]
        )
    }
    
    assert_eq!{ PostfixExpr::with_test_str_ret_size_messages("a[]"), (
        Some(PostfixExpr::new_subscription(
            ident!(post, make_id!(1), make_span!(0, 0)), 
            make_span!(1, 2), vec![]
        )), 
        3,
        make_messages![
            Message::new_by_str("Empty subscription", vec![(make_span!(1, 2), "subscription here")])
        ],
    )}
    
    assert_eq!{ PostfixExpr::with_test_str_ret_size_messages("a[, ]"), (
        Some(PostfixExpr::new_subscription(
            ident!(post, make_id!(1), make_span!(0, 0)), 
            make_span!(1, 4), vec![]
        )), 
        4,
        make_messages![
            Message::new_by_str("Empty subscription", vec![(make_span!(1, 4), "subscription here")])
        ],
    )}
    
    assert_eq!{ PostfixExpr::with_test_str_ret_size_messages("a(, )"), (
        Some(PostfixExpr::new_function_call(
            ident!(post, make_id!(1), make_span!(0, 0)),
            make_span!(1, 4), vec![]
        )), 
        4,
        make_messages![
            Message::new_by_str("Single comma in function call", vec![(make_span!(1, 4), "function call here")])
        ],
    )}
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
            Expr::new_primary(PrimaryExpr::Lit(LitValue::from(463857), make_span!(1, 6))),
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

    assert_eq!{ PrimaryExpr::with_test_str("10363"), PrimaryExpr::Lit(LitValue::from(10363), make_span!(0, 4)) }

    assert_eq!{ PrimaryExpr::with_test_str(
    //   0         1         2         3         4         5         6         7         8         9         0         1         2         3       
    //   01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567 8901234567890123456789012345678901234
        "[(0x7E), FFGqfJe, I4, [(m7A, (41, ([(jL, rAn, K0FgLc7h, true), C, w]), (J3cEFDG, d, (j8h))), (), \neIuArjF), 400, 0o535147505, 0xDB747]]"),
        PrimaryExpr::new_array(make_span!(0, 134), vec![
            Expr::new_paren(make_span!(1, 6), 
                Expr::Lit(LitValue::from(0x7E), make_span!(2, 5))
            ),
            Expr::new_ident("FFGqfJe".to_owned(), make_span!(9, 15)),
            Expr::new_ident("I4".to_owned(), make_span!(18, 19)), 
            Expr::new_array(make_span!(22, 133), vec![
                Expr::new_tuple(make_span!(23, 105), vec![
                    Expr::new_ident("m7A".to_owned(), make_span!(24, 26)),
                    Expr::new_tuple(make_span!(29, 90), vec![
                        Expr::Lit(LitValue::from(41), make_span!(30, 31)),
                        Expr::new_paren(make_span!(34, 68), 
                            Expr::new_array(make_span!(35, 67), vec![
                                Expr::new_tuple(make_span!(36, 60), vec![
                                    Expr::new_ident("jL".to_owned(), make_span!(37, 38)), 
                                    Expr::new_ident("rAn".to_owned(), make_span!(41, 43)),
                                    Expr::new_ident("K0FgLc7h".to_owned(), make_span!(46, 53)),
                                    Expr::Lit(LitValue::from(true), make_span!(56, 59))
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
                Expr::Lit(LitValue::from(400), make_span!(108, 110)),
                Expr::Lit(LitValue::from(0o535147505), make_span!(113, 123)),
                Expr::Lit(LitValue::from(0xDB747), make_span!(126, 132))
            ])
        ])
    }

    assert_eq!{ PrimaryExpr::with_test_str("CMDoF"), PrimaryExpr::new_ident(make_id!(1), make_span!(0, 4)) }
    assert_eq!{ PrimaryExpr::with_test_str("false"), PrimaryExpr::Lit(LitValue::from(false), make_span!(0, 4)) }

    
    //                                      0        1         2         3         4         5         6          7          8         9         A
    //                                      12345678901234567890123456789012345678901234567890123456789012345678901 234 5678901234567890123456789012
    assert_eq!{ PrimaryExpr::with_test_str("[uy6, 4373577, [(q, AJBN0n, MDEgKh5,), KG, (NsL, ((), D, false, d, ), \"H=\"), true, ((vvB3, true, 5))]]"), 
        PrimaryExpr::new_array(make_span!(0, 101), vec![
            Expr::new_ident("uy6".to_owned(), make_span!(1, 3)),
            Expr::Lit(LitValue::from(4373577), make_span!(6, 12)),
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
                        Expr::Lit(LitValue::from(false), make_span!(57, 61)),
                        Expr::new_ident("d".to_owned(), make_span!(64, 64)),
                    ]),
                    Expr::Lit(LitValue::from("H="), make_span!(70, 73))
                ]),
                Expr::Lit(LitValue::from(true), make_span!(77, 80)),
                Expr::new_paren(make_span!(83, 99), 
                    Expr::new_tuple(make_span!(84, 98), vec![
                        Expr::new_ident("vvB3".to_owned(), make_span!(85, 88)),
                        Expr::Lit(LitValue::from(true), make_span!(91, 94)),
                        Expr::Lit(LitValue::from(5), make_span!(97, 97))
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
            Expr::Lit(LitValue::from("o5"), make_span!(1, 4))
        )
    }

    //                                      0        1         2        
    //                                      1234567890123456789012345678
    assert_eq!{ PrimaryExpr::with_test_str("(nn, ([false;true]), 183455)"),
        PrimaryExpr::new_tuple(make_span!(0, 27), vec![
            Expr::new_ident("nn".to_owned(), make_span!(1, 2)),
            Expr::new_paren(make_span!(5, 18), 
                Expr::new_array_dup(make_span!(6, 17), 
                    Expr::Lit(LitValue::from(false), make_span!(7, 11)),
                    Expr::Lit(LitValue::from(true), make_span!(13, 16))
                )
            ),
            Expr::Lit(LitValue::from(183455), make_span!(21, 26))
        ])
    }
    
    //                                      0        1         2         3         4         5         6         7       
    //                                      123456789012345678901234567890123456789012345678901234567890123456789012345678
    assert_eq!{ PrimaryExpr::with_test_str("((true, (mO, [(q5k);a], (((KttG))), (K5DJ, r, ())), (McsaEdfdfalse,)), rIOKt,)"),
        PrimaryExpr::new_tuple(make_span!(0, 77), vec![
            Expr::new_tuple(make_span!(1, 68), vec![
                Expr::Lit(LitValue::from(true), make_span!(2, 5)),
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
            Expr::Lit(LitValue::from("il"), make_span!(1, 4)),
            Expr::Lit(LitValue::from(0o52u32), make_span!(7, 13)), 
            Expr::new_ident("sO04n".to_owned(), make_span!(16, 20))
        ])
    }
    //                                      12345678
    assert_eq!{ PrimaryExpr::with_test_str("['f';()]"), 
        PrimaryExpr::new_array_dup(make_span!(0, 7), 
            Expr::Lit(LitValue::from('f'), make_span!(1, 3)),
            Expr::new_unit(make_span!(5, 6))
        )
    }
    assert_eq!{ PrimaryExpr::with_test_str("[]"), PrimaryExpr::new_array(make_span!(0, 1), vec![]) }

    //                                      0        1           2         3         4      
    //                                      12345 678901 234567890123456789012345678901234
    assert_eq!{ PrimaryExpr::with_test_str("[8, \"@=?GF\", 87f32, 1340323.74f64, FKOxAvx5]"),
        PrimaryExpr::new_array(make_span!(0, 43), vec![
            Expr::Lit(LitValue::from(8), make_span!(1, 1)),
            Expr::Lit(LitValue::from("@=?GF"), make_span!(4, 10)), 
            Expr::Lit(LitValue::from(87f32), make_span!(13, 17)),
            Expr::Lit(LitValue::from(1340323.74f64), make_span!(20, 32)),
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
                Expr::Lit(LitValue::from('\\'), make_span!(25, 28)), 
                Expr::new_ident("p".to_owned(), make_span!(31, 31)),
                Expr::new_tuple(make_span!(34, 43), vec![
                    Expr::new_ident("xGaBwiL".to_owned(), make_span!(35, 41))
                ]),
                Expr::new_ident("DE".to_owned(), make_span!(46, 47))
            ]),
            Expr::Lit(LitValue::from(true), make_span!(51, 54)),
            Expr::new_ident("aB8aE".to_owned(), make_span!(57, 61))
        ])
    } 

    // Previous manual tests
    //                                      0        1           2          3         4         5           6
    //                                      12345678901234 5678 9012 3456789012345678901234567890123 456789 0123456
    assert_eq!{ PrimaryExpr::with_test_str("[abc, 123u32, \"456\", '\\u0065', false, (), (a), (abc, \"hello\", ), ]"),
        PrimaryExpr::new_array(make_span!(0, 65), vec![
            Expr::new_ident("abc".to_owned(), make_span!(1, 3)),
            Expr::Lit(LitValue::from(123u32), make_span!(6, 11)),
            Expr::Lit(LitValue::from("456"), make_span!(14, 18)),
            Expr::Lit(LitValue::from('\u{0065}'), make_span!(21, 28)),
            Expr::Lit(LitValue::from(false), make_span!(31, 35)),
            Expr::new_unit(make_span!(38, 39)),
            Expr::new_paren(make_span!(42, 44), 
                Expr::new_ident("a".to_owned(), make_span!(43, 43))
            ),
            Expr::new_tuple(make_span!(47, 62), vec![
                Expr::new_ident("abc".to_owned(), make_span!(48, 50)),
                Expr::Lit(LitValue::from("hello"), make_span!(53, 59)),
            ])
        ])
    }       

    assert_eq!{ PrimaryExpr::with_test_str("(                             )"), PrimaryExpr::new_unit(make_span!(0, 30)) }

    //                                      0        1         2
    //                                      123456789012345678901
    assert_eq!{ PrimaryExpr::with_test_str("[[123u32, abc]; 4567]"), 
        PrimaryExpr::new_array_dup(make_span!(0, 20), 
            Expr::new_array(make_span!(1, 13), vec![
                Expr::Lit(LitValue::from(123u32), make_span!(2, 7)), 
                Expr::new_ident("abc".to_owned(), make_span!(10, 12))
            ]),
            Expr::Lit(LitValue::from(4567), make_span!(16, 19))
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