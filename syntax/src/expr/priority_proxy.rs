///! fff-lang
///!
///! syntax/priority level proxy
///! they are here because they are dispatcher not containing data
///! primary_expr = ident_expr | lit_expr | unit_lit | paren_expr | tuple_def | array_def
///! postfix_expr = expr { ( member_access | fn_call | indexer_call ) }

use lexical::Token;
use lexical::Keyword;

use super::Expr;
use super::LitExpr;
use super::IdentExpr;
use super::TupleDef;
use super::ArrayDef;
use super::FnCallExpr;
use super::IndexCallExpr;
use super::MemberAccessExpr;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemGrammar;

pub struct PrimaryExpr;
impl ISyntaxItemParse for PrimaryExpr {
    type Target = Expr;
    
    fn parse(sess: &mut ParseSession) -> ParseResult<Expr> {
        #[cfg(feature = "trace_primary_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[PrimaryExpr: {}]", line!()); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_primary_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        trace!("start parsing, current token: {:?}", sess.tk);

        if LitExpr::is_first_final(sess) {
            return LitExpr::parse(sess);
        } else if IdentExpr::is_first_final(sess) {
            return Ok(Expr::Ident(IdentExpr::parse(sess)?));
        } else if TupleDef::is_first_final(sess) {
            return TupleDef::parse(sess);
        } else if ArrayDef::is_first_final(sess) {
            return ArrayDef::parse(sess);
        }

        if let (&Token::Keyword(Keyword::This), this_span) = (sess.tk, sess.pos) {
            sess.move_next();
            return Ok(Expr::Ident(IdentExpr::new(sess.symbols.intern_str("this"), this_span)));
        } else {
            return sess.push_unexpect("primary expr");
        }
    }
}

pub struct PostfixExpr;
impl ISyntaxItemParse for PostfixExpr {
    type Target = Expr;

    fn parse(sess: &mut ParseSession) -> ParseResult<Expr> {   
        #[cfg(feature = "trace_postfix_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ perror!("    [PostfixExpr:{}] ", line!()); perrorln!($($arg)*); }) }
        #[cfg(not(feature = "trace_postfix_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        let mut current_retval = PrimaryExpr::parse(sess)?;
        trace!("parsed primary, current is {:?}", current_retval);

        loop {
            if MemberAccessExpr::is_first_final(sess) {
                let mut postfix = MemberAccessExpr::parse(sess)?;
                postfix.all_span = current_retval.get_all_span().merge(&postfix.name.span);
                postfix.base = Box::new(current_retval);
                current_retval = Expr::MemberAccess(postfix);
            } else if FnCallExpr::is_first_final(sess) {
                let mut postfix = FnCallExpr::parse(sess)?;
                postfix.all_span = current_retval.get_all_span().merge(&postfix.paren_span);
                postfix.base = Box::new(current_retval);
                current_retval = Expr::FnCall(postfix);
            } else if IndexCallExpr::is_first_final(sess) {
                let mut postfix = IndexCallExpr::parse(sess)?;
                postfix.all_span = current_retval.get_all_span().merge(&postfix.bracket_span);
                postfix.base = Box::new(current_retval);
                current_retval = Expr::IndexCall(postfix);
            } else {
                break;
            }
        }

        trace!("parsing postfix finished, get retval: {:?}", current_retval);
        return Ok(current_retval);
    }
}

#[cfg(test)] #[test]
fn primary_expr_parse() {
    use codemap::Span;
    use codemap::SymbolCollection;
    use lexical::LitValue;
    use super::ExprList;
    use super::ParenExpr;
    use super::super::TestInput;
    use super::super::WithTestInput;

    // this is the loop of tokens.nth(current) is left bracket does not cover everything and infinite loop is here
    // update 2017/6/17: this was a bug, but I forget detail
    assert_eq!{ PrimaryExpr::with_test_str("[a]"),  
        Expr::Array(ArrayDef::new(make_span!(0, 2), make_exprs![
            IdentExpr::new(make_id!(1), make_span!(1, 1))
        ]))
    }

    //             0        1         2         3         4
    //             01234567890123456789012345678901234567890123456                  1       2         3       4         5
   TestInput::new("(463857, IEfN, atau8M, [fNAE, ((cAeJN4)), nHg])")
        .set_syms(make_symbols!["IEfN", "atau8M", "fNAE", "cAeJN4", "nHg"])
        .apply::<PrimaryExpr, _>()
        .expect_no_message()
        .expect_result(Expr::Tuple(TupleDef::new(make_span!(0, 46), make_exprs![
            LitExpr::new(LitValue::from(463857), make_span!(1, 6)),
            IdentExpr::new(make_id!(1), make_span!(9, 12)),
            IdentExpr::new(make_id!(2), make_span!(15, 20)),
            ArrayDef::new(make_span!(23, 45), make_exprs![
                IdentExpr::new(make_id!(3), make_span!(24, 27)),
                ParenExpr::new(make_span!(30, 39), 
                    ParenExpr::new(make_span!(31, 38), 
                        IdentExpr::new(make_id!(4), make_span!(32, 37))
                    )
                ),
                IdentExpr::new(make_id!(5), make_span!(42, 44))
            ])
        ])))
    .finish();

    assert_eq!{ PrimaryExpr::with_test_str("10363"), 
        Expr::Lit(LitExpr::new(LitValue::from(10363), make_span!(0, 4)))
    }

    assert_eq!{ PrimaryExpr::with_test_str(
    //   0         1         2         3         4         5         6         7         8         9         0         1         2         3       
    //   01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567 8901234567890123456789012345678901234
        "[(0x7E), FFGqfJe, I4, [(m7A, (41, ([(jL, rAn, K0FgLc7h, true), C, w]), (J3cEFDG, d, (j8h))), (), \neIuArjF), 400, 0o535147505, 0xDB747]]"),
        Expr::Array(ArrayDef::new(make_span!(0, 134), make_exprs![
            ParenExpr::new(make_span!(1, 6), 
               LitExpr::new(LitValue::from(0x7E), make_span!(2, 5))
            ),
            IdentExpr::new(make_id!(1), make_span!(9, 15)),
            IdentExpr::new(make_id!(2), make_span!(18, 19)), 
            ArrayDef::new(make_span!(22, 133), make_exprs![
                TupleDef::new(make_span!(23, 105), make_exprs![
                    IdentExpr::new(make_id!(3), make_span!(24, 26)),
                    TupleDef::new(make_span!(29, 90), make_exprs![
                        LitExpr::new(LitValue::from(41), make_span!(30, 31)),
                        ParenExpr::new(make_span!(34, 68), 
                            ArrayDef::new(make_span!(35, 67), make_exprs![
                                TupleDef::new(make_span!(36, 60), make_exprs![
                                    IdentExpr::new(make_id!(4), make_span!(37, 38)), 
                                    IdentExpr::new(make_id!(5), make_span!(41, 43)),
                                    IdentExpr::new(make_id!(6), make_span!(46, 53)),
                                    LitExpr::new(LitValue::from(true), make_span!(56, 59))
                                ]),
                                IdentExpr::new(make_id!(7), make_span!(63, 63)),
                                IdentExpr::new(make_id!(8), make_span!(66, 66)),
                            ])
                        ),
                        TupleDef::new(make_span!(71, 89), make_exprs![
                            IdentExpr::new(make_id!(9), make_span!(72, 78)),
                            IdentExpr::new(make_id!(10), make_span!(81, 81)),
                            ParenExpr::new(make_span!(84, 88), 
                                IdentExpr::new(make_id!(11), make_span!(85, 87))
                            )
                        ])
                    ]),
                    LitExpr::new(LitValue::Unit, make_span!(93, 94)),
                    IdentExpr::new(make_id!(12), make_span!(98, 104))
                ]),
                LitExpr::new(LitValue::from(400), make_span!(108, 110)),
                LitExpr::new(LitValue::from(0o535147505), make_span!(113, 123)),
                LitExpr::new(LitValue::from(0xDB747), make_span!(126, 132))
            ])
        ]))
    }

    assert_eq!{ PrimaryExpr::with_test_str("CMDoF"), Expr::Ident(IdentExpr::new(make_id!(1), make_span!(0, 4))) }
    assert_eq!{ PrimaryExpr::with_test_str("false"), Expr::Lit(LitExpr::new(LitValue::from(false), make_span!(0, 4))) }

    
    //                                      0        1         2         3         4         5         6          7          8         9         A
    //                                      12345678901234567890123456789012345678901234567890123456789012345678901 234 5678901234567890123456789012
    assert_eq!{ PrimaryExpr::with_test_str("[uy6, 4373577, [(q, AJBN0n, MDEgKh5,), KG, (NsL, ((), D, false, d, ), \"H=\"), true, ((vvB3, true, 5))]]"), 
        Expr::Array(ArrayDef::new(make_span!(0, 101), make_exprs![
            IdentExpr::new(make_id!(1), make_span!(1, 3)),
            LitExpr::new(LitValue::from(4373577), make_span!(6, 12)),
            ArrayDef::new(make_span!(15, 100), make_exprs![
                TupleDef::new(make_span!(16, 36), make_exprs![
                    IdentExpr::new(make_id!(2), make_span!(17, 17)),
                    IdentExpr::new(make_id!(3), make_span!(20, 25)), 
                    IdentExpr::new(make_id!(4), make_span!(28, 34))
                ]), 
                IdentExpr::new(make_id!(5), make_span!(39, 40)),
                TupleDef::new(make_span!(43, 74), make_exprs![
                    IdentExpr::new(make_id!(6), make_span!(44, 46)),
                    TupleDef::new(make_span!(49, 67), make_exprs![
                        LitExpr::new(LitValue::Unit, make_span!(50, 51)),
                        IdentExpr::new(make_id!(7), make_span!(54, 54)),
                        LitExpr::new(LitValue::from(false), make_span!(57, 61)),
                        IdentExpr::new(make_id!(8), make_span!(64, 64)),
                    ]),
                    LitExpr::new(LitValue::new_str_lit(make_id!(9)), make_span!(70, 73))
                ]),
                LitExpr::new(LitValue::from(true), make_span!(77, 80)),
                ParenExpr::new(make_span!(83, 99), 
                    TupleDef::new(make_span!(84, 98), make_exprs![
                        IdentExpr::new(make_id!(10), make_span!(85, 88)),
                        LitExpr::new(LitValue::from(true), make_span!(91, 94)),
                        LitExpr::new(LitValue::from(5), make_span!(97, 97))
                    ])
                )
            ])
        ]))
    }

    assert_eq!{ PrimaryExpr::with_test_str("(() )"), 
        Expr::Paren(ParenExpr::new(make_span!(0, 4), LitExpr::new(LitValue::Unit, make_span!(1, 2))))
    }
    assert_eq!{ PrimaryExpr::with_test_str("((),)"), 
        Expr::Tuple(TupleDef::new(make_span!(0, 4), make_exprs![LitExpr::new(LitValue::Unit, make_span!(1, 2))]))
    }

    assert_eq!{ PrimaryExpr::with_test_str("(\"o5\")"), 
        Expr::Paren(ParenExpr::new(make_span!(0, 5), 
            LitExpr::new(LitValue::new_str_lit(make_id!(1)), make_span!(1, 4))
        ))
    }

    //                                      0        1         2        
    //                                      1234567890123456789012345678
    assert_eq!{ PrimaryExpr::with_test_str("(nn, ([false,true]), 183455)"),
        Expr::Tuple(TupleDef::new(make_span!(0, 27), make_exprs![
            IdentExpr::new(make_id!(1), make_span!(1, 2)),
            ParenExpr::new(make_span!(5, 18), 
                ArrayDef::new(make_span!(6, 17), make_exprs![
                    LitExpr::new(LitValue::from(false), make_span!(7, 11)),
                    LitExpr::new(LitValue::from(true), make_span!(13, 16))
                ])
            ),
            LitExpr::new(LitValue::from(183455), make_span!(21, 26))
        ]))
    }
    
    //                                      0        1         2         3         4         5         6         7       
    //                                      123456789012345678901234567890123456789012345678901234567890123456789012345678
    assert_eq!{ PrimaryExpr::with_test_str("((true, (mO, [(q5k),a], (((KttG))), (K5DJ, r, ())), (McsaEdfdfalse,)), rIOKt,)"),
        Expr::Tuple(TupleDef::new(make_span!(0, 77), make_exprs![
            TupleDef::new(make_span!(1, 68), make_exprs![
                LitExpr::new(LitValue::from(true), make_span!(2, 5)),
                TupleDef::new(make_span!(8, 49), make_exprs![
                    IdentExpr::new(make_id!(1), make_span!(9, 10)),
                    ArrayDef::new(make_span!(13, 21), make_exprs![
                        ParenExpr::new(make_span!(14, 18), 
                            IdentExpr::new(make_id!(2), make_span!(15, 17))
                        ),
                        IdentExpr::new(make_id!(3), make_span!(20, 20))
                    ]),
                    ParenExpr::new(make_span!(24, 33), 
                        ParenExpr::new(make_span!(25, 32), 
                            ParenExpr::new(make_span!(26, 31), 
                                IdentExpr::new(make_id!(4), make_span!(27, 30))
                            )
                        )
                    ),
                    TupleDef::new(make_span!(36, 48), make_exprs![
                        IdentExpr::new(make_id!(5), make_span!(37, 40)), 
                        IdentExpr::new(make_id!(6), make_span!(43, 43)),
                        LitExpr::new(LitValue::Unit, make_span!(46, 47))
                    ]),
                ]),
                TupleDef::new(make_span!(52, 67), make_exprs![
                    IdentExpr::new(make_id!(7), make_span!(53, 65))
                ])
            ]),
            IdentExpr::new(make_id!(8), make_span!(71, 75))
        ]))
    }

    //                                      0          1         2      
    //                                      12 345 67890123456789012
    assert_eq!{ PrimaryExpr::with_test_str("[\"il\", 0o52u32, sO04n]"),
        Expr::Array(ArrayDef::new(make_span!(0, 21), make_exprs![
            LitExpr::new(LitValue::new_str_lit(make_id!(1)), make_span!(1, 4)),
            LitExpr::new(LitValue::from(0o52u32), make_span!(7, 13)), 
            IdentExpr::new(make_id!(2), make_span!(16, 20))
        ]))
    }
    //                                      12345678
    assert_eq!{ PrimaryExpr::with_test_str("['f',()]"), 
        Expr::Array(ArrayDef::new(make_span!(0, 7), make_exprs![
            LitExpr::new(LitValue::from('f'), make_span!(1, 3)),
            LitExpr::new(LitValue::Unit, make_span!(5, 6))
        ]))
    }
    assert_eq!{ PrimaryExpr::with_test_str("[]"), Expr::Array(ArrayDef::new(make_span!(0, 1), make_exprs![])) }

    //                                      0        1           2         3         4      
    //                                      12345 678901 234567890123456789012345678901234
    assert_eq!{ PrimaryExpr::with_test_str("[8, \"@=?GF\", 87f32, 1340323.74f64, FKOxAvx5]"),
        Expr::Array(ArrayDef::new(make_span!(0, 43), make_exprs![
            LitExpr::new(LitValue::from(8), make_span!(1, 1)),
            LitExpr::new(LitValue::new_str_lit(make_id!(1)), make_span!(4, 10)), 
            LitExpr::new(LitValue::from(87f32), make_span!(13, 17)),
            LitExpr::new(LitValue::from(1340323.74f64), make_span!(20, 32)),
            IdentExpr::new(make_id!(2), make_span!(35, 42))
        ]))
    }

    //                                        0        1         2         3         4         5         6     
    //                                        123456789012345678901234567890123456789012345678901234567890123
    assert_eq!{ PrimaryExpr::with_test_str(r#"  [[dnr4, lGFd3yL, tJ], ['\\', p, (xGaBwiL,), DE], true, aB8aE]"#),
        Expr::Array(ArrayDef::new(make_span!(2, 62), make_exprs![
            ArrayDef::new(make_span!(3, 21), make_exprs![
                IdentExpr::new(make_id!(1), make_span!(4, 7)),
                IdentExpr::new(make_id!(2), make_span!(10, 16)),
                IdentExpr::new(make_id!(3), make_span!(19, 20))
            ]),
            ArrayDef::new(make_span!(24, 48), make_exprs![
                LitExpr::new(LitValue::from('\\'), make_span!(25, 28)), 
                IdentExpr::new(make_id!(4), make_span!(31, 31)),
                TupleDef::new(make_span!(34, 43), make_exprs![
                    IdentExpr::new(make_id!(5), make_span!(35, 41))
                ]),
                IdentExpr::new(make_id!(6), make_span!(46, 47))
            ]),
            LitExpr::new(LitValue::from(true), make_span!(51, 54)),
            IdentExpr::new(make_id!(7), make_span!(57, 61))
        ]))
    } 

    // Previous manual tests
    //              0        1           2          3         4         5           6
    //              12345678901234 5678 9012 3456789012345678901234567890123 456789 0123456
    TestInput::new("[abc, 123u32, \"456\", '\\u0065', false, (), (a), (abc, \"hello\", ), ]")
        .set_syms(make_symbols!["abc", "456", "hello", "a"])
        .apply::<PrimaryExpr, _>()
        .expect_no_message()
        .expect_result(Expr::Array(ArrayDef::new(make_span!(0, 65), make_exprs![
            IdentExpr::new(make_id!(1), make_span!(1, 3)),
            LitExpr::new(LitValue::from(123u32), make_span!(6, 11)),
            LitExpr::new(LitValue::new_str_lit(make_id!(2)), make_span!(14, 18)),
            LitExpr::new(LitValue::from('\u{0065}'), make_span!(21, 28)),
            LitExpr::new(LitValue::from(false), make_span!(31, 35)),
            LitExpr::new(LitValue::Unit, make_span!(38, 39)),
            ParenExpr::new(make_span!(42, 44), 
                IdentExpr::new(make_id!(4), make_span!(43, 43))
            ),
            TupleDef::new(make_span!(47, 62), make_exprs![
                IdentExpr::new(make_id!(1), make_span!(48, 50)),
                LitExpr::new(LitValue::new_str_lit(make_id!(3)), make_span!(53, 59)),
            ])
        ])))
    .finish();

    assert_eq!{ PrimaryExpr::with_test_str("(                             )"), 
        Expr::Lit(LitExpr::new(LitValue::Unit, make_span!(0, 30)))
    }
}

#[cfg(test)] #[test]
fn primary_expr_errors() {
    use codemap::Span;
    use message::Message;
    use message::MessageCollection;
    use super::ExprList;
    use super::super::error_strings;
    use super::super::TestInput;

    TestInput::new("(,)")
        .apply::<PrimaryExpr, _>()
        .expect_result(Expr::Tuple(TupleDef::new(make_span!(0, 2), make_exprs![])))
        .expect_messages(make_messages![
            Message::new_by_str(error_strings::UnexpectedSingleComma, vec![(make_span!(0, 2), error_strings::TupleDefHere)])
        ])
    .finish();
}

#[cfg(test)] #[test]
fn postfix_expr_format() {
    use super::super::ISyntaxItemFormat;
    use super::super::WithTestInput;

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
    //                                                     0         1         2         3         4         5        
    //                                                     0123456789012345678901234567890123456789012345678901234567
    test_case!(format!("\n{}", PostfixExpr::with_test_str("a.b(c, d, e).f(g, h, i,)(u,).j[k].l().m[n, o, p][r, s, t,]").format(0)), r##"
IndexerCall <<0>0-57>
  IndexerCall <<0>0-47>
    MemberAccess <<0>0-38>
      FnCall <<0>0-36>
        MemberAccess <<0>0-34>
          IndexerCall <<0>0-32>
            MemberAccess <<0>0-29>
              FnCall <<0>0-27>
                FnCall <<0>0-23>
                  MemberAccess <<0>0-13>
                    FnCall <<0>0-11>
                      MemberAccess <<0>0-2>
                        Ident #1 <<0>0-0>
                        dot <<0>1-1>
                        Ident #2 <<0>2-2>
                      paren <<0>3-11>
                      Ident #3 <<0>4-4>
                      Ident #4 <<0>7-7>
                      Ident #5 <<0>10-10>
                    dot <<0>12-12>
                    Ident #6 <<0>13-13>
                  paren <<0>14-23>
                  Ident #7 <<0>15-15>
                  Ident #8 <<0>18-18>
                  Ident #9 <<0>21-21>
                paren <<0>24-27>
                Ident #10 <<0>25-25>
              dot <<0>28-28>
              Ident #11 <<0>29-29>
            bracket <<0>30-32>
            Ident #12 <<0>31-31>
          dot <<0>33-33>
          Ident #13 <<0>34-34>
        paren <<0>35-36>
        (empty)
      dot <<0>37-37>
      Ident #14 <<0>38-38>
    bracket <<0>39-47>
    Ident #15 <<0>40-40>
    Ident #16 <<0>43-43>
    Ident #17 <<0>46-46>
  bracket <<0>48-57>
  Ident #18 <<0>49-49>
  Ident #19 <<0>52-52>
  Ident #20 <<0>55-55>"##
    );
}

#[cfg(test)] #[test]
fn postfix_expr_parse() {
    use codemap::Span;
    use super::ExprList;
    use super::IdentExpr;
    use super::super::WithTestInput;

    //                                      0        1         2         3         4         5     
    // plain                                0123456789012345678901234567890123456789012345678901234567
    assert_eq!{ PostfixExpr::with_test_str("a.b(c, d, e).f(g, h, i,)(u,).j[k].l().m[n, o, p][r, s, t,]"),
        Expr::IndexCall(IndexCallExpr::new(
            IndexCallExpr::new(
                MemberAccessExpr::new(
                    FnCallExpr::new(
                        MemberAccessExpr::new(
                            IndexCallExpr::new(
                                MemberAccessExpr::new(
                                    FnCallExpr::new(
                                        FnCallExpr::new(
                                            MemberAccessExpr::new(
                                                FnCallExpr::new(
                                                    MemberAccessExpr::new(
                                                        IdentExpr::new(make_id!(1), make_span!(0, 0)),
                                                        make_span!(1, 1),
                                                        IdentExpr::new(make_id!(2), make_span!(2, 2))
                                                    ), 
                                                    make_span!(3, 11), make_exprs![
                                                        IdentExpr::new(make_id!(3), make_span!(4, 4)),
                                                        IdentExpr::new(make_id!(4), make_span!(7, 7)),
                                                        IdentExpr::new(make_id!(5), make_span!(10, 10)),
                                                    ]
                                                ),
                                                make_span!(12, 12),
                                                IdentExpr::new(make_id!(6), make_span!(13, 13))
                                            ),
                                            make_span!(14, 23), make_exprs![
                                                IdentExpr::new(make_id!(7), make_span!(15, 15)),
                                                IdentExpr::new(make_id!(8), make_span!(18, 18)),
                                                IdentExpr::new(make_id!(9), make_span!(21, 21)),
                                            ]
                                        ),
                                        make_span!(24, 27), make_exprs![
                                            IdentExpr::new(make_id!(10), make_span!(25, 25))
                                        ]
                                    ),
                                    make_span!(28, 28),
                                    IdentExpr::new(make_id!(11), make_span!(29, 29))
                                ),
                                make_span!(30, 32), make_exprs![
                                    IdentExpr::new(make_id!(12), make_span!(31, 31))
                                ]
                            ),
                            make_span!(33, 33),
                            IdentExpr::new(make_id!(13), make_span!(34, 34))
                        ),
                        make_span!(35, 36),
                        make_exprs![]
                    ),
                    make_span!(37, 37),
                    IdentExpr::new(make_id!(14), make_span!(38, 38))
                ),
                make_span!(39, 47), make_exprs![
                    IdentExpr::new(make_id!(15), make_span!(40, 40)),
                    IdentExpr::new(make_id!(16), make_span!(43, 43)),
                    IdentExpr::new(make_id!(17), make_span!(46, 46))
                ]
            ),
            make_span!(48, 57), make_exprs![
                IdentExpr::new(make_id!(18), make_span!(49, 49)),
                IdentExpr::new(make_id!(19), make_span!(52, 52)),
                IdentExpr::new(make_id!(20), make_span!(55, 55))
            ]
        ))
    }
}

#[cfg(test)] #[test]
fn postfix_expr_errors() {
    use codemap::Span;
    use message::Message;
    use message::MessageCollection;
    use super::ExprList;
    use super::super::error_strings;
    use super::super::TestInput;
    
    TestInput::new("a[]")
        .apply::<PostfixExpr, _>()
        .expect_result(Expr::IndexCall(IndexCallExpr::new(
            IdentExpr::new(make_id!(1), make_span!(0, 0)), 
            make_span!(1, 2), make_exprs![]
        )))
        .expect_messages(make_messages![
            Message::new_by_str(error_strings::EmptyIndexCall, vec![(make_span!(1, 2), error_strings::IndexCallHere)])
        ])
    .finish();
    
    TestInput::new("a[, ]")
        .apply::<PostfixExpr, _>()
        .expect_result(Expr::IndexCall(IndexCallExpr::new(
            IdentExpr::new(make_id!(1), make_span!(0, 0)), 
            make_span!(1, 4), make_exprs![]
        )))
        .expect_messages(make_messages![
            Message::new_by_str(error_strings::EmptyIndexCall, vec![(make_span!(1, 4), error_strings::IndexCallHere)])
        ])
    .finish();
    
    TestInput::new("a(, )")
        .apply::<PostfixExpr, _>()
        .expect_result(Expr::FnCall(FnCallExpr::new(
            IdentExpr::new(make_id!(1), make_span!(0, 0)),
            make_span!(1, 4), make_exprs![]
        )))
        .expect_messages(make_messages![
            Message::new_by_str(error_strings::UnexpectedSingleComma, vec![(make_span!(1, 4), error_strings::FnCallHere)])
        ])
    .finish();
}