///! fff-lang
///!
///! syntax/priority level proxy
///! they are here because they are dispatcher not containing data
///! primary_expr = ident_expr | lit_expr | unit_lit | paren_expr | tuple_def | array_def
///! postfix_expr = expr { ( member_access | fn_call | indexer_call ) }

use crate::lexical::Keyword;
use super::Expr;
use super::Name;
use super::LitExpr;
use super::SimpleName;
use super::TupleDef;
use super::ArrayDef;
use super::FnCallExpr;
use super::IndexCallExpr;
use super::MemberAccessExpr;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxParse;
use super::super::ISyntaxGrammar;

pub struct PrimaryExpr;
impl ISyntaxParse for PrimaryExpr {
    type Output = Expr;
    
    fn parse(sess: &mut ParseSession) -> ParseResult<Expr> {

        if LitExpr::matches_first(sess.current_tokens()) {
            return LitExpr::parse(sess);
        } else if Name::matches_first(sess.current_tokens()) {
            return Name::parse(sess);
        } else if TupleDef::matches_first(sess.current_tokens()) {
            return TupleDef::parse(sess);
        } else if ArrayDef::matches_first(sess.current_tokens()) {
            return ArrayDef::parse(sess);
        }

        let (this_id, this_span) = sess.expect_ident_or(vec![Keyword::This])?;  // actually identifier is processed by Name, not here
        return Ok(Expr::SimpleName(SimpleName::new(this_id, this_span)));
    }
}

pub struct PostfixExpr;
impl ISyntaxParse for PostfixExpr {
    type Output = Expr;

    fn parse(sess: &mut ParseSession) -> ParseResult<Expr> {   
        #[cfg(feature = "trace_postfix_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ perror!("    [PostfixExpr:{}] ", line!()); perrorln!($($arg)*); }) }
        #[cfg(not(feature = "trace_postfix_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        let mut current_retval = PrimaryExpr::parse(sess)?;
        trace!("parsed primary, current is {:?}", current_retval);

        loop {
            if MemberAccessExpr::matches_first(sess.current_tokens()) {
                let mut postfix = MemberAccessExpr::parse(sess)?;
                postfix.all_span = current_retval.get_all_span().merge(&postfix.name.span);
                postfix.base = Box::new(current_retval);
                current_retval = Expr::MemberAccess(postfix);
            } else if FnCallExpr::matches_first(sess.current_tokens()) {
                let mut postfix = FnCallExpr::parse(sess)?;
                postfix.all_span = current_retval.get_all_span().merge(&postfix.paren_span);
                postfix.base = Box::new(current_retval);
                current_retval = Expr::FnCall(postfix);
            } else if IndexCallExpr::matches_first(sess.current_tokens()) {
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
    use crate::source::Span;
    use crate::source::SymbolCollection;
    use crate::lexical::LitValue;
    use super::ExprList;
    use super::ParenExpr;
    use super::super::TestInput;
    use super::super::WithTestInput;

    // this is the loop of tokens.nth(current) is left bracket does not cover everything and infinite loop is here
    // update 2017/6/17: this was a bug, but I forget detail
    assert_eq!{ make_node!("[a]"),  
        Expr::Array(ArrayDef::new(Span::new(0, 2), make_exprs![
            SimpleName::new(1, Span::new(1, 1))
        ]))
    }

    //             0        1         2         3         4
    //             01234567890123456789012345678901234567890123456                  1       2         3       4         5
   TestInput::new("(463857, IEfN, atau8M, [fNAE, ((cAeJN4)), nHg])")
        .set_syms(make_symbols!["IEfN", "atau8M", "fNAE", "cAeJN4", "nHg"])
        .apply::<PrimaryExpr, _>()
        .expect_no_message()
        .expect_result(Expr::Tuple(TupleDef::new(Span::new(0, 46), make_exprs![
            LitExpr::new(LitValue::from(463857), Span::new(1, 6)),
            SimpleName::new(1, Span::new(9, 12)),
            SimpleName::new(2, Span::new(15, 20)),
            ArrayDef::new(Span::new(23, 45), make_exprs![
                SimpleName::new(3, Span::new(24, 27)),
                ParenExpr::new(Span::new(30, 39), 
                    ParenExpr::new(Span::new(31, 38), 
                        SimpleName::new(4, Span::new(32, 37))
                    )
                ),
                SimpleName::new(5, Span::new(42, 44))
            ])
        ])))
    .finish();

    assert_eq!{ make_node!("10363"), 
        Expr::Lit(LitExpr::new(LitValue::from(10363), Span::new(0, 4)))
    }

    assert_eq!{ make_node!(
    //   0         1         2         3         4         5         6         7         8         9         0         1         2         3       
    //   01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567 8901234567890123456789012345678901234
        "[(0x7E), FFGqfJe, I4, [(m7A, (41, ([(jL, rAn, K0FgLc7h, true), C, w]), (J3cEFDG, d, (j8h))), (), \neIuArjF), 400, 0o535147505, 0xDB747]]"),
        Expr::Array(ArrayDef::new(Span::new(0, 134), make_exprs![
            ParenExpr::new(Span::new(1, 6), 
               LitExpr::new(LitValue::from(0x7E), Span::new(2, 5))
            ),
            SimpleName::new(1, Span::new(9, 15)),
            SimpleName::new(2, Span::new(18, 19)), 
            ArrayDef::new(Span::new(22, 133), make_exprs![
                TupleDef::new(Span::new(23, 105), make_exprs![
                    SimpleName::new(3, Span::new(24, 26)),
                    TupleDef::new(Span::new(29, 90), make_exprs![
                        LitExpr::new(LitValue::from(41), Span::new(30, 31)),
                        ParenExpr::new(Span::new(34, 68), 
                            ArrayDef::new(Span::new(35, 67), make_exprs![
                                TupleDef::new(Span::new(36, 60), make_exprs![
                                    SimpleName::new(4, Span::new(37, 38)), 
                                    SimpleName::new(5, Span::new(41, 43)),
                                    SimpleName::new(6, Span::new(46, 53)),
                                    LitExpr::new(LitValue::from(true), Span::new(56, 59))
                                ]),
                                SimpleName::new(7, Span::new(63, 63)),
                                SimpleName::new(8, Span::new(66, 66)),
                            ])
                        ),
                        TupleDef::new(Span::new(71, 89), make_exprs![
                            SimpleName::new(9, Span::new(72, 78)),
                            SimpleName::new(10, Span::new(81, 81)),
                            ParenExpr::new(Span::new(84, 88), 
                                SimpleName::new(11, Span::new(85, 87))
                            )
                        ])
                    ]),
                    LitExpr::new(LitValue::Unit, Span::new(93, 94)),
                    SimpleName::new(12, Span::new(98, 104))
                ]),
                LitExpr::new(LitValue::from(400), Span::new(108, 110)),
                LitExpr::new(LitValue::from(0o535147505), Span::new(113, 123)),
                LitExpr::new(LitValue::from(0xDB747), Span::new(126, 132))
            ])
        ]))
    }

    assert_eq!{ make_node!("CMDoF"), Expr::SimpleName(SimpleName::new(1, Span::new(0, 4))) }
    assert_eq!{ make_node!("false"), Expr::Lit(LitExpr::new(LitValue::from(false), Span::new(0, 4))) }

    
    //                                      0        1         2         3         4         5         6          7          8         9         A
    //                                      12345678901234567890123456789012345678901234567890123456789012345678901 234 5678901234567890123456789012
    assert_eq!{ make_node!("[uy6, 4373577, [(q, AJBN0n, MDEgKh5,), KG, (NsL, ((), D, false, d, ), \"H=\"), true, ((vvB3, true, 5))]]"), 
        Expr::Array(ArrayDef::new(Span::new(0, 101), make_exprs![
            SimpleName::new(1, Span::new(1, 3)),
            LitExpr::new(LitValue::from(4373577), Span::new(6, 12)),
            ArrayDef::new(Span::new(15, 100), make_exprs![
                TupleDef::new(Span::new(16, 36), make_exprs![
                    SimpleName::new(2, Span::new(17, 17)),
                    SimpleName::new(3, Span::new(20, 25)), 
                    SimpleName::new(4, Span::new(28, 34))
                ]), 
                SimpleName::new(5, Span::new(39, 40)),
                TupleDef::new(Span::new(43, 74), make_exprs![
                    SimpleName::new(6, Span::new(44, 46)),
                    TupleDef::new(Span::new(49, 67), make_exprs![
                        LitExpr::new(LitValue::Unit, Span::new(50, 51)),
                        SimpleName::new(7, Span::new(54, 54)),
                        LitExpr::new(LitValue::from(false), Span::new(57, 61)),
                        SimpleName::new(8, Span::new(64, 64)),
                    ]),
                    LitExpr::new(make_lit!(str, 9), Span::new(70, 73))
                ]),
                LitExpr::new(LitValue::from(true), Span::new(77, 80)),
                ParenExpr::new(Span::new(83, 99), 
                    TupleDef::new(Span::new(84, 98), make_exprs![
                        SimpleName::new(10, Span::new(85, 88)),
                        LitExpr::new(LitValue::from(true), Span::new(91, 94)),
                        LitExpr::new(LitValue::from(5), Span::new(97, 97))
                    ])
                )
            ])
        ]))
    }

    assert_eq!{ make_node!("(() )"), 
        Expr::Paren(ParenExpr::new(Span::new(0, 4), LitExpr::new(LitValue::Unit, Span::new(1, 2))))
    }
    assert_eq!{ make_node!("((),)"), 
        Expr::Tuple(TupleDef::new(Span::new(0, 4), make_exprs![LitExpr::new(LitValue::Unit, Span::new(1, 2))]))
    }

    assert_eq!{ make_node!("(\"o5\")"), 
        Expr::Paren(ParenExpr::new(Span::new(0, 5), 
            LitExpr::new(make_lit!(str, 1), Span::new(1, 4))
        ))
    }

    //                                      0        1         2        
    //                                      1234567890123456789012345678
    assert_eq!{ make_node!("(nn, ([false,true]), 183455)"),
        Expr::Tuple(TupleDef::new(Span::new(0, 27), make_exprs![
            SimpleName::new(1, Span::new(1, 2)),
            ParenExpr::new(Span::new(5, 18), 
                ArrayDef::new(Span::new(6, 17), make_exprs![
                    LitExpr::new(LitValue::from(false), Span::new(7, 11)),
                    LitExpr::new(LitValue::from(true), Span::new(13, 16))
                ])
            ),
            LitExpr::new(LitValue::from(183455), Span::new(21, 26))
        ]))
    }
    
    //                                      0        1         2         3         4         5         6         7       
    //                                      123456789012345678901234567890123456789012345678901234567890123456789012345678
    assert_eq!{ make_node!("((true, (mO, [(q5k),a], (((KttG))), (K5DJ, r, ())), (McsaEdfdfalse,)), rIOKt,)"),
        Expr::Tuple(TupleDef::new(Span::new(0, 77), make_exprs![
            TupleDef::new(Span::new(1, 68), make_exprs![
                LitExpr::new(LitValue::from(true), Span::new(2, 5)),
                TupleDef::new(Span::new(8, 49), make_exprs![
                    SimpleName::new(1, Span::new(9, 10)),
                    ArrayDef::new(Span::new(13, 21), make_exprs![
                        ParenExpr::new(Span::new(14, 18), 
                            SimpleName::new(2, Span::new(15, 17))
                        ),
                        SimpleName::new(3, Span::new(20, 20))
                    ]),
                    ParenExpr::new(Span::new(24, 33), 
                        ParenExpr::new(Span::new(25, 32), 
                            ParenExpr::new(Span::new(26, 31), 
                                SimpleName::new(4, Span::new(27, 30))
                            )
                        )
                    ),
                    TupleDef::new(Span::new(36, 48), make_exprs![
                        SimpleName::new(5, Span::new(37, 40)), 
                        SimpleName::new(6, Span::new(43, 43)),
                        LitExpr::new(LitValue::Unit, Span::new(46, 47))
                    ]),
                ]),
                TupleDef::new(Span::new(52, 67), make_exprs![
                    SimpleName::new(7, Span::new(53, 65))
                ])
            ]),
            SimpleName::new(8, Span::new(71, 75))
        ]))
    }

    //                                      0          1         2      
    //                                      12 345 67890123456789012
    assert_eq!{ make_node!("[\"il\", 0o52u32, sO04n]"),
        Expr::Array(ArrayDef::new(Span::new(0, 21), make_exprs![
            LitExpr::new(make_lit!(str, 1), Span::new(1, 4)),
            LitExpr::new(LitValue::from(0o52u32), Span::new(7, 13)), 
            SimpleName::new(2, Span::new(16, 20))
        ]))
    }
    //                                      12345678
    assert_eq!{ make_node!("['f',()]"), 
        Expr::Array(ArrayDef::new(Span::new(0, 7), make_exprs![
            LitExpr::new(LitValue::from('f'), Span::new(1, 3)),
            LitExpr::new(LitValue::Unit, Span::new(5, 6))
        ]))
    }
    assert_eq!{ make_node!("[]"), Expr::Array(ArrayDef::new(Span::new(0, 1), make_exprs![])) }

    //                                      0        1           2         3         4      
    //                                      12345 678901 234567890123456789012345678901234
    assert_eq!{ make_node!("[8, \"@=?GF\", 87r32, 1340323.74r64, FKOxAvx5]"),
        Expr::Array(ArrayDef::new(Span::new(0, 43), make_exprs![
            LitExpr::new(LitValue::from(8), Span::new(1, 1)),
            LitExpr::new(make_lit!(str, 1), Span::new(4, 10)), 
            LitExpr::new(LitValue::from(87f32), Span::new(13, 17)),
            LitExpr::new(LitValue::from(1340323.74f64), Span::new(20, 32)),
            SimpleName::new(2, Span::new(35, 42))
        ]))
    }

    //                                        0        1         2         3         4         5         6     
    //                                        123456789012345678901234567890123456789012345678901234567890123
    assert_eq!{ make_node!(r#"  [[dnr4, lGFd3yL, tJ], ['\\', p, (xGaBwiL,), DE], true, aB8aE]"#),
        Expr::Array(ArrayDef::new(Span::new(2, 62), make_exprs![
            ArrayDef::new(Span::new(3, 21), make_exprs![
                SimpleName::new(1, Span::new(4, 7)),
                SimpleName::new(2, Span::new(10, 16)),
                SimpleName::new(3, Span::new(19, 20))
            ]),
            ArrayDef::new(Span::new(24, 48), make_exprs![
                LitExpr::new(LitValue::from('\\'), Span::new(25, 28)), 
                SimpleName::new(4, Span::new(31, 31)),
                TupleDef::new(Span::new(34, 43), make_exprs![
                    SimpleName::new(5, Span::new(35, 41))
                ]),
                SimpleName::new(6, Span::new(46, 47))
            ]),
            LitExpr::new(LitValue::from(true), Span::new(51, 54)),
            SimpleName::new(7, Span::new(57, 61))
        ]))
    } 

    // Previous manual tests
    //              0        1           2          3         4         5           6
    //              12345678901234 5678 9012 3456789012345678901234567890123 456789 0123456
    TestInput::new("[abc, 123u32, \"456\", '\\u0065', false, (), (a), (abc, \"hello\", ), ]")
        .set_syms(make_symbols!["abc", "456", "hello", "a"])
        .apply::<PrimaryExpr, _>()
        .expect_no_message()
        .expect_result(Expr::Array(ArrayDef::new(Span::new(0, 65), make_exprs![
            SimpleName::new(1, Span::new(1, 3)),
            LitExpr::new(LitValue::from(123u32), Span::new(6, 11)),
            LitExpr::new(make_lit!(str, 2), Span::new(14, 18)),
            LitExpr::new(LitValue::from('\u{0065}'), Span::new(21, 28)),
            LitExpr::new(LitValue::from(false), Span::new(31, 35)),
            LitExpr::new(LitValue::Unit, Span::new(38, 39)),
            ParenExpr::new(Span::new(42, 44), 
                SimpleName::new(4, Span::new(43, 43))
            ),
            TupleDef::new(Span::new(47, 62), make_exprs![
                SimpleName::new(1, Span::new(48, 50)),
                LitExpr::new(make_lit!(str, 3), Span::new(53, 59)),
            ])
        ])))
    .finish();

    assert_eq!{ make_node!("(                             )"), 
        Expr::Lit(LitExpr::new(LitValue::Unit, Span::new(0, 30)))
    }
}

#[cfg(test)] #[test]
fn primary_expr_errors() {
    use crate::source::Span;
    use crate::diagnostics::Message;
    use crate::diagnostics::MessageCollection;
    use super::ExprList;
    use super::super::error_strings;
    use super::super::TestInput;

    TestInput::new("(,)")
        .apply::<PrimaryExpr, _>()
        .expect_result(Expr::Tuple(TupleDef::new(Span::new(0, 2), make_exprs![])))
        .expect_messages(make_messages![
            Message::new_by_str(error_strings::UnexpectedSingleComma, vec![(Span::new(0, 2), error_strings::TupleDefHere)])
        ])
    .finish();
}

#[cfg(test)] #[test]
fn postfix_expr_format() {
    use super::super::ISyntaxFormat;
    use super::super::WithTestInput;
    use super::super::Formatter;

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
    test_case!(format!("\n{}", make_node!("a.b(c, d, e).f(g, h, i,)(u,).j[k].l().m[n, o, p][r, s, t,]").format(Formatter::empty())), r##"
indexer-call <<0>0-57>
  base-is indexer-call <<0>0-47>
    base-is member-access <<0>0-38>
      base-is fn-call <<0>0-36>
        base-is member-access <<0>0-34>
          base-is indexer-call <<0>0-32>
            base-is member-access <<0>0-29>
              base-is fn-call <<0>0-27>
                base-is fn-call <<0>0-23>
                  base-is member-access <<0>0-13>
                    base-is fn-call <<0>0-11>
                      base-is member-access <<0>0-2>
                        base-is ident-use #1 <<0>0-0>
                        "." <<0>1-1>
                        member-name-is #2 <<0>2-2>
                      parenthenes <<0>3-11>
                      ident-use #3 <<0>4-4>
                      ident-use #4 <<0>7-7>
                      ident-use #5 <<0>10-10>
                    "." <<0>12-12>
                    member-name-is #6 <<0>13-13>
                  parenthenes <<0>14-23>
                  ident-use #7 <<0>15-15>
                  ident-use #8 <<0>18-18>
                  ident-use #9 <<0>21-21>
                parenthenes <<0>24-27>
                ident-use #10 <<0>25-25>
              "." <<0>28-28>
              member-name-is #11 <<0>29-29>
            bracket <<0>30-32>
            ident-use #12 <<0>31-31>
          "." <<0>33-33>
          member-name-is #13 <<0>34-34>
        parenthenes <<0>35-36>
        no-argument
      "." <<0>37-37>
      member-name-is #14 <<0>38-38>
    bracket <<0>39-47>
    ident-use #15 <<0>40-40>
    ident-use #16 <<0>43-43>
    ident-use #17 <<0>46-46>
  bracket <<0>48-57>
  ident-use #18 <<0>49-49>
  ident-use #19 <<0>52-52>
  ident-use #20 <<0>55-55>"##
    );
}

#[cfg(test)] #[test]
fn postfix_expr_parse() {
    use crate::source::Span;
    use super::ExprList;
    use super::SimpleName;
    use super::super::WithTestInput;

    //                                      0        1         2         3         4         5     
    // plain                                0123456789012345678901234567890123456789012345678901234567
    assert_eq!{ make_node!("a.b(c, d, e).f(g, h, i,)(u,).j[k].l().m[n, o, p][r, s, t,]"),
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
                                                        SimpleName::new(1, Span::new(0, 0)),
                                                        Span::new(1, 1),
                                                        SimpleName::new(2, Span::new(2, 2))
                                                    ), 
                                                    Span::new(3, 11), make_exprs![
                                                        SimpleName::new(3, Span::new(4, 4)),
                                                        SimpleName::new(4, Span::new(7, 7)),
                                                        SimpleName::new(5, Span::new(10, 10)),
                                                    ]
                                                ),
                                                Span::new(12, 12),
                                                SimpleName::new(6, Span::new(13, 13))
                                            ),
                                            Span::new(14, 23), make_exprs![
                                                SimpleName::new(7, Span::new(15, 15)),
                                                SimpleName::new(8, Span::new(18, 18)),
                                                SimpleName::new(9, Span::new(21, 21)),
                                            ]
                                        ),
                                        Span::new(24, 27), make_exprs![
                                            SimpleName::new(10, Span::new(25, 25))
                                        ]
                                    ),
                                    Span::new(28, 28),
                                    SimpleName::new(11, Span::new(29, 29))
                                ),
                                Span::new(30, 32), make_exprs![
                                    SimpleName::new(12, Span::new(31, 31))
                                ]
                            ),
                            Span::new(33, 33),
                            SimpleName::new(13, Span::new(34, 34))
                        ),
                        Span::new(35, 36),
                        make_exprs![]
                    ),
                    Span::new(37, 37),
                    SimpleName::new(14, Span::new(38, 38))
                ),
                Span::new(39, 47), make_exprs![
                    SimpleName::new(15, Span::new(40, 40)),
                    SimpleName::new(16, Span::new(43, 43)),
                    SimpleName::new(17, Span::new(46, 46))
                ]
            ),
            Span::new(48, 57), make_exprs![
                SimpleName::new(18, Span::new(49, 49)),
                SimpleName::new(19, Span::new(52, 52)),
                SimpleName::new(20, Span::new(55, 55))
            ]
        ))
    }
}

#[cfg(test)] #[test]
fn postfix_expr_errors() {
    use crate::source::Span;
    use crate::diagnostics::Message;
    use crate::diagnostics::MessageCollection;
    use super::ExprList;
    use super::super::error_strings;
    use super::super::TestInput;
    
    TestInput::new("a[]")
        .apply::<PostfixExpr, _>()
        .expect_result(Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(1, Span::new(0, 0)), 
            Span::new(1, 2), make_exprs![]
        )))
        .expect_messages(make_messages![
            Message::new_by_str(error_strings::EmptyIndexCall, vec![(Span::new(1, 2), error_strings::IndexCallHere)])
        ])
    .finish();
    
    TestInput::new("a[, ]")
        .apply::<PostfixExpr, _>()
        .expect_result(Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(1, Span::new(0, 0)), 
            Span::new(1, 4), make_exprs![]
        )))
        .expect_messages(make_messages![
            Message::new_by_str(error_strings::EmptyIndexCall, vec![(Span::new(1, 4), error_strings::IndexCallHere)])
        ])
    .finish();
    
    TestInput::new("a(, )")
        .apply::<PostfixExpr, _>()
        .expect_result(Expr::FnCall(FnCallExpr::new(
            SimpleName::new(1, Span::new(0, 0)),
            Span::new(1, 4), make_exprs![]
        )))
        .expect_messages(make_messages![
            Message::new_by_str(error_strings::UnexpectedSingleComma, vec![(Span::new(1, 4), error_strings::FnCallHere)])
        ])
    .finish();
}