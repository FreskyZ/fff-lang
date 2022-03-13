///! fff-lang
///!
///! syntax/priority level proxy
///! they are here because they are dispatcher not containing data
///! primary_expr = ident_expr | lit_expr | unit_lit | paren_expr | tuple_def | array_def
///! postfix_expr = expr { ( member_access | fn_call | indexer_call ) }

use super::prelude::*;
use super::{Expr, Name, LitExpr, SimpleName, TupleDef, ArrayDef, FnCallExpr, IndexCallExpr, MemberAccessExpr};

struct PrimaryExpr;
impl Node for PrimaryExpr {
    type ParseOutput = Expr;
    
    fn parse<F>(sess: &mut ParseSession<F>) -> ParseResult<Expr> where F: FileSystem {

        if sess.matches::<LitExpr>() {
            return LitExpr::parse(sess);
        } else if sess.matches::<Name>() {
            return Name::parse(sess);
        } else if sess.matches::<TupleDef>() {
            return TupleDef::parse(sess);
        } else if sess.matches::<ArrayDef>() {
            return ArrayDef::parse(sess);
        }

        let (this_id, this_span) = sess.expect_ident_or(&[Keyword::This])?;  // actually identifier is processed by Name, not here
        return Ok(Expr::SimpleName(SimpleName::new(this_id, this_span)));
    }
}

pub struct PostfixExpr;
impl Node for PostfixExpr {
    type ParseOutput = Expr;

    fn parse<F>(sess: &mut ParseSession<F>) -> ParseResult<Expr> where F: FileSystem {   
        #[cfg(feature = "trace_postfix_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ perror!("    [PostfixExpr:{}] ", line!()); perrorln!($($arg)*); }) }
        #[cfg(not(feature = "trace_postfix_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        let mut current_retval = PrimaryExpr::parse(sess)?;
        trace!("parsed primary, current is {:?}", current_retval);

        loop {
            if sess.matches::<MemberAccessExpr>() {
                let mut postfix = MemberAccessExpr::parse(sess)?;
                postfix.all_span = current_retval.get_all_span() + postfix.name.span;
                postfix.base = Box::new(current_retval);
                current_retval = Expr::MemberAccess(postfix);
            } else if sess.matches::<FnCallExpr>() {
                let mut postfix = FnCallExpr::parse(sess)?;
                postfix.all_span = current_retval.get_all_span() + postfix.paren_span;
                postfix.base = Box::new(current_retval);
                current_retval = Expr::FnCall(postfix);
            } else if sess.matches::<IndexCallExpr>() {
                let mut postfix = IndexCallExpr::parse(sess)?;
                postfix.all_span = current_retval.get_all_span() + postfix.bracket_span;
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
    use super::{make_node, make_exprs, LitValue, ParenExpr};

    // this is the loop of tokens.nth(current) is left bracket does not cover everything and infinite loop is here
    // update 2017/6/17: this was a bug, but I forget detail
    assert_eq!{ make_node!("[a]" as Expr),  
        Expr::Array(ArrayDef::new(Span::new(0, 2), make_exprs![
            SimpleName::new(1, Span::new(1, 1))
        ]))
    }

    //                      0        1         2         3         4
    //                      01234567890123456789012345678901234567890123456     
    assert_eq!{ make_node!("(463857, IEfN, atau8M, [fNAE, ((cAeJN4)), nHg])" as Expr),
        Expr::Tuple(TupleDef::new(Span::new(0, 46), make_exprs![
            LitExpr::new(LitValue::from(463857i32), Span::new(1, 6)),
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
        ]))
    }

    assert_eq!{ make_node!("10363" as Expr),
        Expr::Lit(LitExpr::new(LitValue::from(10363i32), Span::new(0, 4)))
    }

    assert_eq!{ make_node!(
    //   0         1         2         3         4         5         6         7         8         9         0         1         2         3       
    //   01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567 8901234567890123456789012345678901234
        "[(0x7E), FFGqfJe, I4, [(m7A, (41, ([(jL, rAn, K0FgLc7h, true), C, w]), (J3cEFDG, d, (j8h))), (), \neIuArjF), 400, 0o535147505, 0xDB747]]" as Expr),
        Expr::Array(ArrayDef::new(Span::new(0, 134), make_exprs![
            ParenExpr::new(Span::new(1, 6), 
               LitExpr::new(LitValue::from(0x7Ei32), Span::new(2, 5))
            ),
            SimpleName::new(1, Span::new(9, 15)),
            SimpleName::new(2, Span::new(18, 19)), 
            ArrayDef::new(Span::new(22, 133), make_exprs![
                TupleDef::new(Span::new(23, 105), make_exprs![
                    SimpleName::new(3, Span::new(24, 26)),
                    TupleDef::new(Span::new(29, 90), make_exprs![
                        LitExpr::new(LitValue::from(41i32), Span::new(30, 31)),
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
                LitExpr::new(LitValue::from(400i32), Span::new(108, 110)),
                LitExpr::new(LitValue::Num(Numeric::I32(0o535147505)), Span::new(113, 123)),
                LitExpr::new(LitValue::from(0xDB747i32), Span::new(126, 132))
            ])
        ]))
    }

    assert_eq!{ make_node!("CMDoF" as Expr), Expr::SimpleName(SimpleName::new(2, Span::new(0, 4))) }
    assert_eq!{ make_node!("false" as Expr), Expr::Lit(LitExpr::new(LitValue::from(false), Span::new(0, 4))) }

    
    //                      0        1         2         3         4         5         6          7          8         9         A
    //                      12345678901234567890123456789012345678901234567890123456789012345678901 234 5678901234567890123456789012
    assert_eq!{ make_node!("[uy6, 4373577, [(q, AJBN0n, MDEgKh5,), KG, (NsL, ((), D, false, d, ), \"H=\"), true, ((vvB3, true, 5))]]" as Expr), 
        Expr::Array(ArrayDef::new(Span::new(0, 101), make_exprs![
            SimpleName::new(1, Span::new(1, 3)),
            LitExpr::new(LitValue::from(4373577i32), Span::new(6, 12)),
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
                    LitExpr::new(10u32, Span::new(70, 73))
                ]),
                LitExpr::new(LitValue::from(true), Span::new(77, 80)),
                ParenExpr::new(Span::new(83, 99), 
                    TupleDef::new(Span::new(84, 98), make_exprs![
                        SimpleName::new(10, Span::new(85, 88)),
                        LitExpr::new(LitValue::from(true), Span::new(91, 94)),
                        LitExpr::new(LitValue::from(5i32), Span::new(97, 97))
                    ])
                )
            ])
        ]))
    }

    assert_eq!{ make_node!("(() )" as Expr), 
        Expr::Paren(ParenExpr::new(Span::new(0, 4), LitExpr::new(LitValue::Unit, Span::new(1, 2))))
    }
    assert_eq!{ make_node!("((),)" as Expr), 
        Expr::Tuple(TupleDef::new(Span::new(0, 4), make_exprs![LitExpr::new(LitValue::Unit, Span::new(1, 2))]))
    }

    assert_eq!{ make_node!("(\"o5\")" as Expr), 
        Expr::Paren(ParenExpr::new(Span::new(0, 5), 
            LitExpr::new(2u32, Span::new(1, 4))
        ))
    }

    //                      0        1         2        
    //                      1234567890123456789012345678
    assert_eq!{ make_node!("(nn, ([false,true]), 183455)" as Expr),
        Expr::Tuple(TupleDef::new(Span::new(0, 27), make_exprs![
            SimpleName::new(1, Span::new(1, 2)),
            ParenExpr::new(Span::new(5, 18), 
                ArrayDef::new(Span::new(6, 17), make_exprs![
                    LitExpr::new(LitValue::from(false), Span::new(7, 11)),
                    LitExpr::new(LitValue::from(true), Span::new(13, 16))
                ])
            ),
            LitExpr::new(LitValue::from(183455i32), Span::new(21, 26))
        ]))
    }
    
    //                      0        1         2         3         4         5         6         7       
    //                      123456789012345678901234567890123456789012345678901234567890123456789012345678
    assert_eq!{ make_node!("((true, (mO, [(q5k),a], (((KttG))), (K5DJ, r, ())), (McsaEdfdfalse,)), rIOKt,)" as Expr),
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
    assert_eq!{ make_node!("[\"il\", 0o52u32, sO04n]" as Expr),
        Expr::Array(ArrayDef::new(Span::new(0, 21), make_exprs![
            LitExpr::new(2u32, Span::new(1, 4)),
            LitExpr::new(LitValue::from(0o52u32), Span::new(7, 13)), 
            SimpleName::new(2, Span::new(16, 20))
        ]))
    }
    //                                      12345678
    assert_eq!{ make_node!("['f',()]" as Expr), 
        Expr::Array(ArrayDef::new(Span::new(0, 7), make_exprs![
            LitExpr::new(LitValue::from('f'), Span::new(1, 3)),
            LitExpr::new(LitValue::Unit, Span::new(5, 6))
        ]))
    }
    assert_eq!{ make_node!("[]" as Expr), Expr::Array(ArrayDef::new(Span::new(0, 1), make_exprs![])) }

    //                                      0        1           2         3         4      
    //                                      12345 678901 234567890123456789012345678901234
    assert_eq!{ make_node!("[8, \"@=?GF\", 87r32, 1340323.74r64, FKOxAvx5]" as Expr),
        Expr::Array(ArrayDef::new(Span::new(0, 43), make_exprs![
            LitExpr::new(LitValue::from(8i32), Span::new(1, 1)),
            LitExpr::new(2u32, Span::new(4, 10)), 
            LitExpr::new(LitValue::Num(Numeric::R32(87f32)), Span::new(13, 17)),
            LitExpr::new(LitValue::Num(Numeric::R64(1340323.74f64)), Span::new(20, 32)),
            SimpleName::new(2, Span::new(35, 42))
        ]))
    }

    //                                        0        1         2         3         4         5         6     
    //                                        123456789012345678901234567890123456789012345678901234567890123
    assert_eq!{ make_node!(r#"  [[dnr4, lGFd3yL, tJ], ['\\', p, (xGaBwiL,), DE], true, aB8aE]"# as Expr),
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
    //                      0         1           2          3         4         5           6
    //                      012345678901234 5678 9012 3456789012345678901234567890123 456789 0123456
    assert_eq!{ make_node!("[abc, 123u32, \"456\", '\\u0065', false, (), (a), (abc, \"hello\", ), ]" as Expr, [Span::new(1, 3), Span::new(43, 43), Span::new(48, 50)], ["456", "hello"]),
        Expr::Array(ArrayDef::new(Span::new(0, 65), make_exprs![
            SimpleName::new(2, Span::new(1, 3)),
            LitExpr::new(LitValue::from(123u32), Span::new(6, 11)),
            LitExpr::new(5u32, Span::new(14, 18)),
            LitExpr::new(LitValue::from('\u{0065}'), Span::new(21, 28)),
            LitExpr::new(LitValue::from(false), Span::new(31, 35)),
            LitExpr::new(LitValue::Unit, Span::new(38, 39)),
            ParenExpr::new(Span::new(42, 44), 
                SimpleName::new(4, Span::new(43, 43))
            ),
            TupleDef::new(Span::new(47, 62), make_exprs![
                SimpleName::new(1, Span::new(48, 50)),
                LitExpr::new(6u32, Span::new(53, 59)),
            ])
        ]))
    }

    assert_eq!{ make_node!("(                             )" as Expr), 
        Expr::Lit(LitExpr::new(LitValue::Unit, Span::new(0, 30)))
    }
}

#[cfg(test)] #[test]
fn primary_expr_errors() {
    use super::{make_node, make_exprs, make_errors};

    assert_eq!{ make_node!("(,)" as Expr, and messages), (
        Expr::Tuple(TupleDef::new(Span::new(0, 2), make_exprs![])), 
        make_errors!(e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(0, 2), strings::TupleDefHere)),
    )}
}

#[cfg(test)] #[test]
fn postfix_expr_format() {
    use super::make_node;

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
    test_case!(format!("\n{}", make_node!("a.b(c, d, e).f(g, h, i,)(u,).j[k].l().m[n, o, p][r, s, t,]" as Expr).format(Formatter::empty())), r##"
indexer-call <0:57>
  base-is indexer-call <0:47>
    base-is member-access <0:38>
      base-is fn-call <0:36>
        base-is member-access <0:34>
          base-is indexer-call <0:32>
            base-is member-access <0:29>
              base-is fn-call <0:27>
                base-is fn-call <0:23>
                  base-is member-access <0:13>
                    base-is fn-call <0:11>
                      base-is member-access <0:2>
                        base-is ident-use #1 <0:0>
                        "." <1:1>
                        member-name-is #2 <2:2>
                      parenthenes <3:11>
                      ident-use #3 <4:4>
                      ident-use #4 <7:7>
                      ident-use #5 <10:10>
                    "." <12:12>
                    member-name-is #6 <13:13>
                  parenthenes <14:23>
                  ident-use #7 <15:15>
                  ident-use #8 <18:18>
                  ident-use #9 <21:21>
                parenthenes <24:27>
                ident-use #10 <25:25>
              "." <28:28>
              member-name-is #11 <29:29>
            bracket <30:32>
            ident-use #12 <31:31>
          "." <33:33>
          member-name-is #13 <34:34>
        parenthenes <35:36>
        no-argument
      "." <37:37>
      member-name-is #14 <38:38>
    bracket <39:47>
    ident-use #15 <40:40>
    ident-use #16 <43:43>
    ident-use #17 <46:46>
  bracket <48:57>
  ident-use #18 <49:49>
  ident-use #19 <52:52>
  ident-use #20 <55:55>"##
    );
}

#[cfg(test)] #[test]
fn postfix_expr_parse() {
    use super::{make_node, make_exprs, SimpleName};

    //                                      0        1         2         3         4         5     
    // plain                                0123456789012345678901234567890123456789012345678901234567
    assert_eq!{ make_node!("a.b(c, d, e).f(g, h, i,)(u,).j[k].l().m[n, o, p][r, s, t,]" as Expr),
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
    use super::{make_node, make_exprs, make_errors};
    
    assert_eq!{ make_node!("a[]" as PostfixExpr, and messages),
        (Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(1, Span::new(0, 0)), 
            Span::new(1, 2), make_exprs![]
        )), make_errors!(
            e: e.emit(strings::EmptyIndexCall).detail(Span::new(1, 2), strings::IndexCallHere)
        ))
    }
    
    assert_eq!{ make_node!("a[, ]" as PostfixExpr, and messages),
        (Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(1, Span::new(0, 0)), 
            Span::new(1, 4), make_exprs![]
        )), make_errors!(
            e: e.emit(strings::EmptyIndexCall).detail(Span::new(1, 4), strings::IndexCallHere)
        ))
    }
    
    assert_eq!{ make_node!("a(, )" as PostfixExpr, and messages),
        (Expr::FnCall(FnCallExpr::new(
            SimpleName::new(1, Span::new(0, 0)),
            Span::new(1, 4), make_exprs![]
        )), make_errors!(
            e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(1, 4), strings::FnCallHere)
        ))
    }
}