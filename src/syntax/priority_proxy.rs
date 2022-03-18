///! fff-lang
///!
///! syntax/priority level proxy
///! they are here because they are dispatcher not containing data
///! primary_expr = ident_expr | lit_expr | unit_lit | paren_expr | tuple_def | array_def
///! postfix_expr = expr { ( member_access | fn_call | indexer_call ) }

use super::prelude::*;
use super::{Expr, Name, LitExpr, SimpleName, TupleDef, ArrayDef, FnCallExpr, IndexCallExpr, MemberAccessExpr};

struct PrimaryExpr;
impl Parser for PrimaryExpr {
    type Output = Expr;
    
    fn parse(cx: &mut ParseContext) -> Result<Expr, Unexpected> {

        if cx.matches::<LitExpr>() {
            return cx.expect::<LitExpr>();
        } else if cx.matches::<Name>() {
            return cx.expect::<Name>();
        } else if cx.matches::<TupleDef>() {
            return cx.expect::<TupleDef>();
        } else if cx.matches::<ArrayDef>() {
            return cx.expect::<ArrayDef>();
        }

        let (this_id, this_span) = cx.expect_ident_or(&[Keyword::This])?;  // actually identifier is processed by Name, not here
        return Ok(Expr::SimpleName(SimpleName::new(this_id, this_span)));
    }
}

pub struct PostfixExpr;
impl Parser for PostfixExpr {
    type Output = Expr;

    fn parse(cx: &mut ParseContext) -> Result<Expr, Unexpected> {   
        #[cfg(feature = "trace_postfix_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ perror!("    [PostfixExpr:{}] ", line!()); perrorln!($($arg)*); }) }
        #[cfg(not(feature = "trace_postfix_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        let mut current_retval = cx.expect::<PrimaryExpr>()?;
        trace!("parsed primary, current is {:?}", current_retval);

        loop {
            if cx.matches::<MemberAccessExpr>() {
                let mut postfix = cx.expect::<MemberAccessExpr>()?;
                postfix.all_span = current_retval.get_all_span() + postfix.name.span;
                postfix.base = Box::new(current_retval);
                current_retval = Expr::MemberAccess(postfix);
            } else if cx.matches::<FnCallExpr>() {
                let mut postfix = cx.expect::<FnCallExpr>()?;
                postfix.all_span = current_retval.get_all_span() + postfix.paren_span;
                postfix.base = Box::new(current_retval);
                current_retval = Expr::FnCall(postfix);
            } else if cx.matches::<IndexCallExpr>() {
                let mut postfix = cx.expect::<IndexCallExpr>()?;
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
    use super::{ParenExpr};

    // this is the loop of tokens.nth(current) is left bracket does not cover everything and infinite loop is here
    // update 2017/6/17: this was a bug, but I forget detail
    case!{ "[a]" as Expr,  
        Expr::Array(ArrayDef::new(Span::new(0, 2), make_exprs![
            SimpleName::new(2, Span::new(1, 1))
        ]))
    }

    //                      0        1         2         3         4
    //                      01234567890123456789012345678901234567890123456     
    case!{ "(463857, IEfN, atau8M, [fNAE, ((cAeJN4)), nHg])" as Expr,
        Expr::Tuple(TupleDef::new(Span::new(0, 46), make_exprs![
            make_lit!(463857, 1, 6),
            SimpleName::new(2, Span::new(9, 12)),
            SimpleName::new(3, Span::new(15, 20)),
            ArrayDef::new(Span::new(23, 45), make_exprs![
                SimpleName::new(4, Span::new(24, 27)),
                ParenExpr::new(Span::new(30, 39), 
                    ParenExpr::new(Span::new(31, 38), 
                        SimpleName::new(5, Span::new(32, 37))
                    )
                ),
                SimpleName::new(6, Span::new(42, 44))
            ])
        ]))
    }

    case!{ "10363" as Expr,
        Expr::Lit(make_lit!(10363, 0, 4))
    }

    case!{
    //   0         1         2         3         4         5         6         7         8         9         0         1         2         3       
    //   01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567 8901234567890123456789012345678901234
        "[(0x7E), FFGqfJe, I4, [(m7A, (41, ([(jL, rAn, K0FgLc7h, true), C, w]), (J3cEFDG, d, (j8h))), (), \neIuArjF), 400, 0o535147505, 0xDB747]]" as Expr,
        Expr::Array(ArrayDef::new(Span::new(0, 134), make_exprs![
            ParenExpr::new(Span::new(1, 6), 
               make_lit!(0x7E, 2, 5),
            ),
            SimpleName::new(2, Span::new(9, 15)),
            SimpleName::new(3, Span::new(18, 19)), 
            ArrayDef::new(Span::new(22, 133), make_exprs![
                TupleDef::new(Span::new(23, 105), make_exprs![
                    SimpleName::new(4, Span::new(24, 26)),
                    TupleDef::new(Span::new(29, 90), make_exprs![
                        make_lit!(41, 30, 31),
                        ParenExpr::new(Span::new(34, 68), 
                            ArrayDef::new(Span::new(35, 67), make_exprs![
                                TupleDef::new(Span::new(36, 60), make_exprs![
                                    SimpleName::new(5, Span::new(37, 38)), 
                                    SimpleName::new(6, Span::new(41, 43)),
                                    SimpleName::new(7, Span::new(46, 53)),
                                    make_lit!(true, 56, 59)
                                ]),
                                SimpleName::new(8, Span::new(63, 63)),
                                SimpleName::new(9, Span::new(66, 66)),
                            ])
                        ),
                        TupleDef::new(Span::new(71, 89), make_exprs![
                            SimpleName::new(10, Span::new(72, 78)),
                            SimpleName::new(11, Span::new(81, 81)),
                            ParenExpr::new(Span::new(84, 88), 
                                SimpleName::new(12, Span::new(85, 87))
                            )
                        ])
                    ]),
                    make_lit!(unit, 93, 94),
                    SimpleName::new(13, Span::new(98, 104))
                ]),
                make_lit!(400, 108, 110),
                make_lit!(0o535147505, 113, 123),
                make_lit!(0xDB747i32, 126, 132)
            ])
        ]))
    }

    case!{ "CMDoF" as Expr, Expr::SimpleName(SimpleName::new(2, Span::new(0, 4))) }
    case!{ "false" as Expr, Expr::Lit(make_lit!(false, 0, 4)) }

    
    //                      0        1         2         3         4         5         6          7          8         9         A
    //                      12345678901234567890123456789012345678901234567890123456789012345678901 234 5678901234567890123456789012
    case!{ "[uy6, 4373577, [(q, AJBN0n, MDEgKh5,), KG, (NsL, ((), D, false, d, ), \"H=\"), true, ((vvB3, true, 5))]]" as Expr, 
        Expr::Array(ArrayDef::new(Span::new(0, 101), make_exprs![
            SimpleName::new(2, Span::new(1, 3)),
            make_lit!(4373577, 6, 12),
            ArrayDef::new(Span::new(15, 100), make_exprs![
                TupleDef::new(Span::new(16, 36), make_exprs![
                    SimpleName::new(3, Span::new(17, 17)),
                    SimpleName::new(4, Span::new(20, 25)), 
                    SimpleName::new(5, Span::new(28, 34))
                ]), 
                SimpleName::new(6, Span::new(39, 40)),
                TupleDef::new(Span::new(43, 74), make_exprs![
                    SimpleName::new(7, Span::new(44, 46)),
                    TupleDef::new(Span::new(49, 67), make_exprs![
                        make_lit!(unit, 50, 51),
                        SimpleName::new(8, Span::new(54, 54)),
                        make_lit!(false, 57, 61),
                        SimpleName::new(9, Span::new(64, 64)),
                    ]),
                    make_lit!(10: str, 70, 73)
                ]),
                make_lit!(true, 77, 80),
                ParenExpr::new(Span::new(83, 99), 
                    TupleDef::new(Span::new(84, 98), make_exprs![
                        SimpleName::new(11, Span::new(85, 88)),
                        make_lit!(true, 91, 94),
                        make_lit!(5, 97, 97)
                    ])
                )
            ])
        ]))
    }

    case!{ "(() )" as Expr, 
        Expr::Paren(ParenExpr::new(Span::new(0, 4), make_lit!(unit, 1, 2)))
    }
    case!{ "((),)" as Expr, 
        Expr::Tuple(TupleDef::new(Span::new(0, 4), make_exprs![make_lit!(unit, 1, 2)]))
    }

    case!{ "(\"o5\")" as Expr, 
        Expr::Paren(ParenExpr::new(Span::new(0, 5), 
            make_lit!(2: str, 1, 4),
        ))
    }

    //                      0        1         2        
    //                      1234567890123456789012345678
    case!{ "(nn, ([false,true]), 183455)" as Expr,
        Expr::Tuple(TupleDef::new(Span::new(0, 27), make_exprs![
            SimpleName::new(2, Span::new(1, 2)),
            ParenExpr::new(Span::new(5, 18), 
                ArrayDef::new(Span::new(6, 17), make_exprs![
                    make_lit!(false, 7, 11),
                    make_lit!(true, 13, 16)
                ])
            ),
            make_lit!(183455, 21, 26)
        ]))
    }
    
    //                      0        1         2         3         4         5         6         7       
    //                      123456789012345678901234567890123456789012345678901234567890123456789012345678
    case!{ "((true, (mO, [(q5k),a], (((KttG))), (K5DJ, r, ())), (McsaEdfdfalse,)), rIOKt,)" as Expr,
        Expr::Tuple(TupleDef::new(Span::new(0, 77), make_exprs![
            TupleDef::new(Span::new(1, 68), make_exprs![
                make_lit!(true, 2, 5),
                TupleDef::new(Span::new(8, 49), make_exprs![
                    SimpleName::new(2, Span::new(9, 10)),
                    ArrayDef::new(Span::new(13, 21), make_exprs![
                        ParenExpr::new(Span::new(14, 18), 
                            SimpleName::new(3, Span::new(15, 17))
                        ),
                        SimpleName::new(4, Span::new(20, 20))
                    ]),
                    ParenExpr::new(Span::new(24, 33), 
                        ParenExpr::new(Span::new(25, 32), 
                            ParenExpr::new(Span::new(26, 31), 
                                SimpleName::new(5, Span::new(27, 30))
                            )
                        )
                    ),
                    TupleDef::new(Span::new(36, 48), make_exprs![
                        SimpleName::new(6, Span::new(37, 40)), 
                        SimpleName::new(7, Span::new(43, 43)),
                        make_lit!(unit, 46, 47)
                    ]),
                ]),
                TupleDef::new(Span::new(52, 67), make_exprs![
                    SimpleName::new(8, Span::new(53, 65))
                ])
            ]),
            SimpleName::new(9, Span::new(71, 75))
        ]))
    }

    //                                      0          1         2      
    //                                      12 345 67890123456789012
    case!{ "[\"il\", 0o52u32, sO04n]" as Expr,
        Expr::Array(ArrayDef::new(Span::new(0, 21), make_exprs![
            make_lit!(2: str, 1, 4),
            make_lit!(0o52: u32, 7, 13), 
            SimpleName::new(3, Span::new(16, 20))
        ]))
    }
    //                                      12345678
    case!{ "['f',()]" as Expr, 
        Expr::Array(ArrayDef::new(Span::new(0, 7), make_exprs![
            make_lit!('f': char, 1, 3),
            make_lit!(unit, 5, 6)
        ]))
    }
    case!{ "[]" as Expr, Expr::Array(ArrayDef::new(Span::new(0, 1), make_exprs![])) }

    //                                      0        1           2         3         4      
    //                                      12345 678901 234567890123456789012345678901234
    case!{ "[8, \"@=?GF\", 87r32, 1340323.74r64, FKOxAvx5]" as Expr,
        Expr::Array(ArrayDef::new(Span::new(0, 43), make_exprs![
            make_lit!(8, 1, 1),
            make_lit!(2: str, 4, 10),
            make_lit!(87f32: r32, 13, 17),
            make_lit!(1340323.74: r64, 20, 32),
            SimpleName::new(3, Span::new(35, 42))
        ]))
    }

    //                                        0        1         2         3         4         5         6     
    //                                        123456789012345678901234567890123456789012345678901234567890123
    case!{ r#"  [[dnr4, lGFd3yL, tJ], ['\\', p, (xGaBwiL,), DE], true, aB8aE]"# as Expr,
        Expr::Array(ArrayDef::new(Span::new(2, 62), make_exprs![
            ArrayDef::new(Span::new(3, 21), make_exprs![
                SimpleName::new(2, Span::new(4, 7)),
                SimpleName::new(3, Span::new(10, 16)),
                SimpleName::new(4, Span::new(19, 20))
            ]),
            ArrayDef::new(Span::new(24, 48), make_exprs![
                make_lit!('\\': char, 25, 28),
                SimpleName::new(5, Span::new(31, 31)),
                TupleDef::new(Span::new(34, 43), make_exprs![
                    SimpleName::new(6, Span::new(35, 41))
                ]),
                SimpleName::new(7, Span::new(46, 47))
            ]),
            make_lit!(true, 51, 54),
            SimpleName::new(8, Span::new(57, 61))
        ]))
    } 

    // Previous manual tests
    //                      0         1           2          3         4         5           6
    //                      012345678901234 5678 9012 3456789012345678901234567890123 456789 0123456
    case!{ "[abc, 123u32, \"456\", '\\u0065', false, (), (a), (abc, \"hello\", ), ]" as Expr,
        Expr::Array(ArrayDef::new(Span::new(0, 65), make_exprs![
            SimpleName::new(2, Span::new(1, 3)),
            make_lit!(123: u32, 6, 11),
            make_lit!(3: str, 14, 18),
            make_lit!('\u{0065}': char, 21, 28),
            make_lit!(false, 31, 35),
            make_lit!(unit, 38, 39),
            ParenExpr::new(Span::new(42, 44), 
                SimpleName::new(4, Span::new(43, 43))
            ),
            TupleDef::new(Span::new(47, 62), make_exprs![
                SimpleName::new(2, Span::new(48, 50)),
                make_lit!(5: str, 53, 59),
            ])
        ]))
    }

    case!{ "(                             )" as Expr, 
        Expr::Lit(make_lit!(unit, 0, 30))
    }
}

#[cfg(test)] #[test]
fn primary_expr_errors() {

    case!{ "(,)" as Expr,
        Expr::Tuple(TupleDef::new(Span::new(0, 2), make_exprs![])), 
        errors make_errors!(e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(0, 2), strings::TupleDefHere)),
    }
}

#[cfg(test)] #[test]
fn postfix_expr_parse() {
    use super::{SimpleName};

    //                                      0        1         2         3         4         5     
    // plain                                0123456789012345678901234567890123456789012345678901234567
    case!{ "a.b(c, d, e).f(g, h, i,)(u,).j[k].l().m[n, o, p][r, s, t,]" as Expr,
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
                                                        SimpleName::new(2, Span::new(0, 0)),
                                                        Span::new(1, 1),
                                                        SimpleName::new(3, Span::new(2, 2))
                                                    ), 
                                                    Span::new(3, 11), make_exprs![
                                                        SimpleName::new(4, Span::new(4, 4)),
                                                        SimpleName::new(5, Span::new(7, 7)),
                                                        SimpleName::new(6, Span::new(10, 10)),
                                                    ]
                                                ),
                                                Span::new(12, 12),
                                                SimpleName::new(7, Span::new(13, 13))
                                            ),
                                            Span::new(14, 23), make_exprs![
                                                SimpleName::new(8, Span::new(15, 15)),
                                                SimpleName::new(9, Span::new(18, 18)),
                                                SimpleName::new(10, Span::new(21, 21)),
                                            ]
                                        ),
                                        Span::new(24, 27), make_exprs![
                                            SimpleName::new(11, Span::new(25, 25))
                                        ]
                                    ),
                                    Span::new(28, 28),
                                    SimpleName::new(12, Span::new(29, 29))
                                ),
                                Span::new(30, 32), make_exprs![
                                    SimpleName::new(13, Span::new(31, 31))
                                ]
                            ),
                            Span::new(33, 33),
                            SimpleName::new(14, Span::new(34, 34))
                        ),
                        Span::new(35, 36),
                        make_exprs![]
                    ),
                    Span::new(37, 37),
                    SimpleName::new(15, Span::new(38, 38))
                ),
                Span::new(39, 47), make_exprs![
                    SimpleName::new(16, Span::new(40, 40)),
                    SimpleName::new(17, Span::new(43, 43)),
                    SimpleName::new(18, Span::new(46, 46))
                ]
            ),
            Span::new(48, 57), make_exprs![
                SimpleName::new(19, Span::new(49, 49)),
                SimpleName::new(20, Span::new(52, 52)),
                SimpleName::new(21, Span::new(55, 55))
            ]
        ))
    }
}

#[cfg(test)] #[test]
fn postfix_expr_errors() {
    
    case!{ "a[]" as PostfixExpr,
        Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(2, Span::new(0, 0)), 
            Span::new(1, 2), make_exprs![]
        )), errors make_errors!(
            e: e.emit(strings::EmptyIndexCall).detail(Span::new(1, 2), strings::IndexCallHere)
        )
    }
    
    case!{ "a[, ]" as PostfixExpr,
        Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(2, Span::new(0, 0)), 
            Span::new(1, 4), make_exprs![]
        )), errors make_errors!(
            e: e.emit(strings::EmptyIndexCall).detail(Span::new(1, 4), strings::IndexCallHere)
        )
    }
    
    case!{ "a(, )" as PostfixExpr,
        Expr::FnCall(FnCallExpr::new(
            SimpleName::new(2, Span::new(0, 0)),
            Span::new(1, 4), make_exprs![]
        )), errors make_errors!(
            e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(1, 4), strings::FnCallHere)
        )
    }
}
