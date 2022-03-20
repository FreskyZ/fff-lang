///! syntax::priority level proxy
///! primary_expr = ident_expr | lit_expr | unit_lit | paren_expr | tuple_def | array_def
///! postfix_expr = expr { ( member_access | fn_call | indexer_call ) }

use super::prelude::*;
use super::{Expr, Name, NameSegment, LitExpr, TupleDef, ArrayDef, FnCallExpr, IndexCallExpr, MemberAccessExpr};

struct PrimaryExpr;

impl Parser for PrimaryExpr {
    type Output = Expr;
    
    fn parse(cx: &mut ParseContext) -> Result<Expr, Unexpected> {

        if cx.matches::<LitExpr>() {
            return cx.expect::<LitExpr>();
        } else if cx.matches::<Name>() {
            return cx.expect::<Name>().map(|n| Expr::Name(n));
        } else if cx.matches::<TupleDef>() {
            return cx.expect::<TupleDef>();
        } else if cx.matches::<ArrayDef>() {
            return cx.expect::<ArrayDef>();
        }

        let (this_id, this_span) = cx.expect_ident_or_keywords(&[Keyword::This, Keyword::Self_])?;  // actually identifier is processed by Name, not here
        Ok(Expr::Name(Name{ type_as_segment: None, global: false, all_span: this_span, segments: vec![NameSegment::Normal(this_id, this_span)] }))
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
                postfix.all_span = current_retval.get_all_span() + postfix.name.all_span;
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

#[cfg(test)]
#[test]
fn primary_expr_parse() {
    use super::{ParenExpr};

    // this is the loop of tokens.nth(current) is left bracket does not cover everything and infinite loop is here
    // update 2017/6/17: this was a bug, but I forget detail
    case!{ "[a]" as Expr,  
        Expr::Array(ArrayDef::new(Span::new(0, 2), make_exprs![
            make_name!(simple 1:1 #2)
        ]))
    }

    //                      0        1         2         3         4
    //                      01234567890123456789012345678901234567890123456     
    case!{ "(463857, IEfN, atau8M, [fNAE, ((cAeJN4)), nHg])" as Expr,
        Expr::Tuple(TupleDef::new(Span::new(0, 46), make_exprs![
            make_lit!(463857, 1, 6),
            make_name!(simple 9:12 #2),
            make_name!(simple 15:20 #3),
            ArrayDef::new(Span::new(23, 45), make_exprs![
                make_name!(simple 24:27 #4),
                ParenExpr::new(Span::new(30, 39), 
                    ParenExpr::new(Span::new(31, 38), 
                        make_name!(simple 32:37 #5)
                    )
                ),
                make_name!(simple 42:44 #6)
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
            make_name!(simple 9:15 #2),
            make_name!(simple 18:19 #3), 
            ArrayDef::new(Span::new(22, 133), make_exprs![
                TupleDef::new(Span::new(23, 105), make_exprs![
                    make_name!(simple 24:26 #4),
                    TupleDef::new(Span::new(29, 90), make_exprs![
                        make_lit!(41, 30, 31),
                        ParenExpr::new(Span::new(34, 68), 
                            ArrayDef::new(Span::new(35, 67), make_exprs![
                                TupleDef::new(Span::new(36, 60), make_exprs![
                                    make_name!(simple 37:38 #5), 
                                    make_name!(simple 41:43 #6),
                                    make_name!(simple 46:53 #7),
                                    make_lit!(true, 56, 59)
                                ]),
                                make_name!(simple 63:63 #8),
                                make_name!(simple 66:66 #9),
                            ])
                        ),
                        TupleDef::new(Span::new(71, 89), make_exprs![
                            make_name!(simple 72:78 #10),
                            make_name!(simple 81:81 #11),
                            ParenExpr::new(Span::new(84, 88), 
                                make_name!(simple 85:87 #12)
                            )
                        ])
                    ]),
                    make_lit!(unit, 93, 94),
                    make_name!(simple 98:104 #13)
                ]),
                make_lit!(400, 108, 110),
                make_lit!(0o535147505, 113, 123),
                make_lit!(0xDB747i32, 126, 132)
            ])
        ]))
    }

    case!{ "CMDoF" as Expr, make_name!(simple 0:4 #2) }
    case!{ "false" as Expr, make_expr!(false, 0, 4) }

    
    //                      0        1         2         3         4         5         6          7          8         9         A
    //                      12345678901234567890123456789012345678901234567890123456789012345678901 234 5678901234567890123456789012
    case!{ "[uy6, 4373577, [(q, AJBN0n, MDEgKh5,), KG, (NsL, ((), D, false, d, ), \"H=\"), true, ((vvB3, true, 5))]]" as Expr, 
        Expr::Array(ArrayDef::new(Span::new(0, 101), make_exprs![
            make_name!(simple 1:3 #2),
            make_lit!(4373577, 6, 12),
            ArrayDef::new(Span::new(15, 100), make_exprs![
                TupleDef::new(Span::new(16, 36), make_exprs![
                    make_name!(simple 17:17 #3),
                    make_name!(simple 20:25 #4), 
                    make_name!(simple 28:34 #5)
                ]), 
                make_name!(simple 39:40 #6),
                TupleDef::new(Span::new(43, 74), make_exprs![
                    make_name!(simple 44:46 #7),
                    TupleDef::new(Span::new(49, 67), make_exprs![
                        make_lit!(unit, 50, 51),
                        make_name!(simple 54:54 #8),
                        make_lit!(false, 57, 61),
                        make_name!(simple 64:64 #9),
                    ]),
                    make_lit!(10: str, 70, 73)
                ]),
                make_lit!(true, 77, 80),
                ParenExpr::new(Span::new(83, 99), 
                    TupleDef::new(Span::new(84, 98), make_exprs![
                        make_name!(simple 85:88 #11),
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
            make_name!(simple 1:2 #2),
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
                    make_name!(simple 9:10 #2),
                    ArrayDef::new(Span::new(13, 21), make_exprs![
                        ParenExpr::new(Span::new(14, 18), 
                            make_name!(simple 15:17 #3)
                        ),
                        make_name!(simple 20:20 #4)
                    ]),
                    ParenExpr::new(Span::new(24, 33), 
                        ParenExpr::new(Span::new(25, 32), 
                            ParenExpr::new(Span::new(26, 31), 
                                make_name!(simple 27:30 #5)
                            )
                        )
                    ),
                    TupleDef::new(Span::new(36, 48), make_exprs![
                        make_name!(simple 37:40 #6), 
                        make_name!(simple 43:43 #7),
                        make_lit!(unit, 46, 47)
                    ]),
                ]),
                TupleDef::new(Span::new(52, 67), make_exprs![
                    make_name!(simple 53:65 #8)
                ])
            ]),
            make_name!(simple 71:75 #9)
        ]))
    }

    //                                      0          1         2      
    //                                      12 345 67890123456789012
    case!{ "[\"il\", 0o52u32, sO04n]" as Expr,
        Expr::Array(ArrayDef::new(Span::new(0, 21), make_exprs![
            make_lit!(2: str, 1, 4),
            make_lit!(0o52: u32, 7, 13), 
            make_name!(simple 16:20 #3)
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
            make_name!(simple 35:42 #3)
        ]))
    }

    //                                        0        1         2         3         4         5         6     
    //                                        123456789012345678901234567890123456789012345678901234567890123
    case!{ r#"  [[dnr4, lGFd3yL, tJ], ['\\', p, (xGaBwiL,), DE], true, aB8aE]"# as Expr,
        Expr::Array(ArrayDef::new(Span::new(2, 62), make_exprs![
            ArrayDef::new(Span::new(3, 21), make_exprs![
                make_name!(simple 4:7 #2),
                make_name!(simple 10:16 #3),
                make_name!(simple 19:20 #4)
            ]),
            ArrayDef::new(Span::new(24, 48), make_exprs![
                make_lit!('\\': char, 25, 28),
                make_name!(simple 31:31 #5),
                TupleDef::new(Span::new(34, 43), make_exprs![
                    make_name!(simple 35:41 #6)
                ]),
                make_name!(simple 46:47 #7)
            ]),
            make_lit!(true, 51, 54),
            make_name!(simple 57:61 #8)
        ]))
    } 

    // Previous manual tests
    //                      0         1           2          3         4         5           6
    //                      012345678901234 5678 9012 3456789012345678901234567890123 456789 0123456
    case!{ "[abc, 123u32, \"456\", '\\u0065', false, (), (a), (abc, \"hello\", ), ]" as Expr,
        Expr::Array(ArrayDef::new(Span::new(0, 65), make_exprs![
            make_name!(simple 1:3 #2),
            make_lit!(123: u32, 6, 11),
            make_lit!(3: str, 14, 18),
            make_lit!('\u{0065}': char, 21, 28),
            make_lit!(false, 31, 35),
            make_lit!(unit, 38, 39),
            ParenExpr::new(Span::new(42, 44), 
                make_name!(simple 43:43 #4)
            ),
            TupleDef::new(Span::new(47, 62), make_exprs![
                make_name!(simple 48:50 #2),
                make_lit!(5: str, 53, 59),
            ])
        ]))
    }

    case!{ "(                             )" as Expr, 
        Expr::Lit(make_lit!(unit, 0, 30))
    }

    case!{ "(,)" as Expr,
        Expr::Tuple(TupleDef::new(Span::new(0, 2), make_exprs![])), 
        errors make_errors!(e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(0, 2), strings::TupleDefHere)),
    }
}

#[cfg(test)]
#[test]
fn postfix_expr_parse() {

    //      0        1         2         3         4         5     
    //      0123456789012345678901234567890123456789012345678901234567
    case!{ "a.b(c, d, e).f(g, h, i,)(u,).j[k].l().m[n, o, p][r, s, t,]" as Expr,
        Expr::IndexCall(IndexCallExpr::new(
            IndexCallExpr::new(
                make_expr!(member 0:38 dot 37:37
                    FnCallExpr::new(
                        make_expr!(member 0:34 dot 33:33
                            IndexCallExpr::new(
                                make_expr!(member 0:29 dot 28:28
                                    FnCallExpr::new(
                                        FnCallExpr::new(
                                            make_expr!(member 0:13 dot 12:12
                                                FnCallExpr::new(
                                                    make_expr!(member 0:2 dot 1:1
                                                        make_name!(simple 0:0 #2),
                                                        make_name!(simple bare 2:2 #3)), 
                                                    Span::new(3, 11), make_exprs![
                                                        make_name!(simple 4:4 #4),
                                                        make_name!(simple 7:7 #5),
                                                        make_name!(simple 10:10 #6),
                                                    ]
                                                ),
                                                make_name!(simple bare 13:13 #7)),
                                            Span::new(14, 23), make_exprs![
                                                make_name!(simple 15:15 #8),
                                                make_name!(simple 18:18 #9),
                                                make_name!(simple 21:21 #10),
                                            ]
                                        ),
                                        Span::new(24, 27), make_exprs![
                                            make_name!(simple 25:25 #11)
                                        ]
                                    ),
                                    make_name!(simple bare 29:29 #12)),
                                Span::new(30, 32), make_exprs![
                                    make_name!(simple 31:31 #13)
                                ]
                            ),
                            make_name!(simple bare 34:34 #14)),
                        Span::new(35, 36),
                        make_exprs![]
                    ),
                    make_name!(simple bare 38:38 #15)),
                Span::new(39, 47), make_exprs![
                    make_name!(simple 40:40 #16),
                    make_name!(simple 43:43 #17),
                    make_name!(simple 46:46 #18)
                ]
            ),
            Span::new(48, 57), make_exprs![
                make_name!(simple 49:49 #19),
                make_name!(simple 52:52 #20),
                make_name!(simple 55:55 #21)
            ]
        ))
    }

    //      012345678901234567890
    case!{ "i.collect::<Vec<i32>>" as Expr,
        make_expr!(member 0:20 dot 1:1
            make_name!(simple 0:0 #2),
            make_name!(bare 2:20 false, None,
                make_name!(segment 2:8 #3),
                make_name!(segment generic 11:20
                    make_type!(plain 12:19 false, None,
                        make_type!(segment generic 12:19 4 12:14 quote 15:19
                            make_type!(prim 16:18 I32)))))),
    }

    //      01234567
    case!{ "(0, 0).0" as Expr,
        make_expr!(member 0:7 dot 6:6
            make_expr!(tuple 0:5
                make_expr!(0: i32, 1, 1),
                make_expr!(0: i32, 4, 4)),
            make_name!(bare 7:7 false, None,
                make_name!(segment 7:7 #2))),
    }

    case!{ "a[]" as PostfixExpr,
        Expr::IndexCall(IndexCallExpr::new(
            make_name!(simple 0:0 #2), 
            Span::new(1, 2), make_exprs![]
        )), errors make_errors!(
            e: e.emit(strings::EmptyIndexCall).detail(Span::new(1, 2), strings::IndexCallHere)
        )
    }
    
    case!{ "a[, ]" as PostfixExpr,
        Expr::IndexCall(IndexCallExpr::new(
            make_name!(simple 0:0 #2), 
            Span::new(1, 4), make_exprs![]
        )), errors make_errors!(
            e: e.emit(strings::EmptyIndexCall).detail(Span::new(1, 4), strings::IndexCallHere)
        )
    }
    
    case!{ "a(, )" as PostfixExpr,
        Expr::FnCall(FnCallExpr::new(
            make_name!(simple 0:0 #2),
            Span::new(1, 4), make_exprs![]
        )), errors make_errors!(
            e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(1, 4), strings::FnCallHere)
        )
    }

    //      01234
    case!{ "a.0i8" as Expr,
        make_expr!(member 0:4 dot 1:1
            make_name!(simple 0:0 #2),
            make_name!(bare 2:4 false, None,)
        ), errors make_errors!(
            e: e.emit(strings::InvalidTupleIndex).span(Span::new(2, 4)).help(strings::TupleIndexSyntaxHelp)
        )
    }

    //      01234567890123456789012
    case!{ "a.0xFFFF_FFFF_FFFF_FFFF" as Expr,
        make_expr!(member 0:22 dot 1:1
            make_name!(simple 0:0 #2),
            make_name!(bare 2:22 false, None,)
        ), errors make_errors!(
            e: e.emit(strings::InvalidTupleIndex).span(Span::new(2, 22)).help(strings::TupleIndexSyntaxHelp)
        )
    }

    //      0123456789
    case!{ "a.abc::def" as Expr,
        make_expr!(member 0:9 dot 1:1
            make_name!(simple 0:0 #2),
            make_name!(bare 2:9 false, None,
                make_name!(segment 2:4 #3),
                make_name!(segment 7:9 #4))
        ), errors make_errors!(
            e: e.emit(strings::InvalidMemberAccess).span(Span::new(7, 9)).help(strings::GenericMemberAccessSyntaxHelp)
        )
    }

    //      0123456789012345678
    case!{ "a.abc::<def>::<ghi>" as Expr,
        make_expr!(member 0:18 dot 1:1
            make_name!(simple 0:0 #2),
            make_name!(bare 2:18 false, None,
                make_name!(segment 2:4 #3),
                make_name!(segment generic 7:11
                    make_type!(simple 8:10 4)),
                make_name!(segment generic 14:18
                    make_type!(simple 15:17 5)))
        ), errors make_errors!(e: {
            e.emit(strings::InvalidNameSegment).detail(Span::new(14, 14), strings::NameSegmentExpect);
            e.emit(strings::InvalidMemberAccess).span(Span::new(7, 18)).help(strings::GenericMemberAccessSyntaxHelp);
        })
    }
}
