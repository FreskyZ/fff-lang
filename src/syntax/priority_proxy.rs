///! syntax::priority level proxy

#[cfg(test)]
#[test]
fn primary_expr_parse() {use super::prelude::*;

    // this is the loop of tokens.nth(current) is left bracket does not cover everything and infinite loop is here
    // update 2017/6/17: this was a bug, but I forget detail
    case!{ "[a]" as Expr,
        make_expr!(array 0:2
            make_name!(simple 1:1 #2))
    }

    //                      0        1         2         3         4
    //                      01234567890123456789012345678901234567890123456     
    case!{ "(463857, IEfN, atau8M, [fNAE, ((cAeJN4)), nHg])" as Expr,
        make_expr!(tuple 0:46
            make_expr!(i32 463857 1:6),
            make_name!(simple 9:12 #2),
            make_name!(simple 15:20 #3),
            make_expr!(array 23:45
                make_name!(simple 24:27 #4),
                make_expr!(paren 30:39
                    make_expr!(paren 31:38
                        make_name!(simple 32:37 #5))),
                make_name!(simple 42:44 #6)))
    }

    case!{ "10363" as Expr,
        make_expr!(i32 10363 0:4)
    }

    case!{
    //   0         1         2         3         4         5         6         7         8         9         0         1         2         3       
    //   01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567 8901234567890123456789012345678901234
        "[(0x7E), FFGqfJe, I4, [(m7A, (41, ([(jL, rAn, K0FgLc7h, true), C, w]), (J3cEFDG, d, (j8h))), (), \neIuArjF), 400, 0o535147505, 0xDB747]]" as Expr,
        make_expr!(array 0:134
            make_expr!(paren 1:6
               make_expr!(i32 0x7E 2:5)),
            make_name!(simple 9:15 #2),
            make_name!(simple 18:19 #3),
            make_expr!(array 22:133
                make_expr!(tuple 23:105
                    make_name!(simple 24:26 #4),
                    make_expr!(tuple 29:90
                        make_expr!(i32 41 30:31),
                        make_expr!(paren 34:68
                            make_expr!(array 35:67
                                make_expr!(tuple 36:60
                                    make_name!(simple 37:38 #5), 
                                    make_name!(simple 41:43 #6),
                                    make_name!(simple 46:53 #7),
                                    make_expr!(true 56:59)),
                                make_name!(simple 63:63 #8),
                                make_name!(simple 66:66 #9))),
                        make_expr!(tuple 71:89
                            make_name!(simple 72:78 #10),
                            make_name!(simple 81:81 #11),
                            make_expr!(paren 84:88
                                make_name!(simple 85:87 #12)))),
                    make_expr!(unit 93:94),
                    make_name!(simple 98:104 #13)),
                make_expr!(i32 400 108:110),
                make_expr!(i32 0o535147505 113:123),
                make_expr!(i32 0xDB747 126:132))),
    }

    case!{ "CMDoF" as Expr, make_name!(simple 0:4 #2) }
    case!{ "false" as Expr, make_expr!(false 0:4) }

    
    //                      0        1         2         3         4         5         6          7          8         9         A
    //                      12345678901234567890123456789012345678901234567890123456789012345678901 234 5678901234567890123456789012
    case!{ "[uy6, 4373577, [(q, AJBN0n, MDEgKh5,), KG, (NsL, ((), D, false, d, ), \"H=\"), true, ((vvB3, true, 5))]]" as Expr, 
        make_expr!(array 0:101
            make_name!(simple 1:3 #2),
            make_expr!(i32 4373577 6:12),
            make_expr!(array 15:100
                make_expr!(tuple 16:36
                    make_name!(simple 17:17 #3),
                    make_name!(simple 20:25 #4), 
                    make_name!(simple 28:34 #5)), 
                make_name!(simple 39:40 #6),
                make_expr!(tuple 43:74
                    make_name!(simple 44:46 #7),
                    make_expr!(tuple 49:67
                        make_expr!(unit 50:51),
                        make_name!(simple 54:54 #8),
                        make_expr!(false 57:61),
                        make_name!(simple 64:64 #9)),
                    make_expr!(str #10 70:73)),
                make_expr!(true 77:80),
                make_expr!(paren 83:99
                    make_expr!(tuple 84:98
                        make_name!(simple 85:88 #11),
                        make_expr!(true 91:94),
                        make_expr!(i32 5 97:97)))))
    }

    case!{ "(() )" as Expr,
        make_expr!(paren 0:4
            make_expr!(unit 1:2))
    }
    case!{ "((),)" as Expr, 
        make_expr!(tuple 0:4
            make_expr!(unit 1:2))
    }

    case!{ "(\"o5\")" as Expr,
        make_expr!(paren 0:5
            make_expr!(str #2 1:4))
    }

    //                      0        1         2        
    //                      1234567890123456789012345678
    case!{ "(nn, ([false,true]), 183455)" as Expr,
        make_expr!(tuple 0:27
            make_name!(simple 1:2 #2),
            make_expr!(paren 5:18
                make_expr!(array 6:17
                    make_expr!(false 7:11),
                    make_expr!(true 13:16))),
            make_expr!(i32 183455 21:26))
    }
    
    //                      0        1         2         3         4         5         6         7       
    //                      123456789012345678901234567890123456789012345678901234567890123456789012345678
    case!{ "((true, (mO, [(q5k),a], (((KttG))), (K5DJ, r, ())), (McsaEdfdfalse,)), rIOKt,)" as Expr,
        make_expr!(tuple 0:77
            make_expr!(tuple 1:68
                make_expr!(true 2:5),
                make_expr!(tuple 8:49
                    make_name!(simple 9:10 #2),
                    make_expr!(array 13:21
                        make_expr!(paren 14:18
                            make_name!(simple 15:17 #3)),
                        make_name!(simple 20:20 #4)),
                    make_expr!(paren 24:33
                        make_expr!(paren 25:32
                            make_expr!(paren 26:31
                                make_name!(simple 27:30 #5)))),
                    make_expr!(tuple 36:48
                        make_name!(simple 37:40 #6), 
                        make_name!(simple 43:43 #7),
                        make_expr!(unit 46:47))),
                make_expr!(tuple 52:67
                    make_name!(simple 53:65 #8))),
            make_name!(simple 71:75 #9))
    }

    //                                      0          1         2      
    //                                      12 345 67890123456789012
    case!{ "[\"il\", 0o52u32, sO04n]" as Expr,
        make_expr!(array 0:21
            make_expr!(str #2 1:4),
            make_expr!(u32 0o52 7:13), 
            make_name!(simple 16:20 #3))
    }
    //                                      12345678
    case!{ "['f',()]" as Expr, 
        make_expr!(array 0:7
            make_expr!(char 1:3 'f'),
            make_expr!(unit 5:6))
    }
    case!{ "[]" as Expr, make_expr!(array 0:1) }

    //                                      0        1           2         3         4      
    //                                      12345 678901 234567890123456789012345678901234
    case!{ "[8, \"@=?GF\", 87r32, 1340323.74r64, FKOxAvx5]" as Expr,
        make_expr!(array 0:43
            make_expr!(i32 8 1:1),
            make_expr!(str #2 4:10),
            make_expr!(r32 87f32 13:17),
            make_expr!(r64 1340323.74 20:32),
            make_name!(simple 35:42 #3)),
    }

    //                                        0        1         2         3         4         5         6     
    //                                        123456789012345678901234567890123456789012345678901234567890123
    case!{ r#"  [[dnr4, lGFd3yL, tJ], ['\\', p, (xGaBwiL,), DE], true, aB8aE]"# as Expr,
        make_expr!(array 2:62
            make_expr!(array 3:21
                make_name!(simple 4:7 #2),
                make_name!(simple 10:16 #3),
                make_name!(simple 19:20 #4)),
            make_expr!(array 24:48
                make_expr!(char 25:28 '\\'),
                make_name!(simple 31:31 #5),
                make_expr!(tuple 34:43
                    make_name!(simple 35:41 #6)),
                make_name!(simple 46:47 #7)),
            make_expr!(true 51:54),
            make_name!(simple 57:61 #8)),
    } 

    // Previous manual tests
    //                      0         1           2          3         4         5           6
    //                      012345678901234 5678 9012 3456789012345678901234567890123 456789 0123456
    case!{ "[abc, 123u32, \"456\", '\\u0065', false, (), (a), (abc, \"hello\", ), ]" as Expr,
        make_expr!(array 0:65
            make_name!(simple 1:3 #2),
            make_expr!(u32 123 6:11),
            make_expr!(str #3 14:18),
            make_expr!(char 21:28 '\u{0065}'),
            make_expr!(false 31:35),
            make_expr!(unit 38:39),
            make_expr!(paren 42:44
                make_name!(simple 43:43 #4)),
            make_expr!(tuple 47:62
                make_name!(simple 48:50 #2),
                make_expr!(str #5 53:59)))
    }

    case!{ "(                             )" as Expr, 
        make_expr!(unit 0:30)
    }

    case!{ "(,)" as Expr,
        make_expr!(tuple 0:2),
        errors make_errors!(e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(0, 2), strings::TupleDefHere)),
    }
}

#[cfg(test)]
#[test]
fn postfix_expr_parse() {use super::prelude::*;use super::PostfixExpr;

    //      0        1         2         3         4         5     
    //      0123456789012345678901234567890123456789012345678901234567
    case!{ "a.b(c, d, e).f(g, h, i,)(u,).j[k].l().m[n, o, p][r, s, t,]" as Expr,
        make_expr!(index 0:57 bracket 48:57
            make_expr!(index 0:47 bracket 39:47
                make_expr!(member 0:38 dot 37:37
                    make_expr!(fn 0:36 paren 35:36
                        make_expr!(member 0:34 dot 33:33
                            make_expr!(index 0:32 bracket 30:32
                                make_expr!(member 0:29 dot 28:28
                                    make_expr!(fn 0:27 paren 24:27
                                        make_expr!(fn 0:23 paren 14:23
                                            make_expr!(member 0:13 dot 12:12
                                                make_expr!(fn 0:11 paren 3:11
                                                    make_expr!(member 0:2 dot 1:1
                                                        make_name!(simple 0:0 #2),
                                                        make_name!(simple bare 2:2 #3)), 
                                                    make_name!(simple 4:4 #4),
                                                    make_name!(simple 7:7 #5),
                                                    make_name!(simple 10:10 #6)),
                                                make_name!(simple bare 13:13 #7)),
                                            make_name!(simple 15:15 #8),
                                            make_name!(simple 18:18 #9),
                                            make_name!(simple 21:21 #10)),
                                        make_name!(simple 25:25 #11)),
                                    make_name!(simple bare 29:29 #12)),
                                make_name!(simple 31:31 #13)),
                            make_name!(simple bare 34:34 #14)),),
                    make_name!(simple bare 38:38 #15)),
                make_name!(simple 40:40 #16),
                make_name!(simple 43:43 #17),
                make_name!(simple 46:46 #18)),
            make_name!(simple 49:49 #19),
            make_name!(simple 52:52 #20),
            make_name!(simple 55:55 #21))
    }

    //      012345678901234567890
    case!{ "i.collect::<Vec<i32>>" as Expr,
        make_expr!(member 0:20 dot 1:1
            make_name!(simple 0:0 #2),
            make_name!(bare 2:20 false, None,
                make_name!(segment 2:8 #3),
                make_name!(segment generic 11:20
                    make_type!(plain 12:19 false, None,
                        make_type!(segment generic 12:19 #4 12:14 quote 15:19
                            make_type!(prim 16:18 I32)))))),
    }

    //      01234567
    case!{ "(0, 0).0" as Expr,
        make_expr!(member 0:7 dot 6:6
            make_expr!(tuple 0:5
                make_expr!(i32 0 1:1),
                make_expr!(i32 0 4:4)),
            make_name!(bare 7:7 false, None,
                make_name!(segment 7:7 #2))),
    }

    //      0         1         2         3         4         5
    //      012345678901234567890123456789012345678901234567890123456
    case!{ "string::<wchar>{ size: 0, cap: 0, data: uninitialized() }" as Expr,
        make_expr!(object 0:56 quote 15:56
            make_name!(0:14 false, None,
                make_name!(segment 0:5 #2),
                make_name!(segment generic 8:14
                    make_type!(simple 9:13 #3))),
            make_expr!(object field 17:23 #4 17:20 colon 21:21
                make_expr!(i32 0 23:23)),
            make_expr!(object field 26:31 #5 26:28 colon 29:29
                make_expr!(i32 0 31:31)),
            make_expr!(object field 34:54 #6 34:37 colon 38:38
                make_expr!(fn 40:54 paren 53:54
                    make_name!(simple 40:52 #7),))),
    }

    case!{ "a[]" as PostfixExpr,
        make_expr!(index 0:2 bracket 1:2
            make_name!(simple 0:0 #2),
        ), errors make_errors!(
            e: e.emit(strings::EmptyIndexCall).detail(Span::new(1, 2), strings::IndexCallHere)
        )
    }
    
    case!{ "a[, ]" as PostfixExpr,
        make_expr!(index 0:4 bracket 1:4
            make_name!(simple 0:0 #2),
        ), errors make_errors!(
            e: e.emit(strings::EmptyIndexCall).detail(Span::new(1, 4), strings::IndexCallHere)
        )
    }
    
    case!{ "a(, )" as PostfixExpr,
        make_expr!(fn 0:4 paren 1:4
            make_name!(simple 0:0 #2),
        ), errors make_errors!(
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
                    make_type!(simple 8:10 #4)),
                make_name!(segment generic 14:18
                    make_type!(simple 15:17 #5)))
        ), errors make_errors!(e: {
            e.emit(strings::InvalidNameSegment).detail(Span::new(14, 14), strings::NameSegmentExpect);
            e.emit(strings::InvalidMemberAccess).span(Span::new(7, 18)).help(strings::GenericMemberAccessSyntaxHelp);
        })
    }
}
