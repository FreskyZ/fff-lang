use super::*;
// nodes that not expr and not stmt and not item

#[test]
fn type_ref_parse() {

    case!{ parse_type_ref "i32",
        make_type!(prim 0:2 I32),
    }

    case!{ parse_type_ref "custom_type",
        make_type!(simple 0:10 #2),
    }

    case!{ parse_type_ref "[i32; 5]", 
        make_type!(array 0:7
            make_type!(prim 1:3 I32), 
            make_expr!(i32 5 6:6).into()),
    }

    //      0         1         2
    //      01234567890123456789012345
    case!{ parse_type_ref "[[a;1]; 1 + 1 * 1 - 1 / 1]",
        make_type!(array 0:25 
            make_type!(array 1:5
                make_type!(simple 2:2 #2),
                make_expr!(i32 1 4:4)),
            make_expr!(binary 8:24 Sub 18:18 
                make_expr!(binary 8:16 Add 10:10
                    make_expr!(i32 1 8:8),
                    make_expr!(binary 12:16 Mul 14:14
                        make_expr!(i32 1 12:12),
                        make_expr!(i32 1 16:16))),
                make_expr!(binary 20:24 Div 22:22
                    make_expr!(i32 1 20:20),
                    make_expr!(i32 1 24:24)))),
    }

    case!{ parse_type_ref "()", 
        make_type!(tuple 0:1 [])
    }

    case!{ parse_type_ref "(i32, i32)", 
        make_type!(tuple 0:9 [
            make_type!(prim 1:3 I32),
            make_type!(prim 6:8 I32),
        ]),
    }

    case!{ parse_type_ref "(abc, def)", 
        make_type!(tuple 0:9 [
            make_type!(simple 1:3 #2),
            make_type!(simple 6:8 #3),
        ]),
    }

    case!{ parse_type_ref "(string,)", 
        make_type!(tuple 0:8 [
            make_type!(simple 1:6 #2),
        ]),
    }

    case!{ parse_type_ref "(i32)",
        make_type!(tuple 0:4 [
            make_type!(prim 1:3 I32),
        ]), errors make_errors!(e: e.emit(strings::SingleItemTupleType).detail(Span::new(4, 4), strings::TupleTypeExpectCommaMeetRightParen)),
    }

    case!{ parse_type_ref "fn()",
        make_type!(fn 0:3 paren 2:3 []),
    }

    //      01234567890123456789
    case!{ parse_type_ref "fn() -> Result<T, E>",
        make_type!(fn ret 0:19 paren 2:3 [],
            make_type!(plain 8:19 false, None,
                make_type!(segment generic 8:19 #2 8:13 quote 14:19 
                    make_type!(simple 15:15 #3),
                    make_type!(simple 18:18 #4)))),
    }

    //      0         1         2
    //      01234567890123456789012
    case!{ parse_type_ref "fn(i32, &string) -> i32", 
        make_type!(fn ret 0:22 paren 2:15 [
            make_type!(param 3:5 make_type!(prim 3:5 I32)),
            make_type!(param 8:14 make_type!(ref 8:14 make_type!(simple 9:14 #2)))],
            make_type!(prim 20:22 I32)),
    }

    //      0          1          2          3
    //      0123 456789012345 678901234567 89012345678 9
    case!{ parse_type_ref "fn(\nthis: This,\nself: Self,\nthat: That,\n)", 
        make_type!(fn 0:40 paren 2:40 [
            make_type!(param named 4:13 #2 4:7 make_type!(simple 10:13 #3)),
            make_type!(param named 16:25 #4 16:19 make_type!(simple 22:25 #5)),
            make_type!(param named 28:37 #6 28:31 make_type!(simple 34:37 #7)),
        ]), strings ["this", "This", "self", "Self", "that", "That"],
    }

    //      0         1         2         3
    //      01234567890123456789012345678901234
    case!{ parse_type_ref "fn(argc: i32, argv: &string) -> i32", 
        make_type!(fn ret 0:34 paren 2:27 [
            make_type!(param named 3:11 #2 3:6 make_type!(prim 9:11 I32)),
            make_type!(param named 14:26 #3 14:17 make_type!(ref 20:26 make_type!(simple 21:26 #4)))],
            make_type!(prim 32:34 I32)),
    }

    //      0         1         2         3         4         5         6         7         8         9         0         1
    //      0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
    case!{ parse_type_ref "fn(this: &This, node: &/*dyn*/Node, title: string, span: Span, then: fn() -> Result<&This, fmt::Error>) -> fmt::Result",
        make_type!(fn ret 0:117 paren 2:102 [
            make_type!(param named 3:13 #2 3:6 make_type!(ref 9:13 make_type!(simple 10:13 #3))),
            make_type!(param named 16:33 #4 16:19 make_type!(ref 22:33 make_type!(simple 30:33 #5))),
            make_type!(param named 36:48 #6 36:40 make_type!(simple 43:48 #7)),
            make_type!(param named 51:60 #8 51:54 make_type!(simple 57:60 #9)),
            make_type!(param named 63:101 #10 63:66 make_type!(fn ret 69:101 paren 71:72 [],
                make_type!(plain 77:101 false, None,
                    make_type!(segment generic 77:101 #11 77:82 quote 83:101
                        make_type!(ref 84:88 make_type!(simple 85:88 #3)),
                        make_type!(plain 91:100 false, None,
                            make_type!(segment 91:93 #12),
                            make_type!(segment 96:100 #13)))))),
        ], make_type!(plain 107:117 false, None, 
            make_type!(segment 107:109 #12),
            make_type!(segment 112:117 #11))
        //           2       3       4       5       6        7         8       9       10      11        12     13
        ), strings ["this", "This", "node", "Node", "title", "string", "span", "Span", "then", "Result", "fmt", "Error"]
    }
    
    // bare ident followed by comma is type
    //      0         1         2         3
    //      01234567890123456789012345678901234
    case!{ parse_type_ref "fn(argc,            &string) -> i32", 
        make_type!(fn ret 0:34 paren 2:27 [
            make_type!(param 3:6 make_type!(simple 3:6 #2)),
            make_type!(param 20:26 make_type!(ref 20:26 make_type!(simple 21:26 #3)))],
            make_type!(prim 32:34 I32)),
    }

    // bare ident followed by end of parameter list is type
    //      0         1         2         3
    //      01234567890123456789012345678901234
    case!{ parse_type_ref "fn(argc: i32,        string) -> i32", 
        make_type!(fn ret 0:34 paren 2:27 [
            make_type!(param named 3:11 #2 3:6 make_type!(prim 9:11 I32)),
            make_type!(param 21:26 make_type!(simple 21:26 #3))],
            make_type!(prim 32:34 I32)),
    }

    // bare ident followed by end of parameter list is type
    //      0         1         2         3
    //      01234567890123456789012345678901234
    case!{ parse_type_ref "fn(argc: i32,       string,) -> i32", 
        make_type!(fn ret 0:34 paren 2:27 [
            make_type!(param named 3:11 #2 3:6 make_type!(prim 9:11 I32)),
            make_type!(param 20:25 make_type!(simple 20:25 #3))],
            make_type!(prim 32:34 I32)),
    }

    // segmented plain type also should not be recognized as name, found by random test generator
    //      0         1
    //      012345678901234
    case!{ parse_type_ref "fn(ns1::ns2::t)",
        make_type!(fn 0:14 paren 2:14 [
            make_type!(param 3:13
                make_type!(plain 3:13 false, None,
                    make_type!(segment 3:5 #2),
                    make_type!(segment 8:10 #3),
                    make_type!(segment 13:13 #4)))]),
    }

    //      0         1
    //      012345678901234567
    case!{ parse_type_ref "fn(ns1<t1, t2>::t)",
    make_type!(fn 0:17 paren 2:17 [
        make_type!(param 3:16
            make_type!(plain 3:16 false, None,
                make_type!(segment generic 3:13 #2 3:5 quote 6:13
                    make_type!(simple 7:8 #3),
                    make_type!(simple 11:12 #4)),
                make_type!(segment 16:16 #5)))]),
    }

    //      0         1         2         3
    //      012345678901234567890123456789012345678
    case!{ parse_type_ref "ffc::syntax::plain_type::type_ref_parse", 
        make_type!(plain 0:38 false, None,
            make_type!(segment 0:2 #2),
            make_type!(segment 5:10 #3),
            make_type!(segment 13:22 #4),
            make_type!(segment 25:38 #5))
    }

    //      0         1         2         3
    //      012345678901234567890123456789012345678
    case!{ parse_type_ref "::ffc::syntax::plain_type::type_ref_parse", 
        make_type!(plain 0:40 true, None,
            make_type!(segment 2:4 #2),
            make_type!(segment 7:12 #3),
            make_type!(segment 15:24 #4),
            make_type!(segment 27:40 #5))
    }

    //      0         1         2         3         4         5         6
    //      012345678901234567890123456789012345678901234567890123456789012345678
    case!{ parse_type_ref "<::ffc::syntax::PlainType as ffc::syntax::prelude::Parser<F>>::Output",
        make_type!(plain 0:68 false,
            make_type!(segment as 0:60
                make_type!(plain 1:24 true, None,
                    make_type!(segment 3:5 #2),
                    make_type!(segment 8:13 #3),
                    make_type!(segment 16:24 #4)),
                make_type!(plain 29:59 false, None,
                    make_type!(segment 29:31 #2),
                    make_type!(segment 34:39 #3),
                    make_type!(segment 42:48 #5),
                    make_type!(segment generic 51:59 #6 51:56 quote 57:59
                        make_type!(simple 58:58 #7)))),
            make_type!(segment 63:68 #8)),
    }

    // empty type list is allowed in syntax
    case!{ parse_type_ref "a<>",
        make_type!(plain 0:2 false, None,
            make_type!(segment generic 0:2 #2 0:0 quote 1:2)
        ), errors make_errors!(e: e.emit(strings::EmptyTypeList).span(Span::new(1, 2)))
    }

    // split shift right
    case!{ parse_type_ref "a<b<c>>",
        make_type!(plain 0:6 false, None,
            make_type!(segment generic 0:6 #2 0:0 quote 1:6
                make_type!(plain 2:5 false, None,
                    make_type!(segment generic 2:5 #3 2:2 quote 3:5
                        make_type!(simple 4:4 #4))))),
    }

    // this is some what void*
    case!{ parse_type_ref "&()", 
        make_type!(ref 0:2
            make_type!(tuple 1:2 []))
    }

    // split and and
    case!{ parse_type_ref "&&i32",
        make_type!(ref 0:4
            make_type!(ref 1:4
                make_type!(prim 2:4 I32)))
    }

    //      0123456789
    case!{ parse_type_ref "&[sc; 207]",
        make_type!(ref 0:9
            make_type!(array 1:9
                make_type!(simple 2:3 #2),
                make_expr!(i32 207 6:8))),
    }

    //      0         1         2        3       4         5         6         7         8         9         0        1         2         3         4         5         6         7        8
    //      0123456789012345678901234567 01256789012345678901234567890123456789012345678901234567890123456789 234567890123456789012345678901234567890123456789012345678901234567890123456789012
    case!{ parse_type_ref "fn(x3Kv: [Mc; 6326], &((&t6絩ru卒oLy, i64, &f64, u64), fn(Ak: [o7k81A6; 0x5Ad0Cc], OmNaGmqc: [g63凈N;  223], bOars,), [c6eFq8M;  636353.456], [b; 271]), f3,) -> [HrH70sp; 0d447231]",
        make_type!(fn ret 0:182 paren 2:159 [
            make_type!(param named 3:18 #2 3:6
                make_type!(array 9:18 
                    make_type!(simple 10:11 #3),
                    make_expr!(i32 6326 14:17))),
            make_type!(param 21:153
                make_type!(ref 21:153 make_type!(tuple 22:153 [
                    make_type!(tuple 23:54 [
                        make_type!(ref 24:37 
                            make_type!(simple 25:37 #4)),
                        make_type!(prim 40:42 I64),
                        make_type!(ref 45:48
                            make_type!(prim 46:48 F64)),
                        make_type!(prim 51:53 U64),
                    ]),
                    make_type!(fn 57:118 paren 59:118 [
                        make_type!(param named 60:82 #5 60:61
                            make_type!(array 64:82
                                make_type!(simple 65:71 #6),
                                make_expr!(i32 0x5Ad0Cc 74:81))),
                        make_type!(param named 85:109 #7 85:92
                            make_type!(array 95:109
                                make_type!(simple 96:102 #8),
                                make_expr!(i32 223 106:108))),
                        make_type!(param 112:116
                            make_type!(simple 112:116 #9)),
                    ]),
                    make_type!(array 121:142
                        make_type!(simple 122:128 #10),
                        make_expr!(r64 636353.4560000001 132:141)),
                    make_type!(array 145:152
                        make_type!(simple 146:146 #11),
                        make_expr!(i32 271 149:151)),
                ]))),
            make_type!(param 156:157
                make_type!(simple 156:157 #12)),
        ], make_type!(array 164:182
            make_type!(simple 165:171 #13),
            make_expr!(i32 447231 174:181))
        //           2       3     4             5      6         7           8         9        10        11    12    13
        ), strings ["x3Kv", "Mc", "t6絩ru卒oLy", "Ak", "o7k81A6", "OmNaGmqc", "g63凈N", "bOars", "c6eFq8M", "b", "f3", "HrH70sp"]
    }

    // type as segment in front of angle bracket quoted type list requires split shift left
    //      01234567890123
    case!{ parse_type_ref "a<<a as a>::a>",
        make_type!(plain 0:13 false, None,
            make_type!(segment generic 0:13 #2 0:0 quote 1:13
                make_type!(plain 2:12 false,
                    make_type!(segment as 2:9
                        make_type!(simple 3:3 #2),
                        make_type!(simple 8:8 #2)),
                    make_type!(segment 12:12 #2)))),
    }
}
