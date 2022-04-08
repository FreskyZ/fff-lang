use super::*;

#[test]
fn parse_primitive_type() {

    case!{ parse_type_ref "i32",
        make_type!(prim 0:2 I32),
    }
}

#[test]
fn parse_array_type() {

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
}

#[test]
fn parse_tuple_type() {

    case!{ parse_type_ref "()", 
        make_type!(tuple 0:1)
    }

    case!{ parse_type_ref "(i32, i32)", 
        make_type!(tuple 0:9
            make_type!(prim 1:3 I32),
            make_type!(prim 6:8 I32)),
    }

    case!{ parse_type_ref "(abc, def)", 
        make_type!(tuple 0:9
            make_type!(simple 1:3 #2),
            make_type!(simple 6:8 #3)),
    }

    case!{ parse_type_ref "(string,)", 
        make_type!(tuple 0:8
            make_type!(simple 1:6 #2)),
    }

    case!{ parse_type_ref "(i32)",
        make_type!(tuple 0:4
            make_type!(prim 1:3 I32),
        ), errors make_errors!(e: e.emit(strings::SingleItemTupleType).detail(Span::new(4, 4), strings::TupleTypeExpectCommaMeetRightParen)),
    }
}

#[test]
fn parse_fn_type() {

    case!{ parse_type_ref "fn()",
        make_type!(fn 0:3 paren 2:3 []),
    }

    //      01234567890123456789
    case!{ parse_type_ref "fn() -> Result<T, E>",
        make_type!(fn ret 0:19 paren 2:3 [],
            make_type!(path 8:19
                make_path!(segment generic 8:19 #2 8:13 quote 14:19 
                    make_type!(simple 15:15 #3),
                    make_type!(simple 18:18 #4)))),
    }

    //      0         1         2
    //      01234567890123456789012
    case!{ parse_type_ref "fn(i32, &string) -> i32", 
        make_type!(fn ret 0:22 paren 2:15 [
            make_type!(fp 3:5 make_type!(prim 3:5 I32)),
            make_type!(fp 8:14 make_type!(ref 8:14 make_type!(simple 9:14 #2)))],
            make_type!(prim 20:22 I32)),
    }

    //      0          1          2          3
    //      0123 456789012345 678901234567 89012345678 9
    case!{ parse_type_ref "fn(\nthis: This,\nself: Self,\nthat: That,\n)", 
        make_type!(fn 0:40 paren 2:40 [
            make_type!(fp named 4:13 #2 4:7 make_type!(simple 10:13 #3)),
            make_type!(fp named 16:25 #4 16:19 make_type!(simple 22:25 #5)),
            make_type!(fp named 28:37 #6 28:31 make_type!(simple 34:37 #7)),
        ]), strings ["this", "This", "self", "Self", "that", "That"],
    }

    //      0         1         2         3
    //      01234567890123456789012345678901234
    case!{ parse_type_ref "fn(argc: i32, argv: &string) -> i32", 
        make_type!(fn ret 0:34 paren 2:27 [
            make_type!(fp named 3:11 #2 3:6 make_type!(prim 9:11 I32)),
            make_type!(fp named 14:26 #3 14:17 make_type!(ref 20:26 make_type!(simple 21:26 #4)))],
            make_type!(prim 32:34 I32)),
    }

    //      0         1         2         3         4         5         6         7         8         9         0         1
    //      0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
    case!{ parse_type_ref "fn(this: &This, node: &/*dyn*/Node, title: string, span: Span, then: fn() -> Result<&This, fmt::Error>) -> fmt::Result",
        make_type!(fn ret 0:117 paren 2:102 [
            make_type!(fp named 3:13 #2 3:6 make_type!(ref 9:13 make_type!(simple 10:13 #3))),
            make_type!(fp named 16:33 #4 16:19 make_type!(ref 22:33 make_type!(simple 30:33 #5))),
            make_type!(fp named 36:48 #6 36:40 make_type!(simple 43:48 #7)),
            make_type!(fp named 51:60 #8 51:54 make_type!(simple 57:60 #9)),
            make_type!(fp named 63:101 #10 63:66 make_type!(fn ret 69:101 paren 71:72 [],
                make_type!(path 77:101
                    make_path!(segment generic 77:101 #11 77:82 quote 83:101
                        make_type!(ref 84:88 make_type!(simple 85:88 #3)),
                        make_type!(path 91:100
                            make_path!(segment simple 91:93 #12),
                            make_path!(segment simple 96:100 #13)))))),
        ], make_type!(path 107:117 
            make_path!(segment simple 107:109 #12),
            make_path!(segment simple 112:117 #11))
        //           2       3       4       5       6        7         8       9       10      11        12     13
        ), strings ["this", "This", "node", "Node", "title", "string", "span", "Span", "then", "Result", "fmt", "Error"]
    }
    
    // bare ident followed by comma is type
    //      0         1         2         3
    //      01234567890123456789012345678901234
    case!{ parse_type_ref "fn(argc,            &string) -> i32", 
        make_type!(fn ret 0:34 paren 2:27 [
            make_type!(fp 3:6 make_type!(simple 3:6 #2)),
            make_type!(fp 20:26 make_type!(ref 20:26 make_type!(simple 21:26 #3)))],
            make_type!(prim 32:34 I32)),
    }

    // bare ident followed by end of parameter list is type
    //      0         1         2         3
    //      01234567890123456789012345678901234
    case!{ parse_type_ref "fn(argc: i32,        string) -> i32", 
        make_type!(fn ret 0:34 paren 2:27 [
            make_type!(fp named 3:11 #2 3:6 make_type!(prim 9:11 I32)),
            make_type!(fp 21:26 make_type!(simple 21:26 #3))],
            make_type!(prim 32:34 I32)),
    }

    // bare ident followed by end of parameter list is type
    //      0         1         2         3
    //      01234567890123456789012345678901234
    case!{ parse_type_ref "fn(argc: i32,       string,) -> i32", 
        make_type!(fn ret 0:34 paren 2:27 [
            make_type!(fp named 3:11 #2 3:6 make_type!(prim 9:11 I32)),
            make_type!(fp 20:25 make_type!(simple 20:25 #3))],
            make_type!(prim 32:34 I32)),
    }

    // segmented path also should not be recognized as name, found by random test generator
    //      0         1
    //      012345678901234
    case!{ parse_type_ref "fn(ns1::ns2::t)",
        make_type!(fn 0:14 paren 2:14 [
            make_type!(fp 3:13
                make_type!(path 3:13
                    make_path!(segment simple 3:5 #2),
                    make_path!(segment simple 8:10 #3),
                    make_path!(segment simple 13:13 #4)))]),
    }

    //      0         1
    //      012345678901234567
    case!{ parse_type_ref "fn(ns1<t1, t2>::t)",
    make_type!(fn 0:17 paren 2:17 [
        make_type!(fp 3:16
            make_type!(path 3:16
                make_path!(segment generic 3:13 #2 3:5 quote 6:13
                    make_type!(simple 7:8 #3),
                    make_type!(simple 11:12 #4)),
                make_path!(segment simple 16:16 #5)))]),
    }


    //      0         1         2        3       4         5         6         7         8         9         0        1         2         3         4         5         6         7        8
    //      0123456789012345678901234567 01256789012345678901234567890123456789012345678901234567890123456789 234567890123456789012345678901234567890123456789012345678901234567890123456789012
    case!{ parse_type_ref "fn(x3Kv: [Mc; 6326], &((&t6絩ru卒oLy, i64, &f64, u64), fn(Ak: [o7k81A6; 0x5Ad0Cc], OmNaGmqc: [g63凈N;  223], bOars,), [c6eFq8M;  636353.456], [b; 271]), f3,) -> [HrH70sp; 0d447231]",
        make_type!(fn ret 0:182 paren 2:159 [
            make_type!(fp named 3:18 #2 3:6
                make_type!(array 9:18 
                    make_type!(simple 10:11 #3),
                    make_expr!(i32 6326 14:17))),
            make_type!(fp 21:153
                make_type!(ref 21:153 make_type!(tuple 22:153
                    make_type!(tuple 23:54
                        make_type!(ref 24:37 
                            make_type!(simple 25:37 #4)),
                        make_type!(prim 40:42 I64),
                        make_type!(ref 45:48
                            make_type!(prim 46:48 F64)),
                        make_type!(prim 51:53 U64)),
                    make_type!(fn 57:118 paren 59:118 [
                        make_type!(fp named 60:82 #5 60:61
                            make_type!(array 64:82
                                make_type!(simple 65:71 #6),
                                make_expr!(i32 0x5Ad0Cc 74:81))),
                        make_type!(fp named 85:109 #7 85:92
                            make_type!(array 95:109
                                make_type!(simple 96:102 #8),
                                make_expr!(i32 223 106:108))),
                        make_type!(fp 112:116
                            make_type!(simple 112:116 #9)),
                    ]),
                    make_type!(array 121:142
                        make_type!(simple 122:128 #10),
                        make_expr!(r64 636353.4560000001 132:141)),
                    make_type!(array 145:152
                        make_type!(simple 146:146 #11),
                        make_expr!(i32 271 149:151))))),
            make_type!(fp 156:157
                make_type!(simple 156:157 #12)),
        ], make_type!(array 164:182
            make_type!(simple 165:171 #13),
            make_expr!(i32 447231 174:181))
        //           2       3     4             5      6         7           8         9        10        11    12    13
        ), strings ["x3Kv", "Mc", "t6絩ru卒oLy", "Ak", "o7k81A6", "OmNaGmqc", "g63凈N", "bOars", "c6eFq8M", "b", "f3", "HrH70sp"]
    }
}

#[test]
fn parse_ref_type() {

    // this is some what void*
    case!{ parse_type_ref "&()", 
        make_type!(ref 0:2
            make_type!(tuple 1:2))
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
}

#[test]
pub fn parse_path() {

    case!{ parse_type_path "custom_type",
        make_path!(0:10
            make_path!(segment simple 0:10 #2)),
    }
    case!{ parse_value_path "custom_type",
        make_path!(0:10
            make_path!(segment simple 0:10 #2)),
    }

    // // now plain_type is only tested but not tested
    //      0         1         2         3
    //      012345678901234567890123456789012345678
    case!{ parse_type_path "ffc::syntax::plain_type::type_ref_parse",
        make_path!(0:38
            make_path!(segment simple 0:2 #2),
            make_path!(segment simple 5:10 #3),
            make_path!(segment simple 13:22 #4),
            make_path!(segment simple 25:38 #5))
    }
    case!{ parse_value_path "ffc::syntax::plain_type::type_ref_parse",
        make_path!(0:38
            make_path!(segment simple 0:2 #2),
            make_path!(segment simple 5:10 #3),
            make_path!(segment simple 13:22 #4),
            make_path!(segment simple 25:38 #5))
    }

    // single colon
    case!{ parse_type_path "ffc:syntax:plain_type:type_ref_parse",
        make_path!(0:35
            make_path!(segment simple 0:2 #2),
            make_path!(segment simple 4:9 #3),
            make_path!(segment simple 11:20 #4),
            make_path!(segment simple 22:35 #5)
        ), errors make_errors!(e: {
            e.emit(strings::ExpectDoubleColonMeetSingleColon).span(Span::new(3, 3));
            e.emit(strings::ExpectDoubleColonMeetSingleColon).span(Span::new(10, 10));
            e.emit(strings::ExpectDoubleColonMeetSingleColon).span(Span::new(21, 21));
        })
    }
    case!{ parse_value_path "ffc:syntax:plain_type:type_ref_parse",
        make_path!(0:35
            make_path!(segment simple 0:2 #2),
            make_path!(segment simple 4:9 #3),
            make_path!(segment simple 11:20 #4),
            make_path!(segment simple 22:35 #5)
        ), errors make_errors!(e: {
            e.emit(strings::ExpectDoubleColonMeetSingleColon).span(Span::new(3, 3));
            e.emit(strings::ExpectDoubleColonMeetSingleColon).span(Span::new(10, 10));
            e.emit(strings::ExpectDoubleColonMeetSingleColon).span(Span::new(21, 21));
        })
    }

    case!{ parse_type_path "::a",
        make_path!(0:2
            make_path!(segment global),
            make_path!(segment simple 2:2 #2))
    }
    case!{ parse_value_path "::a",
        make_path!(0:2
            make_path!(segment global),
            make_path!(segment simple 2:2 #2))
    }

    //      0         1         2         3
    //      012345678901234567890123456789012345678
    case!{ parse_type_path "::ffc::syntax::plain_type::type_ref_parse", 
        make_path!(0:40
            make_path!(segment global),
            make_path!(segment simple 2:4 #2),
            make_path!(segment simple 7:12 #3),
            make_path!(segment simple 15:24 #4),
            make_path!(segment simple 27:40 #5))
    }
    case!{ parse_value_path "::ffc::syntax::plain_type::type_ref_parse", 
        make_path!(0:40
            make_path!(segment global),
            make_path!(segment simple 2:4 #2),
            make_path!(segment simple 7:12 #3),
            make_path!(segment simple 15:24 #4),
            make_path!(segment simple 27:40 #5))
    }

    // TODO test Unexpected
    // case!{ parse_type_path "<a as a>",
    //     make_path!(0:7
    //         make_path!(segment cast 0:7
    //             make_type!(simple 1:1 #2),
    //             make_type!(simple 6:6 #2))
    //     ), errors make_errors!(e: e.emit(strings::TypeCastSegmentCannotStandalone).span(Span::new(0, 7)))
    // }

    //                      0         1         2         3         4         5         6
    //                      012345678901234567890123456789012345678901234567890123456789012345678
    case!{ parse_type_path "<::ffc::syntax::PlainType as ffc::syntax::prelude::Parser<F>>::Output",
        make_path!(0:68
            make_path!(segment cast 0:60
                make_type!(path 1:24
                    make_path!(segment global),
                    make_path!(segment simple 3:5 #2),
                    make_path!(segment simple 8:13 #3),
                    make_path!(segment simple 16:24 #4)),
                make_type!(path 29:59
                    make_path!(segment simple 29:31 #2),
                    make_path!(segment simple 34:39 #3),
                    make_path!(segment simple 42:48 #5),
                    make_path!(segment generic 51:59 #6 51:56 quote 57:59
                        make_type!(simple 58:58 #7)))),
            make_path!(segment simple 63:68 #8)),
    }
    case!{ parse_value_path "<::ffc::syntax::PlainType as ffc::syntax::prelude::Parser<F>>::Output",
        make_path!(0:68
            make_path!(segment cast 0:60
                make_type!(path 1:24
                    make_path!(segment global),
                    make_path!(segment simple 3:5 #2),
                    make_path!(segment simple 8:13 #3),
                    make_path!(segment simple 16:24 #4)),
                make_type!(path 29:59
                    make_path!(segment simple 29:31 #2),
                    make_path!(segment simple 34:39 #3),
                    make_path!(segment simple 42:48 #5),
                    make_path!(segment generic 51:59 #6 51:56 quote 57:59
                        make_type!(simple 58:58 #7)))),
            make_path!(segment simple 63:68 #8)),
    }

    //                      01234567890123456
    case!{ parse_type_path "<a as a>::a::a::a",
        make_path!(0:16
            make_path!(segment cast 0:7
                make_type!(path 1:1
                    make_path!(segment simple 1:1 #2)),
                make_type!(path 6:6
                    make_path!(segment simple 6:6 #2))),
            make_path!(segment simple 10:10 #2),
            make_path!(segment simple 13:13 #2),
            make_path!(segment simple 16:16 #2))
    }
    case!{ parse_value_path "<a as a>::a::a::a",
        make_path!(0:16
            make_path!(segment cast 0:7
                make_type!(path 1:1
                    make_path!(segment simple 1:1 #2)),
                make_type!(path 6:6
                    make_path!(segment simple 6:6 #2))),
            make_path!(segment simple 10:10 #2),
            make_path!(segment simple 13:13 #2),
            make_path!(segment simple 16:16 #2))
    }

    // empty type list is recognized in syntax
    case!{ parse_type_path "a<>",
        make_path!(0:2
            make_path!(segment generic 0:2 #2 0:0 quote 1:2)
        ), errors make_errors!(e: e.emit(strings::EmptyTypeList).span(Span::new(1, 2)))
    }
    case!{ parse_value_path "a::<>",
        make_path!(0:4
            make_path!(segment generic 0:4 #2 0:0 quote 3:4)
        ), errors make_errors!(e: e.emit(strings::EmptyTypeList).span(Span::new(3, 4)))
    }

    // include single comma
    case!{ parse_type_path "a<,>",
        make_path!(0:3
            make_path!(segment generic 0:3 #2 0:0 quote 1:3)
        ), errors make_errors!(e: e.emit(strings::EmptyTypeList).span(Span::new(1, 3)))
    }
    case!{ parse_value_path "a::<,>",
        make_path!(0:5
            make_path!(segment generic 0:5 #2 0:0 quote 3:5)
        ), errors make_errors!(e: e.emit(strings::EmptyTypeList).span(Span::new(3, 5)))
    }
    
    //                      0123456789
    case!{ parse_type_path "a<b, c, d>",
        make_path!(0:9
            make_path!(segment generic 0:9 #2 0:0 quote 1:9
                make_type!(path 2:2
                    make_path!(segment simple 2:2 #3)),
                make_type!(path 5:5
                    make_path!(segment simple 5:5 #4)),
                make_type!(path 8:8
                    make_path!(segment simple 8:8 #5))))
    }
    //                       012345678901
    case!{ parse_value_path "a::<b, c, d>",
        make_path!(0:11
            make_path!(segment generic 0:11 #2 0:0 quote 3:11
                make_type!(path 4:4
                    make_path!(segment simple 4:4 #3)),
                make_type!(path 7:7
                    make_path!(segment simple 7:7 #4)),
                make_type!(path 10:10
                    make_path!(segment simple 10:10 #5))))
    }

    // trailing comma
    //                      0123456789
    case!{ parse_type_path "a<b, c, d,>",
        make_path!(0:10
            make_path!(segment generic 0:10 #2 0:0 quote 1:10
                make_type!(path 2:2
                    make_path!(segment simple 2:2 #3)),
                make_type!(path 5:5
                    make_path!(segment simple 5:5 #4)),
                make_type!(path 8:8
                    make_path!(segment simple 8:8 #5))))
    }
    //                       012345678901
    case!{ parse_value_path "a::<b, c, d, >",
        make_path!(0:13
            make_path!(segment generic 0:13 #2 0:0 quote 3:13
                make_type!(path 4:4
                    make_path!(segment simple 4:4 #3)),
                make_type!(path 7:7
                    make_path!(segment simple 7:7 #4)),
                make_type!(path 10:10
                    make_path!(segment simple 10:10 #5))))
    }
    
    // ::< is allowed in type path
    case!{ parse_type_path "a::<b, c, d>",
        make_path!(0:11
            make_path!(segment generic 0:11 #2 0:0 quote 3:11
                make_type!(path 4:4
                    make_path!(segment simple 4:4 #3)),
                make_type!(path 7:7
                    make_path!(segment simple 7:7 #4)),
                make_type!(path 10:10
                    make_path!(segment simple 10:10 #5)))
        ), errors make_errors!(e: e.emit(format!("{} `::`", strings::ExpectLtMeet)).span(Span::new(1, 2)))
    }

    // single colon
    case!{ parse_value_path "a:<b, c, d>",
        make_path!(0:10
            make_path!(segment generic 0:10 #2 0:0 quote 2:10
                make_type!(path simple 3:3 #3),
                make_type!(path simple 6:6 #4),
                make_type!(path simple 9:9 #5))
        ), errors make_errors!(e: e.emit(strings::ExpectDoubleColonMeetSingleColon).span(Span::new(1, 1)))
    }

    // single colon and allow colon in type path // seems not happy :<
    case!{ parse_type_path "a:<b, c, d>",
        make_path!(0:10
            make_path!(segment generic 0:10 #2 0:0 quote 2:10
                make_type!(path 3:3
                    make_path!(segment simple 3:3 #3)),
                make_type!(path 6:6
                    make_path!(segment simple 6:6 #4)),
                make_type!(path 9:9
                    make_path!(segment simple 9:9 #5)))
        ), errors make_errors!(e: {
            e.emit(format!("{} `:`", strings::ExpectLtMeet)).span(Span::new(1, 1));
            e.emit(strings::ExpectDoubleColonMeetSingleColon).span(Span::new(1, 1));
        })
    }

    //                 0         1         2      v this is not part of name
    //                 012345678901234567890123456
    case!{ parse_value_path "::abc::def::<ghi, jkl>::mno<", 
        make_path!(0:26
            make_path!(segment global),
            make_path!(segment simple 2:4 #2),
            make_path!(segment generic 7:21 #3 7:9 quote 12:21
                make_type!(simple 13:15 #4),
                make_type!(simple 18:20 #5)),
            make_path!(segment simple 24:26 #6))
    }

    //                      0123456789012
    case!{ parse_type_path "a::b::c<d, e>",
        make_path!(0:12
            make_path!(segment simple 0:0 #2),
            make_path!(segment simple 3:3 #3),
            make_path!(segment generic 6:12 #4 6:6 quote 7:12
                make_type!(simple 8:8 #5),
                make_type!(simple 11:11 #6)))
    }
    //                       0123456789012345
    case!{ parse_value_path "a::b::c::<d, e,>",
        make_path!(0:15
            make_path!(segment simple 0:0 #2),
            make_path!(segment simple 3:3 #3),
            make_path!(segment generic 6:15 #4 6:6 quote 9:15
                make_type!(simple 10:10 #5),
                make_type!(simple 13:13 #6)))
    }

    //                      0123456789012345678901
    case!{ parse_type_path "a::b<c, d>::e::f<g, h>",
        make_path!(0:21
            make_path!(segment simple 0:0 #2),
            make_path!(segment generic 3:9 #3 3:3 quote 4:9
                make_type!(simple 5:5 #4),
                make_type!(simple 8:8 #5)),
            make_path!(segment simple 12:12 #6),
            make_path!(segment generic 15:21 #7 15:15 quote 16:21
                make_type!(simple 17:17 #8),
                make_type!(simple 20:20 #9)))
    }
    //                       01234567890123456789012345
    case!{ parse_value_path "a::b::<c, d>::e::f::<g, h>",
        make_path!(0:25
            make_path!(segment simple 0:0 #2),
            make_path!(segment generic 3:11 #3 3:3 quote 6:11
                make_type!(simple 7:7 #4),
                make_type!(simple 10:10 #5)),
            make_path!(segment simple 14:14 #6),
            make_path!(segment generic 17:25 #7 17:17 quote 20:25
                make_type!(simple 21:21 #8),
                make_type!(simple 24:24 #9)))
    }

    // split shift right
    case!{ parse_type_path "a<b<c>>",
        make_path!(0:6
            make_path!(segment generic 0:6 #2 0:0 quote 1:6
                make_type!(path 2:5
                    make_path!(segment generic 2:5 #3 2:2 quote 3:5
                        make_type!(simple 4:4 #4))))),
    }
    //                       012345678
    case!{ parse_value_path "a::<b<c>>",
        make_path!(0:8
            make_path!(segment generic 0:8 #2 0:0 quote 3:8
                make_type!(path 4:7
                    make_path!(segment generic 4:7 #3 4:4 quote 5:7
                        make_type!(simple 6:6 #4))))
        ),
    }
    // inside angle bracket is type not value
    case!{ parse_value_path "a::<b::<c>>",
        make_path!(0:10
            make_path!(segment generic 0:10 #2 0:0 quote 3:10
                make_type!(path 4:9
                    make_path!(segment generic 4:9 #3 4:4 quote 7:9
                        make_type!(simple 8:8 #4))))
        ), errors make_errors!(e: e.emit(format!("{} `::`", strings::ExpectLtMeet)).span(Span::new(5, 6))),
    }

    // type cast segment in front of angle bracket quoted type list requires split shift left
    //      01234567890123
    case!{ parse_type_path "a<<a as a>::a>",
        make_path!(0:13
            make_path!(segment generic 0:13 #2 0:0 quote 1:13
                make_type!(path 2:12
                    make_path!(segment cast 2:9
                        make_type!(simple 3:3 #2),
                        make_type!(simple 8:8 #2)),
                    make_path!(segment simple 12:12 #2)))),
    }
    case!{ parse_value_path "a::<<a as a>::a>",
        make_path!(0:15
            make_path!(segment generic 0:15 #2 0:0 quote 3:15
                make_type!(path 4:14
                    make_path!(segment cast 4:11
                        make_type!(simple 5:5 #2),
                        make_type!(simple 10:10 #2)),
                    make_path!(segment simple 14:14 #2)))),
    }
    
    // single quote + ltlt
    case!{ parse_type_path "a:<<a as a>::a>",
        make_path!(0:14
            make_path!(segment generic 0:14 #2 0:0 quote 2:14
                make_type!(path 3:13
                    make_path!(segment cast 3:10
                        make_type!(simple 4:4 #2),
                        make_type!(simple 9:9 #2)),
                    make_path!(segment simple 13:13 #2)))
        ), errors make_errors!(e: {
            e.emit(format!("{} `:`", strings::ExpectLtMeet)).span(Span::new(1, 1));
            e.emit(strings::ExpectDoubleColonMeetSingleColon).span(Span::new(1, 1));
        })
    }
    case!{ parse_value_path "a:<<a as a>::a>",
        make_path!(0:14
            make_path!(segment generic 0:14 #2 0:0 quote 2:14
                make_type!(path 3:13
                    make_path!(segment cast 3:10
                        make_type!(simple 4:4 #2),
                        make_type!(simple 9:9 #2)),
                    make_path!(segment simple 13:13 #2)))
        ), errors make_errors!(e: {
            e.emit(strings::ExpectDoubleColonMeetSingleColon).span(Span::new(1, 1));
        })
    }
}

#[test]
fn parse_type_ref() {
    // cases splitted into several functions
    // remain this here "type ref integrated test" from outside
}