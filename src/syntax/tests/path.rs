use super::*;

#[test]
fn parse_primitive_type() {

    case!{ parse_type_ref "i32", |_| make_type!(prim 0:2 I32) }
}

#[test]
fn parse_array_type() {

    case!{ parse_type_ref "[i32; 5]", |_|
        make_type!(array 0:7
            make_type!(prim 1:3 I32),
            make_expr!(i32 5 6:6).into()),
    }

    //      0         1         2
    //      01234567890123456789012345
    case!{ parse_type_ref "[[a;1]; 1 + 1 * 1 - 1 / 1]", |x|
        make_type!(array 0:25
            make_type!(array 1:5
                make_type!(x simple 2:2 #a),
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

    case!{ parse_type_ref "()", |_| make_type!(tuple 0:1) }

    case!{ parse_type_ref "(i32, i32)", |_|
        make_type!(tuple 0:9
            make_type!(prim 1:3 I32),
            make_type!(prim 6:8 I32)),
    }

    case!{ parse_type_ref "(abc, def)", |x|
        make_type!(tuple 0:9
            make_type!(x simple 1:3 #abc),
            make_type!(x simple 6:8 #def)),
    }

    case!{ parse_type_ref "(string,)", |x|
        make_type!(tuple 0:8
            make_type!(x simple 1:6 #string)),
    }

    case!{ parse_type_ref "(i32)", |_|
        make_type!(tuple 0:4
            make_type!(prim 1:3 I32),),
        |e| e.emit(strings::SingleItemTupleType).detail(Span::new(4, 4), strings::TupleTypeExpectCommaMeetRightParen),
    }
}

#[test]
fn parse_fn_type() {

    case!{ parse_type_ref "fn()", |_| make_type!(fn 0:3 paren 2:3 []) }

    //      01234567890123456789
    case!{ parse_type_ref "fn() -> Result<T, E>", |x|
        make_type!(fn ret 0:19 paren 2:3 [],
            make_type!(path 8:19
                make_path!(x segment generic 8:19 #Result 8:13 quote 14:19
                    make_type!(x simple 15:15 #T),
                    make_type!(x simple 18:18 #E)))),
    }

    //      0         1         2
    //      01234567890123456789012
    case!{ parse_type_ref "fn(i32, &string) -> i32", |x|
        make_type!(fn ret 0:22 paren 2:15 [
            make_type!(fp 3:5 make_type!(prim 3:5 I32)),
            make_type!(fp 8:14 make_type!(ref 8:14 make_type!(x simple 9:14 #string)))],
            make_type!(prim 20:22 I32)),
    }

    //      0          1          2          3
    //      0123 456789012345 678901234567 89012345678 9
    case!{ parse_type_ref "fn(\nthis: This,\nself: Self,\nthat: That,\n)", |x|
        make_type!(fn 0:40 paren 2:40 [
            make_type!(x fp named 4:13 #this 4:7 make_type!(x simple 10:13 #This)),
            make_type!(x fp named 16:25 #self 16:19 make_type!(x simple 22:25 #Self)),
            make_type!(x fp named 28:37 #that 28:31 make_type!(x simple 34:37 #That)),
        ])
    }

    //      0         1         2         3
    //      01234567890123456789012345678901234
    case!{ parse_type_ref "fn(argc: i32, argv: &string) -> i32", |x|
        make_type!(fn ret 0:34 paren 2:27 [
            make_type!(x fp named 3:11 #argc 3:6 make_type!(prim 9:11 I32)),
            make_type!(x fp named 14:26 #argv 14:17 make_type!(ref 20:26 make_type!(x simple 21:26 #string)))],
            make_type!(prim 32:34 I32)),
    }

    //                     0         1         2         3         4         5         6         7         8         9         0         1
    //                     0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
    case!{ parse_type_ref "fn(this: &This, node: &/*dyn*/Node, title: string, span: Span, then: fn() -> Result<&This, fmt::Error>) -> fmt::Result", |x|
        make_type!(fn ret 0:117 paren 2:102 [
            make_type!(x fp named 3:13 #this 3:6
                make_type!(ref 9:13 make_type!(x simple 10:13 #This))),
            make_type!(x fp named 16:33 #node 16:19
                make_type!(ref 22:33 make_type!(x simple 30:33 #Node))),
            make_type!(x fp named 36:48 #title 36:40
                make_type!(x simple 43:48 #string)),
            make_type!(x fp named 51:60 #span 51:54
                make_type!(x simple 57:60 #Span)),
            make_type!(x fp named 63:101 #then 63:66
                make_type!(fn ret 69:101 paren 71:72 [],
                    make_type!(path 77:101
                        make_path!(x segment generic 77:101 #Result 77:82 quote 83:101
                            make_type!(ref 84:88 make_type!(x simple 85:88 #This)),
                            make_type!(path 91:100
                                make_path!(x segment simple 91:93 #fmt),
                                make_path!(x segment simple 96:100 #Error)))))),
        ], make_type!(path 107:117
            make_path!(x segment simple 107:109 #fmt),
            make_path!(x segment simple 112:117 #Result)))
    }

    // this was from the method inside the case! macro for some time
    //                     0         1         2         3         4         5         6         7         8         9        10        11        12        13        14        15        16        17        18
    //                     0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
    case!{ parse_type_ref "fn(input: &string, f: F, expect_node: N, expect_diagnostics: crate::diagnostics::Diagnostics, expect_strings: &string, backtrace: u32) -> Result<(), (N, N, SourceContext<VirtualFileSystem>)>", |x|
        make_type!(fn ret 0:189 paren 2:133 [
            make_type!(x fp named 3:16 #input 3:7
                make_type!(ref 10:16 make_type!(x simple 11:16 #string))),
            make_type!(x fp named 19:22 #f 19:19
                make_type!(x simple 22:22 #F)),
            make_type!(x fp named 25:38 #expect_node 25:35
                make_type!(x simple 38:38 #N)),
            make_type!(x fp named 41:91 #expect_diagnostics 41:58
                make_type!(path 61:91
                    make_path!(x segment simple 61:65 #crate),
                    make_path!(x segment simple 68:78 #diagnostics),
                    make_path!(x segment simple 81:91 #Diagnostics))),
            make_type!(x fp named 94:116 #expect_strings 94:107
                make_type!(ref 110:116 make_type!(x simple 111:116 #string))),
            make_type!(x fp named 119:132 #backtrace 119:127
                make_type!(prim 130:132 U32)),
        ], make_type!(path 138:189
            make_path!(x segment generic 138:189 #Result 138:143 quote 144:189
                make_type!(tuple 145:146),
                make_type!(tuple 149:188
                    make_type!(x simple 150:150 #N),
                    make_type!(x simple 153:153 #N),
                    make_type!(path 156:187
                        make_path!(x segment generic 156:187 #SourceContext 156:168 quote 169:187
                                make_type!(x simple 170:186 #VirtualFileSystem))))))),
    }

    // bare ident followed by comma is type
    //      0         1         2         3
    //      01234567890123456789012345678901234
    case!{ parse_type_ref "fn(argc,            &string) -> i32", |x|
        make_type!(fn ret 0:34 paren 2:27 [
            make_type!(fp 3:6 make_type!(x simple 3:6 #argc)),
            make_type!(fp 20:26 make_type!(ref 20:26 make_type!(x simple 21:26 #string)))],
            make_type!(prim 32:34 I32)),
    }

    // bare ident followed by end of parameter list is type
    //      0         1         2         3
    //      01234567890123456789012345678901234
    case!{ parse_type_ref "fn(argc: i32,        string) -> i32", |x|
        make_type!(fn ret 0:34 paren 2:27 [
            make_type!(x fp named 3:11 #argc 3:6 make_type!(prim 9:11 I32)),
            make_type!(fp 21:26 make_type!(x simple 21:26 #string))],
            make_type!(prim 32:34 I32)),
    }

    // bare ident followed by end of parameter list is type
    //      0         1         2         3
    //      01234567890123456789012345678901234
    case!{ parse_type_ref "fn(argc: i32,       string,) -> i32", |x|
        make_type!(fn ret 0:34 paren 2:27 [
            make_type!(x fp named 3:11 #argc 3:6 make_type!(prim 9:11 I32)),
            make_type!(fp 20:25 make_type!(x simple 20:25 #string))],
            make_type!(prim 32:34 I32)),
    }

    // segmented path also should not be recognized as name, found by random test generator
    //      0         1
    //      012345678901234
    case!{ parse_type_ref "fn(ns1::ns2::t)", |x|
        make_type!(fn 0:14 paren 2:14 [
            make_type!(fp 3:13
                make_type!(path 3:13
                    make_path!(x segment simple 3:5 #ns1),
                    make_path!(x segment simple 8:10 #ns2),
                    make_path!(x segment simple 13:13 #t)))]),
    }

    //      0         1
    //      012345678901234567
    case!{ parse_type_ref "fn(ns1<t1, t2>::t)", |x|
    make_type!(fn 0:17 paren 2:17 [
        make_type!(fp 3:16
            make_type!(path 3:16
                make_path!(x segment generic 3:13 #ns1 3:5 quote 6:13
                    make_type!(x simple 7:8 #t1),
                    make_type!(x simple 11:12 #t2)),
                make_path!(x segment simple 16:16 #t)))]),
    }

    //                     0         1         2        3       4         5         6         7         8         9         0        1         2         3         4         5         6         7        8
    //                     0123456789012345678901234567 01256789012345678901234567890123456789012345678901234567890123456789 234567890123456789012345678901234567890123456789012345678901234567890123456789012
    case!{ parse_type_ref "fn(x3Kv: [Mc; 6326], &((&t6絩ru卒oLy, i64, &f64, u64), fn(Ak: [o7k81A6; 0x5Ad0Cc], OmNaGmqc: [g63凈N;  223], bOars,), [c6eFq8M;  636353.456], [b; 271]), f3,) -> [HrH70sp; 0d447231]", |x|
        make_type!(fn ret 0:182 paren 2:159 [
            make_type!(x fp named 3:18 #x3Kv 3:6
                make_type!(array 9:18
                    make_type!(x simple 10:11 #Mc),
                    make_expr!(i32 6326 14:17))),
            make_type!(fp 21:153
                make_type!(ref 21:153 make_type!(tuple 22:153
                    make_type!(tuple 23:54
                        make_type!(ref 24:37
                            make_type!(x simple 25:37 #t6絩ru卒oLy)),
                        make_type!(prim 40:42 I64),
                        make_type!(ref 45:48
                            make_type!(prim 46:48 F64)),
                        make_type!(prim 51:53 U64)),
                    make_type!(fn 57:118 paren 59:118 [
                        make_type!(x fp named 60:82 #Ak 60:61
                            make_type!(array 64:82
                                make_type!(x simple 65:71 #o7k81A6),
                                make_expr!(i32 0x5Ad0Cc 74:81))),
                        make_type!(x fp named 85:109 #OmNaGmqc 85:92
                            make_type!(array 95:109
                                make_type!(x simple 96:102 #g63凈N),
                                make_expr!(i32 223 106:108))),
                        make_type!(fp 112:116
                            make_type!(x simple 112:116 #bOars)),
                    ]),
                    make_type!(array 121:142
                        make_type!(x simple 122:128 #c6eFq8M),
                        make_expr!(r64 636353.4560000001 132:141)),
                    make_type!(array 145:152
                        make_type!(x simple 146:146 #b),
                        make_expr!(i32 271 149:151))))),
            make_type!(fp 156:157
                make_type!(x simple 156:157 #f3)),
        ], make_type!(array 164:182
            make_type!(x simple 165:171 #HrH70sp),
            make_expr!(i32 447231 174:181)))
    }
}

#[test]
fn parse_ref_type() {

    // this is some what void*
    case!{ parse_type_ref "&()", |_|
        make_type!(ref 0:2
            make_type!(tuple 1:2))
    }

    // split and and
    case!{ parse_type_ref "&&i32", |_|
        make_type!(ref 0:4
            make_type!(ref 1:4
                make_type!(prim 2:4 I32)))
    }

    //      0123456789
    case!{ parse_type_ref "&[sc; 207]", |x|
        make_type!(ref 0:9
            make_type!(array 1:9
                make_type!(x simple 2:3 #sc),
                make_expr!(i32 207 6:8))),
    }
}

#[test]
pub fn parse_path() {

    case!{ parse_type_path "custom_type", |x|
        make_path!(0:10
            make_path!(x segment simple 0:10 #custom_type)),
    }
    case!{ parse_value_path "custom_type", |x|
        make_path!(0:10
            make_path!(x segment simple 0:10 #custom_type)),
    }

    // // now plain_type is only tested but not tested
    //      0         1         2         3
    //      012345678901234567890123456789012345678
    case!{ parse_type_path "ffc::syntax::plain_type::type_ref_parse", |x|
        make_path!(0:38
            make_path!(x segment simple 0:2 #ffc),
            make_path!(x segment simple 5:10 #syntax),
            make_path!(x segment simple 13:22 #plain_type),
            make_path!(x segment simple 25:38 #type_ref_parse))
    }
    case!{ parse_value_path "ffc::syntax::plain_type::type_ref_parse", |x|
        make_path!(0:38
            make_path!(x segment simple 0:2 #ffc),
            make_path!(x segment simple 5:10 #syntax),
            make_path!(x segment simple 13:22 #plain_type),
            make_path!(x segment simple 25:38 #type_ref_parse))
    }

    // single colon
    case!{ parse_type_path "ffc:syntax:plain_type:type_ref_parse", |x|
        make_path!(0:35
            make_path!(x segment simple 0:2 #ffc),
            make_path!(x segment simple 4:9 #syntax),
            make_path!(x segment simple 11:20 #plain_type),
            make_path!(x segment simple 22:35 #type_ref_parse)),
        |e| {
            e.emit(strings::ExpectDoubleColonMeetSingleColon).span(Span::new(3, 3));
            e.emit(strings::ExpectDoubleColonMeetSingleColon).span(Span::new(10, 10));
            e.emit(strings::ExpectDoubleColonMeetSingleColon).span(Span::new(21, 21));
        }
    }
    case!{ parse_value_path "ffc:syntax:plain_type:type_ref_parse", |x|
        make_path!(0:35
            make_path!(x segment simple 0:2 #ffc),
            make_path!(x segment simple 4:9 #syntax),
            make_path!(x segment simple 11:20 #plain_type),
            make_path!(x segment simple 22:35 #type_ref_parse)),
        |e| {
            e.emit(strings::ExpectDoubleColonMeetSingleColon).span(Span::new(3, 3));
            e.emit(strings::ExpectDoubleColonMeetSingleColon).span(Span::new(10, 10));
            e.emit(strings::ExpectDoubleColonMeetSingleColon).span(Span::new(21, 21));
        }
    }

    case!{ parse_type_path "::a", |x|
        make_path!(0:2
            make_path!(segment global),
            make_path!(x segment simple 2:2 #a))
    }
    case!{ parse_value_path "::a", |x|
        make_path!(0:2
            make_path!(segment global),
            make_path!(x segment simple 2:2 #a))
    }

    //      0         1         2         3
    //      012345678901234567890123456789012345678
    case!{ parse_type_path "::ffc::syntax::plain_type::type_ref_parse", |x|
        make_path!(0:40
            make_path!(segment global),
            make_path!(x segment simple 2:4 #ffc),
            make_path!(x segment simple 7:12 #syntax),
            make_path!(x segment simple 15:24 #plain_type),
            make_path!(x segment simple 27:40 #type_ref_parse))
    }
    case!{ parse_value_path "::ffc::syntax::plain_type::type_ref_parse", |x|
        make_path!(0:40
            make_path!(segment global),
            make_path!(x segment simple 2:4 #ffc),
            make_path!(x segment simple 7:12 #syntax),
            make_path!(x segment simple 15:24 #plain_type),
            make_path!(x segment simple 27:40 #type_ref_parse))
    }

    // TODO test Unexpected
    // case!{ parse_type_path "<a as a>", None, |e| }

    //                      0         1         2         3         4         5         6
    //                      012345678901234567890123456789012345678901234567890123456789012345678
    case!{ parse_type_path "<::ffc::syntax::PlainType as ffc::syntax::prelude::Parser<F>>::Output", |x|
        make_path!(0:68
            make_path!(segment cast 0:60
                make_type!(path 1:24
                    make_path!(segment global),
                    make_path!(x segment simple 3:5 #ffc),
                    make_path!(x segment simple 8:13 #syntax),
                    make_path!(x segment simple 16:24 #PlainType)),
                make_type!(path 29:59
                    make_path!(x segment simple 29:31 #ffc),
                    make_path!(x segment simple 34:39 #syntax),
                    make_path!(x segment simple 42:48 #prelude),
                    make_path!(x segment generic 51:59 #Parser 51:56 quote 57:59
                        make_type!(x simple 58:58 #F)))),
            make_path!(x segment simple 63:68 #Output)),
    }
    case!{ parse_value_path "<::ffc::syntax::PlainType as ffc::syntax::prelude::Parser<F>>::Output", |x|
        make_path!(0:68
            make_path!(segment cast 0:60
                make_type!(path 1:24
                    make_path!(segment global),
                    make_path!(x segment simple 3:5 #ffc),
                    make_path!(x segment simple 8:13 #syntax),
                    make_path!(x segment simple 16:24 #PlainType)),
                make_type!(path 29:59
                    make_path!(x segment simple 29:31 #ffc),
                    make_path!(x segment simple 34:39 #syntax),
                    make_path!(x segment simple 42:48 #prelude),
                    make_path!(x segment generic 51:59 #Parser 51:56 quote 57:59
                        make_type!(x simple 58:58 #F)))),
            make_path!(x segment simple 63:68 #Output)),
    }

    //                      01234567890123456
    case!{ parse_type_path "<a as a>::a::a::a", |x|
        make_path!(0:16
            make_path!(segment cast 0:7
                make_type!(path 1:1
                    make_path!(x segment simple 1:1 #a)),
                make_type!(path 6:6
                    make_path!(x segment simple 6:6 #a))),
            make_path!(x segment simple 10:10 #a),
            make_path!(x segment simple 13:13 #a),
            make_path!(x segment simple 16:16 #a))
    }
    case!{ parse_value_path "<a as a>::a::a::a", |x|
        make_path!(0:16
            make_path!(segment cast 0:7
                make_type!(path 1:1
                    make_path!(x segment simple 1:1 #a)),
                make_type!(path 6:6
                    make_path!(x segment simple 6:6 #a))),
            make_path!(x segment simple 10:10 #a),
            make_path!(x segment simple 13:13 #a),
            make_path!(x segment simple 16:16 #a))
    }

    // empty type list is recognized in syntax
    case!{ parse_type_path "a<>", |x|
        make_path!(0:2
            make_path!(x segment generic 0:2 #a 0:0 quote 1:2)),
        |e| e.emit(strings::EmptyTypeList).span(Span::new(1, 2))
    }
    case!{ parse_value_path "a::<>", |x|
        make_path!(0:4
            make_path!(x segment generic 0:4 #a 0:0 quote 3:4)),
        |e| e.emit(strings::EmptyTypeList).span(Span::new(3, 4))
    }

    // include single comma
    case!{ parse_type_path "a<,>", |x|
        make_path!(0:3
            make_path!(x segment generic 0:3 #a 0:0 quote 1:3)),
        |e| e.emit(strings::EmptyTypeList).span(Span::new(1, 3))
    }
    case!{ parse_value_path "a::<,>", |x|
        make_path!(0:5
            make_path!(x segment generic 0:5 #a 0:0 quote 3:5)),
        |e| e.emit(strings::EmptyTypeList).span(Span::new(3, 5))
    }

    //                      0123456789
    case!{ parse_type_path "a<b, c, d>", |x|
        make_path!(0:9
            make_path!(x segment generic 0:9 #a 0:0 quote 1:9
                make_type!(path 2:2
                    make_path!(x segment simple 2:2 #b)),
                make_type!(path 5:5
                    make_path!(x segment simple 5:5 #c)),
                make_type!(path 8:8
                    make_path!(x segment simple 8:8 #d))))
    }
    //                       012345678901
    case!{ parse_value_path "a::<b, c, d>", |x|
        make_path!(0:11
            make_path!(x segment generic 0:11 #a 0:0 quote 3:11
                make_type!(path 4:4
                    make_path!(x segment simple 4:4 #b)),
                make_type!(path 7:7
                    make_path!(x segment simple 7:7 #c)),
                make_type!(path 10:10
                    make_path!(x segment simple 10:10 #d))))
    }

    // trailing comma
    //                      0123456789
    case!{ parse_type_path "a<b, c, d,>", |x|
        make_path!(0:10
            make_path!(x segment generic 0:10 #a 0:0 quote 1:10
                make_type!(path 2:2
                    make_path!(x segment simple 2:2 #b)),
                make_type!(path 5:5
                    make_path!(x segment simple 5:5 #c)),
                make_type!(path 8:8
                    make_path!(x segment simple 8:8 #d))))
    }
    //                       012345678901
    case!{ parse_value_path "a::<b, c, d, >", |x|
        make_path!(0:13
            make_path!(x segment generic 0:13 #a 0:0 quote 3:13
                make_type!(path 4:4
                    make_path!(x segment simple 4:4 #b)),
                make_type!(path 7:7
                    make_path!(x segment simple 7:7 #c)),
                make_type!(path 10:10
                    make_path!(x segment simple 10:10 #d))))
    }

    // ::< is allowed in type path
    case!{ parse_type_path "a::<b, c, d>", |x|
        make_path!(0:11
            make_path!(x segment generic 0:11 #a 0:0 quote 3:11
                make_type!(path 4:4
                    make_path!(x segment simple 4:4 #b)),
                make_type!(path 7:7
                    make_path!(x segment simple 7:7 #c)),
                make_type!(path 10:10
                    make_path!(x segment simple 10:10 #d)))),
        |e| e.emit(format!("{} `::`", strings::ExpectLtMeet)).span(Span::new(1, 2))
    }

    // single colon
    case!{ parse_value_path "a:<b, c, d>", |x|
        make_path!(0:10
            make_path!(x segment generic 0:10 #a 0:0 quote 2:10
                make_type!(x simple 3:3 #b),
                make_type!(x simple 6:6 #c),
                make_type!(x simple 9:9 #d))),
        |e| e.emit(strings::ExpectDoubleColonMeetSingleColon).span(Span::new(1, 1))
    }

    // single colon and allow colon in type path // seems not happy :<
    case!{ parse_type_path "a:<b, c, d>", |x|
        make_path!(0:10
            make_path!(x segment generic 0:10 #a 0:0 quote 2:10
                make_type!(path 3:3
                    make_path!(x segment simple 3:3 #b)),
                make_type!(path 6:6
                    make_path!(x segment simple 6:6 #c)),
                make_type!(path 9:9
                    make_path!(x segment simple 9:9 #d)))),
        |e| {
            e.emit(format!("{} `:`", strings::ExpectLtMeet)).span(Span::new(1, 1));
            e.emit(strings::ExpectDoubleColonMeetSingleColon).span(Span::new(1, 1));
        }
    }

    //                 0         1         2      v this is not part of name
    //                 012345678901234567890123456
    case!{ parse_value_path "::abc::def::<ghi, jkl>::mno<", |x|
        make_path!(0:26
            make_path!(segment global),
            make_path!(x segment simple 2:4 #abc),
            make_path!(x segment generic 7:21 #def 7:9 quote 12:21
                make_type!(x simple 13:15 #ghi),
                make_type!(x simple 18:20 #jkl)),
            make_path!(x segment simple 24:26 #mno))
    }

    //                      0123456789012
    case!{ parse_type_path "a::b::c<d, e>", |x|
        make_path!(0:12
            make_path!(x segment simple 0:0 #a),
            make_path!(x segment simple 3:3 #b),
            make_path!(x segment generic 6:12 #c 6:6 quote 7:12
                make_type!(x simple 8:8 #d),
                make_type!(x simple 11:11 #e)))
    }
    //                       0123456789012345
    case!{ parse_value_path "a::b::c::<d, e,>", |x|
        make_path!(0:15
            make_path!(x segment simple 0:0 #a),
            make_path!(x segment simple 3:3 #b),
            make_path!(x segment generic 6:15 #c 6:6 quote 9:15
                make_type!(x simple 10:10 #d),
                make_type!(x simple 13:13 #e)))
    }

    //                      0123456789012345678901
    case!{ parse_type_path "a::b<c, d>::e::f<g, h>", |x|
        make_path!(0:21
            make_path!(x segment simple 0:0 #a),
            make_path!(x segment generic 3:9 #b 3:3 quote 4:9
                make_type!(x simple 5:5 #c),
                make_type!(x simple 8:8 #d)),
            make_path!(x segment simple 12:12 #e),
            make_path!(x segment generic 15:21 #f 15:15 quote 16:21
                make_type!(x simple 17:17 #g),
                make_type!(x simple 20:20 #h)))
    }
    //                       01234567890123456789012345
    case!{ parse_value_path "a::b::<c, d>::e::f::<g, h>", |x|
        make_path!(0:25
            make_path!(x segment simple 0:0 #a),
            make_path!(x segment generic 3:11 #b 3:3 quote 6:11
                make_type!(x simple 7:7 #c),
                make_type!(x simple 10:10 #d)),
            make_path!(x segment simple 14:14 #e),
            make_path!(x segment generic 17:25 #f 17:17 quote 20:25
                make_type!(x simple 21:21 #g),
                make_type!(x simple 24:24 #h)))
    }

    // split shift right
    case!{ parse_type_path "a<b<c>>", |x|
        make_path!(0:6
            make_path!(x segment generic 0:6 #a 0:0 quote 1:6
                make_type!(path 2:5
                    make_path!(x segment generic 2:5 #b 2:2 quote 3:5
                        make_type!(x simple 4:4 #c))))),
    }
    //                       012345678
    case!{ parse_value_path "a::<b<c>>", |x|
        make_path!(0:8
            make_path!(x segment generic 0:8 #a 0:0 quote 3:8
                make_type!(path 4:7
                    make_path!(x segment generic 4:7 #b 4:4 quote 5:7
                        make_type!(x simple 6:6 #c))))
        ),
    }
    // inside angle bracket is type not value
    case!{ parse_value_path "a::<b::<c>>", |x|
        make_path!(0:10
            make_path!(x segment generic 0:10 #a 0:0 quote 3:10
                make_type!(path 4:9
                    make_path!(x segment generic 4:9 #b 4:4 quote 7:9
                        make_type!(x simple 8:8 #c))))),
        |e| e.emit(format!("{} `::`", strings::ExpectLtMeet)).span(Span::new(5, 6)),
    }

    // type cast segment in front of angle bracket quoted type list requires split shift left
    //      01234567890123
    case!{ parse_type_path "a<<a as a>::a>", |x|
        make_path!(0:13
            make_path!(x segment generic 0:13 #a 0:0 quote 1:13
                make_type!(path 2:12
                    make_path!(segment cast 2:9
                        make_type!(x simple 3:3 #a),
                        make_type!(x simple 8:8 #a)),
                    make_path!(x segment simple 12:12 #a)))),
    }
    case!{ parse_value_path "a::<<a as a>::a>", |x|
        make_path!(0:15
            make_path!(x segment generic 0:15 #a 0:0 quote 3:15
                make_type!(path 4:14
                    make_path!(segment cast 4:11
                        make_type!(x simple 5:5 #a),
                        make_type!(x simple 10:10 #a)),
                    make_path!(x segment simple 14:14 #a)))),
    }

    // single quote + ltlt
    case!{ parse_type_path "a:<<a as a>::a>", |x|
        make_path!(0:14
            make_path!(x segment generic 0:14 #a 0:0 quote 2:14
                make_type!(path 3:13
                    make_path!(segment cast 3:10
                        make_type!(x simple 4:4 #a),
                        make_type!(x simple 9:9 #a)),
                    make_path!(x segment simple 13:13 #a)))),
        |e| {
            e.emit(format!("{} `:`", strings::ExpectLtMeet)).span(Span::new(1, 1));
            e.emit(strings::ExpectDoubleColonMeetSingleColon).span(Span::new(1, 1));
        }
    }
    case!{ parse_value_path "a:<<a as a>::a>", |x|
        make_path!(0:14
            make_path!(x segment generic 0:14 #a 0:0 quote 2:14
                make_type!(path 3:13
                    make_path!(segment cast 3:10
                        make_type!(x simple 4:4 #a),
                        make_type!(x simple 9:9 #a)),
                    make_path!(x segment simple 13:13 #a)))),
        |e| e.emit(strings::ExpectDoubleColonMeetSingleColon).span(Span::new(1, 1)),
    }
}

#[test]
fn parse_type_ref() {
    // cases splitted into several functions
    // remain this here "type ref integrated test" from outside
}