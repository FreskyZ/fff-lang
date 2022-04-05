use super::*;

#[test]
fn parse_array_expr() {
    case!{ parse_array_expr "[a]",
        make_expr!(array 0:2
            make_name!(simple 1:1 #2)),
    }

    //                                   01234567
    case!{ parse_array_expr "[1, '2']",
        make_expr!(array 0:7
            make_expr!(i32 1 1:1),
            make_expr!(char 4:6 '2')),
    }
    //                                   01234567
    case!{ parse_array_expr "[1 + 1,]",
        make_expr!(array 0:7
            make_expr!(binary 1:5 Add 3:3
                make_expr!(i32 1 1:1),
                make_expr!(i32 1 5:5))),
    }

    case!{ parse_array_expr "[ , ]",
        make_expr!(array 0:4),
        errors make_errors!(e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(0, 4), strings::ArrayDefHere))
    }
}

#[test]
fn binary_expr_parse() {
    //                                     123456789012345
    case!{ parse_binary_expr "[1] * [2] / [3]",
        make_expr!(binary 0:14 Div 10:10
            make_expr!(binary 0:8 Mul 4:4
                make_expr!(array 0:2
                    make_expr!(i32 1 1:1)),
                make_expr!(array 6:8
                    make_expr!(i32 2 7:7))),
            make_expr!(array 12:14
                make_expr!(i32 3 13:13))),
    }
    //                                     0        1         2
    //                                     123456789012345678901
    case!{ parse_binary_expr "a * b / c + d % e - f",  // ((((a * b) / c) + (d % e)) - f)
        make_expr!(binary 0:20 Sub 18:18
            make_expr!(binary 0:16 Add 10:10
                make_expr!(binary 0:8 Div 6:6
                    make_expr!(binary 0:4 Mul 2:2
                        make_name!(simple 0:0 #2),
                        make_name!(simple 4:4 #3)),
                    make_name!(simple 8:8 #4)),
                make_expr!(binary 12:16 Rem 14:14
                    make_name!(simple 12:12 #5),
                    make_name!(simple 16:16 #6))),
            make_name!(simple 20:20 #7))
    }
    //                                     0        1         2         3
    //                                     1234567890123456789012345678901
    case!{ parse_binary_expr "a * b << h / c + d % e - f >> g", // (((a * b) << (((h / c) + (d % e)) - f)) >> g)
        make_expr!(binary 0:30 GtGt 27:28
            make_expr!(binary 0:25 LtLt 6:7
                make_expr!(binary 0:4 Mul 2:2
                    make_name!(simple 0:0 #2),
                    make_name!(simple 4:4 #3)),
                make_expr!(binary 9:25 Sub 23:23
                    make_expr!(binary 9:21 Add 15:15
                        make_expr!(binary 9:13 Div 11:11
                            make_name!(simple 9:9 #4),
                            make_name!(simple 13:13 #5)),
                        make_expr!(binary 17:21 Rem 19:19
                            make_name!(simple 17:17 #6),
                            make_name!(simple 21:21 #7))),
                    make_name!(simple 25:25 #8))),
            make_name!(simple 30:30 #9))
    }

    // This very huge test case it not useless:
    //     the operator priority impl in `with_test_str` is according to the parser_impl macro definition order
    //     my test case oracle is according to the comments on top of this file, which originally are copy and paste from c++ standard
    //     I accidently mistake the order of xor_expr and or_expr in parser_impl macros, this case help me find this
    // continue story: I changed name of the parsers and found the error not fixed
    //     then I find out that the last parameter of the macros, operator_category is the actual order definition
    //     only change that can I fix the bug
    //                                     0        1         2         3         4         5         6         7         8
    //                                     1234567890123456789012345678901234567890123456789012345678901234567890123456789012345
    case!{ parse_binary_expr "a * b << h / c + d % e - f >> g > h * i < j << k >= m && n || o & p | q ^ r != s == t",
        // */%, +-, <><=>=, <<>>, &, ^, |, ==!=, &&, ||
        // ((((((a * b) << (((h / c) + (d % e)) - f)) >> ((g > (h * i)) < j)) << (k >= m)) && n) || ((((o & p) | (q ^ r)) != s) == t))
        make_expr!(binary 0:84 OrOr 59:60
            make_expr!(binary 0:57 AndAnd 54:55
                make_expr!(binary 0:52 LtLt 44:45
                    make_expr!(binary 0:42 GtGt 27:28
                        make_expr!(binary 0:25 LtLt 6:7
                            make_expr!(binary 0:4 Mul 2:2
                                make_name!(simple 0:0 #2),
                                make_name!(simple 4:4 #3)),
                            make_expr!(binary 9:25 Sub 23:23
                                make_expr!(binary 9:21 Add 15:15
                                    make_expr!(binary 9:13 Div 11:11
                                        make_name!(simple 9:9 #4),
                                        make_name!(simple 13:13 #5)),
                                    make_expr!(binary 17:21 Rem 19:19
                                        make_name!(simple 17:17 #6),
                                        make_name!(simple 21:21 #7))),
                                make_name!(simple 25:25 #8))),
                        make_expr!(binary 30:42 Lt 40:40
                            make_expr!(binary 30:38 Gt 32:32
                                make_name!(simple 30:30 #9),
                                make_expr!(binary 34:38 Mul 36:36
                                    make_name!(simple 34:34 #4),
                                    make_name!(simple 38:38 #10))),
                            make_name!(simple 42:42 #11))),
                    make_expr!(binary 47:52 GtEq 49:50
                        make_name!(simple 47:47 #12),
                        make_name!(simple 52:52 #13))),
                make_name!(simple 57:57 #14)),
            make_expr!(binary 62:84 EqEq 81:82
                make_expr!(binary 62:79 NotEq 76:77
                    make_expr!(binary 62:74 Or 68:68
                        make_expr!(binary 62:66 And 64:64
                            make_name!(simple 62:62 #15),
                            make_name!(simple 66:66 #16)),
                        make_expr!(binary 70:74 Caret 72:72
                            make_name!(simple 70:70 #17),
                            make_name!(simple 74:74 #18))),
                    make_name!(simple 79:79 #19)),
                make_name!(simple 84:84 #20)))
    }
    //                                     1234567890
    case!{ parse_binary_expr "a & b == c", // ((a & b) == c)
        make_expr!(binary 0:9 EqEq 6:7
            make_expr!(binary 0:4 And 2:2
                make_name!(simple 0:0 #2),
                make_name!(simple 4:4 #3)),
            make_name!(simple 9:9 #4))
    }

    // 1 + 2 << 3 => (1 + 2) << 3, don't want this
    // cout << 5 + 6 => cout << (5 + 6), want this

    // program generated random tests
    //                                     0        1         2         3
    //                                     1234567890123456789012345678901234
    case!{ parse_binary_expr "0 + 6 ^ 3 & 3 / 3 - 8 && 2 & 0 + 6", // (((0 + 6) ^ (3 & ((3 / 3) - 8))) && (2 & (0 + 6)))
        make_expr!(binary 0:33 AndAnd 22:23
            make_expr!(binary 0:20 Caret 6:6
                make_expr!(binary 0:4 Add 2:2
                    make_expr!(i32 0 0:0),
                    make_expr!(i32 6 4:4)),
                make_expr!(binary 8:20 And 10:10
                    make_expr!(i32 3 8:8),
                    make_expr!(binary 12:20 Sub 18:18
                        make_expr!(binary 12:16 Div 14:14
                            make_expr!(i32 3 12:12),
                            make_expr!(i32 3 16:16)),
                        make_expr!(i32 8 20:20)))),
            make_expr!(binary 25:33 And 27:27
                make_expr!(i32 2 25:25),
                make_expr!(binary 29:33 Add 31:31
                    make_expr!(i32 0 29:29),
                    make_expr!(i32 6 33:33))))
    }
    //                                     0        1         2         3         4         5         6         7
    //                                     1234567890123456789012345678901234567890123456789012345678901234567890
    case!{ parse_binary_expr "7 > 1 | 0 % 8 | 1 % 7 * 3 % 6 == 1 >> 8 % 3 ^ 6 << 0 ^ 2 >> 6 || 1 - 0",
        // (((((7 > 1) | (0 % 8)) | (((1 % 7) * 3) % 6)) == (((1 >> (8 % 3)) ^ (6 << 0)) ^ (2 >> 6))) || (1 - 0))
        make_expr!(binary 0:69 OrOr 62:63
            make_expr!(binary 0:60 EqEq 30:31
                make_expr!(binary 0:28 Or 14:14
                    make_expr!(binary 0:12 Or 6:6
                        make_expr!(binary 0:4 Gt 2:2
                            make_expr!(i32 7 0:0),
                            make_expr!(i32 1 4:4)),
                        make_expr!(binary 8:12 Rem 10:10
                            make_expr!(i32 0 8:8),
                            make_expr!(i32 8 12:12))),
                    make_expr!(binary 16:28 Rem 26:26
                        make_expr!(binary 16:24 Mul 22:22
                            make_expr!(binary 16:20 Rem 18:18
                                make_expr!(i32 1 16:16),
                                make_expr!(i32 7 20:20)),
                            make_expr!(i32 3 24:24)),
                        make_expr!(i32 6 28:28))),
                make_expr!(binary 33:60 Caret 53:53
                    make_expr!(binary 33:51 Caret 44:44
                        make_expr!(binary 33:42 GtGt 35:36
                            make_expr!(i32 1 33:33),
                            make_expr!(binary 38:42 Rem 40:40
                                make_expr!(i32 8 38:38),
                                make_expr!(i32 3 42:42))),
                        make_expr!(binary 46:51 LtLt 48:49
                            make_expr!(i32 6 46:46),
                            make_expr!(i32 0 51:51))),
                    make_expr!(binary 55:60 GtGt 57:58
                        make_expr!(i32 2 55:55),
                        make_expr!(i32 6 60:60)))),
            make_expr!(binary 65:69 Sub 67:67
                make_expr!(i32 1 65:65),
                make_expr!(i32 0 69:69)))
    }
    //                                     0        1         2         3         4         5         6
    //                                     1234567890123456789012345678901234567890123456789012345678901
    case!{ parse_binary_expr "7 >> 3 == 8 / 1 && 6 == 1 <= 3 % 6 ^ 3 - 1 - 2 >> 7 || 1 >= 1",
        // */%, +-, <><=>=, <<>>, &, ^, |, ==!=, &&, ||
        // ((((7 >> 3) == (8 / 1)) && (6 == ((1 <= (3 % 6)) ^ (((3 - 1) - 2) >> 7)))) || (1 >= 1))
        make_expr!(binary 0:60 OrOr 52:53
            make_expr!(binary 0:50 AndAnd 16:17
                make_expr!(binary 0:14 EqEq 7:8
                    make_expr!(binary 0:5 GtGt 2:3
                        make_expr!(i32 7 0:0),
                        make_expr!(i32 3 5:5)),
                    make_expr!(binary 10:14 Div 12:12
                        make_expr!(i32 8 10:10),
                        make_expr!(i32 1 14:14))),
                make_expr!(binary 19:50 EqEq 21:22
                    make_expr!(i32 6 19:19),
                    make_expr!(binary 24:50 Caret 35:35
                        make_expr!(binary 24:33 LtEq 26:27
                            make_expr!(i32 1 24:24),
                            make_expr!(binary 29:33 Rem 31:31
                                make_expr!(i32 3 29:29),
                                make_expr!(i32 6 33:33))),
                        make_expr!(binary 37:50 GtGt 47:48
                            make_expr!(binary 37:45 Sub 43:43
                                make_expr!(binary 37:41 Sub 39:39
                                    make_expr!(i32 3 37:37),
                                    make_expr!(i32 1 41:41)),
                                make_expr!(i32 2 45:45)),
                            make_expr!(i32 7 50:50))))),
            make_expr!(binary 55:60 GtEq 57:58
                make_expr!(i32 1 55:55),
                make_expr!(i32 1 60:60)))
    }
    //                                     0
    //                                     123456
    case!{ parse_binary_expr "4 >> 7",
        make_expr!(binary 0:5 GtGt 2:3
            make_expr!(i32 4 0:0),
            make_expr!(i32 7 5:5))
    }
    //                                     0        1         2         3         4         5         6
    //                                     12345678901234567890123456789012345678901234567890123456789012345
    case!{ parse_binary_expr "8 & 0 | 7 + 7 | 7 * 0 && 1 - 2 * 3 | 0 - 7 >= 6 >> 5 % 5 || 5 % 3",
        // (((((8 & 0) | (7 + 7)) | (7 * 0)) && ((1 - (2 * 3)) | (((0 - 7) >= 6) >> (5 % 5)))) || (5 % 3))
        make_expr!(binary 0:64 OrOr 57:58
            make_expr!(binary 0:55 AndAnd 22:23
                make_expr!(binary 0:20 Or 14:14
                    make_expr!(binary 0:12 Or 6:6
                        make_expr!(binary 0:4 And 2:2
                            make_expr!(i32 8 0:0),
                            make_expr!(i32 0 4:4)),
                        make_expr!(binary 8:12 Add 10:10
                            make_expr!(i32 7 8:8),
                            make_expr!(i32 7 12:12))),
                    make_expr!(binary 16:20 Mul 18:18
                        make_expr!(i32 7 16:16),
                        make_expr!(i32 0 20:20))),
                make_expr!(binary 25:55 Or 35:35
                    make_expr!(binary 25:33 Sub 27:27
                        make_expr!(i32 1 25:25),
                        make_expr!(binary 29:33 Mul 31:31
                            make_expr!(i32 2 29:29),
                            make_expr!(i32 3 33:33))),
                    make_expr!(binary 37:55 GtGt 48:49
                        make_expr!(binary 37:46 GtEq 43:44
                            make_expr!(binary 37:41 Sub 39:39
                                make_expr!(i32 0 37:37),
                                make_expr!(i32 7 41:41)),
                            make_expr!(i32 6 46:46)),
                        make_expr!(binary 51:55 Rem 53:53
                            make_expr!(i32 5 51:51),
                            make_expr!(i32 5 55:55))))),
            make_expr!(binary 60:64 Rem 62:62
                make_expr!(i32 5 60:60),
                make_expr!(i32 3 64:64)))
    }
    //                                     0        1         2         3         4         5         6
    //                                     12345678901234567890123456789012345678901234567890123456789012345678
    case!{ parse_binary_expr "3 <= 2 + 4 <= 5 && 3 < 3 + 2 >> 1 * 2 & 8 && 1 >= 1 < 0 || 6 < 4 * 4",
        // (((((3 <= (2 + 4)) <= 5) && (((3 < (3 + 2)) >> (1 * 2)) & 8)) && ((1 >= 1) < 0)) || (6 < (4 * 4)))
        make_expr!(binary 0:67 OrOr 56:57
            make_expr!(binary 0:54 AndAnd 42:43
                make_expr!(binary 0:40 AndAnd 16:17
                    make_expr!(binary 0:14 LtEq 11:12
                        make_expr!(binary 0:9 LtEq 2:3
                            make_expr!(i32 3 0:0),
                            make_expr!(binary 5:9 Add 7:7
                                make_expr!(i32 2 5:5),
                                make_expr!(i32 4 9:9))),
                        make_expr!(i32 5 14:14)),
                    make_expr!(binary 19:40 And 38:38
                        make_expr!(binary 19:36 GtGt 29:30
                            make_expr!(binary 19:27 Lt 21:21
                                make_expr!(i32 3 19:19),
                                make_expr!(binary 23:27 Add 25:25
                                    make_expr!(i32 3 23:23),
                                    make_expr!(i32 2 27:27))),
                            make_expr!(binary 32:36 Mul 34:34
                                make_expr!(i32 1 32:32),
                                make_expr!(i32 2 36:36))),
                        make_expr!(i32 8 40:40))),
                make_expr!(binary 45:54 Lt 52:52
                    make_expr!(binary 45:50 GtEq 47:48
                        make_expr!(i32 1 45:45),
                        make_expr!(i32 1 50:50)),
                    make_expr!(i32 0 54:54))),
            make_expr!(binary 59:67 Lt 61:61
                make_expr!(i32 6 59:59),
                make_expr!(binary 63:67 Mul 65:65
                    make_expr!(i32 4 63:63),
                    make_expr!(i32 4 67:67))))
    }
    //                                     0        1         2
    //                                     12345678901234567890
    case!{ parse_binary_expr "5 >= 6 | 3 == 4 && 3",
        // ((((5 >= 6) | 3) == 4) && 3)
        make_expr!(binary 0:19 AndAnd 16:17
            make_expr!(binary 0:14 EqEq 11:12
                make_expr!(binary 0:9 Or 7:7
                    make_expr!(binary 0:5 GtEq 2:3
                        make_expr!(i32 5 0:0),
                        make_expr!(i32 6 5:5)),
                    make_expr!(i32 3 9:9)),
                make_expr!(i32 4 14:14)),
            make_expr!(i32 3 19:19))
    }
    //                                     0        1         2         3         4         5         6         7
    //                                     1234567890123456789012345678901234567890123456789012345678901234567890123456
    case!{ parse_binary_expr "6 && 7 >> 8 && 0 / 8 * 7 + 5 < 5 / 5 >> 5 - 1 >= 6 > 8 | 6 >> 5 > 2 + 1 || 0",
        // */%, +-, <><=>=, <<>>, &, ^, |, ==!=, &&, ||
        // (((6 && (7 >> 8)) && ((((((0 / 8) * 7) + 5) < (5 / 5)) >> (((5 - 1) >= 6) > 8)) | (6 >> (5 > (2 + 1))))) || 0)
        make_expr!(binary 0:75 OrOr 72:73
            make_expr!(binary 0:70 AndAnd 12:13
                make_expr!(binary 0:10 AndAnd 2:3
                    make_expr!(i32 6 0:0),
                    make_expr!(binary 5:10 GtGt 7:8
                        make_expr!(i32 7 5:5),
                        make_expr!(i32 8 10:10))),
                make_expr!(binary 15:70 Or 55:55
                    make_expr!(binary 15:53 GtGt 37:38
                        make_expr!(binary 15:35 Lt 29:29
                            make_expr!(binary 15:27 Add 25:25
                                make_expr!(binary 15:23 Mul 21:21
                                    make_expr!(binary 15:19 Div 17:17
                                        make_expr!(i32 0 15:15),
                                        make_expr!(i32 8 19:19)),
                                    make_expr!(i32 7 23:23)),
                                make_expr!(i32 5 27:27)),
                            make_expr!(binary 31:35 Div 33:33
                                make_expr!(i32 5 31:31),
                                make_expr!(i32 5 35:35))),
                        make_expr!(binary 40:53 Gt 51:51
                            make_expr!(binary 40:49 GtEq 46:47
                                make_expr!(binary 40:44 Sub 42:42
                                    make_expr!(i32 5 40:40),
                                    make_expr!(i32 1 44:44)),
                                make_expr!(i32 6 49:49)),
                            make_expr!(i32 8 53:53))),
                    make_expr!(binary 57:70 GtGt 59:60
                        make_expr!(i32 6 57:57),
                        make_expr!(binary 62:70 Gt 64:64
                            make_expr!(i32 5 62:62),
                            make_expr!(binary 66:70 Add 68:68
                                make_expr!(i32 2 66:66),
                                make_expr!(i32 1 70:70)))))),
            make_expr!(i32 0 75:75))
    }

    case!{ parse_binary_expr "1<2>3",
        make_expr!(binary 0:4 Gt 3:3
            make_expr!(binary 0:2 Lt 1:1
                make_expr!(i32 1 0:0),
                make_expr!(i32 2 2:2)),
            make_expr!(i32 3 4:4)
        ), errors make_errors!(e: e.emit(strings::MaybeGeneric).span(Span::new(1, 1)).span(Span::new(3, 3)).help(strings::MaybeGenericHelp))
    }
}

#[test]
fn expr_list_parse() {

    case!{ notast parse_expr_list "[1, 2, 3]",
        ExprListParseResult::Normal(Span::new(0, 8), ExprList{ items: vec![
            make_expr!(i32 1 1:1),
            make_expr!(i32 2 4:4),
            make_expr!(i32 3 7:7),
        ] })
    }
    
    case!{ notast parse_expr_list "(1, 2, 3,)", 
        ExprListParseResult::EndWithComma(Span::new(0, 9), ExprList{ items: vec![
            make_expr!(i32 1 1:1),
            make_expr!(i32 2 4:4),
            make_expr!(i32 3 7:7),
        ] })
    }

    case!{ notast parse_expr_list "[]", 
        ExprListParseResult::Empty(Span::new(0, 1))
    }

    case!{ notast parse_expr_list "{,}",
        ExprListParseResult::SingleComma(Span::new(0, 2))
    }
}

#[test]
fn parse_expr() {

    case!{ parse_expr "\"abc\"", make_expr!(str #2 0:4) }
    case!{ parse_expr "0xfffu64", make_expr!(u64 0xFFF 0:7) }
    case!{ parse_expr "'f'", make_expr!(char 0:2 'f') }
    case!{ parse_expr "true", make_expr!(true 0:3) }
    case!{ parse_expr "binary_expr", make_name!(simple 0:10 #2) }
    case!{ parse_expr "(  )", make_expr!(unit 0:3) }
    
    // Case from fn_def_parse
    case!{ parse_expr "println(this)", 
        make_expr!(call 0:12 paren 7:12
            make_name!(simple 0:6 #2),
            make_name!(simple 8:11 #3))
    }

    // Very very legacy expr tests which originally contains ExpressionBase and ExpressionOperator
    // update them to current syntax to help improve coverage and make me happy

    // Unit
    case!{ parse_expr "(1)",
        make_expr!(paren 0:2
            make_expr!(i32 1 1:1))
    }
    // I can see future of Ok(())! 
    case!{ parse_expr "(())",
        make_expr!(paren 0:3
            make_expr!(unit 1:2))
    }

    // Tuple def
    case!{ parse_expr "(a, b)",
        make_expr!(tuple 0:5
            make_name!(simple 1:1 #2),
            make_name!(simple 4:4 #3))
    }        //  12345678901
    case!{ parse_expr "(1, 2, 3, )",
        make_expr!(tuple 0:10
            make_expr!(i32 1 1:1),
            make_expr!(i32 2 4:4),
            make_expr!(i32 3 7:7))
    }

    // Array def
    case!{ parse_expr "[a]",
        make_expr!(array 0:2
            make_name!(simple 1:1 #2))
    }        //  12345678
    case!{ parse_expr "[1, 2, ]",
        make_expr!(array 0:7
            make_expr!(i32 1 1:1),
            make_expr!(i32 2 4:4))
    }
    case!{ parse_expr "[]", make_expr!(array 0:1) }

    // Member access
    case!{ parse_expr "a.b",
        make_expr!(member 0:2 dot 1:1
            make_name!(simple 0:0 #2),
            make_expr!(member name 2:2 #3)),
    }

    case!{ parse_expr "a.0",
        make_expr!(member 0:2 dot 1:1
            make_name!(simple 0:0 #2),
            make_expr!(member name 2:2 i32 0)),
    }

    // function call
    case!{ parse_expr "defg()",
        make_expr!(call 0:5 paren 4:5
            make_name!(simple 0:3 #2),)
    }
    case!{ parse_expr "deg(a)",
        make_expr!(call 0:5 paren 3:5
            make_name!(simple 0:2 #2),
            make_name!(simple 4:4 #3))
    }
    case!{ parse_expr "degg(a, b, )",
        make_expr!(call 0:11 paren 4:11
            make_name!(simple 0:3 #2),
            make_name!(simple 5:5 #3),
            make_name!(simple 8:8 #4))
    }
    //           0123456789
    case!{ parse_expr "abc.defg()",
        make_expr!(call 0:9 paren 8:9
            make_expr!(member 0:7 dot 3:3
                make_name!(simple 0:2 #2),
                make_expr!(member name 4:7 #3)),)
    }
    case!{ parse_expr "abc.deg(a)",
        make_expr!(call 0:9 paren 7:9
            make_expr!(member 0:6 dot 3:3
                make_name!(simple 0:2 #2),
                make_expr!(member name 4:6 #3)),
            make_name!(simple 8:8 #4))
    }        //  12345678901234
    case!{ parse_expr "1.degg(a, b, )",
        make_expr!(call 0:13 paren 6:13
            make_expr!(member 0:5 dot 1:1
                make_expr!(i32 1 0:0),
                make_expr!(member name 2:5 #2)),
            make_name!(simple 7:7 #3),
            make_name!(simple 10:10 #4))
    }   

    // get index       //  123456
    case!{ parse_expr "deg[a]",
        make_expr!(index 0:5 bracket 3:5
            make_name!(simple 0:2 #2),
            make_name!(simple 4:4 #3))
    }        //  123456789012
    case!{ parse_expr "degg[a, b, ]",
        make_expr!(index 0:11 bracket 4:11
            make_name!(simple 0:3 #2),
            make_name!(simple 5:5 #3),
            make_name!(simple 8:8 #4))
    }     

    //           123456
    case!{ parse_expr "2[3].a",
        make_expr!(member 0:5 dot 4:4
            make_expr!(index 0:3 bracket 1:3
                make_expr!(i32 2 0:0),
                make_expr!(i32 3 2:2)),
            make_expr!(member name 5:5 #2))
    }   //  1234567890123456
    case!{ parse_expr "print(233, ).bit",
        make_expr!(member 0:15 dot 12:12
            make_expr!(call 0:11 paren 5:11
                make_name!(simple 0:4 #2),
                make_expr!(i32 233 6:8)),
            make_expr!(member name 13:15 #3))
    }            //  12345678901234
    case!{ parse_expr "1.degg[a, b, ]",
        make_expr!(index 0:13 bracket 6:13
            make_expr!(member 0:5 dot 1:1
                make_expr!(i32 1 0:0),
                make_expr!(member name 2:5 #2)),
            make_name!(simple 7:7 #3),
            make_name!(simple 10:10 #4))
    }        

    case!{ parse_expr "!~!1[1]",
        make_expr!(unary 0:6 Not 0:0
            make_expr!(unary 1:6 Tilde 1:1
                make_expr!(unary 2:6 Not 2:2
                    make_expr!(index 3:6 bracket 4:6
                        make_expr!(i32 1 3:3),
                        make_expr!(i32 1 5:5)))))
    }

    //           1234567
    case!{ parse_expr "!!1",
        make_expr!(unary 0:2 Not 0:0
            make_expr!(unary 1:2 Not 1:1
                make_expr!(i32 1 2:2)))
    }

    // range
    case!{ parse_expr "..",
        make_expr!(range full 0:1)
    }

    case!{ parse_expr "..1 + 2",
        make_expr!(range right 0:6
            make_expr!(binary 2:6 Add 4:4
                make_expr!(i32 1 2:2),
                make_expr!(i32 2 6:6)))
    }

    case!{ parse_expr "xxx ..",
        make_expr!(range left 0:5
            make_name!(simple 0:2 #2))
    }

    case!{ parse_expr "1 + 2 .. [4, 5, 6][2]",
        make_expr!(range both 0:20 dotdot 6:7
            make_expr!(binary 0:4 Add 2:2
                make_expr!(i32 1 0:0),
                make_expr!(i32 2 4:4)),
            make_expr!(index 9:20 bracket 18:20
                make_expr!(array 9:17
                    make_expr!(i32 4 10:10),
                    make_expr!(i32 5 13:13),
                    make_expr!(i32 6 16:16)),
                make_expr!(i32 2 19:19)))
    }

    case!{ parse_expr "de(, )",
        make_expr!(call 0:5 paren 2:5
            make_name!(simple 0:1 #2),
        ), errors make_errors!(
            e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(2, 5), strings::FnCallHere)
        )
    }

    //               0 12345678
    case!{ parse_expr "\"\".de(, )",
        make_expr!(call 0:8 paren 5:8
            make_expr!(member 0:4 dot 2:2
                make_expr!(str #1 0:1),
                make_expr!(member name 3:4 #2)),
        ), errors make_errors!(
            e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(5, 8), strings::FnCallHere)
        )
    }

    case!{ parse_expr "defg[]",
        make_expr!(index 0:5 bracket 4:5
            make_name!(simple 0:3 #2),
        ), errors make_errors!(
            e: e.emit(strings::EmptyIndexCall).detail(Span::new(4, 5), strings::IndexCallHere)
        )
    }

    //              123456
    case!{ parse_expr "de[, ]",
        make_expr!(index 0:5 bracket 2:5
            make_name!(simple 0:1 #2),
        ), errors make_errors!(
            e: e.emit(strings::EmptyIndexCall).detail(Span::new(2, 5), strings::IndexCallHere)
        )
    }
}

#[test]
fn parse_call_expr() {

    case!{ notast parse_call_expr "()",
        (Span::new(0, 1), ExprList{ items: Vec::new() })
    }

    case!{ notast parse_call_expr "(\"hello\")",
        (Span::new(0, 8), ExprList{ items: vec![
            make_expr!(str #2 1:7),
        ] })
    }

    case!{ notast parse_call_expr "(,)",
        (Span::new(0, 2), ExprList{ items: Vec::new() }),
        errors make_errors!(e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(0, 2), strings::FnCallHere))
    }
}

#[test]
fn parse_index_expr() {

    case!{ notast parse_index_expr "[1, 2, ]",
        (Span::new(0, 7), ExprList{ items: vec![
            make_expr!(i32 1 1:1),
            make_expr!(i32 2 4:4),
        ] })
    }

    case!{ notast parse_index_expr "[\"hello\"]",
        (Span::new(0, 8), ExprList{ items: vec![
            make_expr!(str #2 1:7)
        ] })
    }

    case!{ notast parse_index_expr "[,]",
        (Span::new(0, 2), ExprList{ items: Vec::new() }),
        errors make_errors!(e: e.emit(strings::EmptyIndexCall).detail(Span::new(0, 2), strings::IndexCallHere)),
    }
}

#[test]
fn name_parse() {

    case!{ parse_name "hello", 
        make_name!(bare 0:4 false, None,
            make_name!(segment 0:4 #2)),
    }
    //              0        1         2         3         4
    //              01234567890123456789012345678901234567890
    case!{ parse_name "std::network::wlan::native::GetWLANHandle",
        make_name!(bare 0:40 false, None,
            make_name!(segment 0:2 #2), 
            make_name!(segment 5:11 #3),
            make_name!(segment 14:17 #4),
            make_name!(segment 20:25 #5),
            make_name!(segment 28:40 #6))
    }

    //      0         1         2      v this is not part of name
    //      012345678901234567890123456
    case!{ parse_name "::abc::def::<ghi, jkl>::mno<", 
        make_name!(bare 0:26 true, None,
            make_name!(segment 2:4 #2),
            make_name!(segment 7:9 #3),
            make_name!(segment generic 12:21
                make_type!(simple 13:15 #4),
                make_type!(simple 18:20 #5)),
            make_name!(segment 24:26 #6))
    }

    //      0         1         2
    //      01234567890123456789012
    case!{ parse_name "<Name as Parser>::parse",
        make_name!(bare 0:22 false,
            make_type!(segment as 0:15
                make_type!(simple 1:4 #2),
                make_type!(simple 9:14 #3)),
            make_name!(segment 18:22 #4)),
    }

    //      01234567890123
    case!{ parse_name "a::<b>::<c>::d",
        make_name!(bare 0:13 false, None,
            make_name!(segment 0:0 #2),
            make_name!(segment generic 3:5
                make_type!(simple 4:4 #3)),
            make_name!(segment generic 8:10
                make_type!(simple 9:9 #4)),
            make_name!(segment 13:13 #5)
        ), errors make_errors!(e: e.emit(strings::InvalidNameSegment).detail(Span::new(8, 8), strings::NameSegmentExpect)),
    }
}

#[test]
fn primary_expr_parse() {

    // this is the loop of tokens.nth(current) is left bracket does not cover everything and infinite loop is here
    // update 2017/6/17: this was a bug, but I forget detail
    case!{ parse_primary_expr "[a]",
        make_expr!(array 0:2
            make_name!(simple 1:1 #2))
    }

    //                      0        1         2         3         4
    //                      01234567890123456789012345678901234567890123456     
    case!{ parse_primary_expr "(463857, IEfN, atau8M, [fNAE, ((cAeJN4)), nHg])",
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

    case!{ parse_primary_expr "10363",
        make_expr!(i32 10363 0:4)
    }

    case!{
    //   0         1         2         3         4         5         6         7         8         9         0         1         2         3       
    //   01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567 8901234567890123456789012345678901234
    parse_primary_expr "[(0x7E), FFGqfJe, I4, [(m7A, (41, ([(jL, rAn, K0FgLc7h, true), C, w]), (J3cEFDG, d, (j8h))), (), \neIuArjF), 400, 0o535147505, 0xDB747]]" ,
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

    case!{ parse_primary_expr "CMDoF", make_name!(simple 0:4 #2) }
    case!{ parse_primary_expr "false", make_expr!(false 0:4) }

    
    //                      0        1         2         3         4         5         6          7          8         9         A
    //                      12345678901234567890123456789012345678901234567890123456789012345678901 234 5678901234567890123456789012
    case!{ parse_primary_expr "[uy6, 4373577, [(q, AJBN0n, MDEgKh5,), KG, (NsL, ((), D, false, d, ), \"H=\"), true, ((vvB3, true, 5))]]", 
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

    case!{ parse_primary_expr "(() )",
        make_expr!(paren 0:4
            make_expr!(unit 1:2))
    }
    case!{ parse_primary_expr "((),)", 
        make_expr!(tuple 0:4
            make_expr!(unit 1:2))
    }

    case!{ parse_primary_expr "(\"o5\")",
        make_expr!(paren 0:5
            make_expr!(str #2 1:4))
    }

    //                      0        1         2        
    //                      1234567890123456789012345678
    case!{ parse_primary_expr "(nn, ([false,true]), 183455)",
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
    case!{ parse_primary_expr "((true, (mO, [(q5k),a], (((KttG))), (K5DJ, r, ())), (McsaEdfdfalse,)), rIOKt,)",
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
    case!{ parse_primary_expr "[\"il\", 0o52u32, sO04n]",
        make_expr!(array 0:21
            make_expr!(str #2 1:4),
            make_expr!(u32 0o52 7:13), 
            make_name!(simple 16:20 #3))
    }
    //                                      12345678
    case!{ parse_primary_expr "['f',()]", 
        make_expr!(array 0:7
            make_expr!(char 1:3 'f'),
            make_expr!(unit 5:6))
    }
    case!{ parse_primary_expr "[]", make_expr!(array 0:1) }

    //                                      0        1           2         3         4      
    //                                      12345 678901 234567890123456789012345678901234
    case!{ parse_primary_expr "[8, \"@=?GF\", 87r32, 1340323.74r64, FKOxAvx5]",
        make_expr!(array 0:43
            make_expr!(i32 8 1:1),
            make_expr!(str #2 4:10),
            make_expr!(r32 87f32 13:17),
            make_expr!(r64 1340323.74 20:32),
            make_name!(simple 35:42 #3)),
    }

    //                                        0        1         2         3         4         5         6     
    //                                        123456789012345678901234567890123456789012345678901234567890123
    case!{ parse_primary_expr r#"  [[dnr4, lGFd3yL, tJ], ['\\', p, (xGaBwiL,), DE], true, aB8aE]"#,
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
    case!{ parse_primary_expr "[abc, 123u32, \"456\", '\\u0065', false, (), (a), (abc, \"hello\", ), ]",
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

    case!{ parse_primary_expr "(                             )", 
        make_expr!(unit 0:30)
    }

    case!{ parse_primary_expr "(,)",
        make_expr!(tuple 0:2),
        errors make_errors!(e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(0, 2), strings::TupleDefHere)),
    }
}

#[test]
fn postfix_expr_parse() {

    //      0        1         2         3         4         5     
    //      0123456789012345678901234567890123456789012345678901234567
    case!{ parse_postfix_expr "a.b(c, d, e).f(g, h, i,)(u,).j[k].l().m[n, o, p][r, s, t,]",
        make_expr!(index 0:57 bracket 48:57
            make_expr!(index 0:47 bracket 39:47
                make_expr!(member 0:38 dot 37:37
                    make_expr!(call 0:36 paren 35:36
                        make_expr!(member 0:34 dot 33:33
                            make_expr!(index 0:32 bracket 30:32
                                make_expr!(member 0:29 dot 28:28
                                    make_expr!(call 0:27 paren 24:27
                                        make_expr!(call 0:23 paren 14:23
                                            make_expr!(member 0:13 dot 12:12
                                                make_expr!(call 0:11 paren 3:11
                                                    make_expr!(member 0:2 dot 1:1
                                                        make_name!(simple 0:0 #2),
                                                        make_expr!(member name 2:2 #3)), 
                                                    make_name!(simple 4:4 #4),
                                                    make_name!(simple 7:7 #5),
                                                    make_name!(simple 10:10 #6)),
                                                make_expr!(member name 13:13 #7)),
                                            make_name!(simple 15:15 #8),
                                            make_name!(simple 18:18 #9),
                                            make_name!(simple 21:21 #10)),
                                        make_name!(simple 25:25 #11)),
                                    make_expr!(member name 29:29 #12)),
                                make_name!(simple 31:31 #13)),
                            make_expr!(member name 34:34 #14)),),
                    make_expr!(member name 38:38 #15)),
                make_name!(simple 40:40 #16),
                make_name!(simple 43:43 #17),
                make_name!(simple 46:46 #18)),
            make_name!(simple 49:49 #19),
            make_name!(simple 52:52 #20),
            make_name!(simple 55:55 #21))
    }

    //      012345678901234567890
    case!{ parse_postfix_expr "i.collect::<Vec<i32>>",
        make_expr!(member 0:20 dot 1:1
            make_name!(simple 0:0 #2),
            make_expr!(member name 2:20 #3 2:8 quote 11:20
                make_type!(plain 12:19 false, None,
                    make_type!(segment generic 12:19 #4 12:14 quote 15:19
                        make_type!(prim 16:18 I32))))),
    }

    //      01234567
    case!{ parse_postfix_expr "(0, 0).0",
        make_expr!(member 0:7 dot 6:6
            make_expr!(tuple 0:5
                make_expr!(i32 0 1:1),
                make_expr!(i32 0 4:4)),
            make_expr!(member name 7:7 i32 0)),
    }

    //      0         1         2         3         4         5
    //      012345678901234567890123456789012345678901234567890123456
    case!{ parse_postfix_expr "string::<wchar>{ size: 0, cap: 0, data: uninitialized() }",
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
                make_expr!(call 40:54 paren 53:54
                    make_name!(simple 40:52 #7),))),
    }

    case!{ parse_postfix_expr "a[]",
        make_expr!(index 0:2 bracket 1:2
            make_name!(simple 0:0 #2),
        ), errors make_errors!(
            e: e.emit(strings::EmptyIndexCall).detail(Span::new(1, 2), strings::IndexCallHere)
        )
    }
    
    case!{ parse_postfix_expr "a[, ]",
        make_expr!(index 0:4 bracket 1:4
            make_name!(simple 0:0 #2),
        ), errors make_errors!(
            e: e.emit(strings::EmptyIndexCall).detail(Span::new(1, 4), strings::IndexCallHere)
        )
    }
    
    case!{ parse_postfix_expr "a(, )",
        make_expr!(call 0:4 paren 1:4
            make_name!(simple 0:0 #2),
        ), errors make_errors!(
            e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(1, 4), strings::FnCallHere)
        )
    }

    //      01234
    case!{ parse_postfix_expr "a.0i8",
        make_expr!(member 0:4 dot 1:1
            make_name!(simple 0:0 #2),
            make_expr!(member name 2:4 numeric Numeric::I8(0))
        ), errors make_errors!(
            e: e.emit(strings::InvalidTupleIndex).span(Span::new(2, 4)).help(strings::TupleIndexSyntaxHelp)
        )
    }

    //      01234567890123456789012
    case!{ parse_postfix_expr "a.0xFFFF_FFFF_FFFF_FFFF",
        make_expr!(member 0:22 dot 1:1
            make_name!(simple 0:0 #2),
            make_expr!(member name 2:22 numeric Numeric::U64(0xFFFF_FFFF_FFFF_FFFF))
        ), errors make_errors!(
            e: e.emit(strings::InvalidTupleIndex).span(Span::new(2, 22)).help(strings::TupleIndexSyntaxHelp)
        )
    }

    // //      0123456789
    // case!{ parse_postfix_expr "a.abc::def",
    //     make_expr!(member 0:9 dot 1:1
    //         make_name!(simple 0:0 #2),
    //         make_name!(bare 2:9 false, None,
    //             make_name!(segment 2:4 #3),
    //             make_name!(segment 7:9 #4))
    //     ), errors make_errors!(
    //         e: e.emit(strings::InvalidMemberAccess).span(Span::new(7, 9)).help(strings::GenericMemberAccessSyntaxHelp)
    //     )
    // }

    // //      0123456789012345678
    // case!{ parse_postfix_expr "a.abc::<def>::<ghi>",
    //     make_expr!(member 0:18 dot 1:1
    //         make_name!(simple 0:0 #2),
    //         make_name!(bare 2:18 false, None,
    //             make_name!(segment 2:4 #3),
    //             make_name!(segment generic 7:11
    //                 make_type!(simple 8:10 #4)),
    //             make_name!(segment generic 14:18
    //                 make_type!(simple 15:17 #5)))
    //     ), errors make_errors!(e: {
    //         e.emit(strings::InvalidNameSegment).detail(Span::new(14, 14), strings::NameSegmentExpect);
    //         e.emit(strings::InvalidMemberAccess).span(Span::new(7, 18)).help(strings::GenericMemberAccessSyntaxHelp);
    //     })
    // }
}

#[test]
fn range_expr_parse() {

    case!{ parse_range_expr "..", 
        make_expr!(range full 0:1)
    }

    case!{ parse_range_expr "..1 + 1",
        make_expr!(range right 0:6
            make_expr!(binary 2:6 Add 4:4
                make_expr!(i32 1 2:2),
                make_expr!(i32 1 6:6)))
    }

    case!{ parse_range_expr "1 ..",
        make_expr!(range left 0:3
            make_expr!(i32 1 0:0))
    }
}

#[test]
fn parse_tuple_expr() {
    //                                   01234567
    case!{ parse_tuple_expr "(1, '2')",
        make_expr!(tuple 0:7
            make_expr!(i32 1 1:1),
            make_expr!(char 4:6 '2'))
    }
    //                                   0123456
    case!{ parse_tuple_expr "(1 + 1)",
        make_expr!(paren 0:6
            make_expr!(binary 1:5 Add 3:3
                make_expr!(i32 1 1:1),
                make_expr!(i32 1 5:5)))
    }
    
    case!{ parse_tuple_expr "( , )",
        make_expr!(tuple 0: 4),
        errors make_errors!(e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(0, 4), strings::TupleDefHere)),
    }
}

#[test]
fn unary_expr_parse() {
    
    case!{ parse_unary_expr "1", 
        make_expr!(i32 1 0:0)
    }

    case!{ parse_unary_expr "!~!1",
        make_expr!(unary 0:3 Not 0:0
            make_expr!(unary 1:3 Tilde 1:1
                make_expr!(unary 2:3 Not 2:2
                    make_expr!(i32 1 3:3))))
    }

    case!{ parse_unary_expr "&a(&b)",
        make_expr!(unary 0:5 And 0:0
            make_expr!(call 1:5 paren 2:5
                make_name!(simple 1:1 #2),
                make_expr!(unary 3:4 And 3:3
                    make_name!(simple 4:4 #3))))
    }
}
