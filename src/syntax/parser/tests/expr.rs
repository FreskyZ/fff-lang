use super::*;

#[test]
fn parse_array_expr() {

    case!{ parse_array_expr "[a]", |x|
        make_expr!(x array 0:2
            make_expr!(x path simple 1:1 #a)),
    }

    //                       01234567
    case!{ parse_array_expr "[1, '2']", |x|
        make_expr!(x array 0:7
            make_expr!(x i32 1 1:1),
            make_expr!(x char 4:6 '2')),
    }
    //                       01234567
    case!{ parse_array_expr "[1 + 1,]", |x|
        make_expr!(x array 0:7
            make_expr!(x binary 1:5 Add 3:3
                make_expr!(x i32 1 1:1),
                make_expr!(x i32 1 5:5))),
    }

    case!{ parse_array_expr "[ , ]",
        |x| make_expr!(x array 0:4),
        |e| e.emit("expect `]`, meet `,`").span(Span::new(2, 2))
    }
}

#[test]
fn parse_binary_expr() {
    //                                     123456789012345
    case!{ parse_binary_expr "[1] * [2] / [3]", |x|
        make_expr!(x binary 0:14 Div 10:10
            make_expr!(x binary 0:8 Mul 4:4
                make_expr!(x array 0:2
                    make_expr!(x i32 1 1:1)),
                make_expr!(x array 6:8
                    make_expr!(x i32 2 7:7))),
            make_expr!(x array 12:14
                make_expr!(x i32 3 13:13))),
    }
    //                                     0        1         2
    //                                     123456789012345678901
    case!{ parse_binary_expr "a * b / c + d % e - f", |x|  // ((((a * b) / c) + (d % e)) - f)
        make_expr!(x binary 0:20 Sub 18:18
            make_expr!(x binary 0:16 Add 10:10
                make_expr!(x binary 0:8 Div 6:6
                    make_expr!(x binary 0:4 Mul 2:2
                        make_expr!(x path simple 0:0 #a),
                        make_expr!(x path simple 4:4 #b)),
                    make_expr!(x path simple 8:8 #c)),
                make_expr!(x binary 12:16 Rem 14:14
                    make_expr!(x path simple 12:12 #d),
                    make_expr!(x path simple 16:16 #e))),
            make_expr!(x path simple 20:20 #f)),
    }
    // // once upon a time, this is recognized as "
    // // a: binary left is simple path
    // // *: binary op
    // // b: generic segment base
    // // <: start generic parameter list
    // // <: start type case segment
    // // h: simple path as type case left
    // // /: expect 'as' meet '/'
    //                        0        1         2         3
    //                        0123456789012345678901234567890
    case!{ parse_binary_expr "a * b << h / c + d % e - f >> g", |x| // (((a * b) << (((h / c) + (d % e)) - f)) >> g)
        make_expr!(x binary 0:30 GtGt 27:28
            make_expr!(x binary 0:25 LtLt 6:7
                make_expr!(x binary 0:4 Mul 2:2
                    make_expr!(x path simple 0:0 #a),
                    make_expr!(x path simple 4:4 #b)),
                make_expr!(x binary 9:25 Sub 23:23
                    make_expr!(x binary 9:21 Add 15:15
                        make_expr!(x binary 9:13 Div 11:11
                            make_expr!(x path simple 9:9 #h),
                            make_expr!(x path simple 13:13 #c)),
                        make_expr!(x binary 17:21 Rem 19:19
                            make_expr!(x path simple 17:17 #d),
                            make_expr!(x path simple 21:21 #e))),
                    make_expr!(x path simple 25:25 #f))),
            make_expr!(x path simple 30:30 #g)),
    }

    // This very huge test case it not useless:
    //     the operator priority impl in `with_test_str` is according to the parser_impl macro definition order
    //     my test case oracle is according to the comments on top of this file, which originally are copy and paste from c++ standard
    //     I accidently mistake the order of xor_expr and or_expr in parser_impl macros, this case help me find this
    // continue story: I changed name of the parsers and found the error not fixed
    //     then I find out that the last parameter of the macros, sep kind is the actual order definition
    //     only change that can I fix the bug
    //                         0        1         2         3         4         5         6         7         8
    //                         1234567890123456789012345678901234567890123456789012345678901234567890123456789012345
    case!{ parse_binary_expr "a * b << h / c + d % e - f >> g > h * i < j << k >= m && n || o & p | q ^ r != s == t", |x|
        // */%, +-, <><=>=, <<>>, &, ^, |, ==!=, &&, ||
        // ((((((a * b) << (((h / c) + (d % e)) - f)) >> ((g > (h * i)) < j)) << (k >= m)) && n) || ((((o & p) | (q ^ r)) != s) == t))
        make_expr!(x binary 0:84 OrOr 59:60
            make_expr!(x binary 0:57 AndAnd 54:55
                make_expr!(x binary 0:52 LtLt 44:45
                    make_expr!(x binary 0:42 GtGt 27:28
                        make_expr!(x binary 0:25 LtLt 6:7
                            make_expr!(x binary 0:4 Mul 2:2
                                make_expr!(x path simple 0:0 #a),
                                make_expr!(x path simple 4:4 #b)),
                            make_expr!(x binary 9:25 Sub 23:23
                                make_expr!(x binary 9:21 Add 15:15
                                    make_expr!(x binary 9:13 Div 11:11
                                        make_expr!(x path simple 9:9 #h),
                                        make_expr!(x path simple 13:13 #c)),
                                    make_expr!(x binary 17:21 Rem 19:19
                                        make_expr!(x path simple 17:17 #d),
                                        make_expr!(x path simple 21:21 #e))),
                                make_expr!(x path simple 25:25 #f))),
                        make_expr!(x binary 30:42 Lt 40:40
                            make_expr!(x binary 30:38 Gt 32:32
                                make_expr!(x path simple 30:30 #g),
                                make_expr!(x binary 34:38 Mul 36:36
                                    make_expr!(x path simple 34:34 #h),
                                    make_expr!(x path simple 38:38 #i))),
                            make_expr!(x path simple 42:42 #j))),
                    make_expr!(x binary 47:52 GtEq 49:50
                        make_expr!(x path simple 47:47 #k),
                        make_expr!(x path simple 52:52 #m))),
                make_expr!(x path simple 57:57 #n)),
            make_expr!(x binary 62:84 EqEq 81:82
                make_expr!(x binary 62:79 NotEq 76:77
                    make_expr!(x binary 62:74 Or 68:68
                        make_expr!(x binary 62:66 And 64:64
                            make_expr!(x path simple 62:62 #o),
                            make_expr!(x path simple 66:66 #p)),
                        make_expr!(x binary 70:74 Caret 72:72
                            make_expr!(x path simple 70:70 #q),
                            make_expr!(x path simple 74:74 #r))),
                    make_expr!(x path simple 79:79 #s)),
                make_expr!(x path simple 84:84 #t))),
    }
    //                                     1234567890
    case!{ parse_binary_expr "a & b == c", |x| // ((a & b) == c)
        make_expr!(x binary 0:9 EqEq 6:7
            make_expr!(x binary 0:4 And 2:2
                make_expr!(x path simple 0:0 #a),
                make_expr!(x path simple 4:4 #b)),
            make_expr!(x path simple 9:9 #c)),
    }

    // 1 + 2 << 3 => (1 + 2) << 3, don't want this
    // cout << 5 + 6 => cout << (5 + 6), want this

    // program generated random tests
    //                                     0        1         2         3
    //                                     1234567890123456789012345678901234
    case!{ parse_binary_expr "0 + 6 ^ 3 & 3 / 3 - 8 && 2 & 0 + 6", |x| // (((0 + 6) ^ (3 & ((3 / 3) - 8))) && (2 & (0 + 6)))
        make_expr!(x binary 0:33 AndAnd 22:23
            make_expr!(x binary 0:20 Caret 6:6
                make_expr!(x binary 0:4 Add 2:2
                    make_expr!(x i32 0 0:0),
                    make_expr!(x i32 6 4:4)),
                make_expr!(x binary 8:20 And 10:10
                    make_expr!(x i32 3 8:8),
                    make_expr!(x binary 12:20 Sub 18:18
                        make_expr!(x binary 12:16 Div 14:14
                            make_expr!(x i32 3 12:12),
                            make_expr!(x i32 3 16:16)),
                        make_expr!(x i32 8 20:20)))),
            make_expr!(x binary 25:33 And 27:27
                make_expr!(x i32 2 25:25),
                make_expr!(x binary 29:33 Add 31:31
                    make_expr!(x i32 0 29:29),
                    make_expr!(x i32 6 33:33)))),
    }
    //                                     0        1         2         3         4         5         6         7
    //                                     1234567890123456789012345678901234567890123456789012345678901234567890
    case!{ parse_binary_expr "7 > 1 | 0 % 8 | 1 % 7 * 3 % 6 == 1 >> 8 % 3 ^ 6 << 0 ^ 2 >> 6 || 1 - 0", |x|
        // (((((7 > 1) | (0 % 8)) | (((1 % 7) * 3) % 6)) == (((1 >> (8 % 3)) ^ (6 << 0)) ^ (2 >> 6))) || (1 - 0))
        make_expr!(x binary 0:69 OrOr 62:63
            make_expr!(x binary 0:60 EqEq 30:31
                make_expr!(x binary 0:28 Or 14:14
                    make_expr!(x binary 0:12 Or 6:6
                        make_expr!(x binary 0:4 Gt 2:2
                            make_expr!(x i32 7 0:0),
                            make_expr!(x i32 1 4:4)),
                        make_expr!(x binary 8:12 Rem 10:10
                            make_expr!(x i32 0 8:8),
                            make_expr!(x i32 8 12:12))),
                    make_expr!(x binary 16:28 Rem 26:26
                        make_expr!(x binary 16:24 Mul 22:22
                            make_expr!(x binary 16:20 Rem 18:18
                                make_expr!(x i32 1 16:16),
                                make_expr!(x i32 7 20:20)),
                            make_expr!(x i32 3 24:24)),
                        make_expr!(x i32 6 28:28))),
                make_expr!(x binary 33:60 Caret 53:53
                    make_expr!(x binary 33:51 Caret 44:44
                        make_expr!(x binary 33:42 GtGt 35:36
                            make_expr!(x i32 1 33:33),
                            make_expr!(x binary 38:42 Rem 40:40
                                make_expr!(x i32 8 38:38),
                                make_expr!(x i32 3 42:42))),
                        make_expr!(x binary 46:51 LtLt 48:49
                            make_expr!(x i32 6 46:46),
                            make_expr!(x i32 0 51:51))),
                    make_expr!(x binary 55:60 GtGt 57:58
                        make_expr!(x i32 2 55:55),
                        make_expr!(x i32 6 60:60)))),
            make_expr!(x binary 65:69 Sub 67:67
                make_expr!(x i32 1 65:65),
                make_expr!(x i32 0 69:69))),
    }
    //                                     0        1         2         3         4         5         6
    //                                     1234567890123456789012345678901234567890123456789012345678901
    case!{ parse_binary_expr "7 >> 3 == 8 / 1 && 6 == 1 <= 3 % 6 ^ 3 - 1 - 2 >> 7 || 1 >= 1", |x|
        // */%, +-, <><=>=, <<>>, &, ^, |, ==!=, &&, ||
        // ((((7 >> 3) == (8 / 1)) && (6 == ((1 <= (3 % 6)) ^ (((3 - 1) - 2) >> 7)))) || (1 >= 1))
        make_expr!(x binary 0:60 OrOr 52:53
            make_expr!(x binary 0:50 AndAnd 16:17
                make_expr!(x binary 0:14 EqEq 7:8
                    make_expr!(x binary 0:5 GtGt 2:3
                        make_expr!(x i32 7 0:0),
                        make_expr!(x i32 3 5:5)),
                    make_expr!(x binary 10:14 Div 12:12
                        make_expr!(x i32 8 10:10),
                        make_expr!(x i32 1 14:14))),
                make_expr!(x binary 19:50 EqEq 21:22
                    make_expr!(x i32 6 19:19),
                    make_expr!(x binary 24:50 Caret 35:35
                        make_expr!(x binary 24:33 LtEq 26:27
                            make_expr!(x i32 1 24:24),
                            make_expr!(x binary 29:33 Rem 31:31
                                make_expr!(x i32 3 29:29),
                                make_expr!(x i32 6 33:33))),
                        make_expr!(x binary 37:50 GtGt 47:48
                            make_expr!(x binary 37:45 Sub 43:43
                                make_expr!(x binary 37:41 Sub 39:39
                                    make_expr!(x i32 3 37:37),
                                    make_expr!(x i32 1 41:41)),
                                make_expr!(x i32 2 45:45)),
                            make_expr!(x i32 7 50:50))))),
            make_expr!(x binary 55:60 GtEq 57:58
                make_expr!(x i32 1 55:55),
                make_expr!(x i32 1 60:60))),
    }
    //                                     0
    //                                     123456
    case!{ parse_binary_expr "4 >> 7", |x|
        make_expr!(x binary 0:5 GtGt 2:3
            make_expr!(x i32 4 0:0),
            make_expr!(x i32 7 5:5)),
    }
    //                                     0        1         2         3         4         5         6
    //                                     12345678901234567890123456789012345678901234567890123456789012345
    case!{ parse_binary_expr "8 & 0 | 7 + 7 | 7 * 0 && 1 - 2 * 3 | 0 - 7 >= 6 >> 5 % 5 || 5 % 3", |x|
        // (((((8 & 0) | (7 + 7)) | (7 * 0)) && ((1 - (2 * 3)) | (((0 - 7) >= 6) >> (5 % 5)))) || (5 % 3))
        make_expr!(x binary 0:64 OrOr 57:58
            make_expr!(x binary 0:55 AndAnd 22:23
                make_expr!(x binary 0:20 Or 14:14
                    make_expr!(x binary 0:12 Or 6:6
                        make_expr!(x binary 0:4 And 2:2
                            make_expr!(x i32 8 0:0),
                            make_expr!(x i32 0 4:4)),
                        make_expr!(x binary 8:12 Add 10:10
                            make_expr!(x i32 7 8:8),
                            make_expr!(x i32 7 12:12))),
                    make_expr!(x binary 16:20 Mul 18:18
                        make_expr!(x i32 7 16:16),
                        make_expr!(x i32 0 20:20))),
                make_expr!(x binary 25:55 Or 35:35
                    make_expr!(x binary 25:33 Sub 27:27
                        make_expr!(x i32 1 25:25),
                        make_expr!(x binary 29:33 Mul 31:31
                            make_expr!(x i32 2 29:29),
                            make_expr!(x i32 3 33:33))),
                    make_expr!(x binary 37:55 GtGt 48:49
                        make_expr!(x binary 37:46 GtEq 43:44
                            make_expr!(x binary 37:41 Sub 39:39
                                make_expr!(x i32 0 37:37),
                                make_expr!(x i32 7 41:41)),
                            make_expr!(x i32 6 46:46)),
                        make_expr!(x binary 51:55 Rem 53:53
                            make_expr!(x i32 5 51:51),
                            make_expr!(x i32 5 55:55))))),
            make_expr!(x binary 60:64 Rem 62:62
                make_expr!(x i32 5 60:60),
                make_expr!(x i32 3 64:64))),
    }
    //                                     0        1         2         3         4         5         6
    //                                     12345678901234567890123456789012345678901234567890123456789012345678
    case!{ parse_binary_expr "3 <= 2 + 4 <= 5 && 3 < 3 + 2 >> 1 * 2 & 8 && 1 >= 1 < 0 || 6 < 4 * 4", |x|
        // (((((3 <= (2 + 4)) <= 5) && (((3 < (3 + 2)) >> (1 * 2)) & 8)) && ((1 >= 1) < 0)) || (6 < (4 * 4)))
        make_expr!(x binary 0:67 OrOr 56:57
            make_expr!(x binary 0:54 AndAnd 42:43
                make_expr!(x binary 0:40 AndAnd 16:17
                    make_expr!(x binary 0:14 LtEq 11:12
                        make_expr!(x binary 0:9 LtEq 2:3
                            make_expr!(x i32 3 0:0),
                            make_expr!(x binary 5:9 Add 7:7
                                make_expr!(x i32 2 5:5),
                                make_expr!(x i32 4 9:9))),
                        make_expr!(x i32 5 14:14)),
                    make_expr!(x binary 19:40 And 38:38
                        make_expr!(x binary 19:36 GtGt 29:30
                            make_expr!(x binary 19:27 Lt 21:21
                                make_expr!(x i32 3 19:19),
                                make_expr!(x binary 23:27 Add 25:25
                                    make_expr!(x i32 3 23:23),
                                    make_expr!(x i32 2 27:27))),
                            make_expr!(x binary 32:36 Mul 34:34
                                make_expr!(x i32 1 32:32),
                                make_expr!(x i32 2 36:36))),
                        make_expr!(x i32 8 40:40))),
                make_expr!(x binary 45:54 Lt 52:52
                    make_expr!(x binary 45:50 GtEq 47:48
                        make_expr!(x i32 1 45:45),
                        make_expr!(x i32 1 50:50)),
                    make_expr!(x i32 0 54:54))),
            make_expr!(x binary 59:67 Lt 61:61
                make_expr!(x i32 6 59:59),
                make_expr!(x binary 63:67 Mul 65:65
                    make_expr!(x i32 4 63:63),
                    make_expr!(x i32 4 67:67)))),
    }
    //                                     0        1         2
    //                                     12345678901234567890
    case!{ parse_binary_expr "5 >= 6 | 3 == 4 && 3", |x|
        // ((((5 >= 6) | 3) == 4) && 3)
        make_expr!(x binary 0:19 AndAnd 16:17
            make_expr!(x binary 0:14 EqEq 11:12
                make_expr!(x binary 0:9 Or 7:7
                    make_expr!(x binary 0:5 GtEq 2:3
                        make_expr!(x i32 5 0:0),
                        make_expr!(x i32 6 5:5)),
                    make_expr!(x i32 3 9:9)),
                make_expr!(x i32 4 14:14)),
            make_expr!(x i32 3 19:19)),
    }
    //                                     0        1         2         3         4         5         6         7
    //                                     1234567890123456789012345678901234567890123456789012345678901234567890123456
    case!{ parse_binary_expr "6 && 7 >> 8 && 0 / 8 * 7 + 5 < 5 / 5 >> 5 - 1 >= 6 > 8 | 6 >> 5 > 2 + 1 || 0", |x|
        // */%, +-, <><=>=, <<>>, &, ^, |, ==!=, &&, ||
        // (((6 && (7 >> 8)) && ((((((0 / 8) * 7) + 5) < (5 / 5)) >> (((5 - 1) >= 6) > 8)) | (6 >> (5 > (2 + 1))))) || 0)
        make_expr!(x binary 0:75 OrOr 72:73
            make_expr!(x binary 0:70 AndAnd 12:13
                make_expr!(x binary 0:10 AndAnd 2:3
                    make_expr!(x i32 6 0:0),
                    make_expr!(x binary 5:10 GtGt 7:8
                        make_expr!(x i32 7 5:5),
                        make_expr!(x i32 8 10:10))),
                make_expr!(x binary 15:70 Or 55:55
                    make_expr!(x binary 15:53 GtGt 37:38
                        make_expr!(x binary 15:35 Lt 29:29
                            make_expr!(x binary 15:27 Add 25:25
                                make_expr!(x binary 15:23 Mul 21:21
                                    make_expr!(x binary 15:19 Div 17:17
                                        make_expr!(x i32 0 15:15),
                                        make_expr!(x i32 8 19:19)),
                                    make_expr!(x i32 7 23:23)),
                                make_expr!(x i32 5 27:27)),
                            make_expr!(x binary 31:35 Div 33:33
                                make_expr!(x i32 5 31:31),
                                make_expr!(x i32 5 35:35))),
                        make_expr!(x binary 40:53 Gt 51:51
                            make_expr!(x binary 40:49 GtEq 46:47
                                make_expr!(x binary 40:44 Sub 42:42
                                    make_expr!(x i32 5 40:40),
                                    make_expr!(x i32 1 44:44)),
                                make_expr!(x i32 6 49:49)),
                            make_expr!(x i32 8 53:53))),
                    make_expr!(x binary 57:70 GtGt 59:60
                        make_expr!(x i32 6 57:57),
                        make_expr!(x binary 62:70 Gt 64:64
                            make_expr!(x i32 5 62:62),
                            make_expr!(x binary 66:70 Add 68:68
                                make_expr!(x i32 2 66:66),
                                make_expr!(x i32 1 70:70)))))),
            make_expr!(x i32 0 75:75)),
    }

    case!{ parse_binary_expr "1<2>3", |x|
        make_expr!(x binary 0:4 Gt 3:3
            make_expr!(x binary 0:2 Lt 1:1
                make_expr!(x i32 1 0:0),
                make_expr!(x i32 2 2:2)),
            make_expr!(x i32 3 4:4)),
        |e| e.emit(strings::MaybeGeneric).span(Span::new(1, 1)).span(Span::new(3, 3)).help(strings::MaybeGenericHelp),
    }
}

#[test]
fn parse_expr() {

    case!{ parse_expr "\"abc\"", |x| make_expr!(x str #"abc" 0:4) }
    case!{ parse_expr "0xfffu64", |x| make_expr!(x u64 0xFFF 0:7) }
    case!{ parse_expr "'f'", |x| make_expr!(x char 0:2 'f') }
    case!{ parse_expr "true", |x| make_expr!(x true 0:3) }
    case!{ parse_expr "binary_expr", |x| make_expr!(x path simple 0:10 #binary_expr) }
    case!{ parse_expr "(  )", |x| make_expr!(x unit 0:3) }

    // Case from fn_def_parse
    case!{ parse_expr "println(this)", |x|
        make_expr!(x call 0:12 paren 7:12
            make_expr!(x path simple 0:6 #println),
            make_expr!(x path simple 8:11 #this)),
    }

    // Very very legacy expr tests which originally contains ExpressionBase and ExpressionOperator
    // update them to current syntax to help improve coverage and make me happy

    // Unit
    case!{ parse_expr "(1)", |x|
        make_expr!(x paren 0:2
            make_expr!(x i32 1 1:1)),
    }
    // I can see future of Ok(())!
    case!{ parse_expr "(())", |x|
        make_expr!(x paren 0:3
            make_expr!(x unit 1:2)),
    }

    // Tuple def
    case!{ parse_expr "(a, b)", |x|
        make_expr!(x tuple 0:5
            make_expr!(x path simple 1:1 #a),
            make_expr!(x path simple 4:4 #b))
    }        //  12345678901
    case!{ parse_expr "(1, 2, 3, )", |x|
        make_expr!(x tuple 0:10
            make_expr!(x i32 1 1:1),
            make_expr!(x i32 2 4:4),
            make_expr!(x i32 3 7:7)),
    }

    // Array def
    case!{ parse_expr "[a]", |x|
        make_expr!(x array 0:2
            make_expr!(x path simple 1:1 #a)),
    }
    //  12345678
    case!{ parse_expr "[1, 2, ]", |x|
        make_expr!(x array 0:7
            make_expr!(x i32 1 1:1),
            make_expr!(x i32 2 4:4)),
    }
    case!{ parse_expr "[]", |x| make_expr!(x array 0:1) }

    // Member access
    case!{ parse_expr "a.b", |x|
        make_expr!(x member 0:2 dot 1:1 #b 2:2
            make_expr!(x path simple 0:0 #a)),
    }

    // function call
    case!{ parse_expr "defg()", |x|
        make_expr!(x call 0:5 paren 4:5
            make_expr!(x path simple 0:3 #defg),)
    }
    case!{ parse_expr "deg(a)", |x|
        make_expr!(x call 0:5 paren 3:5
            make_expr!(x path simple 0:2 #deg),
            make_expr!(x path simple 4:4 #a)),
    }
    case!{ parse_expr "degg(a, b, )", |x|
        make_expr!(x call 0:11 paren 4:11
            make_expr!(x path simple 0:3 #degg),
            make_expr!(x path simple 5:5 #a),
            make_expr!(x path simple 8:8 #b)),
    }
    //           0123456789
    case!{ parse_expr "abc.defg()", |x|
        make_expr!(x call 0:9 paren 8:9
            make_expr!(x member 0:7 dot 3:3 #defg 4:7
                make_expr!(x path simple 0:2 #abc)),),
    }
    case!{ parse_expr "abc.deg(a)", |x|
        make_expr!(x call 0:9 paren 7:9
            make_expr!(x member 0:6 dot 3:3 #deg 4:6
                make_expr!(x path simple 0:2 #abc)),
            make_expr!(x path simple 8:8 #a)),
    }
    //  12345678901234
    case!{ parse_expr "1.degg(a, b, )", |x|
        make_expr!(x call 0:13 paren 6:13
            make_expr!(x member 0:5 dot 1:1 #degg 2:5
                make_expr!(x i32 1 0:0)),
            make_expr!(x path simple 7:7 #a),
            make_expr!(x path simple 10:10 #b))
    }

    // get index       //  123456
    case!{ parse_expr "deg[a]", |x|
        make_expr!(x array index 0:5 bracket 3:5
            make_expr!(x path simple 0:2 #deg),
            make_expr!(x path simple 4:4 #a))
    }        //  123456789012
    case!{ parse_expr "degg[a, b, ]", |x|
        make_expr!(x array index 0:11 bracket 4:11
            make_expr!(x path simple 0:3 #degg),
            make_expr!(x path simple 5:5 #a),
            make_expr!(x path simple 8:8 #b))
    }

    //           123456
    case!{ parse_expr "2[3].a", |x|
        make_expr!(x member 0:5 dot 4:4 #a 5:5
            make_expr!(x array index 0:3 bracket 1:3
                make_expr!(x i32 2 0:0),
                make_expr!(x i32 3 2:2)))
    }   //  1234567890123456
    case!{ parse_expr "print(233, ).bit", |x|
        make_expr!(x member 0:15 dot 12:12 #bit 13:15
            make_expr!(x call 0:11 paren 5:11
                make_expr!(x path simple 0:4 #print),
                make_expr!(x i32 233 6:8)))
    }            //  12345678901234
    case!{ parse_expr "1.degg[a, b, ]", |x|
        make_expr!(x array index 0:13 bracket 6:13
            make_expr!(x member 0:5 dot 1:1 #degg 2:5
                make_expr!(x i32 1 0:0)),
            make_expr!(x path simple 7:7 #a),
            make_expr!(x path simple 10:10 #b))
    }

    case!{ parse_expr "!~!1[1]", |x|
        make_expr!(x unary 0:6 Not 0:0
            make_expr!(x unary 1:6 Tilde 1:1
                make_expr!(x unary 2:6 Not 2:2
                    make_expr!(x array index 3:6 bracket 4:6
                        make_expr!(x i32 1 3:3),
                        make_expr!(x i32 1 5:5)))))
    }

    //           1234567
    case!{ parse_expr "!!1", |x|
        make_expr!(x unary 0:2 Not 0:0
            make_expr!(x unary 1:2 Not 1:1
                make_expr!(x i32 1 2:2)))
    }

    // range
    case!{ parse_expr "..", |x| make_expr!(x range full 0:1) }

    case!{ parse_expr "..1 + 2", |x|
        make_expr!(x range right 0:6
            make_expr!(x binary 2:6 Add 4:4
                make_expr!(x i32 1 2:2),
                make_expr!(x i32 2 6:6)))
    }

    case!{ parse_expr "xxx ..", |x|
        make_expr!(x range left 0:5
            make_expr!(x path simple 0:2 #xxx))
    }

    case!{ parse_expr "1 + 2 .. [4, 5, 6][2]", |x|
        make_expr!(x range both 0:20 dotdot 6:7
            make_expr!(x binary 0:4 Add 2:2
                make_expr!(x i32 1 0:0),
                make_expr!(x i32 2 4:4)),
            make_expr!(x array index 9:20 bracket 18:20
                make_expr!(x array 9:17
                    make_expr!(x i32 4 10:10),
                    make_expr!(x i32 5 13:13),
                    make_expr!(x i32 6 16:16)),
                make_expr!(x i32 2 19:19)))
    }

    case!{ parse_expr "de(, )", |x|
        make_expr!(x call 0:5 paren 2:5
            make_expr!(x path simple 0:1 #de),),
        |e| e.emit("expect `)`, meet `,`").span(Span::new(3, 3))
    }

    //               0 12345678
    case!{ parse_expr "\"\".de(, )", |x|
        make_expr!(x call 0:8 paren 5:8
            make_expr!(x member 0:4 dot 2:2 #de 3:4
                make_expr!(x str #"" 0:1)),),
        |e| e.emit("expect `)`, meet `,`").span(Span::new(6, 6))
    }

    case!{ parse_expr "defg[]", |x|
        make_expr!(x array index 0:5 bracket 4:5
            make_expr!(x path simple 0:3 #defg),),
        |e| e.emit(strings::EmptyIndexCall).span(Span::new(4, 5))
    }

    //              123456
    case!{ parse_expr "de[, ]", |x|
        make_expr!(x array index 0:5 bracket 2:5
            make_expr!(x path simple 0:1 #de),),
        |e| {
            e.emit("expect `]`, meet `,`").span(Span::new(3, 3));
            e.emit(strings::EmptyIndexCall).span(Span::new(2, 5));
        }
    }
}

#[test]
fn parse_call_expr() {
    fn cmp(actual: &(Span, Vec<Expr>), expect: &(Span, Vec<Expr>), arena: &Arena) -> bool {
        actual.0 == expect.0 && asti::Eq::eq(&actual.1, &expect.1, arena)
    }
    fn debug(value: &(Span, Vec<Expr>), arena: &Arena) -> String {
        format!("({:?}, {:?})", value.0, value.1.iter().map(|i| format!("{:?}", asti::debug(i, arena))).collect::<Vec<_>>())
    }

    case!{ novisit(cmp, debug) parse_call_expr "()", 
        |x| (Span::new(0, 1), Vec::new()),
    }

    case!{ novisit(cmp, debug) parse_call_expr "(\"hello\")", |x|
        (Span::new(0, 8), vec![
            make_expr!(x str #"hello" 1:7),
        ])
    }

    case!{ novisit(cmp, debug) parse_call_expr "(,)",
        |x| (Span::new(0, 2), Vec::new()),
        |e| e.emit("expect `)`, meet `,`").span(Span::new(1, 1))
    }
}

#[test]
fn parse_array_index_expr() {
    fn cmp(actual: &(Span, Vec<Expr>), expect: &(Span, Vec<Expr>), arena: &Arena) -> bool {
        actual.0 == expect.0 && asti::Eq::eq(&actual.1, &expect.1, arena)
    }
    fn debug(value: &(Span, Vec<Expr>), arena: &Arena) -> String {
        format!("({:?}, {:?})", value.0, value.1.iter().map(|i| format!("{:?}", asti::debug(i, arena))).collect::<Vec<_>>())
    }

    case!{ novisit(cmp, debug) parse_array_index_expr "[1, 2, ]", |x|
        (Span::new(0, 7), vec![
            make_expr!(x i32 1 1:1),
            make_expr!(x i32 2 4:4),
        ])
    }

    case!{ novisit(cmp, debug) parse_array_index_expr "[\"hello\"]", |x|
        (Span::new(0, 8), vec![
            make_expr!(x str #"hello" 1:7)
        ])
    }

    case!{ novisit(cmp, debug) parse_array_index_expr "[,]",
        |x| (Span::new(0, 2), Vec::new()),
        |e| {
            e.emit("expect `]`, meet `,`").span(Span::new(1, 1));
            e.emit(strings::EmptyIndexCall).span(Span::new(0, 2));
        },
    }
}

#[test]
fn parse_tuple_index_expr() {
    fn cmp(actual: &(Span, (i32, Span)), expect: &(Span, (i32, Span)), _: &Arena) -> bool {
        actual == expect
    }
    fn debug(value: &(Span, (i32, Span)), _: &Arena) -> impl fmt::Debug {
        *value
    }

    case!{ novisit(cmp, debug) parse_tuple_index_expr ".0", 
        |x| (Span::new(0, 0), (0, Span::new(1, 1))),
    }

    case!{ novisit(cmp, debug) parse_tuple_index_expr ".0i32",
        |x| (Span::new(0, 0), (0, Span::new(1, 4))),
        // TODO: cannot have postfix
    }

    case!{ novisit(cmp, debug) parse_tuple_index_expr ".0xDEADBEE",
        |x| (Span::new(0, 0), (0xDEADBEE, Span::new(1, 9))),
        // TODO: cannot have prefix
    }

    //      01234
    case!{ novisit(cmp, debug) parse_tuple_index_expr ".0i8",
        |x| (Span::new(0, 0), (0, Span::new(1, 3))),
        |e| e.emit(strings::InvalidTupleIndex).span(Span::new(1, 3)).help(strings::TupleIndexSyntaxHelp)
    }

    //      01234567890123456789012
    case!{ novisit(cmp, debug) parse_tuple_index_expr ".0xFFFF_FFFF_FFFF_FFFF",
        |x| (Span::new(0, 0), (0, Span::new(1, 21))),
        |e| e.emit(strings::InvalidTupleIndex).span(Span::new(1, 21)).help(strings::TupleIndexSyntaxHelp)
    }
}

#[test]
fn parse_primary_expr() {

    // this is the loop of tokens.nth(current) is left bracket does not cover everything and infinite loop is here
    // update 2017/6/17: this was a bug, but I forget detail
    case!{ parse_primary_expr "[a]", |x|
        make_expr!(x array 0:2
            make_expr!(x path simple 1:1 #a))
    }

    //                      0        1         2         3         4
    //                      01234567890123456789012345678901234567890123456
    case!{ parse_primary_expr "(463857, IEfN, atau8M, [fNAE, ((cAeJN4)), nHg])", |x|
        make_expr!(x tuple 0:46
            make_expr!(x i32 463857 1:6),
            make_expr!(x path simple 9:12 #IEfN),
            make_expr!(x path simple 15:20 #atau8M),
            make_expr!(x array 23:45
                make_expr!(x path simple 24:27 #fNAE),
                make_expr!(x paren 30:39
                    make_expr!(x paren 31:38
                        make_expr!(x path simple 32:37 #cAeJN4))),
                make_expr!(x path simple 42:44 #nHg)))
    }

    case!{ parse_primary_expr "10363", |x| make_expr!(x i32 10363 0:4) }

    case!{
    //   0         1         2         3         4         5         6         7         8         9         0         1         2         3
    //   01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567 8901234567890123456789012345678901234
    parse_primary_expr "[(0x7E), FFGqfJe, I4, [(m7A, (41, ([(jL, rAn, K0FgLc7h, true), C, w]), (J3cEFDG, d, (j8h))), (), \neIuArjF), 400, 0o535147505, 0xDB747]]", |x|
        make_expr!(x array 0:134
            make_expr!(x paren 1:6
               make_expr!(x i32 0x7E 2:5)),
            make_expr!(x path simple 9:15 #FFGqfJe),
            make_expr!(x path simple 18:19 #I4),
            make_expr!(x array 22:133
                make_expr!(x tuple 23:105
                    make_expr!(x path simple 24:26 #m7A),
                    make_expr!(x tuple 29:90
                        make_expr!(x i32 41 30:31),
                        make_expr!(x paren 34:68
                            make_expr!(x array 35:67
                                make_expr!(x tuple 36:60
                                    make_expr!(x path simple 37:38 #jL),
                                    make_expr!(x path simple 41:43 #rAn),
                                    make_expr!(x path simple 46:53 #K0FgLc7h),
                                    make_expr!(x true 56:59)),
                                make_expr!(x path simple 63:63 #C),
                                make_expr!(x path simple 66:66 #w))),
                        make_expr!(x tuple 71:89
                            make_expr!(x path simple 72:78 #J3cEFDG),
                            make_expr!(x path simple 81:81 #d),
                            make_expr!(x paren 84:88
                                make_expr!(x path simple 85:87 #j8h)))),
                    make_expr!(x unit 93:94),
                    make_expr!(x path simple 98:104 #eIuArjF)),
                make_expr!(x i32 400 108:110),
                make_expr!(x i32 0o535147505 113:123),
                make_expr!(x i32 0xDB747 126:132))),
    }

    case!{ parse_primary_expr "CMDoF", |x| make_expr!(x path simple 0:4 #CMDoF) }
    case!{ parse_primary_expr "false", |x| make_expr!(x false 0:4) }


    //                      0        1         2         3         4         5         6          7          8         9         A
    //                      12345678901234567890123456789012345678901234567890123456789012345678901 234 5678901234567890123456789012
    case!{ parse_primary_expr "[uy6, 4373577, [(q, AJBN0n, MDEgKh5,), KG, (NsL, ((), D, false, d, ), \"H=\"), true, ((vvB3, true, 5))]]", |x|
        make_expr!(x array 0:101
            make_expr!(x path simple 1:3 #uy6),
            make_expr!(x i32 4373577 6:12),
            make_expr!(x array 15:100
                make_expr!(x tuple 16:36
                    make_expr!(x path simple 17:17 #q),
                    make_expr!(x path simple 20:25 #AJBN0n),
                    make_expr!(x path simple 28:34 #MDEgKh5)),
                make_expr!(x path simple 39:40 #KG),
                make_expr!(x tuple 43:74
                    make_expr!(x path simple 44:46 #NsL),
                    make_expr!(x tuple 49:67
                        make_expr!(x unit 50:51),
                        make_expr!(x path simple 54:54 #D),
                        make_expr!(x false 57:61),
                        make_expr!(x path simple 64:64 #d)),
                    make_expr!(x str #"H=" 70:73)),
                make_expr!(x true 77:80),
                make_expr!(x paren 83:99
                    make_expr!(x tuple 84:98
                        make_expr!(x path simple 85:88 #vvB3),
                        make_expr!(x true 91:94),
                        make_expr!(x i32 5 97:97)))))
    }

    case!{ parse_primary_expr "(() )", |x|
        make_expr!(x paren 0:4
            make_expr!(x unit 1:2))
    }
    case!{ parse_primary_expr "((),)", |x|
        make_expr!(x tuple 0:4
            make_expr!(x unit 1:2))
    }

    case!{ parse_primary_expr "(\"o5\")", |x|
        make_expr!(x paren 0:5
            make_expr!(x str #"o5" 1:4))
    }

    //                      0        1         2
    //                      1234567890123456789012345678
    case!{ parse_primary_expr "(nn, ([false,true]), 183455)", |x|
        make_expr!(x tuple 0:27
            make_expr!(x path simple 1:2 #nn),
            make_expr!(x paren 5:18
                make_expr!(x array 6:17
                    make_expr!(x false 7:11),
                    make_expr!(x true 13:16))),
            make_expr!(x i32 183455 21:26))
    }

    //                      0        1         2         3         4         5         6         7
    //                      123456789012345678901234567890123456789012345678901234567890123456789012345678
    case!{ parse_primary_expr "((true, (mO, [(q5k),a], (((KttG))), (K5DJ, r, ())), (McsaEdfdfalse,)), rIOKt,)", |x|
        make_expr!(x tuple 0:77
            make_expr!(x tuple 1:68
                make_expr!(x true 2:5),
                make_expr!(x tuple 8:49
                    make_expr!(x path simple 9:10 #mO),
                    make_expr!(x array 13:21
                        make_expr!(x paren 14:18
                            make_expr!(x path simple 15:17 #q5k)),
                        make_expr!(x path simple 20:20 #a)),
                    make_expr!(x paren 24:33
                        make_expr!(x paren 25:32
                            make_expr!(x paren 26:31
                                make_expr!(x path simple 27:30 #KttG)))),
                    make_expr!(x tuple 36:48
                        make_expr!(x path simple 37:40 #K5DJ),
                        make_expr!(x path simple 43:43 #r),
                        make_expr!(x unit 46:47))),
                make_expr!(x tuple 52:67
                    make_expr!(x path simple 53:65 #McsaEdfdfalse))),
            make_expr!(x path simple 71:75 #rIOKt))
    }

    //                                      0          1         2
    //                                      12 345 67890123456789012
    case!{ parse_primary_expr "[\"il\", 0o52u32, sO04n]", |x|
        make_expr!(x array 0:21
            make_expr!(x str #"il" 1:4),
            make_expr!(x u32 0o52 7:13),
            make_expr!(x path simple 16:20 #sO04n))
    }
    //                                      12345678
    case!{ parse_primary_expr "['f',()]", |x|
        make_expr!(x array 0:7
            make_expr!(x char 1:3 'f'),
            make_expr!(x unit 5:6))
    }
    case!{ parse_primary_expr "[]", |x| make_expr!(x array 0:1) }

    //                                      0        1           2         3         4
    //                                      12345 678901 234567890123456789012345678901234
    case!{ parse_primary_expr "[8, \"@=?GF\", 87r32, 1340323.74r64, FKOxAvx5]", |x|
        make_expr!(x array 0:43
            make_expr!(x i32 8 1:1),
            make_expr!(x str #"@=?GF" 4:10),
            make_expr!(x r32 87f32 13:17),
            make_expr!(x r64 1340323.74 20:32),
            make_expr!(x path simple 35:42 #FKOxAvx5)),
    }

    //                                        0        1         2         3         4         5         6
    //                                        123456789012345678901234567890123456789012345678901234567890123
    case!{ parse_primary_expr r#"  [[dnr4, lGFd3yL, tJ], ['\\', p, (xGaBwiL,), DE], true, aB8aE]"#, |x|
        make_expr!(x array 2:62
            make_expr!(x array 3:21
                make_expr!(x path simple 4:7 #dnr4),
                make_expr!(x path simple 10:16 #lGFd3yL),
                make_expr!(x path simple 19:20 #tJ)),
            make_expr!(x array 24:48
                make_expr!(x char 25:28 '\\'),
                make_expr!(x path simple 31:31 #p),
                make_expr!(x tuple 34:43
                    make_expr!(x path simple 35:41 #xGaBwiL)),
                make_expr!(x path simple 46:47 #DE)),
            make_expr!(x true 51:54),
            make_expr!(x path simple 57:61 #aB8aE)),
    }

    // Previous manual tests
    //                      0         1           2          3         4         5           6
    //                      012345678901234 5678 9012 3456789012345678901234567890123 456789 0123456
    case!{ parse_primary_expr "[abc, 123u32, \"456\", '\\u0065', false, (), (a), (abc, \"hello\", ), ]", |x|
        make_expr!(x array 0:65
            make_expr!(x path simple 1:3 #abc),
            make_expr!(x u32 123 6:11),
            make_expr!(x str #"456" 14:18),
            make_expr!(x char 21:28 '\u{0065}'),
            make_expr!(x false 31:35),
            make_expr!(x unit 38:39),
            make_expr!(x paren 42:44
                make_expr!(x path simple 43:43 #a)),
            make_expr!(x tuple 47:62
                make_expr!(x path simple 48:50 #abc),
                make_expr!(x str #"hello" 53:59)))
    }

    case!{ parse_primary_expr "(                             )", |x|
        make_expr!(x unit 0:30)
    }

    case!{ parse_primary_expr "(,)",
        |x| make_expr!(x unit 0:2),
        |e| e.emit("expect `)`, meet `,`").span(Span::new(1, 1)),
    }
}

#[test]
fn parse_postfix_expr() {

    //                         0         1         2         3         4         5
    //                         0123456789012345678901234567890123456789012345678901234567
    case!{ parse_postfix_expr "a.b(c, d, e).f(g, h, i,)(u,).j[k].l().m[n, o, p][r, s, t,]", |x|
        make_expr!(x array index 0:57 bracket 48:57
            make_expr!(x array index 0:47 bracket 39:47
                make_expr!(x member 0:38 dot 37:37 #m 38:38
                    make_expr!(x call 0:36 paren 35:36
                        make_expr!(x member 0:34 dot 33:33 #l 34:34
                            make_expr!(x array index 0:32 bracket 30:32
                                make_expr!(x member 0:29 dot 28:28 #j 29:29
                                    make_expr!(x call 0:27 paren 24:27
                                        make_expr!(x call 0:23 paren 14:23
                                            make_expr!(x member 0:13 dot 12:12 #f 13:13
                                                make_expr!(x call 0:11 paren 3:11
                                                    make_expr!(x member 0:2 dot 1:1 #b 2:2
                                                        make_expr!(x path simple 0:0 #a)),
                                                    make_expr!(x path simple 4:4 #c),
                                                    make_expr!(x path simple 7:7 #d),
                                                    make_expr!(x path simple 10:10 #e))),
                                            make_expr!(x path simple 15:15 #g),
                                            make_expr!(x path simple 18:18 #h),
                                            make_expr!(x path simple 21:21 #i)),
                                        make_expr!(x path simple 25:25 #u))),
                                make_expr!(x path simple 31:31 #k))),)),
                make_expr!(x path simple 40:40 #n),
                make_expr!(x path simple 43:43 #o),
                make_expr!(x path simple 46:46 #p)),
            make_expr!(x path simple 49:49 #r),
            make_expr!(x path simple 52:52 #s),
            make_expr!(x path simple 55:55 #t))
    }

    //      012345678901234567890
    case!{ parse_postfix_expr "i.collect::<Vec<i32>>", |x|
        make_expr!(x member 0:20 dot 1:1 #collect 2:8 quote 11:20
            make_expr!(x path simple 0:0 #i),
            make_type!(x path 12:19
                make_path!(x segment generic 12:19 #Vec 12:14 quote 15:19
                    make_type!(x prim 16:18 I32)))),
    }

    //                         012345678901234567890123
    case!{ parse_postfix_expr "iter.collect::<Vec<_>>()", |x|
        make_expr!(x call 0:23 paren 22:23
            make_expr!(x member 0:21 dot 4:4 #collect 5:11 quote 14:21
                make_expr!(x path simple 0:3 #iter),
                make_type!(x path 15:20
                    make_path!(x segment generic 15:20 #Vec 15:17 quote 18:20
                        make_type!(x path simple 19:19 #_)))),)
    }

    //      01234567
    case!{ parse_postfix_expr "(0, 0).0", |x|
        make_expr!(x tuple index 0:7 dot 6:6 i32 0 7:7
            make_expr!(x tuple 0:5
                make_expr!(x i32 0 1:1),
                make_expr!(x i32 0 4:4))),
    }

    //      0         1         2         3         4         5
    //      012345678901234567890123456789012345678901234567890123456
    case!{ parse_postfix_expr "string::<wchar>{ size: 0, cap: 0, data: uninitialized() }", |x|
        make_expr!(x object 0:56 quote 15:56
            make_expr!(x path 0:14
                make_path!(x segment generic 0:14 #string 0:5 quote 8:14
                    make_type!(x simple 9:13 #wchar))),
            make_expr!(x object field 17:23 #size 17:20 colon 21:21
                make_expr!(x i32 0 23:23)),
            make_expr!(x object field 26:31 #cap 26:28 colon 29:29
                make_expr!(x i32 0 31:31)),
            make_expr!(x object field 34:54 #data 34:37 colon 38:38
                make_expr!(x call 40:54 paren 53:54
                    make_expr!(x path simple 40:52 #uninitialized),))),
    }

    case!{ parse_postfix_expr "a[]", |x|
        make_expr!(x array index 0:2 bracket 1:2
            make_expr!(x path simple 0:0 #a),),
        |e| e.emit(strings::EmptyIndexCall).span(Span::new(1, 2))
    }

    case!{ parse_postfix_expr "a[, ]", |x|
        make_expr!(x array index 0:4 bracket 1:4
            make_expr!(x path simple 0:0 #a),),
        |e| {
            e.emit("expect `]`, meet `,`").span(Span::new(2, 2));
            e.emit(strings::EmptyIndexCall).span(Span::new(1, 4));
        },
    }

    case!{ parse_postfix_expr "a(, )", |x|
        make_expr!(x call 0:4 paren 1:4
            make_expr!(x path simple 0:0 #a),),
        |e| e.emit("expect `)`, meet `,`").span(Span::new(2, 2)),
    }

    //      01234
    case!{ parse_postfix_expr "a.0i8", |x|
        make_expr!(x tuple index 0:4 dot 1:1 i32 0 2:4
            make_expr!(x path simple 0:0 #a)),
        |e| e.emit(strings::InvalidTupleIndex).span(Span::new(2, 4)).help(strings::TupleIndexSyntaxHelp)
    }

    //      01234567890123456789012
    case!{ parse_postfix_expr "a.0xFFFF_FFFF_FFFF_FFFF", |x|
        make_expr!(x tuple index 0:22 dot 1:1 i32 0 2:22
            make_expr!(x path simple 0:0 #a)),
        |e| e.emit(strings::InvalidTupleIndex).span(Span::new(2, 22)).help(strings::TupleIndexSyntaxHelp)
    }

    // //      0123456789
    // case!{ parse_postfix_expr "a.abc::def", None, |e| }

    // //      0123456789012345678
    // case!{ parse_postfix_expr "a.abc::<def>::<ghi>", None, |e| }
}

#[test]
fn range_expr_parse() {

    case!{ parse_range_expr "..", |x|
        make_expr!(x range full 0:1)
    }

    case!{ parse_range_expr "..1 + 1", |x|
        make_expr!(x range right 0:6
            make_expr!(x binary 2:6 Add 4:4
                make_expr!(x i32 1 2:2),
                make_expr!(x i32 1 6:6)))
    }

    case!{ parse_range_expr "1 ..", |x|
        make_expr!(x range left 0:3
            make_expr!(x i32 1 0:0))
    }
}

#[test]
fn parse_tuple_expr() {
    //                                   01234567
    case!{ parse_tuple_expr "(1, '2')", |x|
        make_expr!(x tuple 0:7
            make_expr!(x i32 1 1:1),
            make_expr!(x char 4:6 '2'))
    }
    //                                   0123456
    case!{ parse_tuple_expr "(1 + 1)", |x|
        make_expr!(x paren 0:6
            make_expr!(x binary 1:5 Add 3:3
                make_expr!(x i32 1 1:1),
                make_expr!(x i32 1 5:5)))
    }

    case!{ parse_tuple_expr "( , )",
        |x| make_expr!(x unit 0: 4),
        |e| e.emit("expect `)`, meet `,`").span(Span::new(2, 2)),
    }
}

#[test]
fn unary_expr_parse() {

    case!{ parse_unary_expr "1", |x| make_expr!(x i32 1 0:0) }

    case!{ parse_unary_expr "!~!1", |x|
        make_expr!(x unary 0:3 Not 0:0
            make_expr!(x unary 1:3 Tilde 1:1
                make_expr!(x unary 2:3 Not 2:2
                    make_expr!(x i32 1 3:3))))
    }

    case!{ parse_unary_expr "&a(&b)", |x|
        make_expr!(x unary 0:5 And 0:0
            make_expr!(x call 1:5 paren 2:5
                make_expr!(x path simple 1:1 #a),
                make_expr!(x unary 3:4 And 3:3
                    make_expr!(x path simple 4:4 #b))))
    }
}
