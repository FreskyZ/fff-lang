///! syntax:binary_expr::
// MultiplicativeExpression = UnaryExpr | MultiplicativeExpression MultiplicativeOperator UnaryExpr
// AdditiveExpression = MultiplicativeExpression | AdditiveExpression AdditiveOperator MultiplicativeExpression
// RelationalExpression = AdditiveExpression | RelationalExpression RelationalOperator AdditiveExpression
// ShiftExpression = RelationalExpression | ShiftExpression ShiftOperator RelationalExpression
// BitAndExpression = ShiftExpression | BitAndExpression BitAndOperator ShiftExpression
// BitXorExpression = BitAndExpression | BitXorExpression BitXorOperator BitAndExpression
// BitOrExpression = BitXorExpression | BitOrExpression BitOrOperator BitXorExpression
// EqualityExpression = BitOrExpression | EqualityExpression EqualityOperator BitOrExpression  // `==` and `!=` lower than `|` for `if (enum_var & enum_mem1 == enum_mem1)`
// LogicalAndExpression = EqualityExpression | LogicalAndExpression LogicalAndOperator EqualityExpression
// LogicalOrExpression = LogicalAndExpression | LogicalOrExpression LogicalOrOperator LogicalAndExpression

use super::prelude::*;

impl Parser for BinaryExpr {
    type Output = Expr;

    fn parse(cx: &mut ParseContext) -> Result<Expr, Unexpected> {
        #[cfg(feature = "trace_binary_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[PrimaryExpr] "); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_binary_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        return parse_logical_or(cx);

        fn check_relational_expr(cx: &mut ParseContext, expr: &Expr) {
            if let Expr::Binary(BinaryExpr{ operator: Separator::Gt, operator_span: gt_span, left_expr, .. }) = expr {
                if let Expr::Binary(BinaryExpr{ operator: Separator::Lt, operator_span: lt_span, .. }) = left_expr.as_ref() {
                    cx.emit(strings::MaybeGeneric).span(*lt_span).span(*gt_span).help(strings::MaybeGenericHelp);
                }
            }
        }

        macro_rules! impl_binary_parser {
            ($parser_name:ident, $previous_parser_name:path, $kind:ident $(,$check:path)?) => (
                fn $parser_name(cx: &mut ParseContext) -> Result<Expr, Unexpected> {
                    trace!("parsing {}", stringify!($parser_name));

                    let mut current_expr = $previous_parser_name(cx)?;
                    loop {
                        if let Some((sep, sep_span)) = cx.try_expect_sep_kind(SeparatorKind::$kind) {
                            let right_expr = $previous_parser_name(cx)?;
                            current_expr = Expr::Binary(BinaryExpr{
                                all_span: current_expr.get_all_span() + right_expr.get_all_span(),
                                left_expr: Box::new(current_expr),
                                operator: sep, 
                                operator_span: sep_span, 
                                right_expr: Box::new(right_expr)
                            });
                            $($check(cx, &current_expr))?
                        } else {
                            return Ok(current_expr);
                        }
                    }
                }
            )
        }
        impl_binary_parser! { parse_multiplicative, UnaryExpr::parse, Multiplicative }
        impl_binary_parser! { parse_additive, parse_multiplicative, Additive }
        impl_binary_parser! { parse_relational, parse_additive, Relational, check_relational_expr }
        impl_binary_parser! { parse_shift, parse_relational, Shift }
        impl_binary_parser! { parse_bitand, parse_shift, BitAnd }
        impl_binary_parser! { parse_bitxor, parse_bitand, BitXor }
        impl_binary_parser! { parse_bitor, parse_bitxor, BitOr }
        impl_binary_parser! { parse_equality, parse_bitor, Equality }
        impl_binary_parser! { parse_logical_and, parse_equality, LogicalAnd }
        impl_binary_parser! { parse_logical_or, parse_logical_and, LogicalOr }
    }
}

impl Node for BinaryExpr {

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_binary_expr(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(&self.left_expr)?;
        v.visit_expr(&self.right_expr)
    }
}

#[cfg(test)] #[test]
fn binary_expr_parse() {

    //                                     123456789012345
    case!{ "[1] * [2] / [3]" as BinaryExpr,
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
    case!{ "a * b / c + d % e - f" as BinaryExpr,  // ((((a * b) / c) + (d % e)) - f)
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
    case!{ "a * b << h / c + d % e - f >> g" as BinaryExpr, // (((a * b) << (((h / c) + (d % e)) - f)) >> g)
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
    case!{ "a * b << h / c + d % e - f >> g > h * i < j << k >= m && n || o & p | q ^ r != s == t" as BinaryExpr,
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
    case!{ "a & b == c" as BinaryExpr, // ((a & b) == c)
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
    case!{ "0 + 6 ^ 3 & 3 / 3 - 8 && 2 & 0 + 6" as BinaryExpr, // (((0 + 6) ^ (3 & ((3 / 3) - 8))) && (2 & (0 + 6)))
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
    case!{ "7 > 1 | 0 % 8 | 1 % 7 * 3 % 6 == 1 >> 8 % 3 ^ 6 << 0 ^ 2 >> 6 || 1 - 0" as BinaryExpr,
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
    case!{ "7 >> 3 == 8 / 1 && 6 == 1 <= 3 % 6 ^ 3 - 1 - 2 >> 7 || 1 >= 1" as BinaryExpr,
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
    case!{ "4 >> 7" as BinaryExpr,
        make_expr!(binary 0:5 GtGt 2:3
            make_expr!(i32 4 0:0),
            make_expr!(i32 7 5:5))
    }
    //                                     0        1         2         3         4         5         6
    //                                     12345678901234567890123456789012345678901234567890123456789012345
    case!{ "8 & 0 | 7 + 7 | 7 * 0 && 1 - 2 * 3 | 0 - 7 >= 6 >> 5 % 5 || 5 % 3" as BinaryExpr,
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
    case!{ "3 <= 2 + 4 <= 5 && 3 < 3 + 2 >> 1 * 2 & 8 && 1 >= 1 < 0 || 6 < 4 * 4" as BinaryExpr,
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
    case!{ "5 >= 6 | 3 == 4 && 3" as BinaryExpr,
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
    case!{ "6 && 7 >> 8 && 0 / 8 * 7 + 5 < 5 / 5 >> 5 - 1 >= 6 > 8 | 6 >> 5 > 2 + 1 || 0" as BinaryExpr,
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

    case!{ "1<2>3" as BinaryExpr,
        make_expr!(binary 0:4 Gt 3:3
            make_expr!(binary 0:2 Lt 1:1
                make_expr!(i32 1 0:0),
                make_expr!(i32 2 2:2)),
            make_expr!(i32 3 4:4)
        ), errors make_errors!(e: e.emit(strings::MaybeGeneric).span(Span::new(1, 1)).span(Span::new(3, 3)).help(strings::MaybeGenericHelp))
    }
}
