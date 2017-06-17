///! fff-lang
///!
///! syntax/binary_expr
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

// TODO: try use `enum Expr` instead of `enum Binary{ enum Unary { enum Postfix { enum Primary { ... } } } }

use std::fmt;

use codemap::Span;
use lexical::Token;
use lexical::SeperatorKind;
use lexical::SeperatorCategory;

use super::UnaryExpr;
use super::Expr;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct BinaryExpr {
    pub left_expr: Box<Expr>,
    pub right_expr: Box<Expr>,
    pub operator: SeperatorKind,            // this means every binary operator matches a binary expr
    pub operator_span: Span, 
    pub all_span: Span,
}
impl ISyntaxItemFormat for BinaryExpr {
    fn format(&self, indent: u32) -> String {
        format!("{}BinaryExpr <{:?}>\n{}\n{}{} <{:?}>\n{}", 
            Expr::indent_str(indent), self.all_span,
            self.left_expr.format(indent + 1),
            Expr::indent_str(indent + 1), self.operator, self.operator_span,
            self.right_expr.format(indent + 1),
        )
    }
}
impl fmt::Debug for BinaryExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl From<BinaryExpr> for Expr {
    fn from(binary_expr: BinaryExpr) -> Expr { Expr::Binary(binary_expr) }
}
impl BinaryExpr {

    pub fn new(left_expr: Expr, operator: SeperatorKind, operator_span: Span, right_expr: Expr) -> BinaryExpr {
        BinaryExpr{
            all_span: left_expr.get_all_span().merge(&right_expr.get_all_span()),
            left_expr: Box::new(left_expr),
            right_expr: Box::new(right_expr),
            operator, operator_span
        }
    }
}
// No grammar because Binary::is_first_final <=> Expr::is_first_final
impl ISyntaxItemParse for BinaryExpr {
    type Target = Expr;

    fn parse(sess: &mut ParseSession) -> ParseResult<Expr> {  
        #[cfg(feature = "trace_binary_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[PrimaryExpr] "); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_binary_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        return parse_logical_or(sess);

        macro_rules! impl_binary_parser {
            ($parser_name: ident, $previous_parser: expr, $op_category: expr) => (
                fn $parser_name(sess: &mut ParseSession) -> ParseResult<Expr> {
                    trace!("parsing {}", stringify!($parser_name));

                    let mut current_retval = $previous_parser(sess)?;
                    loop {
                        match (sess.tk, sess.pos) {
                            (&Token::Sep(operator), operator_strpos) if operator.is_category($op_category) => {
                                sess.move_next();
                                let right_expr = $previous_parser(sess)?;
                                current_retval = Expr::Binary(BinaryExpr::new(current_retval, operator, operator_strpos, right_expr));
                                trace!("    changing current ret_val to {:?}", current_retval);
                            }
                            _ => {
                                trace!("   operator or other not '{}', return left: {:?}", stringify!($op_category), current_ret_val);
                                return Ok(current_retval);
                            }
                        }
                    }
                }
            )
        }
        impl_binary_parser! { parse_multiplicative, UnaryExpr::parse, SeperatorCategory::Multiplicative }
        impl_binary_parser! { parse_additive, parse_multiplicative, SeperatorCategory::Additive }
        impl_binary_parser! { parse_relational, parse_additive, SeperatorCategory::Relational }
        impl_binary_parser! { parse_shift, parse_relational, SeperatorCategory::Shift }
        impl_binary_parser! { parse_bitand, parse_shift, SeperatorCategory::BitAnd }
        impl_binary_parser! { parse_bitxor, parse_bitand, SeperatorCategory::BitXor }
        impl_binary_parser! { parse_bitor, parse_bitxor, SeperatorCategory::BitOr }
        impl_binary_parser! { parse_equality, parse_bitor, SeperatorCategory::Equality }
        impl_binary_parser! { parse_logical_and, parse_equality, SeperatorCategory::LogicalAnd }
        impl_binary_parser! { parse_logical_or, parse_logical_and, SeperatorCategory::LogicalOr }    
    }
}

#[cfg(test)] #[test]
fn binary_expr_format() {
    use lexical::LitValue;
    use super::LitExpr;
    
    let binary_expr = Expr::Binary(BinaryExpr::new(
        Expr::Lit(LitExpr::new(LitValue::from(1), make_span!(0, 0))),
        SeperatorKind::Add,
        make_span!(2, 2),
        Expr::Lit(LitExpr::new(LitValue::from(2), make_span!(4, 4))),
    ));
    assert_eq!(binary_expr.format(0), "BinaryExpr <<0>0-4>\n  Literal (i32)1 <<0>0-0>\n  + <<0>2-2>\n  Literal (i32)2 <<0>4-4>");
}

#[cfg(remove_this_after_expr_refactor)]
#[cfg(test)] #[test]
fn binary_expr_parse() {
    use lexical::NumLitValue;
    use super::super::ISyntaxItemWithStr;
    
    macro_rules! ident { ($ident_name: expr, $strpos: expr) => (Expr::new_primary(PrimaryExpr::new_ident($ident_name.to_owned(), $strpos))) }
    macro_rules! int { ($value: expr, $strpos: expr) => (Expr::new_primary(PrimaryExpr::Lit_num(NumLitValue::from($value), $strpos))) }

    let new_binary = Expr::new_binary;

    // my random tests
    //                                     123456789012345
    assert_eq!{ Expr::with_test_str("[1] * [2] / [3]"), 
        Expr::new_binary(
            Expr::new_binary(
                Expr::new_primary(PrimaryExpr::new_array(make_span!(0, 2), vec![
                    int!(1, make_span!(1, 1)),
                ])), 
                SeperatorKind::Mul, make_span!(4, 4),
                Expr::new_primary(PrimaryExpr::new_array(make_span!(6, 8), vec![
                    int!(2, make_span!(7, 7)),
                ])),
            ),
            SeperatorKind::Div, make_span!(10, 10),
            Expr::new_primary(PrimaryExpr::new_array(make_span!(12, 14), vec![
                int!(3, make_span!(13, 13)),
            ]))
        )
    }           
    //                                     0        1         2
    //                                     123456789012345678901
    assert_eq!{ Expr::with_test_str("a * b / c + d % e - f"),  // ((((a * b) / c) + (d % e)) - f)
        Expr::new_binary(
            Expr::new_binary(
                Expr::new_binary(
                    Expr::new_binary(
                        ident!("a", make_span!(0, 0)),
                        SeperatorKind::Mul, make_span!(2, 2), 
                        ident!("b", make_span!(4, 4)),
                    ),
                    SeperatorKind::Div, make_span!(6, 6),
                    ident!("c", make_span!(8, 8)),
                ),
                SeperatorKind::Add, make_span!(10, 10),
                Expr::new_binary(
                    ident!("d", make_span!(12, 12)),
                    SeperatorKind::Rem, make_span!(14, 14),
                    ident!("e", make_span!(16, 16)),
                )
            ),
            SeperatorKind::Sub, make_span!(18, 18),
            ident!("f", make_span!(20, 20)),
        )
    }           
    //                                     0        1         2         3
    //                                     1234567890123456789012345678901
    assert_eq!{ Expr::with_test_str("a * b << h / c + d % e - f >> g"), // (((a * b) << (((h / c) + (d % e)) - f)) >> g)
        Expr::new_binary(
            Expr::new_binary(
                Expr::new_binary(
                    ident!("a", make_span!(0, 0)),
                    SeperatorKind::Mul, make_span!(2, 2),
                    ident!("b", make_span!(4, 4))
                ),
                SeperatorKind::ShiftLeft, make_span!(6, 7),
                Expr::new_binary(
                    Expr::new_binary(
                        Expr::new_binary(
                            ident!("h", make_span!(9, 9)),
                            SeperatorKind::Div, make_span!(11, 11),
                            ident!("c", make_span!(13, 13))
                        ),
                        SeperatorKind::Add, make_span!(15, 15),
                        Expr::new_binary(
                            ident!("d", make_span!(17, 17)),
                            SeperatorKind::Rem, make_span!(19, 19),
                            ident!("e", make_span!(21, 21))
                        )
                    ),
                    SeperatorKind::Sub, make_span!(23, 23),
                    ident!("f", make_span!(25, 25))
                )
            ),
            SeperatorKind::ShiftRight, make_span!(27, 28),
            ident!("g", make_span!(30, 30)),
        )
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
    assert_eq!{ Expr::with_test_str("a * b << h / c + d % e - f >> g > h * i < j << k >= m && n || o & p | q ^ r != s == t"),
        // */%, +-, <><=>=, <<>>, &, ^, |, ==!=, &&, ||
        // ((((((a * b) << (((h / c) + (d % e)) - f)) >> ((g > (h * i)) < j)) << (k >= m)) && n) || ((((o & p) | (q ^ r)) != s) == t))
        Expr::new_binary(
            Expr::new_binary(
                Expr::new_binary(
                    Expr::new_binary(
                        Expr::new_binary(
                            Expr::new_binary(
                                ident!("a", make_span!(0, 0)),
                                SeperatorKind::Mul, make_span!(2, 2),
                                ident!("b", make_span!(4, 4)),
                            ),
                            SeperatorKind::ShiftLeft, make_span!(6, 7),
                            Expr::new_binary(
                                Expr::new_binary(
                                    Expr::new_binary(
                                        ident!("h", make_span!(9, 9)),
                                        SeperatorKind::Div, make_span!(11, 11),
                                        ident!("c", make_span!(13, 13)),
                                    ),
                                    SeperatorKind::Add, make_span!(15, 15),
                                    Expr::new_binary(
                                        ident!("d", make_span!(17, 17)),
                                        SeperatorKind::Rem, make_span!(19, 19),
                                        ident!("e", make_span!(21, 21)),
                                    )
                                ),
                                SeperatorKind::Sub, make_span!(23, 23),
                                ident!("f", make_span!(25, 25))
                            ),
                        ),
                        SeperatorKind::ShiftRight, make_span!(27, 28),
                        Expr::new_binary(
                            Expr::new_binary(
                                ident!("g", make_span!(30, 30)),
                                SeperatorKind::Great, make_span!(32, 32),
                                Expr::new_binary(
                                    ident!("h", make_span!(34, 34)),
                                    SeperatorKind::Mul, make_span!(36, 36),
                                    ident!("i", make_span!(38, 38)),
                                )
                            ),
                            SeperatorKind::Less, make_span!(40, 40),
                            ident!("j", make_span!(42, 42)),
                        )
                    ),
                    SeperatorKind::ShiftLeft, make_span!(44, 45),
                    Expr::new_binary(
                        ident!("k", make_span!(47, 47)),
                        SeperatorKind::GreatEqual, make_span!(49, 50),
                        ident!("m", make_span!(52, 52)),
                    )
                ),
                SeperatorKind::LogicalAnd, make_span!(54, 55),
                ident!("n", make_span!(57, 57))
            ),
            SeperatorKind::LogicalOr, make_span!(59, 60),
            Expr::new_binary(
                Expr::new_binary(
                    Expr::new_binary(
                        Expr::new_binary(
                            ident!("o", make_span!(62, 62)),
                            SeperatorKind::BitAnd, make_span!(64, 64),
                            ident!("p", make_span!(66, 66)),
                        ),
                        SeperatorKind::BitOr, make_span!(68, 68),
                        Expr::new_binary(
                            ident!("q", make_span!(70, 70)),
                            SeperatorKind::BitXor, make_span!(72, 72),
                            ident!("r", make_span!(74, 74)),
                        )
                    ),
                    SeperatorKind::NotEqual, make_span!(76, 77),
                    ident!("s", make_span!(79, 79)),
                ),
                SeperatorKind::Equal, make_span!(81, 82),
                ident!("t", make_span!(84, 84))
            )
        )
    }
    //                                     1234567890
    assert_eq!{ Expr::with_test_str("a & b == c"), // ((a & b) == c)
        Expr::new_binary(
            Expr::new_binary(
                ident!("a", make_span!(0, 0)),
                SeperatorKind::BitAnd, make_span!(2, 2),
                ident!("b", make_span!(4, 4)),
            ),
            SeperatorKind::Equal, make_span!(6, 7),
            ident!("c", make_span!(9, 9)),
        )
    }

    // 1 + 2 << 3 => (1 + 2) << 3, don't want this
    // cout << 5 + 6 => cout << (5 + 6), want this

    // program generated random tests
    //                                     0        1         2         3    
    //                                     1234567890123456789012345678901234
    assert_eq!{ Expr::with_test_str("0 + 6 ^ 3 & 3 / 3 - 8 && 2 & 0 + 6"), // (((0 + 6) ^ (3 & ((3 / 3) - 8))) && (2 & (0 + 6)))
        Expr::new_binary(
            Expr::new_binary(
                Expr::new_binary(
                    int!(0, make_span!(0, 0)),
                    SeperatorKind::Add, make_span!(2, 2),
                    int!(6, make_span!(4, 4))
                ),
                SeperatorKind::BitXor, make_span!(6, 6),
                Expr::new_binary(
                    int!(3, make_span!(8, 8)),
                    SeperatorKind::BitAnd, make_span!(10, 10),
                    Expr::new_binary(
                        Expr::new_binary(
                            int!(3, make_span!(12, 12)),
                            SeperatorKind::Div, make_span!(14, 14),
                            int!(3, make_span!(16, 16))
                        ),
                        SeperatorKind::Sub, make_span!(18, 18),
                        int!(8, make_span!(20, 20))
                    )
                )
            ),
            SeperatorKind::LogicalAnd, make_span!(22, 23),
            Expr::new_binary(
                int!(2, make_span!(25, 25)),
                SeperatorKind::BitAnd, make_span!(27, 27),
                Expr::new_binary(
                    int!(0, make_span!(29, 29)),
                    SeperatorKind::Add, make_span!(31, 31),
                    int!(6, make_span!(33, 33))
                )
            )
        )
    }
    //                                     0        1         2         3         4         5         6         7
    //                                     1234567890123456789012345678901234567890123456789012345678901234567890
    assert_eq!{ Expr::with_test_str("7 > 1 | 0 % 8 | 1 % 7 * 3 % 6 == 1 >> 8 % 3 ^ 6 << 0 ^ 2 >> 6 || 1 - 0"),
        // (((((7 > 1) | (0 % 8)) | (((1 % 7) * 3) % 6)) == (((1 >> (8 % 3)) ^ (6 << 0)) ^ (2 >> 6))) || (1 - 0))
        new_binary(
            new_binary(
                new_binary(
                    new_binary(
                        new_binary(
                            int!(7, make_span!(0, 0)),
                            SeperatorKind::Great, make_span!(2, 2),
                            int!(1, make_span!(4, 4)),
                        ),
                        SeperatorKind::BitOr, make_span!(6, 6),
                        new_binary(
                            int!(0, make_span!(8, 8)),
                            SeperatorKind::Rem, make_span!(10, 10),
                            int!(8, make_span!(12, 12))
                        )
                    ), 
                    SeperatorKind::BitOr, make_span!(14, 14),
                    new_binary(
                        new_binary(
                            new_binary(
                                int!(1, make_span!(16, 16)),
                                SeperatorKind::Rem, make_span!(18, 18),
                                int!(7, make_span!(20, 20)),
                            ),
                            SeperatorKind::Mul, make_span!(22, 22),
                            int!(3, make_span!(24, 24)),
                        ),
                        SeperatorKind::Rem, make_span!(26, 26),
                        int!(6, make_span!(28, 28))
                    )
                ),
                SeperatorKind::Equal, make_span!(30, 31),
                new_binary(
                    new_binary(
                        new_binary(
                            int!(1, make_span!(33, 33)),
                            SeperatorKind::ShiftRight, make_span!(35, 36),
                            new_binary(
                                int!(8, make_span!(38, 38)),
                                SeperatorKind::Rem, make_span!(40, 40),
                                int!(3, make_span!(42, 42)),
                            )
                        ),
                        SeperatorKind::BitXor, make_span!(44, 44),
                        new_binary(
                            int!(6, make_span!(46, 46)),
                            SeperatorKind::ShiftLeft, make_span!(48, 49),
                            int!(0, make_span!(51, 51)),
                        )
                    ),
                    SeperatorKind::BitXor, make_span!(53, 53),
                    new_binary(
                        int!(2, make_span!(55, 55)),
                        SeperatorKind::ShiftRight, make_span!(57, 58),
                        int!(6, make_span!(60, 60)),
                    )
                )
            ),
            SeperatorKind::LogicalOr, make_span!(62, 63),
            new_binary(
                int!(1, make_span!(65, 65)),
                SeperatorKind::Sub, make_span!(67, 67),
                int!(0, make_span!(69, 69))
            )
        )
    }
    //                                     0        1         2         3         4         5         6 
    //                                     1234567890123456789012345678901234567890123456789012345678901
    assert_eq!{ Expr::with_test_str("7 >> 3 == 8 / 1 && 6 == 1 <= 3 % 6 ^ 3 - 1 - 2 >> 7 || 1 >= 1"),
        // */%, +-, <><=>=, <<>>, &, ^, |, ==!=, &&, ||
        // ((((7 >> 3) == (8 / 1)) && (6 == ((1 <= (3 % 6)) ^ (((3 - 1) - 2) >> 7)))) || (1 >= 1))
        Expr::new_binary(
            Expr::new_binary(
                Expr::new_binary(
                    Expr::new_binary(
                        int!(7, make_span!(0, 0)),
                        SeperatorKind::ShiftRight, make_span!(2, 3),
                        int!(3, make_span!(5, 5)),
                    ),
                    SeperatorKind::Equal, make_span!(7, 8),
                    Expr::new_binary(
                        int!(8, make_span!(10, 10)),
                        SeperatorKind::Div, make_span!(12, 12),
                        int!(1, make_span!(14, 14)),
                    )
                ),
                SeperatorKind::LogicalAnd, make_span!(16, 17),
                Expr::new_binary(
                    int!(6, make_span!(19, 19)),
                    SeperatorKind::Equal, make_span!(21, 22),
                    Expr::new_binary(
                        Expr::new_binary(
                            int!(1, make_span!(24, 24)),
                            SeperatorKind::LessEqual, make_span!(26, 27),
                            Expr::new_binary(
                                int!(3, make_span!(29, 29)),
                                SeperatorKind::Rem, make_span!(31, 31),
                                int!(6, make_span!(33, 33))
                            )
                        ),
                        SeperatorKind::BitXor, make_span!(35, 35),
                        Expr::new_binary(
                            Expr::new_binary(
                                Expr::new_binary(
                                    int!(3, make_span!(37, 37)),
                                    SeperatorKind::Sub, make_span!(39, 39),
                                    int!(1, make_span!(41, 41)),
                                ),
                                SeperatorKind::Sub, make_span!(43, 43),
                                int!(2, make_span!(45, 45)),
                            ),
                            SeperatorKind::ShiftRight, make_span!(47, 48),
                            int!(7, make_span!(50, 50))
                        )
                    )
                )
            ),
            SeperatorKind::LogicalOr, make_span!(52, 53),
            Expr::new_binary(
                int!(1, make_span!(55, 55)),
                SeperatorKind::GreatEqual, make_span!(57, 58),
                int!(1, make_span!(60, 60))
            )
        )
    }
    //                                     0     
    //                                     123456
    assert_eq!{ Expr::with_test_str("4 >> 7"),
        Expr::new_binary(
            int!(4, make_span!(0, 0)),
            SeperatorKind::ShiftRight, make_span!(2, 3),
            int!(7, make_span!(5, 5))
        )
    }
    //                                     0        1         2         3         4         5         6     
    //                                     12345678901234567890123456789012345678901234567890123456789012345
    assert_eq!{ Expr::with_test_str("8 & 0 | 7 + 7 | 7 * 0 && 1 - 2 * 3 | 0 - 7 >= 6 >> 5 % 5 || 5 % 3"),
        // (((((8 & 0) | (7 + 7)) | (7 * 0)) && ((1 - (2 * 3)) | (((0 - 7) >= 6) >> (5 % 5)))) || (5 % 3))
        Expr::new_binary(
            Expr::new_binary(
                Expr::new_binary(
                    Expr::new_binary(
                        Expr::new_binary(
                            int!(8, make_span!(0, 0)),
                            SeperatorKind::BitAnd, make_span!(2, 2),
                            int!(0, make_span!(4, 4))
                        ),
                        SeperatorKind::BitOr, make_span!(6, 6),
                        Expr::new_binary(
                            int!(7, make_span!(8, 8)),
                            SeperatorKind::Add, make_span!(10, 10),
                            int!(7, make_span!(12, 12))
                        )
                    ),
                    SeperatorKind::BitOr, make_span!(14, 14),
                    Expr::new_binary(
                        int!(7, make_span!(16, 16)),
                        SeperatorKind::Mul, make_span!(18, 18),
                        int!(0, make_span!(20, 20))
                    )
                ),
                SeperatorKind::LogicalAnd, make_span!(22, 23),
                Expr::new_binary(
                    Expr::new_binary(
                        int!(1, make_span!(25, 25)),
                        SeperatorKind::Sub, make_span!(27, 27),
                        Expr::new_binary(
                            int!(2, make_span!(29, 29)),
                            SeperatorKind::Mul, make_span!(31, 31),
                            int!(3, make_span!(33, 33))
                        )
                    ),
                    SeperatorKind::BitOr, make_span!(35, 35),
                    Expr::new_binary(
                        Expr::new_binary(
                            Expr::new_binary(
                                int!(0, make_span!(37, 37)),
                                SeperatorKind::Sub, make_span!(39, 39),
                                int!(7, make_span!(41, 41))
                            ),
                            SeperatorKind::GreatEqual, make_span!(43, 44),
                            int!(6, make_span!(46, 46))
                        ),
                        SeperatorKind::ShiftRight, make_span!(48, 49),
                        Expr::new_binary(
                            int!(5, make_span!(51, 51)),
                            SeperatorKind::Rem, make_span!(53, 53),
                            int!(5, make_span!(55, 55))
                        )
                    )
                )
            ),
            SeperatorKind::LogicalOr, make_span!(57, 58),
            Expr::new_binary(
                int!(5, make_span!(60, 60)),
                SeperatorKind::Rem, make_span!(62, 62),
                int!(3, make_span!(64, 64))
            )
        )
    }
    //                                     0        1         2         3         4         5         6     
    //                                     12345678901234567890123456789012345678901234567890123456789012345678
    assert_eq!{ Expr::with_test_str("3 <= 2 + 4 <= 5 && 3 < 3 + 2 >> 1 * 2 & 8 && 1 >= 1 < 0 || 6 < 4 * 4"),
        // (((((3 <= (2 + 4)) <= 5) && (((3 < (3 + 2)) >> (1 * 2)) & 8)) && ((1 >= 1) < 0)) || (6 < (4 * 4)))
        Expr::new_binary(
            Expr::new_binary(
                Expr::new_binary(
                    Expr::new_binary(
                        Expr::new_binary(
                            int!(3, make_span!(0, 0)),
                            SeperatorKind::LessEqual, make_span!(2, 3),
                            Expr::new_binary(
                                int!(2, make_span!(5, 5)),
                                SeperatorKind::Add, make_span!(7, 7),
                                int!(4, make_span!(9, 9))
                            )
                        ),
                        SeperatorKind::LessEqual, make_span!(11, 12),
                        int!(5, make_span!(14, 14))
                    ),
                    SeperatorKind::LogicalAnd, make_span!(16, 17),
                    Expr::new_binary(
                        Expr::new_binary(
                            Expr::new_binary(
                                int!(3, make_span!(19, 19)),
                                SeperatorKind::Less, make_span!(21, 21),
                                Expr::new_binary(
                                    int!(3, make_span!(23, 23)),
                                    SeperatorKind::Add, make_span!(25, 25),
                                    int!(2, make_span!(27, 27))
                                )
                            ),
                            SeperatorKind::ShiftRight, make_span!(29, 30),
                            Expr::new_binary(
                                int!(1, make_span!(32, 32)),
                                SeperatorKind::Mul, make_span!(34, 34),
                                int!(2, make_span!(36, 36))
                            )
                        ),
                        SeperatorKind::BitAnd, make_span!(38, 38),
                        int!(8, make_span!(40, 40))
                    )
                ),
                SeperatorKind::LogicalAnd, make_span!(42, 43),
                Expr::new_binary(
                    Expr::new_binary(
                        int!(1, make_span!(45, 45)),
                        SeperatorKind::GreatEqual, make_span!(47, 48),
                        int!(1, make_span!(50, 50))
                    ),
                    SeperatorKind::Less, make_span!(52, 52),
                    int!(0, make_span!(54, 54))
                )
            ),
            SeperatorKind::LogicalOr, make_span!(56, 57),
            Expr::new_binary(
                int!(6, make_span!(59, 59)),
                SeperatorKind::Less, make_span!(61, 61),
                Expr::new_binary(
                    int!(4, make_span!(63, 63)),
                    SeperatorKind::Mul, make_span!(65, 65),
                    int!(4, make_span!(67, 67))
                )
            )
        )
    }
    //                                     0        1         2
    //                                     12345678901234567890
    assert_eq!{ Expr::with_test_str("5 >= 6 | 3 == 4 && 3"),
        // ((((5 >= 6) | 3) == 4) && 3)
        Expr::new_binary(
            Expr::new_binary(
                Expr::new_binary(
                    Expr::new_binary(
                        int!(5, make_span!(0, 0)),
                        SeperatorKind::GreatEqual, make_span!(2, 3),
                        int!(6, make_span!(5, 5))
                    ),
                    SeperatorKind::BitOr, make_span!(7, 7),
                    int!(3, make_span!(9, 9))
                ),
                SeperatorKind::Equal, make_span!(11, 12),
                int!(4, make_span!(14, 14))
            ),
            SeperatorKind::LogicalAnd, make_span!(16, 17),
            int!(3, make_span!(19, 19))
        )
    }
    //                                     0        1         2         3         4         5         6         7
    //                                     1234567890123456789012345678901234567890123456789012345678901234567890123456
    assert_eq!{ Expr::with_test_str("6 && 7 >> 8 && 0 / 8 * 7 + 5 < 5 / 5 >> 5 - 1 >= 6 > 8 | 6 >> 5 > 2 + 1 || 0"),
        // */%, +-, <><=>=, <<>>, &, ^, |, ==!=, &&, ||
        // (((6 && (7 >> 8)) && ((((((0 / 8) * 7) + 5) < (5 / 5)) >> (((5 - 1) >= 6) > 8)) | (6 >> (5 > (2 + 1))))) || 0)
        Expr::new_binary(
            Expr::new_binary(
                Expr::new_binary(
                    int!(6, make_span!(0, 0)),
                    SeperatorKind::LogicalAnd, make_span!(2, 3),
                    Expr::new_binary(
                        int!(7, make_span!(5, 5)),
                        SeperatorKind::ShiftRight, make_span!(7, 8),
                        int!(8, make_span!(10, 10))
                    )
                ),
                SeperatorKind::LogicalAnd, make_span!(12, 13),
                Expr::new_binary(
                    Expr::new_binary(
                        Expr::new_binary(
                            Expr::new_binary(
                                Expr::new_binary(
                                    Expr::new_binary(
                                        int!(0, make_span!(15, 15)),
                                        SeperatorKind::Div, make_span!(17, 17),
                                        int!(8, make_span!(19, 19))
                                    ), 
                                    SeperatorKind::Mul, make_span!(21, 21),
                                    int!(7, make_span!(23, 23))
                                ), 
                                SeperatorKind::Add, make_span!(25, 25),
                                int!(5, make_span!(27, 27))
                            ),
                            SeperatorKind::Less, make_span!(29, 29),
                            Expr::new_binary(
                                int!(5, make_span!(31, 31)),
                                SeperatorKind::Div, make_span!(33, 33),
                                int!(5, make_span!(35, 35))
                            )
                        ),
                        SeperatorKind::ShiftRight, make_span!(37, 38),
                        Expr::new_binary(
                            Expr::new_binary(
                                Expr::new_binary(
                                    int!(5, make_span!(40, 40)),
                                    SeperatorKind::Sub, make_span!(42, 42),
                                    int!(1, make_span!(44, 44))
                                ), 
                                SeperatorKind::GreatEqual, make_span!(46, 47),
                                int!(6, make_span!(49, 49))
                            ), 
                            SeperatorKind::Great, make_span!(51, 51),
                            int!(8, make_span!(53, 53))
                        )
                    ),
                    SeperatorKind::BitOr, make_span!(55, 55),
                    Expr::new_binary(
                        int!(6, make_span!(57, 57)),
                        SeperatorKind::ShiftRight, make_span!(59, 60),
                        Expr::new_binary(
                            int!(5, make_span!(62, 62)),
                            SeperatorKind::Great, make_span!(64, 64),
                            Expr::new_binary(
                                int!(2, make_span!(66, 66)),
                                SeperatorKind::Add, make_span!(68, 68),
                                int!(1, make_span!(70, 70))
                            )
                        )
                    )
                )
            ),
            SeperatorKind::LogicalOr, make_span!(72, 73),
            int!(0, make_span!(75, 75))
        )
    }
}
