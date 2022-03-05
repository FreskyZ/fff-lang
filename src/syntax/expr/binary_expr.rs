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

use std::fmt;
use crate::source::Span;
use crate::lexical::Seperator;
use crate::lexical::SeperatorCategory;
use super::Expr;
use super::UnaryExpr;
use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxFormat;
use super::super::ISyntaxParse;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct BinaryExpr {
    pub left_expr: Box<Expr>,
    pub right_expr: Box<Expr>,
    pub operator: Seperator,            // this means every binary operator matches a binary expr
    pub operator_span: Span, 
    pub all_span: Span,
}
impl ISyntaxFormat for BinaryExpr {
    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("binary-expr").space().span(self.all_span).endl()
            .set_prefix_text("left-is").apply1(self.left_expr.as_ref()).unset_prefix_text().endl()
            .indent1().lit("\"").debug(&self.operator).lit("\"").space().span(self.operator_span).endl()
            .set_prefix_text("right-is").apply1(self.right_expr.as_ref()).unset_prefix_text()
            .finish()
    }
}
impl fmt::Debug for BinaryExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl From<BinaryExpr> for Expr {
    fn from(binary_expr: BinaryExpr) -> Expr { Expr::Binary(binary_expr) }
}
impl BinaryExpr {

    pub fn new<T1: Into<Expr>, T2: Into<Expr>>(left_expr: T1, operator: Seperator, operator_span: Span, right_expr: T2) -> BinaryExpr {
        let left_expr = left_expr.into();
        let right_expr = right_expr.into();
        BinaryExpr{
            all_span: left_expr.get_all_span().merge(&right_expr.get_all_span()),
            left_expr: Box::new(left_expr),
            right_expr: Box::new(right_expr),
            operator, operator_span
        }
    }
}
impl ISyntaxParse for BinaryExpr {
    type Output = Expr;

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
                        match sess.try_expect_sep_cat($op_category) {
                            Some((sep, sep_span)) => {
                                let right_expr = $previous_parser(sess)?;
                                current_retval = Expr::Binary(BinaryExpr::new(current_retval, sep, sep_span, right_expr));
                            }
                            None => {
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
    use crate::lexical::LitValue;
    use super::LitExpr;
    
    assert_eq!{ 
        BinaryExpr::new(
            LitExpr::new(LitValue::from(1), Span::new(0, 0)),
            Seperator::Add, Span::new(2, 2),
            LitExpr::new(LitValue::from(2), Span::new(4, 4))
        ).format(Formatter::with_test_indent(1)),
        r#"  binary-expr <<0>0-4>
    left-is literal (i32)1 <<0>0-0>
    "+" <<0>2-2>
    right-is literal (i32)2 <<0>4-4>"#
    }
}

#[cfg(test)] #[test]
fn binary_expr_parse() {
    use crate::lexical::LitValue;
    use super::LitExpr;
    use super::ArrayDef;
    use super::SimpleName;
    use super::ExprList;
    use super::super::WithTestInput;

    //                                     123456789012345
    assert_eq!{ BinaryExpr::with_test_str("[1] * [2] / [3]"), 
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                ArrayDef::new(Span::new(0, 2), make_exprs![
                    LitExpr::new(LitValue::from(1), Span::new(1, 1))
                ]),
                Seperator::Mul, Span::new(4, 4),
                ArrayDef::new(Span::new(6, 8), make_exprs![
                    LitExpr::new(LitValue::from(2), Span::new(7, 7)),
                ]),
            ),
            Seperator::Div, Span::new(10, 10),
            ArrayDef::new(Span::new(12, 14), make_exprs![
                LitExpr::new(LitValue::from(3), Span::new(13, 13)),
            ]),
        ))
    }           
    //                                     0        1         2
    //                                     123456789012345678901
    assert_eq!{ Expr::with_test_str("a * b / c + d % e - f"),  // ((((a * b) / c) + (d % e)) - f)
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                BinaryExpr::new(
                    BinaryExpr::new(
                        SimpleName::new(make_id!(1), Span::new(0, 0)),
                        Seperator::Mul, Span::new(2, 2), 
                        SimpleName::new(make_id!(2), Span::new(4, 4)),
                    ),
                    Seperator::Div, Span::new(6, 6),
                    SimpleName::new(make_id!(3), Span::new(8, 8)),
                ),
                Seperator::Add, Span::new(10, 10),
                BinaryExpr::new(
                    SimpleName::new(make_id!(4), Span::new(12, 12)),
                    Seperator::Rem, Span::new(14, 14),
                    SimpleName::new(make_id!(5), Span::new(16, 16)),
                )
            ),
            Seperator::Sub, Span::new(18, 18),
            SimpleName::new(make_id!(6), Span::new(20, 20)),
        ))
    }           
    //                                     0        1         2         3
    //                                     1234567890123456789012345678901
    assert_eq!{ Expr::with_test_str("a * b << h / c + d % e - f >> g"), // (((a * b) << (((h / c) + (d % e)) - f)) >> g)
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                BinaryExpr::new(
                    SimpleName::new(make_id!(1), Span::new(0, 0)),
                    Seperator::Mul, Span::new(2, 2),
                    SimpleName::new(make_id!(2), Span::new(4, 4))
                ),
                Seperator::ShiftLeft, Span::new(6, 7),
                BinaryExpr::new(
                    BinaryExpr::new(
                        BinaryExpr::new(
                            SimpleName::new(make_id!(3), Span::new(9, 9)),
                            Seperator::Div, Span::new(11, 11),
                            SimpleName::new(make_id!(4), Span::new(13, 13))
                        ),
                        Seperator::Add, Span::new(15, 15),
                        BinaryExpr::new(
                            SimpleName::new(make_id!(5), Span::new(17, 17)),
                            Seperator::Rem, Span::new(19, 19),
                            SimpleName::new(make_id!(6), Span::new(21, 21))
                        )
                    ),
                    Seperator::Sub, Span::new(23, 23),
                    SimpleName::new(make_id!(7), Span::new(25, 25))
                )
            ),
            Seperator::ShiftRight, Span::new(27, 28),
            SimpleName::new(make_id!(8), Span::new(30, 30)),
        ))
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
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                BinaryExpr::new(
                    BinaryExpr::new(
                        BinaryExpr::new(
                            BinaryExpr::new(
                                SimpleName::new(make_id!(1), Span::new(0, 0)),
                                Seperator::Mul, Span::new(2, 2),
                                SimpleName::new(make_id!(2), Span::new(4, 4)),
                            ),
                            Seperator::ShiftLeft, Span::new(6, 7),
                            BinaryExpr::new(
                                BinaryExpr::new(
                                    BinaryExpr::new(
                                        SimpleName::new(make_id!(3), Span::new(9, 9)),
                                        Seperator::Div, Span::new(11, 11),
                                        SimpleName::new(make_id!(4), Span::new(13, 13)),
                                    ),
                                    Seperator::Add, Span::new(15, 15),
                                    BinaryExpr::new(
                                        SimpleName::new(make_id!(5), Span::new(17, 17)),
                                        Seperator::Rem, Span::new(19, 19),
                                        SimpleName::new(make_id!(6), Span::new(21, 21)),
                                    )
                                ),
                                Seperator::Sub, Span::new(23, 23),
                                SimpleName::new(make_id!(7), Span::new(25, 25))
                            ),
                        ),
                        Seperator::ShiftRight, Span::new(27, 28),
                        BinaryExpr::new(
                            BinaryExpr::new(
                                SimpleName::new(make_id!(8), Span::new(30, 30)),
                                Seperator::Great, Span::new(32, 32),
                                BinaryExpr::new(
                                    SimpleName::new(make_id!(3), Span::new(34, 34)),
                                    Seperator::Mul, Span::new(36, 36),
                                    SimpleName::new(make_id!(9), Span::new(38, 38)),
                                )
                            ),
                            Seperator::Less, Span::new(40, 40),
                            SimpleName::new(make_id!(10), Span::new(42, 42)),
                        )
                    ),
                    Seperator::ShiftLeft, Span::new(44, 45),
                    BinaryExpr::new(
                        SimpleName::new(make_id!(11), Span::new(47, 47)),
                        Seperator::GreatEqual, Span::new(49, 50),
                        SimpleName::new(make_id!(12), Span::new(52, 52)),
                    )
                ),
                Seperator::LogicalAnd, Span::new(54, 55),
                SimpleName::new(make_id!(13), Span::new(57, 57))
            ),
            Seperator::LogicalOr, Span::new(59, 60),
            BinaryExpr::new(
                BinaryExpr::new(
                    BinaryExpr::new(
                        BinaryExpr::new(
                            SimpleName::new(make_id!(14), Span::new(62, 62)),
                            Seperator::BitAnd, Span::new(64, 64),
                            SimpleName::new(make_id!(15), Span::new(66, 66)),
                        ),
                        Seperator::BitOr, Span::new(68, 68),
                        BinaryExpr::new(
                            SimpleName::new(make_id!(16), Span::new(70, 70)),
                            Seperator::BitXor, Span::new(72, 72),
                            SimpleName::new(make_id!(17), Span::new(74, 74)),
                        )
                    ),
                    Seperator::NotEqual, Span::new(76, 77),
                    SimpleName::new(make_id!(18), Span::new(79, 79)),
                ),
                Seperator::Equal, Span::new(81, 82),
                SimpleName::new(make_id!(19), Span::new(84, 84))
            )
        ))
    }
    //                                     1234567890
    assert_eq!{ Expr::with_test_str("a & b == c"), // ((a & b) == c)
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                SimpleName::new(make_id!(1), Span::new(0, 0)),
                Seperator::BitAnd, Span::new(2, 2),
                SimpleName::new(make_id!(2), Span::new(4, 4)),
            ),
            Seperator::Equal, Span::new(6, 7),
            SimpleName::new(make_id!(3), Span::new(9, 9)),
        ))
    }

    // 1 + 2 << 3 => (1 + 2) << 3, don't want this
    // cout << 5 + 6 => cout << (5 + 6), want this

    // program generated random tests
    //                                     0        1         2         3    
    //                                     1234567890123456789012345678901234
    assert_eq!{ Expr::with_test_str("0 + 6 ^ 3 & 3 / 3 - 8 && 2 & 0 + 6"), // (((0 + 6) ^ (3 & ((3 / 3) - 8))) && (2 & (0 + 6)))
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                BinaryExpr::new(
                    Expr::Lit(LitExpr::new(LitValue::from(0), Span::new(0, 0))),
                    Seperator::Add, Span::new(2, 2),
                    Expr::Lit(LitExpr::new(LitValue::from(6), Span::new(4, 4)))
                ),
                Seperator::BitXor, Span::new(6, 6),
                BinaryExpr::new(
                    Expr::Lit(LitExpr::new(LitValue::from(3), Span::new(8, 8))),
                    Seperator::BitAnd, Span::new(10, 10),
                    BinaryExpr::new(
                        BinaryExpr::new(
                            Expr::Lit(LitExpr::new(LitValue::from(3), Span::new(12, 12))),
                            Seperator::Div, Span::new(14, 14),
                            Expr::Lit(LitExpr::new(LitValue::from(3), Span::new(16, 16)))
                        ),
                        Seperator::Sub, Span::new(18, 18),
                        Expr::Lit(LitExpr::new(LitValue::from(8), Span::new(20, 20)))
                    )
                )
            ),
            Seperator::LogicalAnd, Span::new(22, 23),
            BinaryExpr::new(
                Expr::Lit(LitExpr::new(LitValue::from(2), Span::new(25, 25))),
                Seperator::BitAnd, Span::new(27, 27),
                BinaryExpr::new(
                    Expr::Lit(LitExpr::new(LitValue::from(0), Span::new(29, 29))),
                    Seperator::Add, Span::new(31, 31),
                    Expr::Lit(LitExpr::new(LitValue::from(6), Span::new(33, 33)))
                )
            )
        ))
    }
    //                                     0        1         2         3         4         5         6         7
    //                                     1234567890123456789012345678901234567890123456789012345678901234567890
    assert_eq!{ Expr::with_test_str("7 > 1 | 0 % 8 | 1 % 7 * 3 % 6 == 1 >> 8 % 3 ^ 6 << 0 ^ 2 >> 6 || 1 - 0"),
        // (((((7 > 1) | (0 % 8)) | (((1 % 7) * 3) % 6)) == (((1 >> (8 % 3)) ^ (6 << 0)) ^ (2 >> 6))) || (1 - 0))
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                BinaryExpr::new(
                    BinaryExpr::new(
                        BinaryExpr::new(
                            Expr::Lit(LitExpr::new(LitValue::from(7), Span::new(0, 0))),
                            Seperator::Great, Span::new(2, 2),
                            Expr::Lit(LitExpr::new(LitValue::from(1), Span::new(4, 4))),
                        ),
                        Seperator::BitOr, Span::new(6, 6),
                        BinaryExpr::new(
                            Expr::Lit(LitExpr::new(LitValue::from(0), Span::new(8, 8))),
                            Seperator::Rem, Span::new(10, 10),
                            Expr::Lit(LitExpr::new(LitValue::from(8), Span::new(12, 12)))
                        )
                    ), 
                    Seperator::BitOr, Span::new(14, 14),
                    BinaryExpr::new(
                        BinaryExpr::new(
                            BinaryExpr::new(
                                Expr::Lit(LitExpr::new(LitValue::from(1), Span::new(16, 16))),
                                Seperator::Rem, Span::new(18, 18),
                                Expr::Lit(LitExpr::new(LitValue::from(7), Span::new(20, 20))),
                            ),
                            Seperator::Mul, Span::new(22, 22),
                            Expr::Lit(LitExpr::new(LitValue::from(3), Span::new(24, 24))),
                        ),
                        Seperator::Rem, Span::new(26, 26),
                        Expr::Lit(LitExpr::new(LitValue::from(6), Span::new(28, 28)))
                    )
                ),
                Seperator::Equal, Span::new(30, 31),
                BinaryExpr::new(
                    BinaryExpr::new(
                        BinaryExpr::new(
                            Expr::Lit(LitExpr::new(LitValue::from(1), Span::new(33, 33))),
                            Seperator::ShiftRight, Span::new(35, 36),
                            BinaryExpr::new(
                                Expr::Lit(LitExpr::new(LitValue::from(8), Span::new(38, 38))),
                                Seperator::Rem, Span::new(40, 40),
                                Expr::Lit(LitExpr::new(LitValue::from(3), Span::new(42, 42))),
                            )
                        ),
                        Seperator::BitXor, Span::new(44, 44),
                        BinaryExpr::new(
                            Expr::Lit(LitExpr::new(LitValue::from(6), Span::new(46, 46))),
                            Seperator::ShiftLeft, Span::new(48, 49),
                            Expr::Lit(LitExpr::new(LitValue::from(0), Span::new(51, 51))),
                        )
                    ),
                    Seperator::BitXor, Span::new(53, 53),
                    BinaryExpr::new(
                        Expr::Lit(LitExpr::new(LitValue::from(2), Span::new(55, 55))),
                        Seperator::ShiftRight, Span::new(57, 58),
                        Expr::Lit(LitExpr::new(LitValue::from(6), Span::new(60, 60))),
                    )
                )
            ),
            Seperator::LogicalOr, Span::new(62, 63),
            BinaryExpr::new(
                Expr::Lit(LitExpr::new(LitValue::from(1), Span::new(65, 65))),
                Seperator::Sub, Span::new(67, 67),
                Expr::Lit(LitExpr::new(LitValue::from(0), Span::new(69, 69)))
            )
        ))
    }
    //                                     0        1         2         3         4         5         6 
    //                                     1234567890123456789012345678901234567890123456789012345678901
    assert_eq!{ Expr::with_test_str("7 >> 3 == 8 / 1 && 6 == 1 <= 3 % 6 ^ 3 - 1 - 2 >> 7 || 1 >= 1"),
        // */%, +-, <><=>=, <<>>, &, ^, |, ==!=, &&, ||
        // ((((7 >> 3) == (8 / 1)) && (6 == ((1 <= (3 % 6)) ^ (((3 - 1) - 2) >> 7)))) || (1 >= 1))
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                BinaryExpr::new(
                    BinaryExpr::new(
                        Expr::Lit(LitExpr::new(LitValue::from(7), Span::new(0, 0))),
                        Seperator::ShiftRight, Span::new(2, 3),
                        Expr::Lit(LitExpr::new(LitValue::from(3), Span::new(5, 5))),
                    ),
                    Seperator::Equal, Span::new(7, 8),
                    BinaryExpr::new(
                        Expr::Lit(LitExpr::new(LitValue::from(8), Span::new(10, 10))),
                        Seperator::Div, Span::new(12, 12),
                        Expr::Lit(LitExpr::new(LitValue::from(1), Span::new(14, 14))),
                    )
                ),
                Seperator::LogicalAnd, Span::new(16, 17),
                BinaryExpr::new(
                    Expr::Lit(LitExpr::new(LitValue::from(6), Span::new(19, 19))),
                    Seperator::Equal, Span::new(21, 22),
                    BinaryExpr::new(
                        BinaryExpr::new(
                            Expr::Lit(LitExpr::new(LitValue::from(1), Span::new(24, 24))),
                            Seperator::LessEqual, Span::new(26, 27),
                            BinaryExpr::new(
                                Expr::Lit(LitExpr::new(LitValue::from(3), Span::new(29, 29))),
                                Seperator::Rem, Span::new(31, 31),
                                Expr::Lit(LitExpr::new(LitValue::from(6), Span::new(33, 33)))
                            )
                        ),
                        Seperator::BitXor, Span::new(35, 35),
                        BinaryExpr::new(
                            BinaryExpr::new(
                                BinaryExpr::new(
                                    Expr::Lit(LitExpr::new(LitValue::from(3), Span::new(37, 37))),
                                    Seperator::Sub, Span::new(39, 39),
                                    Expr::Lit(LitExpr::new(LitValue::from(1), Span::new(41, 41))),
                                ),
                                Seperator::Sub, Span::new(43, 43),
                                Expr::Lit(LitExpr::new(LitValue::from(2), Span::new(45, 45))),
                            ),
                            Seperator::ShiftRight, Span::new(47, 48),
                            Expr::Lit(LitExpr::new(LitValue::from(7), Span::new(50, 50)))
                        )
                    )
                )
            ),
            Seperator::LogicalOr, Span::new(52, 53),
            BinaryExpr::new(
                Expr::Lit(LitExpr::new(LitValue::from(1), Span::new(55, 55))),
                Seperator::GreatEqual, Span::new(57, 58),
                Expr::Lit(LitExpr::new(LitValue::from(1), Span::new(60, 60)))
            )
        ))
    }
    //                                     0     
    //                                     123456
    assert_eq!{ Expr::with_test_str("4 >> 7"),
        Expr::Binary(BinaryExpr::new(
            Expr::Lit(LitExpr::new(LitValue::from(4), Span::new(0, 0))),
            Seperator::ShiftRight, Span::new(2, 3),
            Expr::Lit(LitExpr::new(LitValue::from(7), Span::new(5, 5)))
        ))
    }
    //                                     0        1         2         3         4         5         6     
    //                                     12345678901234567890123456789012345678901234567890123456789012345
    assert_eq!{ Expr::with_test_str("8 & 0 | 7 + 7 | 7 * 0 && 1 - 2 * 3 | 0 - 7 >= 6 >> 5 % 5 || 5 % 3"),
        // (((((8 & 0) | (7 + 7)) | (7 * 0)) && ((1 - (2 * 3)) | (((0 - 7) >= 6) >> (5 % 5)))) || (5 % 3))
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                BinaryExpr::new(
                    BinaryExpr::new(
                        BinaryExpr::new(
                            Expr::Lit(LitExpr::new(LitValue::from(8), Span::new(0, 0))),
                            Seperator::BitAnd, Span::new(2, 2),
                            Expr::Lit(LitExpr::new(LitValue::from(0), Span::new(4, 4)))
                        ),
                        Seperator::BitOr, Span::new(6, 6),
                        BinaryExpr::new(
                            Expr::Lit(LitExpr::new(LitValue::from(7), Span::new(8, 8))),
                            Seperator::Add, Span::new(10, 10),
                            Expr::Lit(LitExpr::new(LitValue::from(7), Span::new(12, 12)))
                        )
                    ),
                    Seperator::BitOr, Span::new(14, 14),
                    BinaryExpr::new(
                        Expr::Lit(LitExpr::new(LitValue::from(7), Span::new(16, 16))),
                        Seperator::Mul, Span::new(18, 18),
                        Expr::Lit(LitExpr::new(LitValue::from(0), Span::new(20, 20)))
                    )
                ),
                Seperator::LogicalAnd, Span::new(22, 23),
                BinaryExpr::new(
                    BinaryExpr::new(
                        Expr::Lit(LitExpr::new(LitValue::from(1), Span::new(25, 25))),
                        Seperator::Sub, Span::new(27, 27),
                        BinaryExpr::new(
                            Expr::Lit(LitExpr::new(LitValue::from(2), Span::new(29, 29))),
                            Seperator::Mul, Span::new(31, 31),
                            Expr::Lit(LitExpr::new(LitValue::from(3), Span::new(33, 33)))
                        )
                    ),
                    Seperator::BitOr, Span::new(35, 35),
                    BinaryExpr::new(
                        BinaryExpr::new(
                            BinaryExpr::new(
                                Expr::Lit(LitExpr::new(LitValue::from(0), Span::new(37, 37))),
                                Seperator::Sub, Span::new(39, 39),
                                Expr::Lit(LitExpr::new(LitValue::from(7), Span::new(41, 41)))
                            ),
                            Seperator::GreatEqual, Span::new(43, 44),
                            Expr::Lit(LitExpr::new(LitValue::from(6), Span::new(46, 46)))
                        ),
                        Seperator::ShiftRight, Span::new(48, 49),
                        BinaryExpr::new(
                            Expr::Lit(LitExpr::new(LitValue::from(5), Span::new(51, 51))),
                            Seperator::Rem, Span::new(53, 53),
                            Expr::Lit(LitExpr::new(LitValue::from(5), Span::new(55, 55)))
                        )
                    )
                )
            ),
            Seperator::LogicalOr, Span::new(57, 58),
            BinaryExpr::new(
                Expr::Lit(LitExpr::new(LitValue::from(5), Span::new(60, 60))),
                Seperator::Rem, Span::new(62, 62),
                Expr::Lit(LitExpr::new(LitValue::from(3), Span::new(64, 64)))
            )
        ))
    }
    //                                     0        1         2         3         4         5         6     
    //                                     12345678901234567890123456789012345678901234567890123456789012345678
    assert_eq!{ Expr::with_test_str("3 <= 2 + 4 <= 5 && 3 < 3 + 2 >> 1 * 2 & 8 && 1 >= 1 < 0 || 6 < 4 * 4"),
        // (((((3 <= (2 + 4)) <= 5) && (((3 < (3 + 2)) >> (1 * 2)) & 8)) && ((1 >= 1) < 0)) || (6 < (4 * 4)))
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                BinaryExpr::new(
                    BinaryExpr::new(
                        BinaryExpr::new(
                            Expr::Lit(LitExpr::new(LitValue::from(3), Span::new(0, 0))),
                            Seperator::LessEqual, Span::new(2, 3),
                            BinaryExpr::new(
                                Expr::Lit(LitExpr::new(LitValue::from(2), Span::new(5, 5))),
                                Seperator::Add, Span::new(7, 7),
                                Expr::Lit(LitExpr::new(LitValue::from(4), Span::new(9, 9)))
                            )
                        ),
                        Seperator::LessEqual, Span::new(11, 12),
                        Expr::Lit(LitExpr::new(LitValue::from(5), Span::new(14, 14)))
                    ),
                    Seperator::LogicalAnd, Span::new(16, 17),
                    BinaryExpr::new(
                        BinaryExpr::new(
                            BinaryExpr::new(
                                Expr::Lit(LitExpr::new(LitValue::from(3), Span::new(19, 19))),
                                Seperator::Less, Span::new(21, 21),
                                BinaryExpr::new(
                                    Expr::Lit(LitExpr::new(LitValue::from(3), Span::new(23, 23))),
                                    Seperator::Add, Span::new(25, 25),
                                    Expr::Lit(LitExpr::new(LitValue::from(2), Span::new(27, 27)))
                                )
                            ),
                            Seperator::ShiftRight, Span::new(29, 30),
                            BinaryExpr::new(
                                Expr::Lit(LitExpr::new(LitValue::from(1), Span::new(32, 32))),
                                Seperator::Mul, Span::new(34, 34),
                                Expr::Lit(LitExpr::new(LitValue::from(2), Span::new(36, 36)))
                            )
                        ),
                        Seperator::BitAnd, Span::new(38, 38),
                        Expr::Lit(LitExpr::new(LitValue::from(8), Span::new(40, 40)))
                    )
                ),
                Seperator::LogicalAnd, Span::new(42, 43),
                BinaryExpr::new(
                    BinaryExpr::new(
                        Expr::Lit(LitExpr::new(LitValue::from(1), Span::new(45, 45))),
                        Seperator::GreatEqual, Span::new(47, 48),
                        Expr::Lit(LitExpr::new(LitValue::from(1), Span::new(50, 50)))
                    ),
                    Seperator::Less, Span::new(52, 52),
                    Expr::Lit(LitExpr::new(LitValue::from(0), Span::new(54, 54)))
                )
            ),
            Seperator::LogicalOr, Span::new(56, 57),
            BinaryExpr::new(
                Expr::Lit(LitExpr::new(LitValue::from(6), Span::new(59, 59))),
                Seperator::Less, Span::new(61, 61),
                BinaryExpr::new(
                    Expr::Lit(LitExpr::new(LitValue::from(4), Span::new(63, 63))),
                    Seperator::Mul, Span::new(65, 65),
                    Expr::Lit(LitExpr::new(LitValue::from(4), Span::new(67, 67)))
                )
            )
        ))
    }
    //                                     0        1         2
    //                                     12345678901234567890
    assert_eq!{ Expr::with_test_str("5 >= 6 | 3 == 4 && 3"),
        // ((((5 >= 6) | 3) == 4) && 3)
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                BinaryExpr::new(
                    BinaryExpr::new(
                        Expr::Lit(LitExpr::new(LitValue::from(5), Span::new(0, 0))),
                        Seperator::GreatEqual, Span::new(2, 3),
                        Expr::Lit(LitExpr::new(LitValue::from(6), Span::new(5, 5)))
                    ),
                    Seperator::BitOr, Span::new(7, 7),
                    Expr::Lit(LitExpr::new(LitValue::from(3), Span::new(9, 9)))
                ),
                Seperator::Equal, Span::new(11, 12),
                Expr::Lit(LitExpr::new(LitValue::from(4), Span::new(14, 14)))
            ),
            Seperator::LogicalAnd, Span::new(16, 17),
            Expr::Lit(LitExpr::new(LitValue::from(3), Span::new(19, 19)))
        ))
    }
    //                                     0        1         2         3         4         5         6         7
    //                                     1234567890123456789012345678901234567890123456789012345678901234567890123456
    assert_eq!{ Expr::with_test_str("6 && 7 >> 8 && 0 / 8 * 7 + 5 < 5 / 5 >> 5 - 1 >= 6 > 8 | 6 >> 5 > 2 + 1 || 0"),
        // */%, +-, <><=>=, <<>>, &, ^, |, ==!=, &&, ||
        // (((6 && (7 >> 8)) && ((((((0 / 8) * 7) + 5) < (5 / 5)) >> (((5 - 1) >= 6) > 8)) | (6 >> (5 > (2 + 1))))) || 0)
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                BinaryExpr::new(
                    Expr::Lit(LitExpr::new(LitValue::from(6), Span::new(0, 0))),
                    Seperator::LogicalAnd, Span::new(2, 3),
                    BinaryExpr::new(
                        Expr::Lit(LitExpr::new(LitValue::from(7), Span::new(5, 5))),
                        Seperator::ShiftRight, Span::new(7, 8),
                        Expr::Lit(LitExpr::new(LitValue::from(8), Span::new(10, 10)))
                    )
                ),
                Seperator::LogicalAnd, Span::new(12, 13),
                BinaryExpr::new(
                    BinaryExpr::new(
                        BinaryExpr::new(
                            BinaryExpr::new(
                                BinaryExpr::new(
                                    BinaryExpr::new(
                                        Expr::Lit(LitExpr::new(LitValue::from(0), Span::new(15, 15))),
                                        Seperator::Div, Span::new(17, 17),
                                        Expr::Lit(LitExpr::new(LitValue::from(8), Span::new(19, 19)))
                                    ), 
                                    Seperator::Mul, Span::new(21, 21),
                                    Expr::Lit(LitExpr::new(LitValue::from(7), Span::new(23, 23)))
                                ), 
                                Seperator::Add, Span::new(25, 25),
                                Expr::Lit(LitExpr::new(LitValue::from(5), Span::new(27, 27)))
                            ),
                            Seperator::Less, Span::new(29, 29),
                            BinaryExpr::new(
                                Expr::Lit(LitExpr::new(LitValue::from(5), Span::new(31, 31))),
                                Seperator::Div, Span::new(33, 33),
                                Expr::Lit(LitExpr::new(LitValue::from(5), Span::new(35, 35)))
                            )
                        ),
                        Seperator::ShiftRight, Span::new(37, 38),
                        BinaryExpr::new(
                            BinaryExpr::new(
                                BinaryExpr::new(
                                    Expr::Lit(LitExpr::new(LitValue::from(5), Span::new(40, 40))),
                                    Seperator::Sub, Span::new(42, 42),
                                    Expr::Lit(LitExpr::new(LitValue::from(1), Span::new(44, 44)))
                                ), 
                                Seperator::GreatEqual, Span::new(46, 47),
                                Expr::Lit(LitExpr::new(LitValue::from(6), Span::new(49, 49)))
                            ), 
                            Seperator::Great, Span::new(51, 51),
                            Expr::Lit(LitExpr::new(LitValue::from(8), Span::new(53, 53)))
                        )
                    ),
                    Seperator::BitOr, Span::new(55, 55),
                    BinaryExpr::new(
                        Expr::Lit(LitExpr::new(LitValue::from(6), Span::new(57, 57))),
                        Seperator::ShiftRight, Span::new(59, 60),
                        BinaryExpr::new(
                            Expr::Lit(LitExpr::new(LitValue::from(5), Span::new(62, 62))),
                            Seperator::Great, Span::new(64, 64),
                            BinaryExpr::new(
                                Expr::Lit(LitExpr::new(LitValue::from(2), Span::new(66, 66))),
                                Seperator::Add, Span::new(68, 68),
                                Expr::Lit(LitExpr::new(LitValue::from(1), Span::new(70, 70)))
                            )
                        )
                    )
                )
            ),
            Seperator::LogicalOr, Span::new(72, 73),
            Expr::Lit(LitExpr::new(LitValue::from(0), Span::new(75, 75)))
        ))
    }
}
