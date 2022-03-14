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

use super::prelude::*;
use super::{Expr, UnaryExpr};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct BinaryExpr {
    pub left_expr: Box<Expr>,
    pub right_expr: Box<Expr>,
    pub operator: Separator,            // this means every binary operator matches a binary expr
    pub operator_span: Span, 
    pub all_span: Span,
}
impl From<BinaryExpr> for Expr {
    fn from(binary_expr: BinaryExpr) -> Expr { Expr::Binary(binary_expr) }
}
impl BinaryExpr {

    pub fn new<T1: Into<Expr>, T2: Into<Expr>>(left_expr: T1, operator: Separator, operator_span: Span, right_expr: T2) -> BinaryExpr {
        let left_expr = left_expr.into();
        let right_expr = right_expr.into();
        BinaryExpr{
            all_span: left_expr.get_all_span() + right_expr.get_all_span(),
            left_expr: Box::new(left_expr),
            right_expr: Box::new(right_expr),
            operator, operator_span
        }
    }
}
impl Node for BinaryExpr {
    type ParseOutput = Expr;

    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<Expr> {  
        #[cfg(feature = "trace_binary_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[PrimaryExpr] "); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_binary_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        return parse_logical_or(sess);

        macro_rules! impl_binary_parser {
            ($parser_name: ident, $previous_parser: expr, $op_category: expr) => (
                fn $parser_name<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<Expr> {
                    trace!("parsing {}", stringify!($parser_name));

                    let mut current_retval = $previous_parser(sess)?;
                    loop {
                        match sess.try_expect_sep_kind($op_category) {
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
        impl_binary_parser! { parse_multiplicative, UnaryExpr::parse, SeparatorKind::Multiplicative }
        impl_binary_parser! { parse_additive, parse_multiplicative, SeparatorKind::Additive }
        impl_binary_parser! { parse_relational, parse_additive, SeparatorKind::Relational }
        impl_binary_parser! { parse_shift, parse_relational, SeparatorKind::Shift }
        impl_binary_parser! { parse_bitand, parse_shift, SeparatorKind::BitAnd }
        impl_binary_parser! { parse_bitxor, parse_bitand, SeparatorKind::BitXor }
        impl_binary_parser! { parse_bitor, parse_bitxor, SeparatorKind::BitOr }
        impl_binary_parser! { parse_equality, parse_bitor, SeparatorKind::Equality }
        impl_binary_parser! { parse_logical_and, parse_equality, SeparatorKind::LogicalAnd }
        impl_binary_parser! { parse_logical_or, parse_logical_and, SeparatorKind::LogicalOr }    
    }

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
    use super::{make_node, make_exprs, make_lit, ArrayDef, SimpleName};

    //                                     123456789012345
    assert_eq!{ make_node!("[1] * [2] / [3]" as BinaryExpr), 
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                ArrayDef::new(Span::new(0, 2), make_exprs![
                    make_lit!(1, 1, 1)
                ]),
                Separator::Mul, Span::new(4, 4),
                ArrayDef::new(Span::new(6, 8), make_exprs![
                    make_lit!(2, 7, 7),
                ]),
            ),
            Separator::Div, Span::new(10, 10),
            ArrayDef::new(Span::new(12, 14), make_exprs![
                make_lit!(3, 13, 13),
            ]),
        ))
    }           
    //                                     0        1         2
    //                                     123456789012345678901
    assert_eq!{ make_node!("a * b / c + d % e - f" as BinaryExpr),  // ((((a * b) / c) + (d % e)) - f)
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                BinaryExpr::new(
                    BinaryExpr::new(
                        SimpleName::new(2, Span::new(0, 0)),
                        Separator::Mul, Span::new(2, 2), 
                        SimpleName::new(3, Span::new(4, 4)),
                    ),
                    Separator::Div, Span::new(6, 6),
                    SimpleName::new(4, Span::new(8, 8)),
                ),
                Separator::Add, Span::new(10, 10),
                BinaryExpr::new(
                    SimpleName::new(5, Span::new(12, 12)),
                    Separator::Rem, Span::new(14, 14),
                    SimpleName::new(6, Span::new(16, 16)),
                )
            ),
            Separator::Sub, Span::new(18, 18),
            SimpleName::new(7, Span::new(20, 20)),
        ))
    }           
    //                                     0        1         2         3
    //                                     1234567890123456789012345678901
    assert_node_eq!{ make_node!("a * b << h / c + d % e - f >> g" as BinaryExpr), // (((a * b) << (((h / c) + (d % e)) - f)) >> g)
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                BinaryExpr::new(
                    SimpleName::new(2, Span::new(0, 0)),
                    Separator::Mul, Span::new(2, 2),
                    SimpleName::new(3, Span::new(4, 4))
                ),
                Separator::LtLt, Span::new(6, 7),
                BinaryExpr::new(
                    BinaryExpr::new(
                        BinaryExpr::new(
                            SimpleName::new(4, Span::new(9, 9)),
                            Separator::Div, Span::new(11, 11),
                            SimpleName::new(5, Span::new(13, 13))
                        ),
                        Separator::Add, Span::new(15, 15),
                        BinaryExpr::new(
                            SimpleName::new(6, Span::new(17, 17)),
                            Separator::Rem, Span::new(19, 19),
                            SimpleName::new(7, Span::new(21, 21))
                        )
                    ),
                    Separator::Sub, Span::new(23, 23),
                    SimpleName::new(8, Span::new(25, 25))
                )
            ),
            Separator::GtGt, Span::new(27, 28),
            SimpleName::new(9, Span::new(30, 30)),
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
    assert_node_eq!{ make_node!("a * b << h / c + d % e - f >> g > h * i < j << k >= m && n || o & p | q ^ r != s == t" as BinaryExpr),
        // */%, +-, <><=>=, <<>>, &, ^, |, ==!=, &&, ||
        // ((((((a * b) << (((h / c) + (d % e)) - f)) >> ((g > (h * i)) < j)) << (k >= m)) && n) || ((((o & p) | (q ^ r)) != s) == t))
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                BinaryExpr::new(
                    BinaryExpr::new(
                        BinaryExpr::new(
                            BinaryExpr::new(
                                SimpleName::new(2, Span::new(0, 0)),
                                Separator::Mul, Span::new(2, 2),
                                SimpleName::new(3, Span::new(4, 4)),
                            ),
                            Separator::LtLt, Span::new(6, 7),
                            BinaryExpr::new(
                                BinaryExpr::new(
                                    BinaryExpr::new(
                                        SimpleName::new(4, Span::new(9, 9)),
                                        Separator::Div, Span::new(11, 11),
                                        SimpleName::new(5, Span::new(13, 13)),
                                    ),
                                    Separator::Add, Span::new(15, 15),
                                    BinaryExpr::new(
                                        SimpleName::new(6, Span::new(17, 17)),
                                        Separator::Rem, Span::new(19, 19),
                                        SimpleName::new(7, Span::new(21, 21)),
                                    )
                                ),
                                Separator::Sub, Span::new(23, 23),
                                SimpleName::new(8, Span::new(25, 25))
                            ),
                        ),
                        Separator::GtGt, Span::new(27, 28),
                        BinaryExpr::new(
                            BinaryExpr::new(
                                SimpleName::new(9, Span::new(30, 30)),
                                Separator::Gt, Span::new(32, 32),
                                BinaryExpr::new(
                                    SimpleName::new(4, Span::new(34, 34)),
                                    Separator::Mul, Span::new(36, 36),
                                    SimpleName::new(10, Span::new(38, 38)),
                                )
                            ),
                            Separator::Lt, Span::new(40, 40),
                            SimpleName::new(11, Span::new(42, 42)),
                        )
                    ),
                    Separator::LtLt, Span::new(44, 45),
                    BinaryExpr::new(
                        SimpleName::new(12, Span::new(47, 47)),
                        Separator::GtEq, Span::new(49, 50),
                        SimpleName::new(13, Span::new(52, 52)),
                    )
                ),
                Separator::AndAnd, Span::new(54, 55),
                SimpleName::new(14, Span::new(57, 57))
            ),
            Separator::OrOr, Span::new(59, 60),
            BinaryExpr::new(
                BinaryExpr::new(
                    BinaryExpr::new(
                        BinaryExpr::new(
                            SimpleName::new(15, Span::new(62, 62)),
                            Separator::And, Span::new(64, 64),
                            SimpleName::new(16, Span::new(66, 66)),
                        ),
                        Separator::Or, Span::new(68, 68),
                        BinaryExpr::new(
                            SimpleName::new(17, Span::new(70, 70)),
                            Separator::Caret, Span::new(72, 72),
                            SimpleName::new(18, Span::new(74, 74)),
                        )
                    ),
                    Separator::NotEq, Span::new(76, 77),
                    SimpleName::new(19, Span::new(79, 79)),
                ),
                Separator::EqEq, Span::new(81, 82),
                SimpleName::new(20, Span::new(84, 84))
            )
        ))
    }
    //                                     1234567890
    assert_node_eq!{ make_node!("a & b == c" as BinaryExpr), // ((a & b) == c)
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                SimpleName::new(2, Span::new(0, 0)),
                Separator::And, Span::new(2, 2),
                SimpleName::new(3, Span::new(4, 4)),
            ),
            Separator::EqEq, Span::new(6, 7),
            SimpleName::new(4, Span::new(9, 9)),
        ))
    }

    // 1 + 2 << 3 => (1 + 2) << 3, don't want this
    // cout << 5 + 6 => cout << (5 + 6), want this

    // program generated random tests
    //                                     0        1         2         3    
    //                                     1234567890123456789012345678901234
    assert_eq!{ make_node!("0 + 6 ^ 3 & 3 / 3 - 8 && 2 & 0 + 6" as BinaryExpr), // (((0 + 6) ^ (3 & ((3 / 3) - 8))) && (2 & (0 + 6)))
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                BinaryExpr::new(
                    Expr::Lit(make_lit!(0, 0, 0)),
                    Separator::Add, Span::new(2, 2),
                    Expr::Lit(make_lit!(6, 4, 4))
                ),
                Separator::Caret, Span::new(6, 6),
                BinaryExpr::new(
                    Expr::Lit(make_lit!(3, 8, 8)),
                    Separator::And, Span::new(10, 10),
                    BinaryExpr::new(
                        BinaryExpr::new(
                            Expr::Lit(make_lit!(3, 12, 12)),
                            Separator::Div, Span::new(14, 14),
                            Expr::Lit(make_lit!(3, 16, 16))
                        ),
                        Separator::Sub, Span::new(18, 18),
                        Expr::Lit(make_lit!(8, 20, 20))
                    )
                )
            ),
            Separator::AndAnd, Span::new(22, 23),
            BinaryExpr::new(
                Expr::Lit(make_lit!(2, 25, 25)),
                Separator::And, Span::new(27, 27),
                BinaryExpr::new(
                    Expr::Lit(make_lit!(0, 29, 29)),
                    Separator::Add, Span::new(31, 31),
                    Expr::Lit(make_lit!(6, 33, 33))
                )
            )
        ))
    }
    //                                     0        1         2         3         4         5         6         7
    //                                     1234567890123456789012345678901234567890123456789012345678901234567890
    assert_eq!{ make_node!("7 > 1 | 0 % 8 | 1 % 7 * 3 % 6 == 1 >> 8 % 3 ^ 6 << 0 ^ 2 >> 6 || 1 - 0" as BinaryExpr),
        // (((((7 > 1) | (0 % 8)) | (((1 % 7) * 3) % 6)) == (((1 >> (8 % 3)) ^ (6 << 0)) ^ (2 >> 6))) || (1 - 0))
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                BinaryExpr::new(
                    BinaryExpr::new(
                        BinaryExpr::new(
                            Expr::Lit(make_lit!(7, 0, 0)),
                            Separator::Gt, Span::new(2, 2),
                            Expr::Lit(make_lit!(1, 4, 4)),
                        ),
                        Separator::Or, Span::new(6, 6),
                        BinaryExpr::new(
                            Expr::Lit(make_lit!(0, 8, 8)),
                            Separator::Rem, Span::new(10, 10),
                            Expr::Lit(make_lit!(8, 12, 12))
                        )
                    ), 
                    Separator::Or, Span::new(14, 14),
                    BinaryExpr::new(
                        BinaryExpr::new(
                            BinaryExpr::new(
                                Expr::Lit(make_lit!(1, 16, 16)),
                                Separator::Rem, Span::new(18, 18),
                                Expr::Lit(make_lit!(7, 20, 20)),
                            ),
                            Separator::Mul, Span::new(22, 22),
                            Expr::Lit(make_lit!(3, 24, 24)),
                        ),
                        Separator::Rem, Span::new(26, 26),
                        Expr::Lit(make_lit!(6, 28, 28))
                    )
                ),
                Separator::EqEq, Span::new(30, 31),
                BinaryExpr::new(
                    BinaryExpr::new(
                        BinaryExpr::new(
                            Expr::Lit(make_lit!(1, 33, 33)),
                            Separator::GtGt, Span::new(35, 36),
                            BinaryExpr::new(
                                Expr::Lit(make_lit!(8, 38, 38)),
                                Separator::Rem, Span::new(40, 40),
                                Expr::Lit(make_lit!(3, 42, 42)),
                            )
                        ),
                        Separator::Caret, Span::new(44, 44),
                        BinaryExpr::new(
                            Expr::Lit(make_lit!(6, 46, 46)),
                            Separator::LtLt, Span::new(48, 49),
                            Expr::Lit(make_lit!(0, 51, 51)),
                        )
                    ),
                    Separator::Caret, Span::new(53, 53),
                    BinaryExpr::new(
                        Expr::Lit(make_lit!(2, 55, 55)),
                        Separator::GtGt, Span::new(57, 58),
                        Expr::Lit(make_lit!(6, 60, 60)),
                    )
                )
            ),
            Separator::OrOr, Span::new(62, 63),
            BinaryExpr::new(
                Expr::Lit(make_lit!(1, 65, 65)),
                Separator::Sub, Span::new(67, 67),
                Expr::Lit(make_lit!(0, 69, 69))
            )
        ))
    }
    //                                     0        1         2         3         4         5         6 
    //                                     1234567890123456789012345678901234567890123456789012345678901
    assert_eq!{ make_node!("7 >> 3 == 8 / 1 && 6 == 1 <= 3 % 6 ^ 3 - 1 - 2 >> 7 || 1 >= 1" as BinaryExpr),
        // */%, +-, <><=>=, <<>>, &, ^, |, ==!=, &&, ||
        // ((((7 >> 3) == (8 / 1)) && (6 == ((1 <= (3 % 6)) ^ (((3 - 1) - 2) >> 7)))) || (1 >= 1))
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                BinaryExpr::new(
                    BinaryExpr::new(
                        Expr::Lit(make_lit!(7, 0, 0)),
                        Separator::GtGt, Span::new(2, 3),
                        Expr::Lit(make_lit!(3, 5, 5)),
                    ),
                    Separator::EqEq, Span::new(7, 8),
                    BinaryExpr::new(
                        Expr::Lit(make_lit!(8, 10, 10)),
                        Separator::Div, Span::new(12, 12),
                        Expr::Lit(make_lit!(1, 14, 14)),
                    )
                ),
                Separator::AndAnd, Span::new(16, 17),
                BinaryExpr::new(
                    Expr::Lit(make_lit!(6, 19, 19)),
                    Separator::EqEq, Span::new(21, 22),
                    BinaryExpr::new(
                        BinaryExpr::new(
                            Expr::Lit(make_lit!(1, 24, 24)),
                            Separator::LtEq, Span::new(26, 27),
                            BinaryExpr::new(
                                Expr::Lit(make_lit!(3, 29, 29)),
                                Separator::Rem, Span::new(31, 31),
                                Expr::Lit(make_lit!(6, 33, 33))
                            )
                        ),
                        Separator::Caret, Span::new(35, 35),
                        BinaryExpr::new(
                            BinaryExpr::new(
                                BinaryExpr::new(
                                    Expr::Lit(make_lit!(3, 37, 37)),
                                    Separator::Sub, Span::new(39, 39),
                                    Expr::Lit(make_lit!(1, 41, 41)),
                                ),
                                Separator::Sub, Span::new(43, 43),
                                Expr::Lit(make_lit!(2, 45, 45)),
                            ),
                            Separator::GtGt, Span::new(47, 48),
                            Expr::Lit(make_lit!(7, 50, 50))
                        )
                    )
                )
            ),
            Separator::OrOr, Span::new(52, 53),
            BinaryExpr::new(
                Expr::Lit(make_lit!(1, 55, 55)),
                Separator::GtEq, Span::new(57, 58),
                Expr::Lit(make_lit!(1, 60, 60))
            )
        ))
    }
    //                                     0     
    //                                     123456
    assert_eq!{ make_node!("4 >> 7" as BinaryExpr),
        Expr::Binary(BinaryExpr::new(
            Expr::Lit(make_lit!(4, 0, 0)),
            Separator::GtGt, Span::new(2, 3),
            Expr::Lit(make_lit!(7, 5, 5))
        ))
    }
    //                                     0        1         2         3         4         5         6     
    //                                     12345678901234567890123456789012345678901234567890123456789012345
    assert_eq!{ make_node!("8 & 0 | 7 + 7 | 7 * 0 && 1 - 2 * 3 | 0 - 7 >= 6 >> 5 % 5 || 5 % 3" as BinaryExpr),
        // (((((8 & 0) | (7 + 7)) | (7 * 0)) && ((1 - (2 * 3)) | (((0 - 7) >= 6) >> (5 % 5)))) || (5 % 3))
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                BinaryExpr::new(
                    BinaryExpr::new(
                        BinaryExpr::new(
                            Expr::Lit(make_lit!(8, 0, 0)),
                            Separator::And, Span::new(2, 2),
                            Expr::Lit(make_lit!(0, 4, 4))
                        ),
                        Separator::Or, Span::new(6, 6),
                        BinaryExpr::new(
                            Expr::Lit(make_lit!(7, 8, 8)),
                            Separator::Add, Span::new(10, 10),
                            Expr::Lit(make_lit!(7, 12, 12))
                        )
                    ),
                    Separator::Or, Span::new(14, 14),
                    BinaryExpr::new(
                        Expr::Lit(make_lit!(7, 16, 16)),
                        Separator::Mul, Span::new(18, 18),
                        Expr::Lit(make_lit!(0, 20, 20))
                    )
                ),
                Separator::AndAnd, Span::new(22, 23),
                BinaryExpr::new(
                    BinaryExpr::new(
                        Expr::Lit(make_lit!(1, 25, 25)),
                        Separator::Sub, Span::new(27, 27),
                        BinaryExpr::new(
                            Expr::Lit(make_lit!(2, 29, 29)),
                            Separator::Mul, Span::new(31, 31),
                            Expr::Lit(make_lit!(3, 33, 33))
                        )
                    ),
                    Separator::Or, Span::new(35, 35),
                    BinaryExpr::new(
                        BinaryExpr::new(
                            BinaryExpr::new(
                                Expr::Lit(make_lit!(0, 37, 37)),
                                Separator::Sub, Span::new(39, 39),
                                Expr::Lit(make_lit!(7, 41, 41))
                            ),
                            Separator::GtEq, Span::new(43, 44),
                            Expr::Lit(make_lit!(6, 46, 46))
                        ),
                        Separator::GtGt, Span::new(48, 49),
                        BinaryExpr::new(
                            Expr::Lit(make_lit!(5, 51, 51)),
                            Separator::Rem, Span::new(53, 53),
                            Expr::Lit(make_lit!(5, 55, 55))
                        )
                    )
                )
            ),
            Separator::OrOr, Span::new(57, 58),
            BinaryExpr::new(
                Expr::Lit(make_lit!(5, 60, 60)),
                Separator::Rem, Span::new(62, 62),
                Expr::Lit(make_lit!(3, 64, 64))
            )
        ))
    }
    //                                     0        1         2         3         4         5         6     
    //                                     12345678901234567890123456789012345678901234567890123456789012345678
    assert_eq!{ make_node!("3 <= 2 + 4 <= 5 && 3 < 3 + 2 >> 1 * 2 & 8 && 1 >= 1 < 0 || 6 < 4 * 4" as BinaryExpr),
        // (((((3 <= (2 + 4)) <= 5) && (((3 < (3 + 2)) >> (1 * 2)) & 8)) && ((1 >= 1) < 0)) || (6 < (4 * 4)))
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                BinaryExpr::new(
                    BinaryExpr::new(
                        BinaryExpr::new(
                            Expr::Lit(make_lit!(3, 0, 0)),
                            Separator::LtEq, Span::new(2, 3),
                            BinaryExpr::new(
                                Expr::Lit(make_lit!(2, 5, 5)),
                                Separator::Add, Span::new(7, 7),
                                Expr::Lit(make_lit!(4, 9, 9))
                            )
                        ),
                        Separator::LtEq, Span::new(11, 12),
                        Expr::Lit(make_lit!(5, 14, 14))
                    ),
                    Separator::AndAnd, Span::new(16, 17),
                    BinaryExpr::new(
                        BinaryExpr::new(
                            BinaryExpr::new(
                                Expr::Lit(make_lit!(3, 19, 19)),
                                Separator::Lt, Span::new(21, 21),
                                BinaryExpr::new(
                                    Expr::Lit(make_lit!(3, 23, 23)),
                                    Separator::Add, Span::new(25, 25),
                                    Expr::Lit(make_lit!(2, 27, 27))
                                )
                            ),
                            Separator::GtGt, Span::new(29, 30),
                            BinaryExpr::new(
                                Expr::Lit(make_lit!(1, 32, 32)),
                                Separator::Mul, Span::new(34, 34),
                                Expr::Lit(make_lit!(2, 36, 36))
                            )
                        ),
                        Separator::And, Span::new(38, 38),
                        Expr::Lit(make_lit!(8, 40, 40))
                    )
                ),
                Separator::AndAnd, Span::new(42, 43),
                BinaryExpr::new(
                    BinaryExpr::new(
                        Expr::Lit(make_lit!(1, 45, 45)),
                        Separator::GtEq, Span::new(47, 48),
                        Expr::Lit(make_lit!(1, 50, 50))
                    ),
                    Separator::Lt, Span::new(52, 52),
                    Expr::Lit(make_lit!(0, 54, 54))
                )
            ),
            Separator::OrOr, Span::new(56, 57),
            BinaryExpr::new(
                Expr::Lit(make_lit!(6, 59, 59)),
                Separator::Lt, Span::new(61, 61),
                BinaryExpr::new(
                    Expr::Lit(make_lit!(4, 63, 63)),
                    Separator::Mul, Span::new(65, 65),
                    Expr::Lit(make_lit!(4, 67, 67))
                )
            )
        ))
    }
    //                                     0        1         2
    //                                     12345678901234567890
    assert_eq!{ make_node!("5 >= 6 | 3 == 4 && 3" as BinaryExpr),
        // ((((5 >= 6) | 3) == 4) && 3)
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                BinaryExpr::new(
                    BinaryExpr::new(
                        Expr::Lit(make_lit!(5, 0, 0)),
                        Separator::GtEq, Span::new(2, 3),
                        Expr::Lit(make_lit!(6, 5, 5))
                    ),
                    Separator::Or, Span::new(7, 7),
                    Expr::Lit(make_lit!(3, 9, 9))
                ),
                Separator::EqEq, Span::new(11, 12),
                Expr::Lit(make_lit!(4, 14, 14))
            ),
            Separator::AndAnd, Span::new(16, 17),
            Expr::Lit(make_lit!(3, 19, 19))
        ))
    }
    //                                     0        1         2         3         4         5         6         7
    //                                     1234567890123456789012345678901234567890123456789012345678901234567890123456
    assert_eq!{ make_node!("6 && 7 >> 8 && 0 / 8 * 7 + 5 < 5 / 5 >> 5 - 1 >= 6 > 8 | 6 >> 5 > 2 + 1 || 0" as BinaryExpr),
        // */%, +-, <><=>=, <<>>, &, ^, |, ==!=, &&, ||
        // (((6 && (7 >> 8)) && ((((((0 / 8) * 7) + 5) < (5 / 5)) >> (((5 - 1) >= 6) > 8)) | (6 >> (5 > (2 + 1))))) || 0)
        Expr::Binary(BinaryExpr::new(
            BinaryExpr::new(
                BinaryExpr::new(
                    Expr::Lit(make_lit!(6, 0, 0)),
                    Separator::AndAnd, Span::new(2, 3),
                    BinaryExpr::new(
                        Expr::Lit(make_lit!(7, 5, 5)),
                        Separator::GtGt, Span::new(7, 8),
                        Expr::Lit(make_lit!(8, 10, 10))
                    )
                ),
                Separator::AndAnd, Span::new(12, 13),
                BinaryExpr::new(
                    BinaryExpr::new(
                        BinaryExpr::new(
                            BinaryExpr::new(
                                BinaryExpr::new(
                                    BinaryExpr::new(
                                        Expr::Lit(make_lit!(0, 15, 15)),
                                        Separator::Div, Span::new(17, 17),
                                        Expr::Lit(make_lit!(8, 19, 19))
                                    ), 
                                    Separator::Mul, Span::new(21, 21),
                                    Expr::Lit(make_lit!(7, 23, 23))
                                ), 
                                Separator::Add, Span::new(25, 25),
                                Expr::Lit(make_lit!(5, 27, 27))
                            ),
                            Separator::Lt, Span::new(29, 29),
                            BinaryExpr::new(
                                Expr::Lit(make_lit!(5, 31, 31)),
                                Separator::Div, Span::new(33, 33),
                                Expr::Lit(make_lit!(5, 35, 35))
                            )
                        ),
                        Separator::GtGt, Span::new(37, 38),
                        BinaryExpr::new(
                            BinaryExpr::new(
                                BinaryExpr::new(
                                    Expr::Lit(make_lit!(5, 40, 40)),
                                    Separator::Sub, Span::new(42, 42),
                                    Expr::Lit(make_lit!(1, 44, 44))
                                ), 
                                Separator::GtEq, Span::new(46, 47),
                                Expr::Lit(make_lit!(6, 49, 49))
                            ), 
                            Separator::Gt, Span::new(51, 51),
                            Expr::Lit(make_lit!(8, 53, 53))
                        )
                    ),
                    Separator::Or, Span::new(55, 55),
                    BinaryExpr::new(
                        Expr::Lit(make_lit!(6, 57, 57)),
                        Separator::GtGt, Span::new(59, 60),
                        BinaryExpr::new(
                            Expr::Lit(make_lit!(5, 62, 62)),
                            Separator::Gt, Span::new(64, 64),
                            BinaryExpr::new(
                                Expr::Lit(make_lit!(2, 66, 66)),
                                Separator::Add, Span::new(68, 68),
                                Expr::Lit(make_lit!(1, 70, 70))
                            )
                        )
                    )
                )
            ),
            Separator::OrOr, Span::new(72, 73),
            Expr::Lit(make_lit!(0, 75, 75))
        ))
    }
}
