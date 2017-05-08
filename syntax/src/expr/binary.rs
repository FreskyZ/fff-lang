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

// TODO: according to docs/bits-type, change the priority to
// mul, add, relation, shift, bitand, bitxor, bitor, equal, logicaland, logicalor
// integral binray operators in advance, then is bits binary operators, they are should precede equality operators, then logical operators

use std::fmt;

use codepos::StringPosition;
use lexical::Token;
use lexical::SeperatorKind;
use lexical::SeperatorCategory;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;
use super::unary::UnaryExpr;
use super::postfix::PostfixExpr;
use super::primary::PrimaryExpr;
use lexical::LitValue;

#[cfg_attr(test, derive(Eq, PartialEq))]
struct BinaryBinaryExpr {
    left: BinaryExpr,
    operator: SeperatorKind,            // this means every binary operator matches a binary expr
    operator_strpos: StringPosition, 
    right: BinaryExpr,
    all_strpos: StringPosition,
}
#[cfg_attr(test, derive(Eq, PartialEq))]
enum BinaryExprImpl {
    Unary(UnaryExpr),
    Binary(BinaryBinaryExpr),
}
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct BinaryExpr(Box<BinaryExprImpl>); // wrapper for make it not public

impl ISyntaxItemFormat for BinaryExpr {
    fn format(&self, indent: u32) -> String {
        match self.0.as_ref() {
            &BinaryExprImpl::Unary(ref unary_expr) => unary_expr.format(indent),
            &BinaryExprImpl::Binary(BinaryBinaryExpr{ ref left, ref right, ref operator, ref operator_strpos, ref all_strpos }) => {
                format!("{}BinaryExpr <{:?}>\n{}\n{}{} <{:?}>\n{}", 
                    BinaryExpr::indent_str(indent), all_strpos,
                    left.format(indent + 1),
                    BinaryExpr::indent_str(indent + 1), operator, operator_strpos,
                    right.format(indent + 1),
                )
            }
        }
    }
}
impl fmt::Debug for BinaryExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n{}", self.format(0))
    }
}
impl BinaryExpr { // New

    pub fn new_binary(left: BinaryExpr, operator: SeperatorKind, operator_strpos: StringPosition, right: BinaryExpr) -> BinaryExpr {

        let all_strpos =  StringPosition::merge(left.get_all_strpos(), right.get_all_strpos());
        BinaryExpr(Box::new(BinaryExprImpl::Binary(BinaryBinaryExpr{
            left: left,
            right: right,
            operator: operator,
            operator_strpos: operator_strpos,
            all_strpos: all_strpos
        })))
    }
    pub fn new_unary(unary_expr: UnaryExpr) -> BinaryExpr {
        BinaryExpr(Box::new(BinaryExprImpl::Unary(unary_expr)))
    }
    pub fn new_postfix(postfix_expr: PostfixExpr) -> BinaryExpr {
        BinaryExpr(Box::new(BinaryExprImpl::Unary(UnaryExpr::new_postfix(postfix_expr))))
    }
    pub fn new_primary(primary_expr: PrimaryExpr) -> BinaryExpr {
        BinaryExpr(Box::new(BinaryExprImpl::Unary(UnaryExpr::new_primary(primary_expr))))
    }

    // Primary helpers
    pub fn new_ident(ident: String, ident_strpos: StringPosition) -> BinaryExpr {
        BinaryExpr::new_primary(PrimaryExpr::new_ident(ident, ident_strpos))
    }
    pub fn new_tuple(paren_strpos: StringPosition, exprs: Vec<BinaryExpr>) -> BinaryExpr {
        BinaryExpr::new_primary(PrimaryExpr::new_tuple(paren_strpos, exprs))
    }
    pub fn new_array(bracket_strpos: StringPosition, exprs: Vec<BinaryExpr>) -> BinaryExpr {
        BinaryExpr::new_primary(PrimaryExpr::new_array(bracket_strpos, exprs))
    }
    pub fn new_lit(value: LitValue, lit_strpos: StringPosition) -> BinaryExpr {
        BinaryExpr::new_primary(PrimaryExpr::new_lit(value, lit_strpos))
    }
    pub fn new_unit(unit_strpos: StringPosition) -> BinaryExpr {
        BinaryExpr::new_primary(PrimaryExpr::new_unit(unit_strpos))
    }
    pub fn new_paren(paren_strpos: StringPosition, inner: BinaryExpr) -> BinaryExpr {
        BinaryExpr::new_primary(PrimaryExpr::new_paren(paren_strpos, inner))
    }
    pub fn new_array_dup(bracket_strpos: StringPosition, expr1: BinaryExpr, expr2: BinaryExpr) -> BinaryExpr {
        BinaryExpr::new_primary(PrimaryExpr::new_array_dup(bracket_strpos, expr1, expr2))
    }
}
impl BinaryExpr { // get

    pub fn is_unary(&self) -> bool {
        match self.0.as_ref() {
            &BinaryExprImpl::Unary(_) => true,
            &BinaryExprImpl::Binary(_) => false,
        }
    }
    pub fn is_binary(&self) -> bool {
        match self.0.as_ref() {
            &BinaryExprImpl::Unary(_) => false,
            &BinaryExprImpl::Binary(_) => true,
        }
    }

    pub fn get_unary(&self) -> Option<&UnaryExpr> {
        match self.0.as_ref() {
            &BinaryExprImpl::Unary(ref unary_expr) => Some(unary_expr),
            &BinaryExprImpl::Binary(_) => None,
        }
    }
    pub fn get_left(&self) -> Option<&BinaryExpr> {
        match self.0.as_ref() {
            &BinaryExprImpl::Unary(_) => None,
            &BinaryExprImpl::Binary(BinaryBinaryExpr{ ref left, right: ref _1, operator: ref _2, operator_strpos: ref _3, all_strpos: ref _4 }) => Some(left),
        }
    }
    pub fn get_right(&self) -> Option<&BinaryExpr> {
        match self.0.as_ref() {
            &BinaryExprImpl::Unary(_) => None,
            &BinaryExprImpl::Binary(BinaryBinaryExpr{ ref right, left: ref _1, operator: ref _2, operator_strpos: ref _3, all_strpos: ref _4 }) => Some(right),
        }
    }
    pub fn get_operator(&self) -> Option<&SeperatorKind> {
        match self.0.as_ref() {
            &BinaryExprImpl::Unary(_) => None,
            &BinaryExprImpl::Binary(BinaryBinaryExpr{ ref operator, right: ref _1, left: ref _2, operator_strpos: ref _3, all_strpos: ref _4 }) => Some(operator),
        }
    }
    pub fn get_operator_strpos(&self) -> StringPosition {
        match self.0.as_ref() {
            &BinaryExprImpl::Unary(_) => StringPosition::new(),
            &BinaryExprImpl::Binary(BinaryBinaryExpr{ ref operator_strpos, right: ref _1, operator: ref _2, left: ref _3, all_strpos: ref _4 }) => *operator_strpos,
        }
    }
    pub fn get_all_strpos(&self) -> StringPosition {
        match self.0.as_ref() {
            &BinaryExprImpl::Unary(ref unary_expr) => unary_expr.get_all_strpos(),
            &BinaryExprImpl::Binary(BinaryBinaryExpr{ ref all_strpos, right: ref _1, operator: ref _2, operator_strpos: ref _3, left: ref _4 }) => *all_strpos,
        }
    }
}

#[cfg(feature = "trace_binary_expr_parse")]
macro_rules! trace { ($($arg:tt)*) => ({ print!("[PrimaryExpr] "); println!($($arg)*); }) }
#[cfg(not(feature = "trace_binary_expr_parse"))]
macro_rules! trace { ($($arg:tt)*) => () }

fn parse_unary_wrapper(sess: &mut ParseSession) -> ParseResult<BinaryExpr> { Ok(BinaryExpr::new_unary(UnaryExpr::parse(sess)?)) }
macro_rules! impl_binary_parser {
    ($parser_name: ident, $previous_parser: ident, $op_category: expr) => (
        fn $parser_name(sess: &mut ParseSession) -> ParseResult<BinaryExpr> {
            trace!("parsing {}", stringify!($parser_name));

            let mut current_retval = $previous_parser(sess)?;
            loop {
                match (sess.tk, sess.pos) {
                    (&Token::Sep(operator), operator_strpos) if operator.is_category($op_category) => {
                        sess.move_next();
                        let right_expr = $previous_parser(sess)?;
                        current_retval = BinaryExpr::new_binary(current_retval, operator, operator_strpos, right_expr);
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
impl_binary_parser! { parse_multiplicative, parse_unary_wrapper, SeperatorCategory::Multiplicative }
impl_binary_parser! { parse_additive, parse_multiplicative, SeperatorCategory::Additive }
impl_binary_parser! { parse_relational, parse_additive, SeperatorCategory::Relational }
impl_binary_parser! { parse_shift, parse_relational, SeperatorCategory::Shift }
impl_binary_parser! { parse_bitand, parse_shift, SeperatorCategory::BitAnd }
impl_binary_parser! { parse_bitxor, parse_bitand, SeperatorCategory::BitXor }
impl_binary_parser! { parse_bitor, parse_bitxor, SeperatorCategory::BitOr }
impl_binary_parser! { parse_equality, parse_bitor, SeperatorCategory::Equality }
impl_binary_parser! { parse_logical_and, parse_equality, SeperatorCategory::LogicalAnd }
impl_binary_parser! { parse_logical_or, parse_logical_and, SeperatorCategory::LogicalOr }

impl ISyntaxItemGrammar for BinaryExpr {
    fn is_first_final(sess: &ParseSession) -> bool { UnaryExpr::is_first_final(sess) }
}
impl ISyntaxItemParse for BinaryExpr {
    fn parse(sess: &mut ParseSession) -> ParseResult<BinaryExpr> { parse_logical_or(sess) }
}

#[cfg(test)] #[test]
fn binary_expr_format() {
    
    let binary_expr = BinaryExpr::new_binary(
        BinaryExpr::new_primary(PrimaryExpr::new_lit(LitValue::from(1), make_str_pos!(1, 1, 1, 1))),
        SeperatorKind::Add,
        make_str_pos!(1, 3, 1, 3),
        BinaryExpr::new_primary(PrimaryExpr::new_lit(LitValue::from(2), make_str_pos!(1, 5, 1, 5))),
    );
    assert_eq!(binary_expr.format(0), "BinaryExpr <<0>1:1-1:5>\n  Literal (i32)1 <<0>1:1-1:1>\n  + <<0>1:3-1:3>\n  Literal (i32)2 <<0>1:5-1:5>");
}

#[cfg(test)] #[test]
fn binary_expr_parse() {
    use lexical::NumLitValue;
    use super::super::ISyntaxItemWithStr;
    
    macro_rules! ident { ($ident_name: expr, $strpos: expr) => (BinaryExpr::new_primary(PrimaryExpr::new_ident($ident_name.to_owned(), $strpos))) }
    macro_rules! int { ($value: expr, $strpos: expr) => (BinaryExpr::new_primary(PrimaryExpr::new_lit_num(NumLitValue::from($value), $strpos))) }

    let new_binary = BinaryExpr::new_binary;

    // my random tests
    //                                     123456789012345
    assert_eq!{ BinaryExpr::with_test_str("[1] * [2] / [3]"), 
        BinaryExpr::new_binary(
            BinaryExpr::new_binary(
                BinaryExpr::new_primary(PrimaryExpr::new_array(make_str_pos!(1, 1, 1, 3), vec![
                    int!(1, make_str_pos!(1, 2, 1, 2)),
                ])), 
                SeperatorKind::Mul, make_str_pos!(1, 5, 1, 5),
                BinaryExpr::new_primary(PrimaryExpr::new_array(make_str_pos!(1, 7, 1, 9), vec![
                    int!(2, make_str_pos!(1, 8, 1, 8)),
                ])),
            ),
            SeperatorKind::Div, make_str_pos!(1, 11, 1, 11),
            BinaryExpr::new_primary(PrimaryExpr::new_array(make_str_pos!(1, 13, 1, 15), vec![
                int!(3, make_str_pos!(1, 14, 1, 14)),
            ]))
        )
    }           
    //                                     0        1         2
    //                                     123456789012345678901
    assert_eq!{ BinaryExpr::with_test_str("a * b / c + d % e - f"),  // ((((a * b) / c) + (d % e)) - f)
        BinaryExpr::new_binary(
            BinaryExpr::new_binary(
                BinaryExpr::new_binary(
                    BinaryExpr::new_binary(
                        ident!("a", make_str_pos!(1, 1, 1, 1)),
                        SeperatorKind::Mul, make_str_pos!(1, 3, 1, 3), 
                        ident!("b", make_str_pos!(1, 5, 1, 5)),
                    ),
                    SeperatorKind::Div, make_str_pos!(1, 7, 1, 7),
                    ident!("c", make_str_pos!(1, 9, 1, 9)),
                ),
                SeperatorKind::Add, make_str_pos!(1, 11, 1, 11),
                BinaryExpr::new_binary(
                    ident!("d", make_str_pos!(1, 13, 1, 13)),
                    SeperatorKind::Rem, make_str_pos!(1, 15, 1, 15),
                    ident!("e", make_str_pos!(1, 17, 1, 17)),
                )
            ),
            SeperatorKind::Sub, make_str_pos!(1, 19, 1, 19),
            ident!("f", make_str_pos!(1, 21, 1, 21)),
        )
    }           
    //                                     0        1         2         3
    //                                     1234567890123456789012345678901
    assert_eq!{ BinaryExpr::with_test_str("a * b << h / c + d % e - f >> g"), // (((a * b) << (((h / c) + (d % e)) - f)) >> g)
        BinaryExpr::new_binary(
            BinaryExpr::new_binary(
                BinaryExpr::new_binary(
                    ident!("a", make_str_pos!(1, 1, 1, 1)),
                    SeperatorKind::Mul, make_str_pos!(1, 3, 1, 3),
                    ident!("b", make_str_pos!(1, 5, 1, 5))
                ),
                SeperatorKind::ShiftLeft, make_str_pos!(1, 7, 1, 8),
                BinaryExpr::new_binary(
                    BinaryExpr::new_binary(
                        BinaryExpr::new_binary(
                            ident!("h", make_str_pos!(1, 10, 1, 10)),
                            SeperatorKind::Div, make_str_pos!(1, 12, 1, 12),
                            ident!("c", make_str_pos!(1, 14, 1, 14))
                        ),
                        SeperatorKind::Add, make_str_pos!(1, 16, 1, 16),
                        BinaryExpr::new_binary(
                            ident!("d", make_str_pos!(1, 18, 1, 18)),
                            SeperatorKind::Rem, make_str_pos!(1, 20, 1, 20),
                            ident!("e", make_str_pos!(1, 22, 1, 22))
                        )
                    ),
                    SeperatorKind::Sub, make_str_pos!(1, 24, 1, 24),
                    ident!("f", make_str_pos!(1, 26, 1, 26))
                )
            ),
            SeperatorKind::ShiftRight, make_str_pos!(1, 28, 1, 29),
            ident!("g", make_str_pos!(1, 31, 1, 31)),
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
    assert_eq!{ BinaryExpr::with_test_str("a * b << h / c + d % e - f >> g > h * i < j << k >= m && n || o & p | q ^ r != s == t"),
        // */%, +-, <><=>=, <<>>, &, ^, |, ==!=, &&, ||
        // ((((((a * b) << (((h / c) + (d % e)) - f)) >> ((g > (h * i)) < j)) << (k >= m)) && n) || ((((o & p) | (q ^ r)) != s) == t))
        BinaryExpr::new_binary(
            BinaryExpr::new_binary(
                BinaryExpr::new_binary(
                    BinaryExpr::new_binary(
                        BinaryExpr::new_binary(
                            BinaryExpr::new_binary(
                                ident!("a", make_str_pos!(1, 1, 1, 1)),
                                SeperatorKind::Mul, make_str_pos!(1, 3, 1, 3),
                                ident!("b", make_str_pos!(1, 5, 1, 5)),
                            ),
                            SeperatorKind::ShiftLeft, make_str_pos!(1, 7, 1, 8),
                            BinaryExpr::new_binary(
                                BinaryExpr::new_binary(
                                    BinaryExpr::new_binary(
                                        ident!("h", make_str_pos!(1, 10, 1, 10)),
                                        SeperatorKind::Div, make_str_pos!(1, 12, 1, 12),
                                        ident!("c", make_str_pos!(1, 14, 1, 14)),
                                    ),
                                    SeperatorKind::Add, make_str_pos!(1, 16, 1, 16),
                                    BinaryExpr::new_binary(
                                        ident!("d", make_str_pos!(1, 18, 1, 18)),
                                        SeperatorKind::Rem, make_str_pos!(1, 20, 1, 20),
                                        ident!("e", make_str_pos!(1, 22, 1, 22)),
                                    )
                                ),
                                SeperatorKind::Sub, make_str_pos!(1, 24, 1, 24),
                                ident!("f", make_str_pos!(1, 26, 1, 26))
                            ),
                        ),
                        SeperatorKind::ShiftRight, make_str_pos!(1, 28, 1, 29),
                        BinaryExpr::new_binary(
                            BinaryExpr::new_binary(
                                ident!("g", make_str_pos!(1, 31, 1, 31)),
                                SeperatorKind::Great, make_str_pos!(1, 33, 1, 33),
                                BinaryExpr::new_binary(
                                    ident!("h", make_str_pos!(1, 35, 1, 35)),
                                    SeperatorKind::Mul, make_str_pos!(1, 37, 1, 37),
                                    ident!("i", make_str_pos!(1, 39, 1, 39)),
                                )
                            ),
                            SeperatorKind::Less, make_str_pos!(1, 41, 1, 41),
                            ident!("j", make_str_pos!(1, 43, 1, 43)),
                        )
                    ),
                    SeperatorKind::ShiftLeft, make_str_pos!(1, 45, 1, 46),
                    BinaryExpr::new_binary(
                        ident!("k", make_str_pos!(1, 48, 1, 48)),
                        SeperatorKind::GreatEqual, make_str_pos!(1, 50, 1, 51),
                        ident!("m", make_str_pos!(1, 53, 1, 53)),
                    )
                ),
                SeperatorKind::LogicalAnd, make_str_pos!(1, 55, 1, 56),
                ident!("n", make_str_pos!(1, 58, 1, 58))
            ),
            SeperatorKind::LogicalOr, make_str_pos!(1, 60, 1, 61),
            BinaryExpr::new_binary(
                BinaryExpr::new_binary(
                    BinaryExpr::new_binary(
                        BinaryExpr::new_binary(
                            ident!("o", make_str_pos!(1, 63, 1, 63)),
                            SeperatorKind::BitAnd, make_str_pos!(1, 65, 1, 65),
                            ident!("p", make_str_pos!(1, 67, 1, 67)),
                        ),
                        SeperatorKind::BitOr, make_str_pos!(1, 69, 1, 69),
                        BinaryExpr::new_binary(
                            ident!("q", make_str_pos!(1, 71, 1, 71)),
                            SeperatorKind::BitXor, make_str_pos!(1, 73, 1, 73),
                            ident!("r", make_str_pos!(1, 75, 1, 75)),
                        )
                    ),
                    SeperatorKind::NotEqual, make_str_pos!(1, 77, 1, 78),
                    ident!("s", make_str_pos!(1, 80, 1, 80)),
                ),
                SeperatorKind::Equal, make_str_pos!(1, 82, 1, 83),
                ident!("t", make_str_pos!(1, 85, 1, 85))
            )
        )
    }
    //                                     1234567890
    assert_eq!{ BinaryExpr::with_test_str("a & b == c"), // ((a & b) == c)
        BinaryExpr::new_binary(
            BinaryExpr::new_binary(
                ident!("a", make_str_pos!(1, 1, 1, 1)),
                SeperatorKind::BitAnd, make_str_pos!(1, 3, 1, 3),
                ident!("b", make_str_pos!(1, 5, 1, 5)),
            ),
            SeperatorKind::Equal, make_str_pos!(1, 7, 1, 8),
            ident!("c", make_str_pos!(1, 10, 1, 10)),
        )
    }

    // 1 + 2 << 3 => (1 + 2) << 3, don't want this
    // cout << 5 + 6 => cout << (5 + 6), want this

    // program generated random tests
    //                                     0        1         2         3    
    //                                     1234567890123456789012345678901234
    assert_eq!{ BinaryExpr::with_test_str("0 + 6 ^ 3 & 3 / 3 - 8 && 2 & 0 + 6"), // (((0 + 6) ^ (3 & ((3 / 3) - 8))) && (2 & (0 + 6)))
        BinaryExpr::new_binary(
            BinaryExpr::new_binary(
                BinaryExpr::new_binary(
                    int!(0, make_str_pos!(1, 1, 1, 1)),
                    SeperatorKind::Add, make_str_pos!(1, 3, 1, 3),
                    int!(6, make_str_pos!(1, 5, 1, 5))
                ),
                SeperatorKind::BitXor, make_str_pos!(1, 7, 1, 7),
                BinaryExpr::new_binary(
                    int!(3, make_str_pos!(1, 9, 1, 9)),
                    SeperatorKind::BitAnd, make_str_pos!(1, 11, 1, 11),
                    BinaryExpr::new_binary(
                        BinaryExpr::new_binary(
                            int!(3, make_str_pos!(1, 13, 1, 13)),
                            SeperatorKind::Div, make_str_pos!(1, 15, 1, 15),
                            int!(3, make_str_pos!(1, 17, 1, 17))
                        ),
                        SeperatorKind::Sub, make_str_pos!(1, 19, 1, 19),
                        int!(8, make_str_pos!(1, 21, 1, 21))
                    )
                )
            ),
            SeperatorKind::LogicalAnd, make_str_pos!(1, 23, 1, 24),
            BinaryExpr::new_binary(
                int!(2, make_str_pos!(1, 26, 1, 26)),
                SeperatorKind::BitAnd, make_str_pos!(1, 28, 1, 28),
                BinaryExpr::new_binary(
                    int!(0, make_str_pos!(1, 30, 1, 30)),
                    SeperatorKind::Add, make_str_pos!(1, 32, 1, 32),
                    int!(6, make_str_pos!(1, 34, 1, 34))
                )
            )
        )
    }
    //                                     0        1         2         3         4         5         6         7
    //                                     1234567890123456789012345678901234567890123456789012345678901234567890
    assert_eq!{ BinaryExpr::with_test_str("7 > 1 | 0 % 8 | 1 % 7 * 3 % 6 == 1 >> 8 % 3 ^ 6 << 0 ^ 2 >> 6 || 1 - 0"),
        // (((((7 > 1) | (0 % 8)) | (((1 % 7) * 3) % 6)) == (((1 >> (8 % 3)) ^ (6 << 0)) ^ (2 >> 6))) || (1 - 0))
        new_binary(
            new_binary(
                new_binary(
                    new_binary(
                        new_binary(
                            int!(7, make_str_pos!(1, 1, 1, 1)),
                            SeperatorKind::Great, make_str_pos!(1, 3, 1, 3),
                            int!(1, make_str_pos!(1, 5, 1, 5)),
                        ),
                        SeperatorKind::BitOr, make_str_pos!(1, 7, 1, 7),
                        new_binary(
                            int!(0, make_str_pos!(1, 9, 1, 9)),
                            SeperatorKind::Rem, make_str_pos!(1, 11, 1, 11),
                            int!(8, make_str_pos!(1, 13, 1, 13))
                        )
                    ), 
                    SeperatorKind::BitOr, make_str_pos!(1, 15, 1, 15),
                    new_binary(
                        new_binary(
                            new_binary(
                                int!(1, make_str_pos!(1, 17, 1, 17)),
                                SeperatorKind::Rem, make_str_pos!(1, 19, 1, 19),
                                int!(7, make_str_pos!(1, 21, 1, 21)),
                            ),
                            SeperatorKind::Mul, make_str_pos!(1, 23, 1, 23),
                            int!(3, make_str_pos!(1, 25, 1, 25)),
                        ),
                        SeperatorKind::Rem, make_str_pos!(1, 27, 1, 27),
                        int!(6, make_str_pos!(1, 29, 1, 29))
                    )
                ),
                SeperatorKind::Equal, make_str_pos!(1, 31, 1, 32),
                new_binary(
                    new_binary(
                        new_binary(
                            int!(1, make_str_pos!(1, 34, 1, 34)),
                            SeperatorKind::ShiftRight, make_str_pos!(1, 36, 1, 37),
                            new_binary(
                                int!(8, make_str_pos!(1, 39, 1, 39)),
                                SeperatorKind::Rem, make_str_pos!(1, 41, 1, 41),
                                int!(3, make_str_pos!(1, 43, 1, 43)),
                            )
                        ),
                        SeperatorKind::BitXor, make_str_pos!(1, 45, 1, 45),
                        new_binary(
                            int!(6, make_str_pos!(1, 47, 1, 47)),
                            SeperatorKind::ShiftLeft, make_str_pos!(1, 49, 1, 50),
                            int!(0, make_str_pos!(1, 52, 1, 52)),
                        )
                    ),
                    SeperatorKind::BitXor, make_str_pos!(1, 54, 1, 54),
                    new_binary(
                        int!(2, make_str_pos!(1, 56, 1, 56)),
                        SeperatorKind::ShiftRight, make_str_pos!(1, 58, 1, 59),
                        int!(6, make_str_pos!(1, 61, 1, 61)),
                    )
                )
            ),
            SeperatorKind::LogicalOr, make_str_pos!(1, 63, 1, 64),
            new_binary(
                int!(1, make_str_pos!(1, 66, 1, 66)),
                SeperatorKind::Sub, make_str_pos!(1, 68, 1, 68),
                int!(0, make_str_pos!(1, 70, 1, 70))
            )
        )
    }
    //                                     0        1         2         3         4         5         6 
    //                                     1234567890123456789012345678901234567890123456789012345678901
    assert_eq!{ BinaryExpr::with_test_str("7 >> 3 == 8 / 1 && 6 == 1 <= 3 % 6 ^ 3 - 1 - 2 >> 7 || 1 >= 1"),
        // */%, +-, <><=>=, <<>>, &, ^, |, ==!=, &&, ||
        // ((((7 >> 3) == (8 / 1)) && (6 == ((1 <= (3 % 6)) ^ (((3 - 1) - 2) >> 7)))) || (1 >= 1))
        BinaryExpr::new_binary(
            BinaryExpr::new_binary(
                BinaryExpr::new_binary(
                    BinaryExpr::new_binary(
                        int!(7, make_str_pos!(1, 1, 1, 1)),
                        SeperatorKind::ShiftRight, make_str_pos!(1, 3, 1, 4),
                        int!(3, make_str_pos!(1, 6, 1, 6)),
                    ),
                    SeperatorKind::Equal, make_str_pos!(1, 8, 1, 9),
                    BinaryExpr::new_binary(
                        int!(8, make_str_pos!(1, 11, 1, 11)),
                        SeperatorKind::Div, make_str_pos!(1, 13, 1, 13),
                        int!(1, make_str_pos!(1, 15, 1, 15)),
                    )
                ),
                SeperatorKind::LogicalAnd, make_str_pos!(1, 17, 1, 18),
                BinaryExpr::new_binary(
                    int!(6, make_str_pos!(1, 20, 1, 20)),
                    SeperatorKind::Equal, make_str_pos!(1, 22, 1, 23),
                    BinaryExpr::new_binary(
                        BinaryExpr::new_binary(
                            int!(1, make_str_pos!(1, 25, 1, 25)),
                            SeperatorKind::LessEqual, make_str_pos!(1, 27, 1, 28),
                            BinaryExpr::new_binary(
                                int!(3, make_str_pos!(1, 30, 1, 30)),
                                SeperatorKind::Rem, make_str_pos!(1, 32, 1, 32),
                                int!(6, make_str_pos!(1, 34, 1, 34))
                            )
                        ),
                        SeperatorKind::BitXor, make_str_pos!(1, 36, 1, 36),
                        BinaryExpr::new_binary(
                            BinaryExpr::new_binary(
                                BinaryExpr::new_binary(
                                    int!(3, make_str_pos!(1, 38, 1, 38)),
                                    SeperatorKind::Sub, make_str_pos!(1, 40, 1, 40),
                                    int!(1, make_str_pos!(1, 42, 1, 42)),
                                ),
                                SeperatorKind::Sub, make_str_pos!(1, 44, 1, 44),
                                int!(2, make_str_pos!(1, 46, 1, 46)),
                            ),
                            SeperatorKind::ShiftRight, make_str_pos!(1, 48, 1, 49),
                            int!(7, make_str_pos!(1, 51, 1, 51))
                        )
                    )
                )
            ),
            SeperatorKind::LogicalOr, make_str_pos!(1, 53, 1, 54),
            BinaryExpr::new_binary(
                int!(1, make_str_pos!(1, 56, 1, 56)),
                SeperatorKind::GreatEqual, make_str_pos!(1, 58, 1, 59),
                int!(1, make_str_pos!(1, 61, 1, 61))
            )
        )
    }
    //                                     0     
    //                                     123456
    assert_eq!{ BinaryExpr::with_test_str("4 >> 7"),
        BinaryExpr::new_binary(
            int!(4, make_str_pos!(1, 1, 1, 1)),
            SeperatorKind::ShiftRight, make_str_pos!(1, 3, 1, 4),
            int!(7, make_str_pos!(1, 6, 1, 6))
        )
    }
    //                                     0        1         2         3         4         5         6     
    //                                     12345678901234567890123456789012345678901234567890123456789012345
    assert_eq!{ BinaryExpr::with_test_str("8 & 0 | 7 + 7 | 7 * 0 && 1 - 2 * 3 | 0 - 7 >= 6 >> 5 % 5 || 5 % 3"),
        // (((((8 & 0) | (7 + 7)) | (7 * 0)) && ((1 - (2 * 3)) | (((0 - 7) >= 6) >> (5 % 5)))) || (5 % 3))
        BinaryExpr::new_binary(
            BinaryExpr::new_binary(
                BinaryExpr::new_binary(
                    BinaryExpr::new_binary(
                        BinaryExpr::new_binary(
                            int!(8, make_str_pos!(1, 1, 1, 1)),
                            SeperatorKind::BitAnd, make_str_pos!(1, 3, 1, 3),
                            int!(0, make_str_pos!(1, 5, 1, 5))
                        ),
                        SeperatorKind::BitOr, make_str_pos!(1, 7, 1, 7),
                        BinaryExpr::new_binary(
                            int!(7, make_str_pos!(1, 9, 1, 9)),
                            SeperatorKind::Add, make_str_pos!(1, 11, 1, 11),
                            int!(7, make_str_pos!(1, 13, 1, 13))
                        )
                    ),
                    SeperatorKind::BitOr, make_str_pos!(1, 15, 1, 15),
                    BinaryExpr::new_binary(
                        int!(7, make_str_pos!(1, 17, 1, 17)),
                        SeperatorKind::Mul, make_str_pos!(1, 19, 1, 19),
                        int!(0, make_str_pos!(1, 21, 1, 21))
                    )
                ),
                SeperatorKind::LogicalAnd, make_str_pos!(1, 23, 1, 24),
                BinaryExpr::new_binary(
                    BinaryExpr::new_binary(
                        int!(1, make_str_pos!(1, 26, 1, 26)),
                        SeperatorKind::Sub, make_str_pos!(1, 28, 1, 28),
                        BinaryExpr::new_binary(
                            int!(2, make_str_pos!(1, 30, 1, 30)),
                            SeperatorKind::Mul, make_str_pos!(1, 32, 1, 32),
                            int!(3, make_str_pos!(1, 34, 1, 34))
                        )
                    ),
                    SeperatorKind::BitOr, make_str_pos!(1, 36, 1, 36),
                    BinaryExpr::new_binary(
                        BinaryExpr::new_binary(
                            BinaryExpr::new_binary(
                                int!(0, make_str_pos!(1, 38, 1, 38)),
                                SeperatorKind::Sub, make_str_pos!(1, 40, 1, 40),
                                int!(7, make_str_pos!(1, 42, 1, 42))
                            ),
                            SeperatorKind::GreatEqual, make_str_pos!(1, 44, 1, 45),
                            int!(6, make_str_pos!(1, 47, 1, 47))
                        ),
                        SeperatorKind::ShiftRight, make_str_pos!(1, 49, 1, 50),
                        BinaryExpr::new_binary(
                            int!(5, make_str_pos!(1, 52, 1, 52)),
                            SeperatorKind::Rem, make_str_pos!(1, 54, 1, 54),
                            int!(5, make_str_pos!(1, 56, 1, 56))
                        )
                    )
                )
            ),
            SeperatorKind::LogicalOr, make_str_pos!(1, 58, 1, 59),
            BinaryExpr::new_binary(
                int!(5, make_str_pos!(1, 61, 1, 61)),
                SeperatorKind::Rem, make_str_pos!(1, 63, 1, 63),
                int!(3, make_str_pos!(1, 65, 1, 65))
            )
        )
    }
    //                                     0        1         2         3         4         5         6     
    //                                     12345678901234567890123456789012345678901234567890123456789012345678
    assert_eq!{ BinaryExpr::with_test_str("3 <= 2 + 4 <= 5 && 3 < 3 + 2 >> 1 * 2 & 8 && 1 >= 1 < 0 || 6 < 4 * 4"),
        // (((((3 <= (2 + 4)) <= 5) && (((3 < (3 + 2)) >> (1 * 2)) & 8)) && ((1 >= 1) < 0)) || (6 < (4 * 4)))
        BinaryExpr::new_binary(
            BinaryExpr::new_binary(
                BinaryExpr::new_binary(
                    BinaryExpr::new_binary(
                        BinaryExpr::new_binary(
                            int!(3, make_str_pos!(1, 1, 1, 1)),
                            SeperatorKind::LessEqual, make_str_pos!(1, 3, 1, 4),
                            BinaryExpr::new_binary(
                                int!(2, make_str_pos!(1, 6, 1, 6)),
                                SeperatorKind::Add, make_str_pos!(1, 8, 1, 8),
                                int!(4, make_str_pos!(1, 10, 1, 10))
                            )
                        ),
                        SeperatorKind::LessEqual, make_str_pos!(1, 12, 1, 13),
                        int!(5, make_str_pos!(1, 15, 1, 15))
                    ),
                    SeperatorKind::LogicalAnd, make_str_pos!(1, 17, 1, 18),
                    BinaryExpr::new_binary(
                        BinaryExpr::new_binary(
                            BinaryExpr::new_binary(
                                int!(3, make_str_pos!(1, 20, 1, 20)),
                                SeperatorKind::Less, make_str_pos!(1, 22, 1, 22),
                                BinaryExpr::new_binary(
                                    int!(3, make_str_pos!(1, 24, 1, 24)),
                                    SeperatorKind::Add, make_str_pos!(1, 26, 1, 26),
                                    int!(2, make_str_pos!(1, 28, 1, 28))
                                )
                            ),
                            SeperatorKind::ShiftRight, make_str_pos!(1, 30, 1, 31),
                            BinaryExpr::new_binary(
                                int!(1, make_str_pos!(1, 33, 1, 33)),
                                SeperatorKind::Mul, make_str_pos!(1, 35, 1, 35),
                                int!(2, make_str_pos!(1, 37, 1, 37))
                            )
                        ),
                        SeperatorKind::BitAnd, make_str_pos!(1, 39, 1, 39),
                        int!(8, make_str_pos!(1, 41, 1, 41))
                    )
                ),
                SeperatorKind::LogicalAnd, make_str_pos!(1, 43, 1, 44),
                BinaryExpr::new_binary(
                    BinaryExpr::new_binary(
                        int!(1, make_str_pos!(1, 46, 1, 46)),
                        SeperatorKind::GreatEqual, make_str_pos!(1, 48, 1, 49),
                        int!(1, make_str_pos!(1, 51, 1, 51))
                    ),
                    SeperatorKind::Less, make_str_pos!(1, 53, 1, 53),
                    int!(0, make_str_pos!(1, 55, 1, 55))
                )
            ),
            SeperatorKind::LogicalOr, make_str_pos!(1, 57, 1, 58),
            BinaryExpr::new_binary(
                int!(6, make_str_pos!(1, 60, 1, 60)),
                SeperatorKind::Less, make_str_pos!(1, 62, 1, 62),
                BinaryExpr::new_binary(
                    int!(4, make_str_pos!(1, 64, 1, 64)),
                    SeperatorKind::Mul, make_str_pos!(1, 66, 1, 66),
                    int!(4, make_str_pos!(1, 68, 1, 68))
                )
            )
        )
    }
    //                                     0        1         2
    //                                     12345678901234567890
    assert_eq!{ BinaryExpr::with_test_str("5 >= 6 | 3 == 4 && 3"),
        // ((((5 >= 6) | 3) == 4) && 3)
        BinaryExpr::new_binary(
            BinaryExpr::new_binary(
                BinaryExpr::new_binary(
                    BinaryExpr::new_binary(
                        int!(5, make_str_pos!(1, 1, 1, 1)),
                        SeperatorKind::GreatEqual, make_str_pos!(1, 3, 1, 4),
                        int!(6, make_str_pos!(1, 6, 1, 6))
                    ),
                    SeperatorKind::BitOr, make_str_pos!(1, 8, 1, 8),
                    int!(3, make_str_pos!(1, 10, 1, 10))
                ),
                SeperatorKind::Equal, make_str_pos!(1, 12, 1, 13),
                int!(4, make_str_pos!(1, 15, 1, 15))
            ),
            SeperatorKind::LogicalAnd, make_str_pos!(1, 17, 1, 18),
            int!(3, make_str_pos!(1, 20, 1, 20))
        )
    }
    //                                     0        1         2         3         4         5         6         7
    //                                     1234567890123456789012345678901234567890123456789012345678901234567890123456
    assert_eq!{ BinaryExpr::with_test_str("6 && 7 >> 8 && 0 / 8 * 7 + 5 < 5 / 5 >> 5 - 1 >= 6 > 8 | 6 >> 5 > 2 + 1 || 0"),
        // */%, +-, <><=>=, <<>>, &, ^, |, ==!=, &&, ||
        // (((6 && (7 >> 8)) && ((((((0 / 8) * 7) + 5) < (5 / 5)) >> (((5 - 1) >= 6) > 8)) | (6 >> (5 > (2 + 1))))) || 0)
        BinaryExpr::new_binary(
            BinaryExpr::new_binary(
                BinaryExpr::new_binary(
                    int!(6, make_str_pos!(1, 1, 1, 1)),
                    SeperatorKind::LogicalAnd, make_str_pos!(1, 3, 1, 4),
                    BinaryExpr::new_binary(
                        int!(7, make_str_pos!(1, 6, 1, 6)),
                        SeperatorKind::ShiftRight, make_str_pos!(1, 8, 1, 9),
                        int!(8, make_str_pos!(1, 11, 1, 11))
                    )
                ),
                SeperatorKind::LogicalAnd, make_str_pos!(1, 13, 1, 14),
                BinaryExpr::new_binary(
                    BinaryExpr::new_binary(
                        BinaryExpr::new_binary(
                            BinaryExpr::new_binary(
                                BinaryExpr::new_binary(
                                    BinaryExpr::new_binary(
                                        int!(0, make_str_pos!(1, 16, 1, 16)),
                                        SeperatorKind::Div, make_str_pos!(1, 18, 1, 18),
                                        int!(8, make_str_pos!(1, 20, 1, 20))
                                    ), 
                                    SeperatorKind::Mul, make_str_pos!(1, 22, 1, 22),
                                    int!(7, make_str_pos!(1, 24, 1, 24))
                                ), 
                                SeperatorKind::Add, make_str_pos!(1, 26, 1, 26),
                                int!(5, make_str_pos!(1, 28, 1, 28))
                            ),
                            SeperatorKind::Less, make_str_pos!(1, 30, 1, 30),
                            BinaryExpr::new_binary(
                                int!(5, make_str_pos!(1, 32, 1, 32)),
                                SeperatorKind::Div, make_str_pos!(1, 34, 1, 34),
                                int!(5, make_str_pos!(1, 36, 1, 36))
                            )
                        ),
                        SeperatorKind::ShiftRight, make_str_pos!(1, 38, 1, 39),
                        BinaryExpr::new_binary(
                            BinaryExpr::new_binary(
                                BinaryExpr::new_binary(
                                    int!(5, make_str_pos!(1, 41, 1, 41)),
                                    SeperatorKind::Sub, make_str_pos!(1, 43, 1, 43),
                                    int!(1, make_str_pos!(1, 45, 1, 45))
                                ), 
                                SeperatorKind::GreatEqual, make_str_pos!(1, 47, 1, 48),
                                int!(6, make_str_pos!(1, 50, 1, 50))
                            ), 
                            SeperatorKind::Great, make_str_pos!(1, 52, 1, 52),
                            int!(8, make_str_pos!(1, 54, 1, 54))
                        )
                    ),
                    SeperatorKind::BitOr, make_str_pos!(1, 56, 1, 56),
                    BinaryExpr::new_binary(
                        int!(6, make_str_pos!(1, 58, 1, 58)),
                        SeperatorKind::ShiftRight, make_str_pos!(1, 60, 1, 61),
                        BinaryExpr::new_binary(
                            int!(5, make_str_pos!(1, 63, 1, 63)),
                            SeperatorKind::Great, make_str_pos!(1, 65, 1, 65),
                            BinaryExpr::new_binary(
                                int!(2, make_str_pos!(1, 67, 1, 67)),
                                SeperatorKind::Add, make_str_pos!(1, 69, 1, 69),
                                int!(1, make_str_pos!(1, 71, 1, 71))
                            )
                        )
                    )
                )
            ),
            SeperatorKind::LogicalOr, make_str_pos!(1, 73, 1, 74),
            int!(0, make_str_pos!(1, 76, 1, 76))
        )
    }
}
