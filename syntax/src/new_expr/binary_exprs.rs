///! fff-lang
///!
///! syntax/binary_expr

use std::fmt;

use codemap::Span;
use lexical::SeperatorKind;

use super::Expr;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub operator: SeperatorKind,
    pub operator_span: Span,
}
impl ISyntaxItemFormat for BinaryExpr {
    fn format(&self, indent: u32) -> String {
        format!("{}BinaryExpr <{:?}>\n{}\n{}{} <{:?}>\n{}", 
            BinaryExpr::indent_str(indent), self.left.get_all_span().merge(&self.right.get_all_span()),
            left.format(indent + 1),
            BinaryExpr::indent_str(indent + 1), self.operator, self.operator_span,
            right.format(indent + 1),
        )
    }
}
impl fmt::Debug for BinaryExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl BinaryExpr {   

    pub fn new(left: Expr, op: SeperatorKind, op_span: Span, right: Expr) -> BinaryExpr {
        BinaryExpr{ left, right, operator: op, operator_span: op_span }
    }
}
impl BinaryExpr {

    pub fn get_all_span(&self) -> Span { self.left.get_all_span().merge(&self.right.get_all_span()) }

    pub fn parse<F: Fn(&mut ParseSession) -> ParseResult<Expr>>(sess: &mut ParseSession, last_parser: F) -> ParseResult<BinaryExpr> {
        #[cfg(feature = "trace_binary_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[PrimaryExpr] "); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_binary_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

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
        impl_binary_parser! { parse_multiplicative, last_parser, SeperatorCategory::Multiplicative }
        impl_binary_parser! { parse_additive, parse_multiplicative, SeperatorCategory::Additive }
        impl_binary_parser! { parse_relational, parse_additive, SeperatorCategory::Relational }
        impl_binary_parser! { parse_shift, parse_relational, SeperatorCategory::Shift }
        impl_binary_parser! { parse_bitand, parse_shift, SeperatorCategory::BitAnd }
        impl_binary_parser! { parse_bitxor, parse_bitand, SeperatorCategory::BitXor }
        impl_binary_parser! { parse_bitor, parse_bitxor, SeperatorCategory::BitOr }
        impl_binary_parser! { parse_equality, parse_bitor, SeperatorCategory::Equality }
        impl_binary_parser! { parse_logical_and, parse_equality, SeperatorCategory::LogicalAnd }
        impl_binary_parser! { parse_logical_or, parse_logical_and, SeperatorCategory::LogicalOr }
        
        match parse_logical_or(sess) { _ => unreachable!(), Expr::Binary(binary) => binary }
    }
}

#[cfg(test)] #[test]
fn binary_expr_parse() {

    assert_eq!{ 
    }
}