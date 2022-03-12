///! fff-lang
///!
///! syntax/range_expr
///! range_full = '..'
///! range_left = binary_expr '..'
///! range_right = '..' binary_expr
///! range_both = binary_expr '..' binary_expr

use crate::syntax::prelude::*;
use super::{Expr, BinaryExpr};

// RangeFull
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct RangeFullExpr {
    pub all_span: Span,
}
impl ISyntaxFormat for RangeFullExpr {
    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("range-full").space().span(self.all_span).finish()
    }
}
impl fmt::Debug for RangeFullExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl From<RangeFullExpr> for Expr {
    fn from(range_expr: RangeFullExpr) -> Expr { Expr::RangeFull(range_expr) }
}
impl RangeFullExpr {
    pub fn new(all_span: Span) -> RangeFullExpr { RangeFullExpr{ all_span } }
}

// RangeRight
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct RangeRightExpr {
    pub all_span: Span,  // all_span.slice(2) is range_op_span
    pub expr: Box<Expr>,
}
impl ISyntaxFormat for RangeRightExpr {
    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("range-right").space().span(self.all_span).endl()
            .apply1(self.expr.as_ref())
            .finish()
    }
}
impl fmt::Debug for RangeRightExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl From<RangeRightExpr> for Expr {
    fn from(range_expr: RangeRightExpr) -> Expr { Expr::RangeRight(range_expr) }
}
impl RangeRightExpr {
    pub fn new<T: Into<Expr>>(all_span: Span, expr: T) -> RangeRightExpr { 
        RangeRightExpr{ all_span, expr: Box::new(expr.into()) }
    }
}

// RangeLeft
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct RangeLeftExpr {
    pub expr: Box<Expr>,
    pub all_span: Span, // all_span.slice(-2, 0) should get range_op_span
}
impl ISyntaxFormat for RangeLeftExpr {
    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("range-left").space().span(self.all_span).endl()
            .apply1(self.expr.as_ref())
            .finish()
    }
}
impl fmt::Debug for RangeLeftExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl From<RangeLeftExpr> for Expr {
    fn from(range_expr: RangeLeftExpr) -> Expr { Expr::RangeLeft(range_expr) }
}
impl RangeLeftExpr {
    pub fn new<T: Into<Expr>>(all_span: Span, expr: T) -> RangeLeftExpr {
        RangeLeftExpr{ all_span, expr: Box::new(expr.into()) }
    }
}

// RangeBoth
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct RangeBothExpr {
    pub left_expr: Box<Expr>,
    pub op_span: Span,
    pub right_expr: Box<Expr>,
    pub all_span: Span,
}
impl ISyntaxFormat for RangeBothExpr {
    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("range-both").space().span(self.all_span).endl()
            .set_prefix_text("left-is").apply1(self.left_expr.as_ref()).unset_prefix_text().endl()
            .indent1().lit("\"..\"").space().span(self.op_span).endl()
            .set_prefix_text("right-is").apply1(self.right_expr.as_ref())
            .finish()
    }
}
impl fmt::Debug for RangeBothExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl From<RangeBothExpr> for Expr {
    fn from(range_expr: RangeBothExpr) -> Expr { Expr::RangeBoth(range_expr) }
}
impl RangeBothExpr {
    pub fn new<T1: Into<Expr>, T2: Into<Expr>>(left_expr: T1, op_span: Span, right_expr: T2) -> RangeBothExpr {
        let (left_expr, right_expr) = (left_expr.into(), right_expr.into());
        RangeBothExpr{
            op_span,
            all_span: left_expr.get_all_span() + right_expr.get_all_span(),
            left_expr: Box::new(left_expr), right_expr: Box::new(right_expr),
        }
    }
}

// actually also a priority proxy
pub(super) struct RangeExpr;
impl<'ecx, 'scx, F> ISyntaxParse<'ecx, 'scx, F> for RangeExpr where F: FileSystem {
    type Output = Expr;

    fn parse(sess: &mut ParseSession<'ecx, 'scx, F>) -> ParseResult<Expr> {
        match sess.try_expect_sep(Separator::DotDot) {
            Some(range_op_span) => {
                if Expr::matches_first(sess.current_tokens()) {
                    let expr = BinaryExpr::parse(sess)?;
                    Ok(Expr::RangeRight(RangeRightExpr::new(range_op_span + expr.get_all_span(), expr)))
                } else {
                    Ok(Expr::RangeFull(RangeFullExpr::new(range_op_span)))
                }
            }
            None => {
                let left_expr = BinaryExpr::parse(sess)?;
                if let Some(op_span) = sess.try_expect_sep(Separator::DotDot) {
                    if Expr::matches_first(sess.current_tokens()) {
                        let right_expr = BinaryExpr::parse(sess)?;
                        Ok(Expr::RangeBoth(RangeBothExpr::new(left_expr, op_span, right_expr)))
                    } else {
                        Ok(Expr::RangeLeft(RangeLeftExpr::new(left_expr.get_all_span() + op_span, left_expr)))
                    }
                } else {
                    Ok(left_expr)
                }
            }
        }
    }
}

#[cfg(test)] #[test]
fn range_expr_parse() {
    use super::super::make_node;
    use super::{LitExpr, LitValue};

    assert_eq!{ make_node!(".." as RangeExpr), Expr::RangeFull(RangeFullExpr::new(Span::new(0, 1))) }

    assert_eq!{ make_node!("..1 + 1" as RangeExpr), 
        Expr::RangeRight(RangeRightExpr::new(Span::new(0, 6), BinaryExpr::new(
            LitExpr::new(LitValue::from(1i32), Span::new(2, 2)),
            Separator::Add, Span::new(4, 4),
            LitExpr::new(LitValue::from(1i32), Span::new(6, 6))
        )))
    }

    assert_eq!{ make_node!("1 .." as RangeExpr),
        Expr::RangeLeft(RangeLeftExpr::new(Span::new(0, 3), LitExpr::new(LitValue::from(1i32), Span::new(0, 0))))
    }
}