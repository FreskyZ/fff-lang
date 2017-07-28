///! fff-lang
///!
///! syntax/range_expr
///! range_expr = [ binary_expr ] '..' [ binary_expr ]

use std::fmt;

use codemap::Span;
use lexical::Token;
use lexical::Seperator;

use super::Expr;
use super::BinaryExpr;

use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxParse;
use super::super::ISyntaxFormat;
use super::super::ISyntaxGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct RangeFullExpr {
    pub all_span: Span,
}
impl ISyntaxFormat for RangeFullExpr {
    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("RangeFull").space().span(self.all_span).finish()
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

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct RangeRightExpr {
    pub all_span: Span,  // all_span.slice(2) is range_op_span
    pub expr: Box<Expr>,
}
impl ISyntaxFormat for RangeRightExpr {
    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("RangeRight").space().span(self.all_span).endl()
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

impl ISyntaxGrammar for RangeFullExpr {
    fn matches_first(tokens: &[&Token]) -> bool { tokens[0] == &Token::Sep(Seperator::Range) }
}
impl ISyntaxParse for RangeFullExpr {
    type Output = Expr;

    fn parse(sess: &mut ParseSession) -> ParseResult<Expr> {
        let span = sess.expect_sep(Seperator::Range)?;
        if Expr::matches_first(sess.current_tokens()) {
            let expr = BinaryExpr::parse(sess)?;
            return Ok(Expr::RangeRight(RangeRightExpr::new(span.merge(&expr.get_all_span()), expr)));
        }
        return Ok(Expr::RangeFull(RangeFullExpr::new(span)));
    }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct RangeLeftExpr {
    pub expr: Box<Expr>,
    pub all_span: Span, // all_span.slice(-2, 0) should get range_op_span
}
impl ISyntaxFormat for RangeLeftExpr {
    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("RangeLeft").space().span(self.all_span).endl()
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
            all_span: left_expr.get_all_span().merge(&right_expr.get_all_span()),
            left_expr: Box::new(left_expr), right_expr: Box::new(right_expr),
        }
    }
}

impl ISyntaxParse for RangeLeftExpr {
    type Output = Expr;
    
    fn parse(sess: &mut ParseSession) -> ParseResult<Expr> {
        let left_expr = BinaryExpr::parse(sess)?;
        if let Some(op_span) = sess.try_expect_sep(Seperator::Range) {
            if Expr::matches_first(sess.current_tokens()) {
                let right_expr = BinaryExpr::parse(sess)?;
                return Ok(Expr::RangeBoth(RangeBothExpr::new(left_expr, op_span, right_expr)));
            } else {
                return Ok(Expr::RangeLeft(RangeLeftExpr::new(left_expr.get_all_span().merge(&op_span), left_expr)));
            }
        } else {
            return Ok(left_expr);
        }
    }
}

#[cfg(test)] #[test]
fn range_expr_parse() {
    use lexical::LitValue;
    use super::LitExpr;
    use super::super::WithTestInput;

    assert_eq!{ RangeFullExpr::with_test_str(".."), Expr::RangeFull(RangeFullExpr::new(make_span!(0, 1))) }

    assert_eq!{ RangeFullExpr::with_test_str("..1 + 1"), 
        Expr::RangeRight(RangeRightExpr::new(make_span!(0, 6), BinaryExpr::new(
            LitExpr::new(LitValue::from(1), make_span!(2, 2)),
            Seperator::Add, make_span!(4, 4),
            LitExpr::new(LitValue::from(1), make_span!(6, 6))
        )))
    }
}