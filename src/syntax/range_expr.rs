///! fff-lang
///!
///! syntax/range_expr
///! range_full = '..'
///! range_left = binary_expr '..'
///! range_right = '..' binary_expr
///! range_both = binary_expr '..' binary_expr

use super::prelude::*;
use super::{Expr, BinaryExpr};

// RangeFull
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct RangeFullExpr {
    pub all_span: Span,
}

impl RangeFullExpr {
    pub fn new(all_span: Span) -> RangeFullExpr { RangeFullExpr{ all_span } }
}

impl Node for RangeFullExpr {
    type ParseOutput = Expr;
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_range_full_expr(self)
    }
}

// RangeRight
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct RangeRightExpr {
    pub all_span: Span,  // all_span.slice(2) is range_op_span
    pub expr: Box<Expr>,
}

impl RangeRightExpr {
    pub fn new<T: Into<Expr>>(all_span: Span, expr: T) -> RangeRightExpr { 
        RangeRightExpr{ all_span, expr: Box::new(expr.into()) }
    }
}

impl Node for RangeRightExpr {
    type ParseOutput = Expr;
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_range_right_expr(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(self.expr.as_ref())
    }
}

// RangeLeft
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct RangeLeftExpr {
    pub expr: Box<Expr>,
    pub all_span: Span, // all_span.slice(-2, 0) should get range_op_span
}

impl RangeLeftExpr {
    pub fn new<T: Into<Expr>>(all_span: Span, expr: T) -> RangeLeftExpr {
        RangeLeftExpr{ all_span, expr: Box::new(expr.into()) }
    }
}

impl Node for RangeLeftExpr {
    type ParseOutput = Expr;
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_range_left_expr(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(self.expr.as_ref())
    }
}

// RangeBoth
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct RangeBothExpr {
    pub left_expr: Box<Expr>,
    pub op_span: Span,
    pub right_expr: Box<Expr>,
    pub all_span: Span,
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

impl Node for RangeBothExpr {
    type ParseOutput = Expr;
    fn parse(cx: &mut ParseContext) -> ParseResult<Expr> {
        cx.expect_node::<RangeExpr>()
    }
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_range_both_expr(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(self.left_expr.as_ref())?;
        v.visit_expr(self.right_expr.as_ref())
    }
}

// actually also a priority proxy
pub struct RangeExpr;
impl Node for RangeExpr {
    type ParseOutput = Expr;

    fn parse(cx: &mut ParseContext) -> ParseResult<Expr> {
        match cx.try_expect_sep(Separator::DotDot) {
            Some(range_op_span) => {
                if cx.matches::<Expr>() {
                    let expr = cx.expect_node::<BinaryExpr>()?;
                    Ok(Expr::RangeRight(RangeRightExpr::new(range_op_span + expr.get_all_span(), expr)))
                } else {
                    Ok(Expr::RangeFull(RangeFullExpr::new(range_op_span)))
                }
            }
            None => {
                let left_expr = cx.expect_node::<BinaryExpr>()?;
                if let Some(op_span) = cx.try_expect_sep(Separator::DotDot) {
                    if cx.matches::<Expr>() {
                        let right_expr = cx.expect_node::<BinaryExpr>()?;
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

    case!{ ".." as RangeExpr, Expr::RangeFull(RangeFullExpr::new(Span::new(0, 1))) }

    case!{ "..1 + 1" as RangeExpr, 
        Expr::RangeRight(RangeRightExpr::new(Span::new(0, 6), BinaryExpr::new(
            make_lit!(1, 2, 2),
            Separator::Add, Span::new(4, 4),
            make_lit!(1, 6, 6)
        )))
    }

    case!{ "1 .." as RangeExpr,
        Expr::RangeLeft(RangeLeftExpr::new(Span::new(0, 3), make_lit!(1, 0, 0)))
    }
}
