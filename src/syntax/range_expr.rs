///! fff-lang
///!
///! syntax/range_expr
///! range_full = '..'
///! range_left = binary_expr '..'
///! range_right = '..' binary_expr
///! range_both = binary_expr '..' binary_expr

use super::prelude::*;

impl Node for RangeFullExpr {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_range_full_expr(self)
    }
}


impl Node for RangeRightExpr {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_range_right_expr(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(self.expr.as_ref())
    }
}


impl Node for RangeLeftExpr {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_range_left_expr(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(self.expr.as_ref())
    }
}

impl Node for RangeBothExpr {
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
impl Parser for RangeExpr {
    type Output = Expr;

    fn parse(cx: &mut ParseContext) -> Result<Expr, Unexpected> {
        match cx.try_expect_sep(Separator::DotDot) {
            Some(range_op_span) => {
                if cx.matches::<Expr>() {
                    let expr = cx.expect::<BinaryExpr>()?;
                    Ok(Expr::RangeRight(RangeRightExpr{ all_span: range_op_span + expr.get_all_span(), expr: Box::new(expr) }))
                } else {
                    Ok(Expr::RangeFull(RangeFullExpr{ all_span: range_op_span }))
                }
            }
            None => {
                let left_expr = cx.expect::<BinaryExpr>()?;
                if let Some(op_span) = cx.try_expect_sep(Separator::DotDot) {
                    if cx.matches::<Expr>() {
                        let right_expr = cx.expect::<BinaryExpr>()?;
                        let all_span = left_expr.get_all_span() + right_expr.get_all_span();
                        Ok(Expr::RangeBoth(RangeBothExpr{ all_span, op_span, left_expr: Box::new(left_expr), right_expr: Box::new(right_expr) }))
                    } else {
                        Ok(Expr::RangeLeft(RangeLeftExpr{ all_span: left_expr.get_all_span() + op_span, expr: Box::new(left_expr) }))
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

    case!{ ".." as RangeExpr, 
        make_expr!(range full 0:1)
    }

    case!{ "..1 + 1" as RangeExpr,
        make_expr!(range right 0:6
            make_expr!(binary 2:6 Add 4:4
                make_expr!(i32 1 2:2),
                make_expr!(i32 1 6:6)))
    }

    case!{ "1 .." as RangeExpr,
        make_expr!(range left 0:3
            make_expr!(i32 1 0:0))
    }
}
