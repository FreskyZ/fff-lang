///! fff-lang
///!
///! syntax/unary_expr
///! unary_expr = { unary_operator } postfix_expr

use super::prelude::*;
use super::{PostfixExpr};

impl Parser for UnaryExpr {
    type Output = Expr;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Sep(sep) if sep.kind(SeparatorKind::Unary))
    }

    fn parse(cx: &mut ParseContext) -> Result<Expr, Unexpected> {
        
        let mut op_spans = Vec::new();
        loop {
            match cx.try_expect_sep_kind(SeparatorKind::Unary) {
                Some((sep, sep_span)) => op_spans.push((sep, sep_span)),
                None => {
                    let base = cx.expect::<PostfixExpr>()?;
                    return Ok(op_spans.into_iter().rev().fold(base, |base, (op, span)| { 
                        Expr::Unary(UnaryExpr{ all_span: span + base.get_all_span(), base: Box::new(base), operator: op, operator_span: span }) 
                    }));
                }
            }
        }
    }
}

impl Node for UnaryExpr {

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_unary_expr(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(self.base.as_ref())
    }
}

#[cfg(test)] #[test]
fn unary_expr_parse() {
    
    case!{ "1" as Expr, 
        make_expr!(i32 1 0:0)
    }

    case!{ "!~!1" as UnaryExpr,
        make_expr!(unary 0:3 Not 0:0
            make_expr!(unary 1:3 Tilde 1:1
                make_expr!(unary 2:3 Not 2:2
                    make_expr!(i32 1 3:3))))
    }

    case!{ "&a(&b)" as UnaryExpr,
        make_expr!(unary 0:5 And 0:0
            make_expr!(fn 1:5 paren 2:5
                make_name!(simple 1:1 #2),
                make_expr!(unary 3:4 And 3:3
                    make_name!(simple 4:4 #3))))
    }
}
