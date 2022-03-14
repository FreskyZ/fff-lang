///! fff-lang
///!
///! syntax/unary_expr
///! unary_expr = { unary_operator } postfix_expr

use super::prelude::*;
use super::{Expr, PostfixExpr};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct UnaryExpr {
    pub base: Box<Expr>, 
    pub operator: Separator, 
    pub operator_span: Span,
    pub all_span: Span,
}
impl From<UnaryExpr> for Expr {
    fn from(unary_expr: UnaryExpr) -> Expr { Expr::Unary(unary_expr) }
}
impl UnaryExpr {

    pub fn new<T: Into<Expr>>(operator: Separator, operator_span: Span, base: T) -> UnaryExpr {
        let base = base.into();
        UnaryExpr{
            all_span: operator_span + base.get_all_span(),
            base: Box::new(base),
            operator, operator_span,
        }
    }
}

impl Node for UnaryExpr {
    type ParseOutput = Expr;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Sep(sep) if sep.kind(SeparatorKind::Unary))
    }

    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<Expr> {
        
        let mut op_spans = Vec::new();
        loop {
            match sess.try_expect_sep_kind(SeparatorKind::Unary) {
                Some((sep, sep_span)) => op_spans.push((sep, sep_span)),
                None => {
                    let base = PostfixExpr::parse(sess)?;
                    return Ok(op_spans.into_iter().rev().fold(base, |base, (op, span)| { Expr::Unary(UnaryExpr::new(op, span, base)) }));
                }
            }
        }
    }

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_unary_expr(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(self.base.as_ref())
    }
}

#[cfg(test)] #[test]
fn unary_expr_parse() {
    use super::{make_node, make_lit};
    
    assert_eq!{ make_node!("1" as Expr), 
        Expr::Lit(make_lit!(1, 0, 0)) 
    }

    assert_eq!{ make_node!("!~!1" as UnaryExpr),
        Expr::Unary(UnaryExpr::new(
            Separator::Not, Span::new(0, 0),
            Expr::Unary(UnaryExpr::new(
                Separator::Tilde, Span::new(1, 1),            
                Expr::Unary(UnaryExpr::new(
                    Separator::Not, Span::new(2, 2),
                    Expr::Lit(make_lit!(1, 3, 3)),
                ))
            ))
        ))
    }
}