///! fff-lang
///! 
///! syntax/member_access
///! member_access = expr '.' identifier

use super::prelude::*;
use super::{Expr, SimpleName};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct MemberAccessExpr {
    pub base: Box<Expr>,
    pub dot_span: Span,
    pub name: SimpleName,
    pub all_span: Span,
}

impl MemberAccessExpr {
    pub fn new<T: Into<Expr>>(base: T, dot_span: Span, name: SimpleName) -> MemberAccessExpr {
        let base = base.into();
        MemberAccessExpr{
            all_span: base.get_all_span() + name.span,
            base: Box::new(base),
            dot_span, name
        }
    }

    fn new_by_parse_result(dot_span: Span, name: SimpleName) -> MemberAccessExpr {
        MemberAccessExpr{
            all_span: Span::new(0, 0),
            base: Box::new(Expr::default()),
            dot_span, name
        }
    }
}

impl Node for MemberAccessExpr {
    type ParseOutput = MemberAccessExpr;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Sep(Separator::Dot)) 
    }

    // these 3 postfix exprs are kind of different because
    // although their structure contains their base expr (which actually is primary expr)
    // but this parser only accept cx.tk after the first expr and return the structure without base and all_span set
    // the postfix expr dispatcher is responsible for fullfilling the missing part
    fn parse(cx: &mut ParseContext) -> ParseResult<MemberAccessExpr> {
        
        let dot_span = cx.expect_sep(Separator::Dot)?;
        let name = cx.expect_node::<SimpleName>()?;
        Ok(MemberAccessExpr::new_by_parse_result(dot_span, name))
    }

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_member_access(self)
    }

    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(self.base.as_ref())?;
        v.visit_simple_name(&self.name)
    }
}
