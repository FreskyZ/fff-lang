///! fff-lang
///! 
///! syntax/member_access
///! member_access = expr '.' identifier

use super::prelude::*;
use super::{Expr};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct MemberAccessExpr {
    pub base: Box<Expr>,
    pub dot_span: Span,
    pub name: IsId,
    pub name_span: Span,
    pub all_span: Span,
}

impl Parser for MemberAccessExpr {
    type Output = Self;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Sep(Separator::Dot)) 
    }

    // these 3 postfix exprs are special because
    // their node contains base expr, but their parser only expects token after that (dot for member access expr)
    // the postfix expr dispatcher is responsible for fullfilling the missing part
    fn parse(cx: &mut ParseContext) -> Result<MemberAccessExpr, Unexpected> {
        
        let dot_span = cx.expect_sep(Separator::Dot)?;
        let (name, name_span) = cx.expect_ident()?;
        Ok(MemberAccessExpr{ base: Box::new(Expr::default()), dot_span, name, name_span, all_span: Span::new(0, 0) })
    }
}

impl Node for MemberAccessExpr {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_member_access(self)
    }

    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(self.base.as_ref())
    }
}
