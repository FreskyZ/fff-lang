///! fff-lang
///! 
///! syntax/member_access
///! member_access = expr '.' identifier

use super::prelude::*;
use super::{Expr, SimpleName};

#[cfg_attr(test, derive(PartialEq))]
pub struct MemberAccessExpr {
    pub base: Box<Expr>,
    pub dot_span: Span,
    pub name: SimpleName,
    pub all_span: Span,
}
impl ISyntaxFormat for MemberAccessExpr {
    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("member-access").space().span(self.all_span).endl()
            .set_prefix_text("base-is").apply1(self.base.as_ref()).unset_prefix_text().endl()
            .indent1().lit("\".\"").space().span(self.dot_span).endl()
            .set_header_text("member-name-is").apply1(&self.name)
            .finish()
    }
}
impl fmt::Debug for MemberAccessExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl From<MemberAccessExpr> for Expr {
    fn from(member_access_expr: MemberAccessExpr) -> Expr { Expr::MemberAccess(member_access_expr) }
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
    // but this parser only accept sess.tk after the first expr and return the structure without base and all_span set
    // the postfix expr dispatcher is responsible for fullfilling the missing part
    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<MemberAccessExpr> {
        
        let dot_span = sess.expect_sep(Separator::Dot)?;
        let name = SimpleName::parse(sess)?;
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
