///! fff-lang
///! 
///! syntax/member_access
///! member_access = expr '.' identifier

use std::fmt;
use crate::source::Span;
use crate::lexical::Token;
use crate::lexical::Seperator;
use super::Expr;
use super::SimpleName;
use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxParse;
use super::super::ISyntaxFormat;
use super::super::ISyntaxGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
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
            all_span: base.get_all_span().merge(&name.span),
            base: Box::new(base),
            dot_span, name
        }
    }

    fn new_by_parse_result(dot_span: Span, name: SimpleName) -> MemberAccessExpr {
        MemberAccessExpr{
            all_span: Span::default(),
            base: Box::new(Expr::default()),
            dot_span, name
        }
    }
}
impl ISyntaxGrammar for MemberAccessExpr {
    fn matches_first(tokens: &[&Token]) -> bool { tokens[0] == &Token::Sep(Seperator::Dot) }
}
impl ISyntaxParse for MemberAccessExpr {
    type Output = MemberAccessExpr;

    // these 3 postfix exprs are kind of different because
    // although their structure contains their base expr (which actually is primary expr)
    // but this parser only accept sess.tk after the first expr and return the structure without base and all_span set
    // the postfix expr dispatcher is responsible for fullfilling the missing part
    fn parse(sess: &mut ParseSession) -> ParseResult<MemberAccessExpr> {
        
        let dot_span = sess.expect_sep(Seperator::Dot)?;
        let name = SimpleName::parse(sess)?;
        Ok(MemberAccessExpr::new_by_parse_result(dot_span, name))
    }
}