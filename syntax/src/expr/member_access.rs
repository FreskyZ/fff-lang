///! fff-lang
///! 
///! syntax/member_access
///! member_access = expr '.' identifier

use std::fmt;

use codemap::Span;
use lexical::Token;
use lexical::SeperatorKind;

use super::Expr;
use super::IdentExpr;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct MemberAccessExpr {
    pub base: Box<Expr>,
    pub dot_span: Span,
    pub name: IdentExpr,
    pub all_span: Span,
}
impl ISyntaxItemFormat for MemberAccessExpr {
    fn format(&self, indent: u32) -> String {
        format!("{}MemberAccess <{:?}>\n{}\n{}dot <{:?}>\n{}", 
            MemberAccessExpr::indent_str(indent), self.all_span,
            self.base.as_ref().format(indent + 1),
            MemberAccessExpr::indent_str(indent + 1), self.dot_span,
            self.name.format(indent + 1)
        )
    }
}
impl fmt::Debug for MemberAccessExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(0)) }
}
impl From<MemberAccessExpr> for Expr {
    fn from(member_access_expr: MemberAccessExpr) -> Expr { Expr::MemberAccess(member_access_expr) }
}
impl MemberAccessExpr {
    pub fn new<T: Into<Expr>>(base: T, dot_span: Span, name: IdentExpr) -> MemberAccessExpr {
        let base = base.into();
        MemberAccessExpr{
            all_span: base.get_all_span().merge(&name.span),
            base: Box::new(base),
            dot_span, name
        }
    }

    fn new_by_parse_result(dot_span: Span, name: IdentExpr) -> MemberAccessExpr {
        MemberAccessExpr{
            all_span: Span::default(),
            base: Box::new(Expr::default()),
            dot_span, name
        }
    }
}
impl ISyntaxItemGrammar for MemberAccessExpr {
    fn is_first_final(sess: &ParseSession) -> bool { sess.tk == &Token::Sep(SeperatorKind::Dot) }
}
impl ISyntaxItemParse for MemberAccessExpr {
    type Target = MemberAccessExpr;

    // these 3 postfix exprs are kind of different because
    // although their structure contains their base expr (which actually is primary expr)
    // but this parser only accept sess.tk after the first expr and return the structure without base and all_span set
    // the postfix expr dispatcher is responsible for fullfilling the missing part
    fn parse(sess: &mut ParseSession) -> ParseResult<MemberAccessExpr> {
        
        let dot_span = sess.expect_sep(SeperatorKind::Dot)?;
        let name = IdentExpr::parse(sess)?;
        Ok(MemberAccessExpr::new_by_parse_result(dot_span, name))
    }
}