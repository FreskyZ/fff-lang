///! fff-lang
///!
///! syntax/lit_expr
///! literal_expr = literal

use std::fmt; 
use crate::codemap::Span;
use crate::lexical::Token;
use crate::lexical::LitValue;
use crate::lexical::StrLitValue;
use super::Expr;
use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxParse;
use super::super::ISyntaxFormat;
use super::super::ISyntaxGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct LitExpr {
    pub value: LitValue, // FIXME: use syntax::LitValue instead of lexical::LitValue becasue they are different when format string
    pub span: Span,
}
impl ISyntaxFormat for LitExpr {
    fn format(&self, f: Formatter) -> String {
        let f = f.indent().header_text_or("literal").space();
        let f = match self.value { LitValue::Str(Some(StrLitValue::Simple(ref id))) => f.sym(*id), ref other => f.debug(other) };
        f.space().span(self.span).finish()
    }
}
impl fmt::Debug for LitExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl From<LitExpr> for Expr {
    fn from(lit_expr: LitExpr) -> Expr { Expr::Lit(lit_expr) }
}
impl LitExpr {
    pub fn new(value: LitValue, span: Span) -> LitExpr { LitExpr{ value, span } }
}
impl ISyntaxGrammar for LitExpr {
    fn matches_first(tokens: &[&Token]) -> bool { if let &Token::Lit(_) = tokens[0] { true } else { false } }
}
impl ISyntaxParse for LitExpr {
    type Output = Expr;

    fn parse(sess: &mut ParseSession) -> ParseResult<Expr> {
        
        let (lit, lit_span) = sess.expect_lit()?;
        Ok(Expr::Lit(LitExpr::new(lit, lit_span)))
    }
}