///! fff-lang
///!
///! syntax/lit_expr
///! literal_expr = literal

use std::fmt; 

use codemap::Span;
use lexical::Token;
use lexical::LitValue;

use super::Expr;

use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct LitExpr {
    pub value: LitValue,
    pub span: Span,
}
impl ISyntaxFormat for LitExpr {
    fn format(&self, f: Formatter) -> String {
        let f = f.indent().header_text_or("literal").space();
        let f = match self.value { LitValue::Str(Some(ref id)) => f.sym(*id), ref other => f.debug(other) };
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
impl ISyntaxItemGrammar for LitExpr {
    fn is_first_final(sess: &ParseSession) -> bool { if let &Token::Lit(_) = sess.current_tokens()[0] { true } else { false } }
}
impl ISyntaxItemParse for LitExpr {
    type Target = Expr;

    fn parse(sess: &mut ParseSession) -> ParseResult<Expr> {
        
        let (lit, lit_span) = sess.expect_lit()?;
        Ok(Expr::Lit(LitExpr::new(lit, lit_span)))
    }
}