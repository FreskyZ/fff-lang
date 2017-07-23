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
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct LitExpr {
    pub value: LitValue,
    pub span: Span,
}
impl ISyntaxItemFormat for LitExpr {
    fn format(&self, f: Formatter) -> String {
        format!("{}Literal {:?} <{}>", f.indent(), self.value, f.span(self.span))
    }
}
impl fmt::Debug for LitExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::default())) }
}
impl From<LitExpr> for Expr {
    fn from(lit_expr: LitExpr) -> Expr { Expr::Lit(lit_expr) }
}
impl LitExpr {
    pub fn new(value: LitValue, span: Span) -> LitExpr { LitExpr{ value, span } }
}
impl ISyntaxItemGrammar for LitExpr {
    fn is_first_final(sess: &ParseSession) -> bool { if let &Token::Lit(_) = sess.tk { true } else { false } }
}
impl ISyntaxItemParse for LitExpr {
    type Target = Expr;

    fn parse(sess: &mut ParseSession) -> ParseResult<Expr> {
        
        if let (&Token::Lit(ref lit_val), ref lit_val_span) = (sess.tk, sess.pos) {
            sess.move_next();
            Ok(Expr::Lit(LitExpr::new(*lit_val, *lit_val_span)))
        } else {
            sess.push_unexpect("literal")
        }
    }
}