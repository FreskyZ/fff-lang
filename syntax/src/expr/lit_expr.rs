///! fff-lang
///!
///! syntax/lit_expr
///! literal_expr = literal

use std::fmt; 

use codemap::Span;
use lexical::Token;
use lexical::LitValue;

use super::PrimaryExpr;

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
    fn format(&self, indent: u32) -> String {
        format!("{}Literal {:?} <{:?}>", LitExpr::indent_str(indent), self.value, self.span)
    }
}
impl fmt::Debug for LitExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl LitExpr {
    pub fn new(value: LitValue, span: Span) -> LitExpr { LitExpr{ value, span } }
}
impl ISyntaxItemGrammar for LitExpr {
    fn is_first_final(sess: &ParseSession) -> bool { if let &Token::Lit(_) = sess.tk { true } else { false } }
}
impl ISyntaxItemParse for LitExpr {
    type Target = PrimaryExpr;

    fn parse(sess: &mut ParseSession) -> ParseResult<PrimaryExpr> {
        
        if let (&Token::Lit(ref lit_val), ref lit_val_span) = (sess.tk, sess.pos) {
            sess.move_next();
            Ok(PrimaryExpr::Lit(LitExpr::new(*lit_val, *lit_val_span)))
        } else {
            sess.push_unexpect("literal")
        }
    }
}