///! fff-lang
///!
///! syntax/ident_expr
///! ident_expr = identifier

use std::fmt; 

use codemap::Span;
use codemap::SymbolID;
use lexical::Token;

use super::PrimaryExpr;

use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct IdentExpr {
    pub value: SymbolID,
    pub span: Span,
}
impl ISyntaxItemFormat for IdentExpr {
    fn format(&self, indent: u32) -> String {
        format!("{}Ident {:?} <{:?}>", IdentExpr::indent_str(indent), self.value, self.span)
    }
}
impl fmt::Debug for IdentExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl IdentExpr {
    pub fn new(value: SymbolID, span: Span) -> IdentExpr { IdentExpr{ value, span } }
}
impl ISyntaxItemGrammar for IdentExpr {
    fn is_first_final(sess: &ParseSession) -> bool { if let &Token::Ident(_) = sess.tk { true } else { false } }
}
impl ISyntaxItemParse for IdentExpr {
    type Target = PrimaryExpr;

    fn parse(sess: &mut ParseSession) -> ParseResult<PrimaryExpr> {
        
        if let (&Token::Ident(ref sid), ref ident_span) = (sess.tk, sess.pos) {
            sess.move_next();
            Ok(PrimaryExpr::Ident(IdentExpr::new(*sid, *ident_span)))
        } else {
            sess.push_unexpect("identifier")
        }
    }
}