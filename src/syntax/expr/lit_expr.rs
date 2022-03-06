///! fff-lang
///!
///! syntax/lit_expr
///! literal_expr = literal

use std::fmt; 
use crate::source::{Span, IsId};
use crate::lexical::{Token, Numeric};
use super::Expr;
use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxParse;
use super::super::ISyntaxFormat;
use super::super::ISyntaxGrammar;


#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub enum LitValue {
    Bool(bool),
    Char(char),
    Str(IsId),
    Num(Numeric),
}

impl From<bool> for LitValue {
    fn from(v: bool) -> Self {
        Self::Bool(v)
    }
}
impl From<char> for LitValue {
    fn from(v: char) -> Self {
        Self::Char(v)
    }
}

// specially include u32 for string id for test
impl From<u32> for LitValue {
    fn from(v: u32) -> Self {
        Self::Str(IsId::new(v))
    }
}
// specially i32 for most case in test
impl From<i32> for LitValue {
    fn from(v: i32) -> Self {
        Self::Num(Numeric::I32(v))
    }
}

// TODO: remove this when all Eq removed from cfg_attr(test)
#[cfg(test)]
impl std::cmp::Eq for LitValue {
}

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct LitExpr {
    pub value: LitValue,
    pub span: Span,
}
impl ISyntaxFormat for LitExpr {
    fn format(&self, f: Formatter) -> String {
        let f = f.indent().header_text_or("literal").space();
        // TODO: isyntaxformat not implemented for syntax::LitValue
        f.debug(&self.value);
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
    pub fn new(value: impl Into<LitValue>, span: Span) -> LitExpr { LitExpr{ value: value.into(), span } }
}
impl ISyntaxGrammar for LitExpr {
    fn matches_first(tokens: &[&Token]) -> bool { if let Token::Char(_) | Token::Bool(_) | Token::Str(..) | Token::Num(_) = tokens[0] { true } else { false } }
}
impl ISyntaxParse for LitExpr {
    type Output = Expr;

    fn parse(sess: &mut ParseSession) -> ParseResult<Expr> {
        
        let (lit, lit_span) = sess.expect_lit()?;
        Ok(Expr::Lit(LitExpr::new(lit, lit_span)))
    }
}
