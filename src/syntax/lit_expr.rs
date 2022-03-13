///! fff-lang
///!
///! syntax/lit_expr
///! literal_expr = literal

use super::prelude::*;
use super::Expr;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub enum LitValue {
    Unit,
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

#[cfg_attr(test, derive(PartialEq))]
pub struct LitExpr {
    pub value: LitValue,
    pub span: Span,
}
impl ISyntaxFormat for LitExpr {
    fn format(&self, f: Formatter) -> String {
        let f = f.indent().header_text_or("literal").space();
        // TODO: isyntaxformat not implemented for syntax::LitValue
        f.debug(&self.value).space().span(self.span).finish()
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
impl Node for LitExpr {
    type ParseOutput = Expr;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Char(_) | Token::Bool(_) | Token::Str(..) | Token::Num(_)) 
    }

    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<Expr> {
        
        let (lit, lit_span) = sess.expect_lit()?;
        Ok(Expr::Lit(LitExpr::new(lit, lit_span)))
    }
}
