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

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct LitExpr {
    pub value: LitValue,
    pub span: Span,
}
impl From<LitExpr> for Expr {
    fn from(lit_expr: LitExpr) -> Expr { Expr::Lit(lit_expr) }
}
impl LitExpr {
    pub fn new(value: LitValue, span: Span) -> LitExpr { LitExpr{ value, span } }
}

impl Node for LitExpr {
    type ParseOutput = Expr;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Char(_) | Token::Bool(_) | Token::Str(..) | Token::Num(_)) 
    }

    fn parse(cx: &mut ParseContext) -> ParseResult<Expr> {
        
        let (lit, lit_span) = cx.expect_lit()?;
        Ok(Expr::Lit(LitExpr::new(lit, lit_span)))
    }

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_lit_expr(self)
    }
}

#[cfg(test)]
macro_rules! make_lit {
    (unit, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Unit, span: Span::new($start, $end) });
    (true, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Bool(true), span: Span::new($start, $end) });
    (false, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Bool(false), span: Span::new($start, $end) });
    ($v:literal: char, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Char($v), span: Span::new($start, $end) });
    ($v:literal: str, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Str(IsId::new($v)), span: Span::new($start, $end) });
    // only i32 can omit type
    ($v:literal, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Num(Numeric::I32($v)), span: Span::new($start, $end) });
    ($v:literal: u8, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Num(Numeric::U8($v)), span: Span::new($start, $end) });
    ($v:literal: u32, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Num(Numeric::U32($v)), span: Span::new($start, $end) });
    ($v:literal: u64, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Num(Numeric::U64($v)), span: Span::new($start, $end) });
    ($v:literal: r32, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Num(Numeric::R32($v)), span: Span::new($start, $end) });
    ($v:literal: r64, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Num(Numeric::R64($v)), span: Span::new($start, $end) });
}
#[cfg(test)]
pub(crate) use make_lit;
