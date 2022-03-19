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

impl LitExpr {
    pub fn new(value: LitValue, span: Span) -> LitExpr { 
        LitExpr{ value, span } 
    }
}

impl Parser for LitExpr {
    type Output = Expr;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Char(_) | Token::Bool(_) | Token::Str(..) | Token::Num(_)) 
    }

    fn parse(cx: &mut ParseContext) -> Result<Expr, Unexpected> {
        
        let (lit, lit_span) = cx.expect_lit()?;
        Ok(Expr::Lit(LitExpr::new(lit, lit_span)))
    }
}

impl Node for LitExpr {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_lit_expr(self)
    }
}
