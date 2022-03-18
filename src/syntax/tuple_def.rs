///! fff-lang
///! 
///! syntax/tuple_def, paren_expr
///! tuple_def = '(' expr_list ')'
///! paren_expr = '(' expr ')'
///! unit_lit = '(' ')'

use super::prelude::*;
use super::{Expr, LitExpr, LitValue, ExprList, ExprListParseResult};

// Paren expr is a side effect of TupleDef
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct ParenExpr {
    pub expr: Box<Expr>,
    pub span: Span,  // paren_span also all_span
}

impl ParenExpr {
    pub fn new<T: Into<Expr>>(span: Span, expr: T) -> ParenExpr { ParenExpr{ expr: Box::new(expr.into()), span } }
}

impl Node for ParenExpr {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_paren_expr(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(self.expr.as_ref())
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct TupleDef {
    pub items: ExprList,
    pub paren_span: Span,
}

impl TupleDef {
    pub fn new(paren_span: Span, items: ExprList) -> TupleDef { 
        TupleDef{ paren_span, items } 
    }
}

impl Parser for TupleDef {
    type Output = Expr;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Sep(Separator::LeftParen)) 
    }

    fn parse(cx: &mut ParseContext) -> Result<Expr, Unexpected> {

        match cx.expect::<ExprList>()? {
            ExprListParseResult::Empty(span) => {
                return Ok(Expr::Lit(LitExpr::new(LitValue::Unit, span)));
            }
            ExprListParseResult::SingleComma(span) => {
                cx.emit(strings::UnexpectedSingleComma).detail(span, strings::TupleDefHere);
                return Ok(Expr::Tuple(TupleDef::new(span, ExprList::new(Vec::new()))));
            }
            ExprListParseResult::Normal(span, exprlist) => {
                if exprlist.items.len() == 1 {
                    return Ok(Expr::Paren(ParenExpr::new(span, exprlist.items.into_iter().last().unwrap())));
                } else {
                    return Ok(Expr::Tuple(TupleDef::new(span, exprlist)));
                }
            }
            ExprListParseResult::EndWithComma(span, exprlist) => {
                return Ok(Expr::Tuple(TupleDef::new(span, exprlist)));
            }
        }
    }
}

impl Node for TupleDef {

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_tuple_def(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr_list(&self.items)
    }
}

#[cfg(test)] #[test]
fn tuple_def_parse() {
    use super::{BinaryExpr};

    //                                   01234567
    case!{ "(1, '2')" as TupleDef,
        Expr::Tuple(TupleDef::new(Span::new(0, 7), make_exprs![
            make_lit!(1, 1, 1),
            make_lit!('2': char, 4, 6),
        ]))
    }
    //                                   0123456
    case!{ "(1 + 1)" as TupleDef,
        Expr::Paren(ParenExpr::new(Span::new(0, 6), 
            BinaryExpr::new(
                make_lit!(1, 1, 1), 
                Separator::Add, Span::new(3, 3),
                make_lit!(1, 5, 5),
            )
        ))
    }
}

#[cfg(test)] #[test]
fn tuple_def_errors() {
    
    case!{ "( , )" as TupleDef,
        Expr::Tuple(TupleDef::new(Span::new(0, 4), make_exprs![])), 
        errors make_errors!(e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(0, 4), strings::TupleDefHere)),
    }
}
