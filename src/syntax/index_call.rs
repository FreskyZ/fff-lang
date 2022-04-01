///! syntax::index_call_expr:
///! index_call_expr = expr '[' [ expr_list ] ']'
///! renamed from postfix_expr::subscription to make it shorter

use super::prelude::*;
use super::{Expr, ExprList, ExprListParseResult};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct IndexCallExpr {
    pub base: Box<Expr>,
    pub params: ExprList,
    pub bracket_span: Span,
    pub all_span: Span,
}

impl Parser for IndexCallExpr {
    type Output = IndexCallExpr;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Sep(Separator::LeftBracket)) 
    }

    fn parse(cx: &mut ParseContext) -> Result<IndexCallExpr, Unexpected> {

        match cx.expect::<ExprList>()? {
            ExprListParseResult::Normal(span, expr_list) 
            | ExprListParseResult::EndWithComma(span, expr_list) => 
                Ok(IndexCallExpr{ all_span: Span::new(0, 0), base: Box::new(Expr::default()), bracket_span: span, params: expr_list }),
            ExprListParseResult::Empty(span) 
            | ExprListParseResult::SingleComma(span) => {
                // empty subscription is meaningless, refuse it here
                // update: but for trying to get more message in the rest program, make it not none
                cx.emit(strings::EmptyIndexCall).detail(span, strings::IndexCallHere);
                Ok(IndexCallExpr{ all_span: Span::new(0, 0), base: Box::new(Expr::default()), bracket_span: span, params: ExprList{ items: Vec::new() } })
            }
        }
    }
}

impl Node for IndexCallExpr {

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_index_call_expr(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(&self.base)?;
        v.visit_expr_list(&self.params)
    }
}

#[cfg(test)]
#[test]
fn index_call_parse() {

    case!{ "[1, 2, ]" as IndexCallExpr,
        IndexCallExpr{ all_span: Span::new(0, 0), base: Box::new(Expr::default()), bracket_span: Span::new(0, 7), params: ExprList{ items: vec![
            make_expr!(i32 1 1:1),
            make_expr!(i32 2 4:4),
        ] } }
    }

    case!{ "[\"hello\"]" as IndexCallExpr,
        IndexCallExpr{ all_span: Span::new(0, 0), base: Box::new(Expr::default()), bracket_span: Span::new(0, 8), params: ExprList{ items: vec![
            make_expr!(str #2 1:7)
        ] } }
    }

    case!{ "[,]" as IndexCallExpr,
        IndexCallExpr{ all_span: Span::new(0, 0), base: Box::new(Expr::default()), bracket_span: Span::new(0, 2), params: ExprList{ items: Vec::new() } }, 
        errors make_errors!(e: e.emit(strings::EmptyIndexCall).detail(Span::new(0, 2), strings::IndexCallHere)),
    }
}
