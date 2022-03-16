///! fff-lang
///!
///! syntax/index_call_expr
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

impl IndexCallExpr {

    pub fn new<T: Into<Expr>>(base: T, bracket_span: Span, params: ExprList) -> IndexCallExpr {
        let base = base.into();
        IndexCallExpr{
            all_span: base.get_all_span() + bracket_span,
            base: Box::new(base),
            params,
            bracket_span
        }
    }

    fn new_with_parse_result(bracket_span: Span, params: ExprList) -> IndexCallExpr {
        IndexCallExpr{
            all_span: Span::new(0, 0), 
            base: Box::new(Expr::default()),
            bracket_span, params
        }
    }
}

impl Node for IndexCallExpr {
    type ParseOutput = IndexCallExpr;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Sep(Separator::LeftBracket)) 
    }

    fn parse(cx: &mut ParseContext) -> ParseResult<IndexCallExpr> {

        match cx.expect_node::<ExprList>()? {
            ExprListParseResult::Normal(span, expr_list) | ExprListParseResult::EndWithComma(span, expr_list) => 
                return Ok(IndexCallExpr::new_with_parse_result(span, expr_list)),
            ExprListParseResult::Empty(span) | ExprListParseResult::SingleComma(span) => {
                // empty subscription is meaningless, refuse it here
                // update: but for trying to get more message in the rest program, make it not none
                cx.emit(strings::EmptyIndexCall).detail(span, strings::IndexCallHere);
                return Ok(IndexCallExpr::new_with_parse_result(span, ExprList::new(Vec::new())))
            }
        }
    }

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_index_call_expr(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(&self.base)?;
        v.visit_expr_list(&self.params)
    }
}

#[cfg(test)] #[test]
fn index_call_parse() {

    case!{ "[1, 2, ]" as IndexCallExpr,
        IndexCallExpr::new_with_parse_result(Span::new(0, 7), make_exprs![
            make_lit!(1, 1, 1),
            make_lit!(2, 4, 4),
        ])
    }

    case!{ "[\"hello\"]" as IndexCallExpr,
        IndexCallExpr::new_with_parse_result(Span::new(0, 8), make_exprs![
            make_lit!(2: str, 1, 7)
        ])
    }
}

#[cfg(test)] #[test]
fn index_call_errors() {

    case!{ "[,]" as IndexCallExpr,
        IndexCallExpr::new_with_parse_result(Span::new(0, 2), ExprList::new(Vec::new())), 
        errors make_errors!(e: e.emit(strings::EmptyIndexCall).detail(Span::new(0, 2), strings::IndexCallHere)),
    }
}
