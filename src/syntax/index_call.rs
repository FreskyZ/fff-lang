///! fff-lang
///!
///! syntax/index_call_expr
///! index_call_expr = expr '[' [ expr_list ] ']'
///! renamed from postfix_expr::subscription to make it shorter

use super::prelude::*;
use super::{Expr, ExprList, ExprListParseResult};

#[cfg_attr(test, derive(PartialEq))]
pub struct IndexCallExpr {
    pub base: Box<Expr>,
    pub params: ExprList,
    pub bracket_span: Span,
    pub all_span: Span,
}
impl ISyntaxFormat for IndexCallExpr {
    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("indexer-call").space().span(self.all_span).endl()
            .set_prefix_text("base-is").apply1(self.base.as_ref()).unset_prefix_text().endl()
            .indent1().lit("bracket").space().span(self.bracket_span).endl()
            .apply1(&self.params)
            .finish()
    }
}
impl fmt::Debug for IndexCallExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl From<IndexCallExpr> for Expr {
    fn from(index_call_expr: IndexCallExpr) -> Expr { Expr::IndexCall(index_call_expr) }
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

    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<IndexCallExpr> {

        match ExprList::parse(sess)? {
            ExprListParseResult::Normal(span, expr_list) | ExprListParseResult::EndWithComma(span, expr_list) => 
                return Ok(IndexCallExpr::new_with_parse_result(span, expr_list)),
            ExprListParseResult::Empty(span) | ExprListParseResult::SingleComma(span) => {
                // empty subscription is meaningless, refuse it here
                // update: but for trying to get more message in the rest program, make it not none
                sess.emit(strings::EmptyIndexCall).detail(span, strings::IndexCallHere);
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
    use super::{make_node, make_lit, make_exprs};

    assert_eq!{ make_node!("[1, 2, ]" as IndexCallExpr),
        IndexCallExpr::new_with_parse_result(Span::new(0, 7), make_exprs![
            make_lit!(1, 1, 1),
            make_lit!(2, 4, 4),
        ])
    }

    assert_eq!{ make_node!("[\"hello\"]" as IndexCallExpr),
        IndexCallExpr::new_with_parse_result(Span::new(0, 8), make_exprs![
            make_lit!(2: str, 1, 7)
        ])
    }
}

#[cfg(test)] #[test]
fn index_call_errors() {
    use super::{make_node, make_errors};

    assert_eq!{ make_node!("[,]" as IndexCallExpr, and messages), (
        IndexCallExpr::new_with_parse_result(Span::new(0, 2), ExprList::new(Vec::new())), 
        make_errors!(e: e.emit(strings::EmptyIndexCall).detail(Span::new(0, 2), strings::IndexCallHere))
    )}
}
