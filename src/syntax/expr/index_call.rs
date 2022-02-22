///! fff-lang
///!
///! syntax/index_call_expr
///! index_call_expr = expr '[' [ expr_list ] ']'
///! renamed from postfix_expr::subscription to make it shorter

use std::fmt;
use crate::codemap::Span;
use crate::message::Message;
use crate::lexical::Token;
use crate::lexical::Seperator;
use super::Expr;
use super::ExprList;
use super::ExprListParseResult;
use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::error_strings;
use super::super::ISyntaxParse;
use super::super::ISyntaxFormat;
use super::super::ISyntaxGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
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
            all_span: base.get_all_span().merge(&bracket_span),
            base: Box::new(base),
            params,
            bracket_span
        }
    }

    fn new_with_parse_result(bracket_span: Span, params: ExprList) -> IndexCallExpr {
        IndexCallExpr{
            all_span: Span::default(), 
            base: Box::new(Expr::default()),
            bracket_span, params
        }
    }
}
impl ISyntaxGrammar for IndexCallExpr {
    fn matches_first(tokens: &[&Token]) -> bool { tokens[0] == &Token::Sep(Seperator::LeftBracket) }
}
impl ISyntaxParse for IndexCallExpr {
    type Output = IndexCallExpr;

    fn parse(sess: &mut ParseSession) -> ParseResult<IndexCallExpr> {

        match ExprList::parse(sess)? {
            ExprListParseResult::Normal(span, expr_list) | ExprListParseResult::EndWithComma(span, expr_list) => 
                return Ok(IndexCallExpr::new_with_parse_result(span, expr_list)),
            ExprListParseResult::Empty(span) | ExprListParseResult::SingleComma(span) => {
                // empty subscription is meaningless, refuse it here
                // update: but for trying to get more message in the rest program, make it not none
                sess.push_message(Message::new_by_str(error_strings::EmptyIndexCall, vec![(span, error_strings::IndexCallHere)]));
                return Ok(IndexCallExpr::new_with_parse_result(span, ExprList::new(Vec::new())))
            }
        }
    }
}

#[cfg(test)] #[test]
fn index_call_parse() {
    use lexical::LitValue;
    use super::super::WithTestInput;
    use super::LitExpr;

    assert_eq!{ IndexCallExpr::with_test_str("[1, 2, ]"),
        IndexCallExpr::new_with_parse_result(make_span!(0, 7), ExprList::new(vec![
            Expr::Lit(LitExpr::new(LitValue::from(1), make_span!(1, 1))),
            Expr::Lit(LitExpr::new(LitValue::from(2), make_span!(4, 4))),
        ]))
    }

    assert_eq!{ IndexCallExpr::with_test_str("[\"hello\"]"),
        IndexCallExpr::new_with_parse_result(make_span!(0, 8), 
            ExprList::new(vec![Expr::Lit(LitExpr::new(make_lit!(str, 1), make_span!(1, 7)))])
        )
    }
}

#[cfg(test)] #[test]
fn index_call_errors() {
    use message::MessageCollection;
    use super::super::TestInput;

    TestInput::new("[,]")
        .apply::<IndexCallExpr, _>()
        .expect_result(IndexCallExpr::new_with_parse_result(make_span!(0, 2), ExprList::new(vec![])))
        .expect_messages(make_messages![
            Message::new_by_str(error_strings::EmptyIndexCall, vec![(make_span!(0, 2), error_strings::IndexCallHere)])
        ])
    .finish();
}