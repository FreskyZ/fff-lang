///! fff-lang
///!
///! syntax/index_call_expr
///! index_call_expr = expr '[' [ expr_list ] ']'
///! renamed from postfix_expr::subscription to make it shorter

use std::fmt;

use codemap::Span;
use message::Message;
use lexical::Token;
use lexical::SeperatorKind;

use super::Expr;
use super::ExprList;
use super::ExprListParseResult;

use super::super::error_strings;
use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct IndexCallExpr {
    pub base: Box<Expr>,
    pub params: ExprList,
    pub bracket_span: Span,
    pub all_span: Span,
}
impl ISyntaxItemFormat for IndexCallExpr {
    fn format(&self, indent: u32) -> String {
        format!("{}IndexerCall <{:?}>\n{}\n{}bracket <{:?}>\n{}", 
            IndexCallExpr::indent_str(indent), self.all_span,
            self.base.format(indent + 1),
            IndexCallExpr::indent_str(indent + 1), self.bracket_span, 
            self.params.format(indent + 1))
    }
}
impl fmt::Debug for IndexCallExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
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
impl ISyntaxItemGrammar for IndexCallExpr {
    fn is_first_final(sess: &ParseSession) -> bool { sess.tk == &Token::Sep(SeperatorKind::LeftBracket) }
}
impl ISyntaxItemParse for IndexCallExpr {
    type Target = IndexCallExpr;

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
    use super::super::ISyntaxItemWithStr;
    use super::LitExpr;

    assert_eq!{ IndexCallExpr::with_test_str("[1, 2, ]"),
        IndexCallExpr::new_with_parse_result(make_span!(0, 7), ExprList::new(vec![
            Expr::Lit(LitExpr::new(LitValue::from(1), make_span!(1, 1))),
            Expr::Lit(LitExpr::new(LitValue::from(2), make_span!(4, 4))),
        ]))
    }

    assert_eq!{ IndexCallExpr::with_test_str("[\"hello\"]"),
        IndexCallExpr::new_with_parse_result(make_span!(0, 8), 
            ExprList::new(vec![Expr::Lit(LitExpr::new(LitValue::new_str_lit(make_id!(1)), make_span!(1, 7)))])
        )
    }
}

#[cfg(test)] #[test]
fn index_call_errors() {
    use message::MessageCollection;
    use super::super::ISyntaxItemWithStr;

    assert_eq!{ IndexCallExpr::with_test_str_ret_messages("(,)"), (
        Some(IndexCallExpr::new_with_parse_result(make_span!(0, 2), ExprList::new(Vec::new()))),
        make_messages![
            Message::new_by_str(error_strings::EmptyIndexCall, vec![(make_span!(0, 2), error_strings::IndexCallHere)])
        ]
    )}
}