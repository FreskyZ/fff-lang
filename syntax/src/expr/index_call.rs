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
        format!("{}IndexerCall <{:?}>\n{}\n{}Bracket <{:?}>\n{}", 
            IndexCallExpr::indent_str(indent), self.all_span,
            self.base.format(indent + 1),
            IndexCallExpr::indent_str(indent + 1), self.bracket_span, 
            self.params.format(indent + 1))
    }
}
impl fmt::Debug for IndexCallExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl IndexCallExpr {

    pub fn new<T: Into<Expr>>(base: T, bracket_span: Span, params: ExprList) -> IndexCallExpr {
        let base = base.into();
        IndexCallExpr{
            all_span: base.get_all_span().merge(&bracket_span),
            base: Box::new(base),
            bracket_span, params
        }
    }

    fn new_with_parse_result(params: ExprList, bracket_span: Span) -> IndexCallExpr {
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
                return Ok(IndexCallExpr::new_with_parse_result(expr_list, span)),
            ExprListParseResult::Empty(span) | ExprListParseResult::SingleComma(span) => {
                // empty subscription is meaningless, refuse it here
                sess.push_message(Message::new_by_str("empty indexer call", vec![(span, "indexer call here")]));
                return Err(());
            }
        }
    }
}