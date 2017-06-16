///! fff-lang
///!
///! array_def = '[' [ expr_list ] ']'
use std::fmt;

use codemap::Span;
use message::Message;

use lexical::Token;
use lexical::SeperatorKind;

use super::ExprList;
use super::ExprListParseResult;
use super::PrimaryExpr;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct ArrayDef {
    pub items: ExprList,
    pub bracket_span: Span,
}
impl ISyntaxItemFormat for ArrayDef {
    fn format(&self, indent: u32) -> String {
        format!("{}ArrayDef <{:?}>\n{}", ArrayDef::indent_str(indent), self.bracket_span, self.items.format(0))
    }
}
impl fmt::Debug for ArrayDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(0)) }
}
impl ArrayDef {
    pub fn new(bracket_span: Span, items: ExprList) -> ArrayDef { ArrayDef{ bracket_span, items } }
}
impl ISyntaxItemGrammar for ArrayDef {
    fn is_first_final(sess: &ParseSession) -> bool { sess.tk == &Token::Sep(SeperatorKind::LeftBracket) }
}
impl ISyntaxItemParse for ArrayDef {
    type Target = PrimaryExpr;

    fn parse(sess: &mut ParseSession) -> ParseResult<PrimaryExpr> {

        match ExprList::parse(sess)? {
            ExprListParseResult::Empty(span) => {
                return Ok(PrimaryExpr::Array(ArrayDef::new(span, ExprList::new(Vec::new()))));
            }
            ExprListParseResult::SingleComma(span) => {
                sess.push_message(Message::new_by_str("single comma in array def", vec![(span, "")]));
                return Ok(PrimaryExpr::Array(ArrayDef::new(span, ExprList::new(Vec::new()))));
            }
            ExprListParseResult::Normal(span, exprlist) | ExprListParseResult::EndWithComma(span, exprlist) => {
                return Ok(PrimaryExpr::Array(ArrayDef::new(span, exprlist)));
            }
        }
    }
}

#[cfg(test)] #[test]
fn array_def_parse() {
    use lexical::LitValue;
    use super::LitExpr;
    use super::Expr;
    use super::super::ISyntaxItemWithStr;

    //                                   01234567
    assert_eq!{ ArrayDef::with_test_str("[1, '2']"),
        PrimaryExpr::Array(ArrayDef::new(make_span!(0, 7), ExprList::new(vec![
            Expr::new_lit(LitExpr::new(LitValue::from(1), make_span!(1, 1))),
            Expr::new_lit(LitExpr::new(LitValue::from('2'), make_span!(4, 6)))
        ])))
    }
    //                                   01234567
    assert_eq!{ ArrayDef::with_test_str("[1 + 1,]"),
        PrimaryExpr::Array(ArrayDef::new(make_span!(0, 7), ExprList::new(vec![
            Expr::new_binary(
                Expr::new_lit(LitExpr::new(LitValue::from(1), make_span!(1, 1))), 
                SeperatorKind::Add, make_span!(3, 3),
                Expr::new_lit(LitExpr::new(LitValue::from(1), make_span!(5, 5))),
            )
        ])))
    }
}