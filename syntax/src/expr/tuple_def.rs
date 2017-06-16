///! fff-lang
///! 
///! syntax/tuple_def, paren_expr
///! tuple_def = '(' expr_list ')'
///! paren_expr = '(' expr ')'
///! unit_lit = '(' ')'

use std::fmt;

use codemap::Span;
use message::Message;

use lexical::Token;
use lexical::SeperatorKind;
use lexical::LitValue;

use super::LitExpr;
use super::ExprList;
use super::ExprListParseResult;
use super::PrimaryExpr;
use super::Expr;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

// Paren expr is a side effect of TupleDef
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct ParenExpr {
    pub expr: Expr,
    pub span: Span,  // paren_span also all_span
}
impl ISyntaxItemFormat for ParenExpr {
    fn format(&self, indent: u32) -> String {
        format!("{}ParenExpr <{:?}>\n{}", ParenExpr::indent_str(indent), self.span, self.expr.format(0))
    }
}
impl fmt::Debug for ParenExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl ParenExpr {
    pub fn new(span: Span, expr: Expr) -> ParenExpr { ParenExpr{ expr, span } }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct TupleDef {
    pub items: ExprList,
    pub paren_span: Span,
}
impl ISyntaxItemFormat for TupleDef {
    fn format(&self, indent: u32) -> String {
        format!("{}TupleDef <{:?}>\n{}", TupleDef::indent_str(indent), self.paren_span, self.items.format(0))
    }
}
impl fmt::Debug for TupleDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(0)) }
}
impl TupleDef {
    pub fn new(paren_span: Span, items: ExprList) -> TupleDef { TupleDef{ paren_span, items } }
}
impl ISyntaxItemGrammar for TupleDef {
    fn is_first_final(sess: &ParseSession) -> bool { sess.tk == &Token::Sep(SeperatorKind::LeftParenthenes) }
}
impl ISyntaxItemParse for TupleDef {
    type Target = PrimaryExpr;

    fn parse(sess: &mut ParseSession) -> ParseResult<PrimaryExpr> {

        match ExprList::parse(sess)? {
            ExprListParseResult::Empty(span) => {
                return Ok(PrimaryExpr::Lit(LitExpr::new(LitValue::Unit, span)));
            }
            ExprListParseResult::SingleComma(span) => {
                sess.push_message(Message::new_by_str("single comma in tuple def", vec![(span, "")]));
                return Ok(PrimaryExpr::Tuple(TupleDef::new(span, ExprList::new(Vec::new()))));
            }
            ExprListParseResult::Normal(span, exprlist) => {
                if Vec::len(&exprlist.items) == 1 {
                    return Ok(PrimaryExpr::Paren(ParenExpr::new(span, exprlist.items.into_iter().last().unwrap())));
                } else {
                    return Ok(PrimaryExpr::Tuple(TupleDef::new(span, exprlist)));
                }
            }
            ExprListParseResult::EndWithComma(span, exprlist) => {
                return Ok(PrimaryExpr::Tuple(TupleDef::new(span, exprlist)));
            }
        }
    }
}

#[cfg(test)] #[test]
fn tuple_def_parse() {
    use super::super::ISyntaxItemWithStr;

    //                                   01234567
    assert_eq!{ TupleDef::with_test_str("(1, '2')"),
        PrimaryExpr::Tuple(TupleDef::new(make_span!(0, 7), ExprList::new(vec![
            Expr::new_lit(LitExpr::new(LitValue::from(1), make_span!(1, 1))),
            Expr::new_lit(LitExpr::new(LitValue::from('2'), make_span!(4, 6)))
        ])))
    }
    //                                   0123456
    assert_eq!{ TupleDef::with_test_str("(1 + 1)"),
        PrimaryExpr::Paren(ParenExpr::new(make_span!(0, 6), 
            Expr::new_binary(
                Expr::new_lit(LitExpr::new(LitValue::from(1), make_span!(1, 1))), 
                SeperatorKind::Add, make_span!(3, 3),
                Expr::new_lit(LitExpr::new(LitValue::from(1), make_span!(5, 5))),
            )
        ))
    }
}