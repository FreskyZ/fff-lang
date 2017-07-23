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
use lexical::Seperator;
use lexical::LitValue;

use super::Expr;
use super::LitExpr;
use super::ExprList;
use super::ExprListParseResult;

use super::super::error_strings;
use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

// Paren expr is a side effect of TupleDef
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct ParenExpr {
    pub expr: Box<Expr>,
    pub span: Span,  // paren_span also all_span
}
impl ISyntaxItemFormat for ParenExpr {
    fn format(&self, indent: u32) -> String {
        format!("{}ParenExpr <{:?}>\n{}", ParenExpr::indent_str(indent), self.span, self.expr.format(indent + 1))
    }
}
impl fmt::Debug for ParenExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl From<ParenExpr> for Expr {
    fn from(paren_expr: ParenExpr) -> Expr { Expr::Paren(paren_expr) }
}
impl ParenExpr {
    pub fn new<T: Into<Expr>>(span: Span, expr: T) -> ParenExpr { ParenExpr{ expr: Box::new(expr.into()), span } }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct TupleDef {
    pub items: ExprList,
    pub paren_span: Span,
}
impl ISyntaxItemFormat for TupleDef {
    fn format(&self, indent: u32) -> String {
        format!("{}TupleDef <{:?}>\n{}", TupleDef::indent_str(indent), self.paren_span, 
            if self.items.items.len() == 0 { format!("{}(empty)", TupleDef::indent_str(indent + 1)) } else { self.items.format(indent + 1) }
        )
    }
}
impl fmt::Debug for TupleDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(0)) }
}
impl From<TupleDef> for Expr {
    fn from(tuple_def: TupleDef) -> Expr { Expr::Tuple(tuple_def) }
}
impl TupleDef {
    pub fn new(paren_span: Span, items: ExprList) -> TupleDef { TupleDef{ paren_span, items } }
}
impl ISyntaxItemGrammar for TupleDef {
    fn is_first_final(sess: &ParseSession) -> bool { sess.tk == &Token::Sep(Seperator::LeftParenthenes) }
}
impl ISyntaxItemParse for TupleDef {
    type Target = Expr;

    fn parse(sess: &mut ParseSession) -> ParseResult<Expr> {

        match ExprList::parse(sess)? {
            ExprListParseResult::Empty(span) => {
                return Ok(Expr::Lit(LitExpr::new(LitValue::Unit, span)));
            }
            ExprListParseResult::SingleComma(span) => {
                sess.push_message(Message::new_by_str(error_strings::UnexpectedSingleComma, vec![(span, error_strings::TupleDefHere)]));
                return Ok(Expr::Tuple(TupleDef::new(span, ExprList::new(Vec::new()))));
            }
            ExprListParseResult::Normal(span, exprlist) => {
                if Vec::len(&exprlist.items) == 1 {
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

#[cfg(test)] #[test]
fn tuple_def_format() {
    use lexical::LitValue;
    use super::LitExpr;

    assert_eq!{
        TupleDef::new(make_span!(0, 42), make_exprs![]).format(1),
        "  TupleDef <<0>0-42>\n    (empty)"
    }

    assert_eq!{
        TupleDef::new(make_span!(0, 42), make_exprs![
            LitExpr::new(LitValue::from(1), make_span!(1, 2)),
            LitExpr::new(LitValue::from(2), make_span!(3, 4)),
            LitExpr::new(LitValue::from(48), make_span!(5, 6)),
        ]).format(1),
        "  TupleDef <<0>0-42>\n    Literal (i32)1 <<0>1-2>\n    Literal (i32)2 <<0>3-4>\n    Literal (i32)48 <<0>5-6>"
    }
}

#[cfg(test)] #[test]
fn tuple_def_parse() {
    use super::BinaryExpr;
    use super::super::WithTestInput;

    //                                   01234567
    assert_eq!{ TupleDef::with_test_str("(1, '2')"),
        Expr::Tuple(TupleDef::new(make_span!(0, 7), make_exprs![
            LitExpr::new(LitValue::from(1), make_span!(1, 1)),
            LitExpr::new(LitValue::from('2'), make_span!(4, 6))
        ]))
    }
    //                                   0123456
    assert_eq!{ TupleDef::with_test_str("(1 + 1)"),
        Expr::Paren(ParenExpr::new(make_span!(0, 6), 
            BinaryExpr::new(
                LitExpr::new(LitValue::from(1), make_span!(1, 1)), 
                Seperator::Add, make_span!(3, 3),
                LitExpr::new(LitValue::from(1), make_span!(5, 5)),
            )
        ))
    }
}

#[cfg(test)] #[test]
fn tuple_def_errors() {
    use message::MessageCollection;
    use super::super::TestInput;
    
    TestInput::new("( , )")
        .apply::<TupleDef, _>()
        .expect_result(Expr::Tuple(TupleDef::new(make_span!(0, 4), make_exprs![])))
        .expect_messages(make_messages![
            Message::new_by_str(error_strings::UnexpectedSingleComma, vec![(make_span!(0, 4), error_strings::TupleDefHere)])
        ])
    .finish();
}