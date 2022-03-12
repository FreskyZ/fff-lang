///! fff-lang
///! 
///! syntax/tuple_def, paren_expr
///! tuple_def = '(' expr_list ')'
///! paren_expr = '(' expr ')'
///! unit_lit = '(' ')'

use crate::syntax::prelude::*;
use super::{Expr, LitExpr, LitValue, ExprList, ExprListParseResult};

// Paren expr is a side effect of TupleDef
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct ParenExpr {
    pub expr: Box<Expr>,
    pub span: Span,  // paren_span also all_span
}
impl ISyntaxFormat for ParenExpr {
    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("paren-expr").space().span(self.span).endl()
            .apply1(self.expr.as_ref())
            .finish()
    }
}
impl fmt::Debug for ParenExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
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
impl ISyntaxFormat for TupleDef {
    fn format(&self, f: Formatter) -> String {
        let f = f.indent().header_text_or("tuple-def").space().span(self.paren_span).endl();
        if self.items.items.len() == 0 {
            f.indent1().lit("no-item").finish()
        } else {
            f.apply1(&self.items).finish()
        }
    }
}
impl fmt::Debug for TupleDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(Formatter::empty())) }
}
impl From<TupleDef> for Expr {
    fn from(tuple_def: TupleDef) -> Expr { Expr::Tuple(tuple_def) }
}
impl TupleDef {
    pub fn new(paren_span: Span, items: ExprList) -> TupleDef { TupleDef{ paren_span, items } }
}
impl ISyntaxGrammar for TupleDef {
    fn matches_first(tokens: [&Token; 3]) -> bool { matches!(tokens[0], &Token::Sep(Separator::LeftParen)) }
}
impl<'ecx, 'scx, F> ISyntaxParse<'ecx, 'scx, F> for TupleDef where F: FileSystem {
    type Output = Expr;

    fn parse(sess: &mut ParseSession<'ecx, 'scx, F>) -> ParseResult<Expr> {

        match ExprList::parse(sess)? {
            ExprListParseResult::Empty(span) => {
                return Ok(Expr::Lit(LitExpr::new(LitValue::Unit, span)));
            }
            ExprListParseResult::SingleComma(span) => {
                sess.push_message(Message::new_by_str(strings::UnexpectedSingleComma, vec![(span, strings::TupleDefHere)]));
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

#[cfg(test)] #[test]
fn tuple_def_format() {
    use crate::lexical::LitValue;
    use super::LitExpr;

    assert_eq!{
        TupleDef::new(Span::new(0, 42), make_exprs![]).format(Formatter::with_test_indent(1)),
        "  tuple-def <<0>0-42>\n    no-item"
    }

    assert_eq!{
        TupleDef::new(Span::new(0, 42), make_exprs![
            LitExpr::new(LitValue::from(1), Span::new(1, 2)),
            LitExpr::new(LitValue::from(2), Span::new(3, 4)),
            LitExpr::new(LitValue::from(48), Span::new(5, 6)),
        ]).format(Formatter::with_test_indent(1)),
        "  tuple-def <<0>0-42>\n    literal (i32)1 <<0>1-2>\n    literal (i32)2 <<0>3-4>\n    literal (i32)48 <<0>5-6>"
    }
}

#[cfg(test)] #[test]
fn tuple_def_parse() {
    use super::BinaryExpr;
    use super::super::WithTestInput;

    //                                   01234567
    assert_eq!{ make_node!("(1, '2')"),
        Expr::Tuple(TupleDef::new(Span::new(0, 7), make_exprs![
            LitExpr::new(LitValue::from(1), Span::new(1, 1)),
            LitExpr::new(LitValue::from('2'), Span::new(4, 6))
        ]))
    }
    //                                   0123456
    assert_eq!{ make_node!("(1 + 1)"),
        Expr::Paren(ParenExpr::new(Span::new(0, 6), 
            BinaryExpr::new(
                LitExpr::new(LitValue::from(1), Span::new(1, 1)), 
                Separator::Add, Span::new(3, 3),
                LitExpr::new(LitValue::from(1), Span::new(5, 5)),
            )
        ))
    }
}

#[cfg(test)] #[test]
fn tuple_def_errors() {
    use crate::diagnostics::MessageCollection;
    use super::super::TestInput;
    
    TestInput::new("( , )")
        .apply::<TupleDef, _>()
        .expect_result(Expr::Tuple(TupleDef::new(Span::new(0, 4), make_exprs![])))
        .expect_messages(make_messages![
            Message::new_by_str(strings::UnexpectedSingleComma, vec![(Span::new(0, 4), strings::TupleDefHere)])
        ])
    .finish();
}