///! fff-lang
///! 
///! syntax/tuple_def, paren_expr
///! tuple_def = '(' expr_list ')'
///! paren_expr = '(' expr ')'
///! unit_lit = '(' ')'

use super::prelude::*;
use super::{Expr, LitExpr, LitValue, ExprList, ExprListParseResult};

// Paren expr is a side effect of TupleDef
#[cfg_attr(test, derive(PartialEq))]
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
impl Node for ParenExpr {
    type ParseOutput = Expr;
    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<Expr> {
        TupleDef::parse(sess)
    }
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_paren_expr(self)
    }
}

#[cfg_attr(test, derive(PartialEq))]
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
impl Node for TupleDef {
    type ParseOutput = Expr;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Sep(Separator::LeftParen)) 
    }

    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<Expr> {

        match ExprList::parse(sess)? {
            ExprListParseResult::Empty(span) => {
                return Ok(Expr::Lit(LitExpr::new(LitValue::Unit, span)));
            }
            ExprListParseResult::SingleComma(span) => {
                sess.emit(strings::UnexpectedSingleComma).detail(span, strings::TupleDefHere);
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

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_tuple_def(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr_list(&self.items)
    }
}

#[cfg(test)] #[test]
fn tuple_def_display() {
    use super::{make_exprs, make_lit, make_source};

    let mut scx = make_source!("1231241241231412341234");
    scx.entry("1").finish();
    assert_eq!{
        TupleDef::new(Span::new(0, 21), make_exprs![]).display(&scx).to_string(),
        "tuple-def <1:1-1:22>\n"
    }

    let mut scx = make_source!("1231241241231412341234");
    scx.entry("1").finish();
    assert_eq!{
        TupleDef::new(Span::new(0, 8), make_exprs![
            make_lit!(1, 1, 2),
            make_lit!(2, 3, 4),
            make_lit!(48, 5, 6),
        ]).display(&scx).to_string(),
        "tuple-def <1:1-1:9>\n  literal i32 1 <1:2-1:3>\n  literal i32 2 <1:4-1:5>\n  literal i32 48 <1:6-1:7>\n"
    }
}

#[cfg(test)] #[test]
fn tuple_def_parse() {
    use super::{make_node, make_exprs, make_lit, BinaryExpr};

    //                                   01234567
    assert_eq!{ make_node!("(1, '2')" as TupleDef),
        Expr::Tuple(TupleDef::new(Span::new(0, 7), make_exprs![
            make_lit!(1, 1, 1),
            make_lit!('2': char, 4, 6),
        ]))
    }
    //                                   0123456
    assert_eq!{ make_node!("(1 + 1)" as TupleDef),
        Expr::Paren(ParenExpr::new(Span::new(0, 6), 
            BinaryExpr::new(
                make_lit!(1, 1, 1), 
                Separator::Add, Span::new(3, 3),
                make_lit!(1, 5, 5),
            )
        ))
    }
}

#[cfg(test)] #[test]
fn tuple_def_errors() {
    use super::{make_node, make_exprs, make_errors};
    
    assert_eq!{ make_node!("( , )" as TupleDef, and messages), (
        Expr::Tuple(TupleDef::new(Span::new(0, 4), make_exprs![])), 
        make_errors!(e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(0, 4), strings::TupleDefHere))
    )}
}
