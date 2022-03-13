///! fff-lang
///!
///! array_def = '[' [ expr_list ] ']'

use super::prelude::*;
use super::{Expr, ExprList, ExprListParseResult};

#[cfg_attr(test, derive(PartialEq))]
pub struct ArrayDef {
    pub items: ExprList,
    pub bracket_span: Span,
}
impl ISyntaxFormat for ArrayDef {
    fn format(&self, f: Formatter) -> String {
        let f = f.indent().header_text_or("array-def").space().span(self.bracket_span).endl();
        (if self.items.items.len() == 0 { f.indent1().lit("no-init-item") } else { f.apply1(&self.items) }).finish()
    }
}
impl fmt::Debug for ArrayDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(Formatter::empty())) }
}
impl From<ArrayDef> for Expr {
    fn from(array_def: ArrayDef) -> Expr { Expr::Array(array_def) }
}
impl ArrayDef {
    pub fn new(bracket_span: Span, items: ExprList) -> ArrayDef { ArrayDef{ bracket_span, items: items } }
}
impl Node for ArrayDef {
    type ParseOutput = Expr;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Sep(Separator::LeftBracket)) 
    }

    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<Expr> {
        
        match ExprList::parse(sess)? {
            ExprListParseResult::Empty(span) => {
                return Ok(Expr::Array(ArrayDef::new(span, ExprList::new(Vec::new()))));
            }
            ExprListParseResult::SingleComma(span) => {
                sess.emit(strings::UnexpectedSingleComma).detail(span, strings::ArrayDefHere);
                return Ok(Expr::Array(ArrayDef::new(span, ExprList::new(Vec::new()))));
            }
            ExprListParseResult::Normal(span, exprlist) | ExprListParseResult::EndWithComma(span, exprlist) => {
                return Ok(Expr::Array(ArrayDef::new(span, exprlist)));
            }
        }
    }
}

#[cfg(test)] #[test]
fn array_def_format() {
    use super::{make_exprs, LitExpr};

    assert_eq!{
        ArrayDef::new(Span::new(0, 42), make_exprs![]).format(Formatter::with_test_indent(1)),
        "  array-def <<0>0-42>\n    no-init-item"
    }

    assert_eq!{
        ArrayDef::new(Span::new(0, 42), make_exprs![
            LitExpr::new(1, Span::new(1, 2)),
            LitExpr::new(2, Span::new(3, 4)),
            LitExpr::new(48, Span::new(5, 6)),
        ]).format(Formatter::with_test_indent(1)),
        "  array-def <<0>0-42>\n    literal (i32)1 <<0>1-2>\n    literal (i32)2 <<0>3-4>\n    literal (i32)48 <<0>5-6>"
    }
}

#[cfg(test)] #[test]
fn array_def_parse() {
    use super::{make_node, make_exprs, LitExpr, BinaryExpr, SimpleName};

    assert_eq!{ make_node!("[a]" as ArrayDef),
        Expr::Array(ArrayDef::new(Span::new(0, 2), make_exprs![
            SimpleName::new(1, Span::new(1, 1))
        ]))
    }

    //                                   01234567
    assert_eq!{ make_node!("[1, '2']" as ArrayDef),
        Expr::Array(ArrayDef::new(Span::new(0, 7), make_exprs![
            LitExpr::new(1i32, Span::new(1, 1)),
            LitExpr::new('2', Span::new(4, 6))
        ]))
    }
    //                                   01234567
    assert_eq!{ make_node!("[1 + 1,]" as ArrayDef),
        Expr::Array(ArrayDef::new(Span::new(0, 7), make_exprs![
            BinaryExpr::new(
                LitExpr::new(1i32, Span::new(1, 1)), 
                Separator::Add, Span::new(3, 3),
                LitExpr::new(1i32, Span::new(5, 5)),
            )
        ]))
    }
}

#[cfg(test)] #[test]
fn array_def_errors() {
    use super::{make_node, make_exprs, make_errors};

    assert_eq!{ make_node!("[ , ]" as ArrayDef, and messages), (
        Expr::Array(ArrayDef::new(Span::new(0, 4), make_exprs![])),
        make_errors!(e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(0, 4), strings::ArrayDefHere)),
    )}
}
