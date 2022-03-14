///! fff-lang
///!
///! array_def = '[' [ expr_list ] ']'

use super::prelude::*;
use super::{Expr, ExprList, ExprListParseResult};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
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

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_array_def(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr_list(&self.items)
    }
}

#[cfg(test)] #[test]
fn array_def_display() {
    use super::{make_source, make_exprs, make_lit};

    let mut scx = make_source!("abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz");
    scx.entry("1").finish();
    assert_eq!{
        ArrayDef::new(Span::new(0, 42), make_exprs![]).display(&scx).to_string(),
        "array-def <1:1-1:43>\n"
    }

    let mut scx = make_source!("abcde\nfg\nhi\njklm");
    scx.entry("1").finish();
    assert_eq!{
        ArrayDef::new(Span::new(0, 9), make_exprs![
            make_lit!(1, 1, 1),
            make_lit!(2, 4, 4),
            make_lit!(48, 7, 8),
        ]).display(&scx).to_string(),
        "array-def <1:1-3:1>
  literal i32 1 <1:2-1:2>
  literal i32 2 <1:5-1:5>
  literal i32 48 <2:2-2:3>
"
    }
}

#[cfg(test)] #[test]
fn array_def_parse() {
    use super::{make_node, make_exprs, make_lit, BinaryExpr, SimpleName};

    assert_eq!{ make_node!("[a]" as ArrayDef),
        Expr::Array(ArrayDef::new(Span::new(0, 2), make_exprs![
            SimpleName::new(1, Span::new(1, 1))
        ]))
    }

    //                                   01234567
    assert_eq!{ make_node!("[1, '2']" as ArrayDef),
        Expr::Array(ArrayDef::new(Span::new(0, 7), make_exprs![
            make_lit!(1, 1, 1),
            make_lit!('2': char, 4, 6),
        ]))
    }
    //                                   01234567
    assert_eq!{ make_node!("[1 + 1,]" as ArrayDef),
        Expr::Array(ArrayDef::new(Span::new(0, 7), make_exprs![
            BinaryExpr::new(
                make_lit!(1, 1, 1),
                Separator::Add, Span::new(3, 3),
                make_lit!(1, 5, 5),
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
