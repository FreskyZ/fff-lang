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
impl ArrayDef {
    pub fn new(bracket_span: Span, items: ExprList) -> ArrayDef { ArrayDef{ bracket_span, items: items } }
}
impl Node for ArrayDef {
    type ParseOutput = Expr;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Sep(Separator::LeftBracket)) 
    }

    fn parse(cx: &mut ParseContext) -> ParseResult<Expr> {
        
        match cx.expect_node::<ExprList>()? {
            ExprListParseResult::Empty(span) => {
                return Ok(Expr::Array(ArrayDef::new(span, ExprList::new(Vec::new()))));
            }
            ExprListParseResult::SingleComma(span) => {
                cx.emit(strings::UnexpectedSingleComma).detail(span, strings::ArrayDefHere);
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

#[cfg(test)]
#[test]
fn array_def_parse() {
    use super::{BinaryExpr, SimpleName};

    case!{ "[a]" as ArrayDef,
        Expr::Array(ArrayDef::new(Span::new(0, 2), make_exprs![
            SimpleName::new(2, Span::new(1, 1))
        ]))
    }

    //                                   01234567
    case!{ "[1, '2']" as ArrayDef,
        Expr::Array(ArrayDef::new(Span::new(0, 7), make_exprs![
            make_lit!(1, 1, 1),
            make_lit!('2': char, 4, 6),
        ]))
    }
    //                                   01234567
    case!{ "[1 + 1,]" as ArrayDef,
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

    case!{ "[ , ]" as ArrayDef,
        Expr::Array(ArrayDef::new(Span::new(0, 4), make_exprs![])),
        errors make_errors!(e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(0, 4), strings::ArrayDefHere))
    }
}
