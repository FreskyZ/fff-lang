///! syntax/tuple_def_expr, paren_expr
///! expr_list = expr { ',' expr } [ ',' ]

use super::prelude::*;
use super::Expr;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct ExprList {
    pub items: Vec<Expr>,
}
impl ExprList {
    pub fn new(items: Vec<Expr>) -> ExprList { ExprList{ items } }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub enum ExprListParseResult {
    Empty(Span),
    SingleComma(Span),              // and quote span
    Normal(Span, ExprList),         // and quote span
    EndWithComma(Span, ExprList),   // and quote span
}

impl Node for ExprListParseResult {

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        match self {
            Self::Empty(_) | Self::SingleComma(_) => Ok(Default::default()),
            // go visit expr list not this
            Self::Normal(_, e) | Self::EndWithComma(_, e) => v.visit_expr_list(e),
        }
    }
}

impl Parser for ExprList {
    type Output = ExprListParseResult;

    fn matches(current: &Token) -> bool {
        matches!(current, Token::Sep(Separator::LeftBrace | Separator::LeftBracket | Separator::LeftParen))
    }

    /// This is special, when calling `parse`, `cx.current` should point to the quote token
    /// Then the parser will check end token to determine end of parsing process
    fn parse(cx: &mut ParseContext) -> Result<ExprListParseResult, Unexpected> {

        let (starting_sep, starting_span) = cx.expect_seps(&[Separator::LeftBrace, Separator::LeftBracket, Separator::LeftParen])?;
        let expect_end_sep = match starting_sep { 
            Separator::LeftBrace => Separator::RightBrace, 
            Separator::LeftBracket => Separator::RightBracket,
            Separator::LeftParen => Separator::RightParen,
            _ => unreachable!(),
        };

        if let Some((ending_span, skipped_comma)) = cx.try_expect_closing_bracket(expect_end_sep) {
            return if skipped_comma {
                Ok(ExprListParseResult::SingleComma(starting_span + ending_span))
            } else {
                Ok(ExprListParseResult::Empty(starting_span + ending_span))
            };
        }

        let mut items = Vec::new();
        loop {
            items.push(cx.expect::<Expr>()?);
            if let Some((ending_span, skipped_comma)) = cx.try_expect_closing_bracket(expect_end_sep) {
                return if skipped_comma {
                    Ok(ExprListParseResult::EndWithComma(starting_span + ending_span, ExprList::new(items)))
                } else {
                    Ok(ExprListParseResult::Normal(starting_span + ending_span, ExprList::new(items)))
                };
            }
            cx.expect_sep(Separator::Comma)?;
        }
    }
}

impl Node for ExprList {

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr_list(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        for item in &self.items {
            v.visit_expr(item)?;
        }
        Ok(Default::default())
    }
}

// test helper
#[cfg(test)]
macro_rules! make_exprs {
    ($($x:expr),*) => ({
        let mut retval = Vec::new();
        {
            let _retval = &mut retval; // `&mut` for statisfy 'unused mut', `_` for statisfy unused var
            $(
                _retval.push(From::from($x));
            )*
        }
        crate::syntax::ExprList::new(retval)
    });
    ($($x:expr,)*) => (make_exprs![$($x),*])
}
#[cfg(test)]
pub(crate) use make_exprs;

#[cfg(test)] #[test]
fn expr_list_parse() {

    case!{ "[1, 2, 3]" as ExprList, 
        ExprListParseResult::Normal(Span::new(0, 8), ExprList::new(vec![
            Expr::Lit(make_lit!(1, 1, 1)),
            Expr::Lit(make_lit!(2, 4, 4)),
            Expr::Lit(make_lit!(3, 7, 7)),
        ]))
    }
    
    case!{ "(1, 2, 3,)" as ExprList, 
        ExprListParseResult::EndWithComma(Span::new(0, 9), ExprList::new(vec![
            Expr::Lit(make_lit!(1, 1, 1)),
            Expr::Lit(make_lit!(2, 4, 4)),
            Expr::Lit(make_lit!(3, 7, 7)),
        ]))
    }

    case!{ "[]" as ExprList, 
        ExprListParseResult::Empty(Span::new(0, 1))
    }

    case!{ "{,}" as ExprList,
        ExprListParseResult::SingleComma(Span::new(0, 2))
    }
}