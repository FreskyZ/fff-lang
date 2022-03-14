///! syntax/tuple_def_expr, paren_expr
///! expr_list = expr { ',' expr } [ ',' ]

use super::prelude::*;
use super::Expr;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct ExprList {
    pub items: Vec<Expr>,
}
impl ISyntaxFormat for ExprList {
    fn format(&self, f: Formatter) -> String {
        self.items.iter().map(|expr| f.clone().apply(expr).finish()).collect::<Vec<String>>().join("\n")
    }
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

impl Node for ExprList {
    type ParseOutput = ExprListParseResult;

    fn matches(current: &Token) -> bool {
        matches!(current, Token::Sep(Separator::LeftBrace | Separator::LeftBracket | Separator::LeftParen))
    }

    /// This is special, when calling `parse`, `sess.tk` should point to the quote token
    /// Then the parser will check end token to determine end of parsing process
    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<ExprListParseResult> {

        let (starting_sep, starting_span) = sess.expect_seps(&[Separator::LeftBrace, Separator::LeftBracket, Separator::LeftParen])?;
        let expect_end_sep = match starting_sep { 
            Separator::LeftBrace => Separator::RightBrace, 
            Separator::LeftBracket => Separator::RightBracket,
            Separator::LeftParen => Separator::RightParen,
            _ => unreachable!(),
        };

        if let Some(ending_span) = sess.try_expect_sep(expect_end_sep) {
            return Ok(ExprListParseResult::Empty(starting_span + ending_span));
        }
        if let Some((_, ending_span)) = sess.try_expect_2_sep(Separator::Comma, expect_end_sep) {
            return Ok(ExprListParseResult::SingleComma(starting_span + ending_span));
        }

        let mut items = Vec::new();
        loop {
            items.push(Expr::parse(sess)?);
            
            if let Some((_, ending_span)) = sess.try_expect_2_sep(Separator::Comma, expect_end_sep) {
                return Ok(ExprListParseResult::EndWithComma(starting_span + ending_span, ExprList::new(items)));
            } else if let Some(ending_span) = sess.try_expect_sep(expect_end_sep) {
                return Ok(ExprListParseResult::Normal(starting_span + ending_span, ExprList::new(items)));
            }
            let _comma_span = sess.expect_sep(Separator::Comma)?;
        }
    }

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
    use super::{make_node, make_lit};

    assert_eq!{ make_node!("[1, 2, 3]" as ExprList), 
        ExprListParseResult::Normal(Span::new(0, 8), ExprList::new(vec![
            Expr::Lit(make_lit!(1, 1, 1)),
            Expr::Lit(make_lit!(2, 4, 4)),
            Expr::Lit(make_lit!(3, 7, 7)),
        ]))
    }
    
    assert_eq!{ make_node!("(1, 2, 3,)" as ExprList), 
        ExprListParseResult::EndWithComma(Span::new(0, 9), ExprList::new(vec![
            Expr::Lit(make_lit!(1, 1, 1)),
            Expr::Lit(make_lit!(2, 4, 4)),
            Expr::Lit(make_lit!(3, 7, 7)),
        ]))
    }

    assert_eq!{ make_node!("[]" as ExprList), 
        ExprListParseResult::Empty(Span::new(0, 1))
    }

    assert_eq!{ make_node!("{,}" as ExprList),
        ExprListParseResult::SingleComma(Span::new(0, 2))
    }
}