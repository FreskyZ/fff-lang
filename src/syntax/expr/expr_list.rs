///! fff-lang
///! 
///! syntax/tuple_def_expr, paren_expr
///! expr_list = expr { ',' expr } [ ',' ]

use crate::syntax::prelude::*;
use super::Expr;

#[cfg_attr(test, derive(PartialEq))]
pub struct ExprList {
    pub items: Vec<Expr>,
}
impl ISyntaxFormat for ExprList {
    fn format(&self, f: Formatter) -> String {
        self.items.iter().map(|expr| f.clone().apply(expr).finish()).collect::<Vec<String>>().join("\n")
    }
}
impl fmt::Debug for ExprList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl ExprList {
    pub fn new(items: Vec<Expr>) -> ExprList { ExprList{ items } }
}
impl ISyntaxGrammar for ExprList {
    fn matches_first(tokens: [&Token; 3]) -> bool {
        matches!(tokens[0], &Token::Sep(Separator::LeftBrace) | &Token::Sep(Separator::LeftBracket) | &Token::Sep(Separator::LeftParen))
    }
}

#[cfg_attr(test, derive(PartialEq, Debug))]
pub enum ExprListParseResult {
    Empty(Span),
    SingleComma(Span),              // and quote span
    Normal(Span, ExprList),         // and quote span
    EndWithComma(Span, ExprList),   // and quote span
}
impl<'ecx, 'scx, F> ISyntaxParse<'ecx, 'scx, F> for ExprList where F: FileSystem {
    type Output = ExprListParseResult;

    /// This is special, when calling `parse`, `sess.tk` should point to the quote token
    /// Then the parser will check end token to determine end of parsing process
    fn parse(sess: &mut ParseSession<'ecx, 'scx, F>) -> ParseResult<ExprListParseResult> {

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
}

// test helper
#[macro_export]
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

#[cfg(test)] #[test]
fn expr_list_format() {
    use super::{LitExpr, LitValue};

    assert_eq!{
        ExprList::new(vec![
            Expr::Lit(LitExpr::new(LitValue::from(1i32), Span::new(1, 2))),
            Expr::Lit(LitExpr::new(LitValue::from(2i32), Span::new(3, 4))),
            Expr::Lit(LitExpr::new(LitValue::from(3i32), Span::new(5, 6))),
        ]).format(Formatter::with_test_indent(1)),
        "  literal (i32)1 <<0>1-2>\n  literal (i32)2 <<0>3-4>\n  literal (i32)3 <<0>5-6>"
    }
}

#[cfg(test)] #[test]
fn expr_list_parse() {
    use super::super::{make_node};
    use super::{LitExpr, LitValue};

    assert_eq!{ make_node!("[1, 2, 3]" as ExprList), 
        ExprListParseResult::Normal(Span::new(0, 8), ExprList::new(vec![
            Expr::Lit(LitExpr::new(LitValue::from(1i32), Span::new(1, 1))),
            Expr::Lit(LitExpr::new(LitValue::from(2i32), Span::new(4, 4))),
            Expr::Lit(LitExpr::new(LitValue::from(3i32), Span::new(7, 7))),
        ]))
    }
    
    assert_eq!{ make_node!("(1, 2, 3,)" as ExprList), 
        ExprListParseResult::EndWithComma(Span::new(0, 9), ExprList::new(vec![
            Expr::Lit(LitExpr::new(LitValue::from(1i32), Span::new(1, 1))),
            Expr::Lit(LitExpr::new(LitValue::from(2i32), Span::new(4, 4))),
            Expr::Lit(LitExpr::new(LitValue::from(3i32), Span::new(7, 7))),
        ]))
    }

    assert_eq!{ make_node!("[]" as ExprList), 
        ExprListParseResult::Empty(Span::new(0, 1))
    }

    assert_eq!{ make_node!("{,}" as ExprList),
        ExprListParseResult::SingleComma(Span::new(0, 2))
    }
}