///! fff-lang
///! 
///! syntax/tuple_def_expr, paren_expr
///! expr_list = expr { ',' expr } [ ',' ]

use std::fmt;

use codemap::Span;
use lexical::Token;
use lexical::SeperatorKind;

use super::Expr;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct ExprList {
    pub items: Vec<Expr>,
}
impl ISyntaxItemFormat for ExprList {
    fn format(&self, indent: u32) -> String {
        self.items.iter().map(|expr| expr.format(indent)).collect::<Vec<String>>().join("\n")
    }
}
impl fmt::Debug for ExprList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl From<Vec<Expr>> for ExprList {
    fn from(items: Vec<Expr>) -> ExprList { ExprList{ items } }
}
impl ExprList {
    pub fn new(items: Vec<Expr>) -> ExprList { ExprList{ items } }
}
impl ISyntaxItemGrammar for ExprList {
    fn is_first_final(sess: &ParseSession) -> bool {
        match sess.tk {
            &Token::Sep(SeperatorKind::LeftBrace)
            | &Token::Sep(SeperatorKind::LeftBracket) 
            | &Token::Sep(SeperatorKind::LeftParenthenes) => true,
            _ => false,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub enum ExprListParseResult {
    Empty(Span),
    SingleComma(Span),              // and quote span
    Normal(Span, ExprList),         // and quote span
    EndWithComma(Span, ExprList),   // and quote span
}
impl ISyntaxItemParse for ExprList {
    type Target = ExprListParseResult;

    /// This is special, when calling `parse`, `sess.tk` should point to the quote token
    /// Then the parser will check end token to determine end of parsing process
    fn parse(sess: &mut ParseSession) -> ParseResult<ExprListParseResult> {

        let (expect_end_token, starting_quote_span) = match (sess.tk, sess.pos) {
            (&Token::Sep(SeperatorKind::LeftParenthenes), left_paren_span) => (Token::Sep(SeperatorKind::RightParenthenes), left_paren_span),
            (&Token::Sep(SeperatorKind::LeftBracket), left_bracket_span) => (Token::Sep(SeperatorKind::RightBracket), left_bracket_span),
            (&Token::Sep(SeperatorKind::LeftBrace), left_brace_span) => (Token::Sep(SeperatorKind::RightBrace), left_brace_span),
            _ => return sess.push_unexpect("left paired token"),
        };
        sess.move_next();

        if sess.tk == &expect_end_token {
            let ending_span = sess.pos;
            sess.move_next();
            return Ok(ExprListParseResult::Empty(starting_quote_span.merge(&ending_span)));
        }
        if let (&Token::Sep(SeperatorKind::Comma), next_tk, next_span) = (sess.tk, sess.next_tk, sess.next_pos) {
            if next_tk == &expect_end_token {
                sess.move_next2();
                return Ok(ExprListParseResult::SingleComma(starting_quote_span.merge(&next_span)));
            }
        }

        let mut items = Vec::new();
        loop {
            items.push(Expr::parse(sess)?);
            
            match (sess.tk, sess.pos, sess.next_tk, sess.next_pos) {
                (&Token::Sep(SeperatorKind::Comma), _, next_token, next_span) if next_token == &expect_end_token => {
                    sess.move_next2();
                    return Ok(ExprListParseResult::EndWithComma(starting_quote_span.merge(&next_span), ExprList::new(items)));
                }
                (token, right_span, _, _) if token == &expect_end_token => {
                    sess.move_next();
                    return Ok(ExprListParseResult::Normal(starting_quote_span.merge(&right_span), ExprList::new(items)));
                }
                (&Token::Sep(SeperatorKind::Comma), _, _, _) => { 
                    sess.move_next(); 
                    continue; 
                }
                _ => return sess.push_unexpect(&format!("comma, {:?}", expect_end_token)),
            }
        }
    }
}

#[cfg(test)] #[test]
fn expr_list_format() {
    use lexical::LitValue;
    use super::LitExpr;

    assert_eq!{
        ExprList::new(vec![
            Expr::Lit(LitExpr::new(LitValue::from(1), make_span!(1, 2))),
            Expr::Lit(LitExpr::new(LitValue::from(2), make_span!(3, 4))),
            Expr::Lit(LitExpr::new(LitValue::from(3), make_span!(5, 6))),
        ]).format(1),
        "  Literal (i32)1 <<0>1-2>\n  Literal (i32)2 <<0>3-4>\n  Literal (i32)3 <<0>5-6>"
    }
}

#[cfg(test)] #[test]
fn expr_list_parse() {
    use lexical::LitValue;
    use super::LitExpr;
    use super::super::ISyntaxItemWithStr;

    assert_eq!{ ExprList::with_test_str("[1, 2, 3]"), 
        ExprListParseResult::Normal(make_span!(0, 8), ExprList::new(vec![
            Expr::Lit(LitExpr::new(LitValue::from(1), make_span!(1, 1))),
            Expr::Lit(LitExpr::new(LitValue::from(2), make_span!(4, 4))),
            Expr::Lit(LitExpr::new(LitValue::from(3), make_span!(7, 7))),
        ]))
    }
    
    assert_eq!{ ExprList::with_test_str("(1, 2, 3,)"), 
        ExprListParseResult::EndWithComma(make_span!(0, 9), ExprList::new(vec![
            Expr::Lit(LitExpr::new(LitValue::from(1), make_span!(1, 1))),
            Expr::Lit(LitExpr::new(LitValue::from(2), make_span!(4, 4))),
            Expr::Lit(LitExpr::new(LitValue::from(3), make_span!(7, 7))),
        ]))
    }

    assert_eq!{ ExprList::with_test_str("[]"), 
        ExprListParseResult::Empty(make_span!(0, 1))
    }

    assert_eq!{ ExprList::with_test_str("{,}"),
        ExprListParseResult::SingleComma(make_span!(0, 2))
    }
}