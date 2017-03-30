///! fff-lang
///!
///! syntax/expr/common
///! common parser for comma seperated expr  
///! parenthnese quoted comma seperated expr  
///! bracket quoted comma seperated expr  

use codepos::StringPosition;

use lexical::LitValue;
use lexical::TokenStream;
use lexical::SeperatorKind;
use message::MessageCollection;

use super::BinaryExpr;
use super::PrimaryExpression;
use super::super::ISyntaxItem;

fn parse_comma_seperated_exprs(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Vec<BinaryExpr>, usize) {

    let mut ret_exprs = Vec::new();
    let mut current_length = 0;
    loop {
        match BinaryExpr::parse(tokens, messages, index + current_length) {
            (None, none_length) => { current_length += none_length; break; }
            (Some(expr), expr_len) => {
                current_length += expr_len;
                ret_exprs.push(expr);
                if tokens.nth(index + current_length).is_seperator(SeperatorKind::Comma) {
                    current_length += 1;
                    if !BinaryExpr::is_first_final(tokens, index + current_length) {
                        break;
                    }
                } else {
                    break;
                }
            }
        }
    }

    (ret_exprs, current_length)
}

#[cfg_attr(test, derive(Debug, Eq, PartialEq))]
pub enum ParseParenQuotedExprsResult {
    Tuple(Vec<BinaryExpr>, usize),
    Paren(BinaryExpr, usize),
    Unit,
}
pub fn parse_paren_quoted_exprs(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> ParseParenQuotedExprsResult {
    
    // let ret_exprs = Vec::new();
    ParseParenQuotedExprsResult::Unit
}

#[cfg(test)] #[test]
fn expr_common_parse_comma_seperated() {

    let mut tokens = TokenStream::with_test_str("1, 2, 3");
    let mut messages = MessageCollection::new();
    assert_eq!(parse_comma_seperated_exprs(&mut tokens, &mut messages, 0), (vec![
        BinaryExpr::new_primary(PrimaryExpression::Lit(LitValue::from(1), make_str_pos!(1, 1, 1, 1))),
        BinaryExpr::new_primary(PrimaryExpression::Lit(LitValue::from(2), make_str_pos!(1, 4, 1, 4))),
        BinaryExpr::new_primary(PrimaryExpression::Lit(LitValue::from(3), make_str_pos!(1, 7, 1, 7))),
    ], 5));

    let mut tokens = TokenStream::with_test_str("1, 2, 3, ");
    let mut messages = MessageCollection::new();
    assert_eq!(parse_comma_seperated_exprs(&mut tokens, &mut messages, 0), (vec![
        BinaryExpr::new_primary(PrimaryExpression::Lit(LitValue::from(1), make_str_pos!(1, 1, 1, 1))),
        BinaryExpr::new_primary(PrimaryExpression::Lit(LitValue::from(2), make_str_pos!(1, 4, 1, 4))),
        BinaryExpr::new_primary(PrimaryExpression::Lit(LitValue::from(3), make_str_pos!(1, 7, 1, 7))),
    ], 6));
}

#[cfg(test)] #[test]
fn expr_common_parse_paren_quoted_exprs() {

    let mut tokens = TokenStream::with_test_str("()");
    let mut messages = MessageCollection::new();
    assert_eq!(parse_paren_quoted_exprs(&mut tokens, &mut messages, 0), 
        ParseParenQuotedExprsResult::Unit
    );

    let mut tokens = TokenStream::with_test_str("(1)");
    let mut messages = MessageCollection::new();
    assert_eq!(parse_paren_quoted_exprs(&mut tokens, &mut messages, 0), 
        ParseParenQuotedExprsResult::Paren(BinaryExpr::new_primary(PrimaryExpression::Lit(LitValue::from(1), make_str_pos!(1, 2, 1, 2))), 3)
    );

    let mut tokens = TokenStream::with_test_str("(1, )");
    let mut messages = MessageCollection::new();
    assert_eq!(parse_paren_quoted_exprs(&mut tokens, &mut messages, 0), 
        ParseParenQuotedExprsResult::Tuple(vec![
            BinaryExpr::new_primary(PrimaryExpression::Lit(LitValue::from(1), make_str_pos!(1, 2, 1, 2)))
        ], 3)
    );
}