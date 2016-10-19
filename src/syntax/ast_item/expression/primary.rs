
// PrimaryExpression = 
//     fLeftParen Expression fRightParen 
//     | fIdentifier | fLiteral 
//     | fLeftBracket [Expression [fComma Expression]*] fRightBracket     // var array = [1, 2, 3, a, b, c]
//     | fLeftBracket Expression fSemiColon Expression fRightBracket // var array = [false; 100]

use lexical::Lexer;
use lexical::IToken;
use lexical::NumericLiteralValue;
use lexical::SeperatorKind;

use syntax::ast_item::IASTItem;
use syntax::Expression;

#[derive(Debug, Eq, PartialEq)]
pub enum PrimaryExpression {
    Identifier(String),
    StringLiteral(String),
    CharLiteral(char),
    NumericLiteral(NumericLiteralValue),
    BooleanLiteral(bool),
    ParenExpression(Box<Expression>),
    ArrayDef(Vec<Expression>),
    ArrayDupDef(Box<Expression>, Box<Expression>),
}

impl IASTItem for PrimaryExpression {

    fn symbol_len(&self) -> usize {
        match *self {
            PrimaryExpression::Identifier(_)
            | PrimaryExpression::StringLiteral(_)
            | PrimaryExpression::CharLiteral(_)
            | PrimaryExpression::NumericLiteral(_)
            | PrimaryExpression::BooleanLiteral(_) => 1,
            PrimaryExpression::ParenExpression(ref expr) => 2 + expr.symbol_len(),
            PrimaryExpression::ArrayDef(ref exprs) => exprs.iter().fold(1, |counter, ref expr| counter + expr.symbol_len() + 1),
            PrimaryExpression::ArrayDupDef(ref expr_val, ref expr_size) => expr_val.symbol_len() + expr_size.symbol_len() + 3,
        }
    }
    
    fn parse(lexer: &mut Lexer, index: usize) -> Option<PrimaryExpression> {

        match lexer.nth(index).get_str_lit_val() {
            Some(&Some(ref val)) => return Some(PrimaryExpression::StringLiteral(val.clone())),
            Some(&None) => return Some(PrimaryExpression::StringLiteral("<invalid>".to_owned())),
            None => (),
        }
        match lexer.nth(index).get_char_lit_val() {
            Some(&Some(val)) => return Some(PrimaryExpression::CharLiteral(val)),
            Some(&None) => return Some(PrimaryExpression::CharLiteral('\u{FFFE}')),
            None => (),
        }
        match lexer.nth(index).get_num_lit_val() {
            Some(&Some(ref val)) => return Some(PrimaryExpression::NumericLiteral(val.clone())),
            Some(&None) => return Some(PrimaryExpression::NumericLiteral(NumericLiteralValue::I32(0))),
            None => (),
        }
        match lexer.nth(index).get_bool_lit_val() {
            Some(val) => return Some(PrimaryExpression::BooleanLiteral(val)),
            None => (),
        }
        match lexer.nth(index).get_identifier() {
            Some(val) => return Some(PrimaryExpression::Identifier(val.clone())),
            None => (),
        }

        perrorln!("parsing prim expr at index {} with token {:?} not literal or identifier", index, lexer.nth(index));
        if lexer.nth(index).is_seperator(SeperatorKind::LeftParenthenes) {
            match Expression::parse(lexer, index + 1) {
                Some(expr) => {
                    if lexer.nth(index + 1 + expr.symbol_len()).is_seperator(SeperatorKind::RightParenthenes) {
                        return test_perrorln_and_val!("Paren expr successed"; Some(PrimaryExpression::ParenExpression(Box::new(expr))));
                    } else {
                        return test_perrorln_and_val!("Paren expr failed, expect right paren"; lexer.push_expect_symbol("Right paren", index + 1 + expr.symbol_len()));
                    }
                }
                None => return test_perrorln_and_val!("Paren expr failed, expect expr"; lexer.push_expect_symbol("Some expression", index + 1)),
            }
        }

        if lexer.nth(index).is_seperator(SeperatorKind::LeftBracket) {
            match Expression::parse(lexer, index + 1) {
                Some(expr1) => {
                    if lexer.nth(index + 1 + expr1.symbol_len()).is_seperator(SeperatorKind::SemiColon) {
                        match Expression::parse(lexer, index + 1 + expr1.symbol_len() + 1) {
                            None => return test_perrorln_and_val!("Failed in array dup def parse, expect expr after left semicolon"; 
                                lexer.push_expect_symbol("Some expression in array dup def", index + 1 + expr1.symbol_len() + 1)),
                            Some(expr2) => {
                                if lexer.nth(index + 1 + expr1.symbol_len() + 1 + expr2.symbol_len() + 1).is_seperator(SeperatorKind::RightBracket) {
                                    return test_perrorln_and_val!("Success in array dup def"; 
                                        Some(PrimaryExpression::ArrayDupDef(Box::new(expr1), Box::new(expr2))));
                                } else {
                                    return test_perrorln_and_val!("expect expr after semicolon in array dup def";
                                        lexer.push_expect_symbol("Right bracket after array dup def", index + 3 + expr1.symbol_len() + expr2.symbol_len()));
                                }
                            }
                        }
                    }

                    let mut exprs_len = expr1.symbol_len();
                    let mut exprs = vec![expr1];
                    loop {
                        if lexer.nth(index + 1 + exprs_len).is_seperator(SeperatorKind::RightBracket) {
                            return test_perrorln_and_val!("Success in array def"; Some(PrimaryExpression::ArrayDef(exprs)));
                        }
                        if lexer.nth(index + 1 + exprs_len).is_seperator(SeperatorKind::Comma) {
                            exprs_len += 1;
                            match Expression::parse(lexer, index + 1 + exprs_len) {
                                Some(exprn) => {
                                    exprs_len += exprn.symbol_len();
                                    exprs.push(exprn);
                                }
                                None => test_perrorln_and_val!("Fail in array def, expect some expr after comma";
                                    return lexer.push_expect_symbol("Some expression", index + 1 + exprs_len)),
                            }
                        }
                    }
                }
                None => return test_perrorln_and_val!("Failed in array (dup) def, expect some expr after left bracket"; 
                    lexer.push_expect_symbol("Some expression", index + 1)),
            }
        }

        perrorln!("Failed in prim expr parse, not start with left paren or left bracket");
        return lexer.push_expect_symbol("identifier or literal or array def", index);
    }
} 

#[cfg(test)]
mod tests {

    #[test]
    fn ast_expr_prim_parse() {
        use super::PrimaryExpression;
        use syntax::ast_item::IASTItem;
        use message::MessageEmitter;
        use lexical::Lexer;
        use syntax::Expression;
        use lexical::NumericLiteralValue;

        // Case 1
        assert_eq!(
            PrimaryExpression::parse(&mut Lexer::new_test("[1, 2, 3f128, 0u64]", MessageEmitter::new()), 0), 
            Some(PrimaryExpression::ArrayDef(vec![
                Expression::Primary(PrimaryExpression::NumericLiteral(NumericLiteralValue::I32(1))),
                Expression::Primary(PrimaryExpression::NumericLiteral(NumericLiteralValue::I32(2))), 
                Expression::Primary(PrimaryExpression::NumericLiteral(NumericLiteralValue::I32(0))),
                Expression::Primary(PrimaryExpression::NumericLiteral(NumericLiteralValue::U64(0)))
            ]))
        );

        // Case 2
                                                              // 0123456 78  9 ABCDE FGH I    JKLMN
        let res = PrimaryExpression::parse(&mut Lexer::new_test("[[(1)], [abc, (3)], [4, defg, [6]]]", MessageEmitter::new()), 0);
        match res.unwrap() {
            PrimaryExpression::ArrayDef(exprs) => {
                assert_eq!(exprs[0], 
                    Expression::Primary(PrimaryExpression::ArrayDef(vec![
                        Expression::Primary(PrimaryExpression::ParenExpression(Box::new(
                            Expression::Primary(PrimaryExpression::NumericLiteral(NumericLiteralValue::I32(1)))
                        )))
                    ]))
                );
                assert_eq!(exprs[1], 
                    Expression::Primary(PrimaryExpression::ArrayDef(vec![
                        Expression::Primary(PrimaryExpression::Identifier("abc".to_owned())),
                        Expression::Primary(PrimaryExpression::ParenExpression(Box::new(
                            Expression::Primary(PrimaryExpression::NumericLiteral(NumericLiteralValue::I32(3)))
                        )))
                    ]))
                );
                assert_eq!(exprs[2], 
                    Expression::Primary(PrimaryExpression::ArrayDef(vec![
                        Expression::Primary(PrimaryExpression::NumericLiteral(NumericLiteralValue::I32(4))),
                        Expression::Primary(PrimaryExpression::Identifier("defg".to_owned())),
                        Expression::Primary(PrimaryExpression::ArrayDef(vec![
                            Expression::Primary(PrimaryExpression::NumericLiteral(NumericLiteralValue::I32(6)))
                        ])),
                    ]))
                );
            }
            other => panic!("Not expected, but {:?}", other),
        }

        // Case 3
        assert_eq!(
            PrimaryExpression::parse(&mut Lexer::new_test("[abc, 123, \"456\", '\\u0065', false, (a)]", MessageEmitter::new()), 0),
            Some(PrimaryExpression::ArrayDef(vec![
                Expression::Primary(PrimaryExpression::Identifier("abc".to_owned())),
                Expression::Primary(PrimaryExpression::NumericLiteral(NumericLiteralValue::I32(123))),
                Expression::Primary(PrimaryExpression::StringLiteral("456".to_owned())),
                Expression::Primary(PrimaryExpression::CharLiteral('\u{0065}')),
                Expression::Primary(PrimaryExpression::BooleanLiteral(false)),
                Expression::Primary(PrimaryExpression::ParenExpression(Box::new(
                    Expression::Primary(PrimaryExpression::Identifier("a".to_owned()))
                ))),
            ]))
        );        
        
        // Case 4
        assert_eq!(
            PrimaryExpression::parse(&mut Lexer::new_test("[abc, 123f, \"456\\u\", '\\u00', false, (a)]", MessageEmitter::new()), 0),
            Some(PrimaryExpression::ArrayDef(vec![
                Expression::Primary(PrimaryExpression::Identifier("abc".to_owned())),
                Expression::Primary(PrimaryExpression::NumericLiteral(NumericLiteralValue::I32(0))),
                Expression::Primary(PrimaryExpression::StringLiteral("<invalid>".to_owned())),
                Expression::Primary(PrimaryExpression::CharLiteral('\u{FFFE}')),
                Expression::Primary(PrimaryExpression::BooleanLiteral(false)),
                Expression::Primary(PrimaryExpression::ParenExpression(Box::new(
                    Expression::Primary(PrimaryExpression::Identifier("a".to_owned()))
                ))),
            ]))
        );
    }
}