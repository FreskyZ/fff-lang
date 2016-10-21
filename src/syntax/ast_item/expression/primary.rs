
// PrimaryExpression = 
//     fLeftParen Expression fRightParen 
//     | fIdentifier | fLiteral 
//     | fLeftBracket [Expression [fComma Expression]*] fRightBracket     // var array = [1, 2, 3, a, b, c]
//     | fLeftBracket Expression fSemiColon Expression fRightBracket // var array = [false; 100]

use std::fmt;

use common::From2;
use common::StringPosition;

use lexical::Lexer;
use lexical::IToken;
use lexical::NumericLiteralValue;
use lexical::SeperatorKind;

use syntax::ast_item::IASTItem;
use syntax::ast_item::expression::d3::D3Expression;

#[derive(Eq, PartialEq, Clone)]
pub enum PrimaryExpressionBase {
    Identifier(String),
    StringLiteral(String),
    CharLiteral(char),
    NumericLiteral(NumericLiteralValue),
    BooleanLiteral(bool),
    ParenExpression(Box<D3Expression>),
    ArrayDef(Vec<D3Expression>),
    ArrayDupDef(Box<D3Expression>, Box<D3Expression>),
}

impl fmt::Debug for PrimaryExpressionBase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            PrimaryExpressionBase::Identifier(ref name) => write!(f, "{}", name),
            PrimaryExpressionBase::StringLiteral(ref val) => write!(f, "{:?}", val),
            PrimaryExpressionBase::CharLiteral(ref val) => write!(f, "{:?}", val),
            PrimaryExpressionBase::NumericLiteral(ref val) => write!(f, "{:?}", val),
            PrimaryExpressionBase::BooleanLiteral(ref val) => write!(f, "{}", val),
            PrimaryExpressionBase::ParenExpression(ref expr) => write!(f, "({:?})", expr),
            PrimaryExpressionBase::ArrayDupDef(ref expr1, ref expr2) => 
                write!(f, "[{:?}; {:?}]", expr1, expr2),
            PrimaryExpressionBase::ArrayDef(ref exprs) => 
                write!(f, "[{}]", exprs.iter().fold(String::new(), |mut buf, expr| { buf.push_str(&format!("{:?}, ", expr)); buf })),
        }
    }
}
impl fmt::Display for PrimaryExpressionBase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            PrimaryExpressionBase::Identifier(ref name) => write!(f, "{}", name),
            PrimaryExpressionBase::StringLiteral(ref val) => write!(f, "{:?}", val),
            PrimaryExpressionBase::CharLiteral(ref val) => write!(f, "{:?}", val),
            PrimaryExpressionBase::NumericLiteral(ref val) => write!(f, "{}", val),
            PrimaryExpressionBase::BooleanLiteral(ref val) => write!(f, "{}", val),
            PrimaryExpressionBase::ParenExpression(ref expr) => write!(f, "({})", expr),
            PrimaryExpressionBase::ArrayDupDef(ref expr1, ref expr2) => 
                write!(f, "[{}; {}]", expr1, expr2),
            PrimaryExpressionBase::ArrayDef(ref exprs) => 
                write!(f, "[{}]", exprs.iter().fold(String::new(), |mut buf, expr| { buf.push_str(&format!("{}, ", expr)); buf })),
        }
    }
}

#[derive(Eq, PartialEq, Clone)]
pub struct PrimaryExpression(pub PrimaryExpressionBase, pub StringPosition);

impl fmt::Debug for PrimaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} @ {:?}", self.0, self.1)
    }
}
impl fmt::Display for PrimaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl PrimaryExpression {

    pub fn make_ident(name: String, pos: StringPosition) -> PrimaryExpression {
        PrimaryExpression(PrimaryExpressionBase::Identifier(name), pos)
    }
    pub fn make_str_lit(val: String, pos: StringPosition) -> PrimaryExpression {
        PrimaryExpression(PrimaryExpressionBase::StringLiteral(val), pos)
    }
    pub fn make_char_lit(val: char, pos: StringPosition) -> PrimaryExpression {
        PrimaryExpression(PrimaryExpressionBase::CharLiteral(val), pos)
    }
    pub fn make_num_lit(val: NumericLiteralValue, pos: StringPosition) -> PrimaryExpression {
        PrimaryExpression(PrimaryExpressionBase::NumericLiteral(val), pos)
    }
    pub fn make_bool_lit(val: bool, pos: StringPosition) -> PrimaryExpression {
        PrimaryExpression(PrimaryExpressionBase::BooleanLiteral(val), pos)
    }

    pub fn make_paren(expr: D3Expression, pos: StringPosition) -> PrimaryExpression {
        PrimaryExpression(PrimaryExpressionBase::ParenExpression(Box::new(expr)), pos)
    }
    pub fn make_array_def(exprs: Vec<D3Expression>, pos: StringPosition) -> PrimaryExpression {
        PrimaryExpression(PrimaryExpressionBase::ArrayDef(exprs), pos)
    }
    pub fn make_array_dup_def(expr1: D3Expression, expr2: D3Expression, pos: StringPosition) -> PrimaryExpression {
        PrimaryExpression(PrimaryExpressionBase::ArrayDupDef(Box::new(expr1), Box::new(expr2)), pos)
    }
}

impl IASTItem for PrimaryExpression {
    
    fn pos_all(&self) -> StringPosition {
        self.1
    }

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<PrimaryExpression>, usize) {
        let log_enable = false;

        match lexer.nth(index).get_str_lit_val() {
            Some(&Some(ref val)) => return (Some(PrimaryExpression::make_str_lit(val.clone(), lexer.pos(index))), 1),
            Some(&None) => return (Some(PrimaryExpression::make_str_lit("<invalid>".to_owned(), lexer.pos(index))), 1),
            None => (),
        }
        match lexer.nth(index).get_char_lit_val() {
            Some(&Some(val)) => return (Some(PrimaryExpression::make_char_lit(val, lexer.pos(index))), 1), 
            Some(&None) => return (Some(PrimaryExpression::make_char_lit('\u{FFFE}', lexer.pos(index))), 1),
            None => (),
        }
        match lexer.nth(index).get_num_lit_val() {
            Some(&Some(ref val)) => return (Some(PrimaryExpression::make_num_lit(val.clone(), lexer.pos(index))), 1), 
            Some(&None) => return (Some(PrimaryExpression::make_num_lit(NumericLiteralValue::I32(0), lexer.pos(index))), 1),
            None => (),
        }
        match lexer.nth(index).get_bool_lit_val() {
            Some(val) => return (Some(PrimaryExpression::make_bool_lit(val, lexer.pos(index))), 1),
            None => (),
        }
        match lexer.nth(index).get_identifier() {
            Some(val) => return (Some(PrimaryExpression::make_ident(val.clone(), lexer.pos(index))), 1),
            None => (),
        }

        test_condition_perrorln!{ log_enable, "parsing primary not literal or identifier" }
        if lexer.nth(index).is_seperator(SeperatorKind::LeftParenthenes) {
            match D3Expression::parse(lexer, index + 1) {
                (None, length) => {
                    test_condition_perrorln!{ log_enable, "parsing paren expression get expression failed" }
                    return (None, 1 + length);
                }
                (Some(expr), expr_len) => {
                    if lexer.nth(index + 1 + expr_len).is_seperator(SeperatorKind::RightParenthenes) {
                        test_condition_perrorln!{ log_enable, "parsing paren successed, paren inner is {}", expr, } 
                        return (
                            Some(PrimaryExpression::make_paren(
                                expr, 
                                StringPosition::from2(lexer.pos(index).start_pos, lexer.pos(index + 1 + expr_len).end_pos)
                            )),
                            1 + expr_len + 1
                        );
                    } else {
                        test_condition_perrorln!{ log_enable, "parsing paren failed, next is not right paren" }
                        return lexer.push_expect("Right paren", index + 1 + expr_len, 1 + expr_len);
                    }
                }
            }
        }

        if lexer.nth(index).is_seperator(SeperatorKind::LeftBracket) {
            match D3Expression::parse(lexer, index + 1) {
                (None, length) => {
                    test_condition_perrorln!{ log_enable, "parsing array (dup) def failed, parse expr1 return none" }
                    return (None, length);  // recover by find paired right bracket
                }
                (Some(expr1), expr1_len) => {
                    test_condition_perrorln!{ log_enable, "parsing array (dup) def get expr1: {} with length {} and next is {:?}", expr1, expr1_len, lexer.nth(index + 1 + expr1_len), }
                    if lexer.nth(index + 1 + expr1_len).is_seperator(SeperatorKind::SemiColon) {
                        match D3Expression::parse(lexer, index + 2 + expr1_len) {
                            (None, length) => {
                                test_condition_perrorln!{ log_enable, "parsing array dup def failed, parse expr2 failed" }
                                return (None, expr1_len + 2 + length);
                            } 
                            (Some(expr2), expr2_len) => {
                                if lexer.nth(index + 2 + expr1_len + expr2_len).is_seperator(SeperatorKind::RightBracket) {
                                    test_condition_perrorln!{ log_enable, "parsing array dup def succeed, expr1: {}, expr2: {}", expr1, expr2, } 
                                    return (
                                        Some(PrimaryExpression::make_array_dup_def(expr1, expr2, 
                                            StringPosition::from2(lexer.pos(index).start_pos, lexer.pos(index + expr1_len + expr2_len + 2).end_pos))),
                                        expr1_len + expr2_len + 3
                                    );
                                } else {
                                    test_condition_perrorln!{ log_enable, "parsing array dup def failed, not followed right bracket" }
                                    return lexer.push_expect("Right bracket after array dup def", index + 3 + expr1_len + expr2_len, expr1_len + expr2_len + 1);
                                }
                            }
                        }
                    }

                    test_condition_perrorln!{ log_enable, "parsing array def, before loop" }
                    let mut current_len = 1 + expr1_len; // 1 for left bracket
                    let mut exprs = vec![expr1];
                    loop {
                        if lexer.nth(index + current_len).is_seperator(SeperatorKind::RightBracket) {
                            test_condition_perrorln!{ log_enable, "parsing array def succeed, exprs: {:?}", exprs, }
                            return (
                                Some(PrimaryExpression::make_array_def(
                                    exprs, 
                                    StringPosition::from2(lexer.pos(index).start_pos, lexer.pos(index + current_len).end_pos)
                                )), 
                                current_len + 1
                            );
                        }
                        if lexer.nth(index + current_len).is_seperator(SeperatorKind::Comma)  // Accept [1, 2, 3, abc, ] 
                            && lexer.nth(index + current_len + 1).is_seperator(SeperatorKind::RightBracket) {
                            test_condition_perrorln!{ log_enable, "parsing array def succeed, exprs: {:?}", exprs, }
                            return (
                                Some(PrimaryExpression::make_array_def(
                                    exprs, 
                                    StringPosition::from2(lexer.pos(index).start_pos, lexer.pos(index + 1 + current_len).end_pos)
                                )), 
                                current_len + 2
                            );
                        }
                        if lexer.nth(index + current_len).is_seperator(SeperatorKind::Comma) {
                            current_len += 1;
                            match D3Expression::parse(lexer, index + current_len) {
                                (Some(exprn), exprn_len) => {
                                    test_condition_perrorln!{ log_enable, "parsing array def, get expression n {}", exprn, }
                                    current_len += exprn_len;
                                    exprs.push(exprn);
                                }
                                (None, length) => {
                                    test_condition_perrorln!{ log_enable, "parsing array def failed, parse expression return none" }
                                    return (None, current_len + length);
                                }
                            }
                        }
                    }
                }
            }
        }

        test_condition_perrorln!{ log_enable, "Failed in prim expr parse, not start with left paren or left bracket" }
        return lexer.push_expect("identifier or literal or array def", index, 0);
    }
} 
