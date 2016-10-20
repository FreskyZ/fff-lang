
// PrimaryExpression = 
//     fLeftParen Expression fRightParen 
//     | fIdentifier | fLiteral 
//     | fLeftBracket [Expression [fComma Expression]*] fRightBracket     // var array = [1, 2, 3, a, b, c]
//     | fLeftBracket Expression fSemiColon Expression fRightBracket // var array = [false; 100]

use common::From2;
use common::StringPosition;

use lexical::Lexer;
use lexical::IToken;
use lexical::NumericLiteralValue;
use lexical::SeperatorKind;

use syntax::ast_item::IASTItem;
use syntax::Expression;

#[derive(Debug, Eq, PartialEq)]
pub enum PrimaryExpressionBase {
    Identifier(String),
    StringLiteral(String),
    CharLiteral(char),
    NumericLiteral(NumericLiteralValue),
    BooleanLiteral(bool),
    ParenExpression(Box<Expression>),
    ArrayDef(Vec<Expression>),
    ArrayDupDef(Box<Expression>, Box<Expression>),
}

#[derive(Debug, Eq, PartialEq)]
pub struct PrimaryExpression(PrimaryExpressionBase, StringPosition);

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

    pub fn make_paren(expr: Expression, pos: StringPosition) -> PrimaryExpression {
        PrimaryExpression(PrimaryExpressionBase::ParenExpression(Box::new(expr)), pos)
    }
    pub fn make_array_def(exprs: Vec<Expression>, pos: StringPosition) -> PrimaryExpression {
        PrimaryExpression(PrimaryExpressionBase::ArrayDef(exprs), pos)
    }
    pub fn make_array_dup_def(expr1: Expression, expr2: Expression, pos: StringPosition) -> PrimaryExpression {
        PrimaryExpression(PrimaryExpressionBase::ArrayDupDef(Box::new(expr1), Box::new(expr2)), pos)
    }

    pub fn pos_all(&self) -> StringPosition {
        self.1
    }
}

impl IASTItem for PrimaryExpression {
    
    fn parse(lexer: &mut Lexer, index: usize) -> (Option<PrimaryExpression>, usize) {

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

        perrorln!("parsing prim expr at index {} with token {:?} not literal or identifier", index, lexer.nth(index));
        if lexer.nth(index).is_seperator(SeperatorKind::LeftParenthenes) {
            match Expression::parse(lexer, index + 1) {
                (None, length) => return test_perrorln_and_val!("Paren expr failed, expect expr"; 
                    lexer.push_expect("Some expression", index + 1, 1 + length)),
                (Some(expr), expr_len) => {
                    if lexer.nth(index + 1 + expr_len).is_seperator(SeperatorKind::RightParenthenes) {
                        return test_perrorln_and_val!("Paren expr successed"; 
                            (
                                Some(PrimaryExpression::make_paren(
                                    expr, 
                                    StringPosition::from2(lexer.pos(index).start_pos, lexer.pos(index + 1 + expr_len).end_pos)
                                )),
                                1 + expr_len 
                            )
                        );
                    } else {
                        return test_perrorln_and_val!("Paren expr failed, expect right paren"; 
                            lexer.push_expect("Right paren", index + 1 + expr_len, 1 + expr_len));
                    }
                }
            }
        }

        if lexer.nth(index).is_seperator(SeperatorKind::LeftBracket) {
            match Expression::parse(lexer, index + 1) {
                (None, length) => return (None, length),  // recover by find paired right bracket
                (Some(expr1), expr1_len) => {
                    if lexer.nth(index + 1 + expr1_len).is_seperator(SeperatorKind::SemiColon) {
                        match Expression::parse(lexer, index + 2 + expr1_len) {
                            (None, length) => return (None, expr1_len + 2 + length),
                            (Some(expr2), expr2_len) => {
                                if lexer.nth(index + 2 + expr1_len + expr2_len).is_seperator(SeperatorKind::RightBracket) {
                                    return test_perrorln_and_val!("Success in array dup def"; 
                                        (
                                            Some(PrimaryExpression::make_array_dup_def(expr1, expr2, 
                                                StringPosition::from2(lexer.pos(index).start_pos, lexer.pos(index + expr1_len + expr2_len + 2).end_pos))),
                                            expr1_len + expr2_len + 3
                                        )
                                    );
                                } else {
                                    return test_perrorln_and_val!("expect expr after semicolon in array dup def";
                                        lexer.push_expect("Right bracket after array dup def", index + 3 + expr1_len + expr2_len, expr1_len + expr2_len + 1));
                                }
                            }
                        }
                    }

                    let mut current_len = expr1_len;
                    let mut exprs = vec![expr1];
                    loop {
                        if lexer.nth(index + 1 + current_len).is_seperator(SeperatorKind::RightBracket) {
                            return test_perrorln_and_val!("Success in array def"; 
                                (Some(PrimaryExpression::make_array_def(exprs, lexer.pos(index + 1 + current_len))), current_len + 1));
                        }
                        if lexer.nth(index + 1 + current_len).is_seperator(SeperatorKind::Comma) {
                            current_len += 1;
                            match Expression::parse(lexer, index + 1 + current_len) {
                                (Some(exprn), exprn_len) => {
                                    current_len += exprn_len;
                                    exprs.push(exprn);
                                }
                                (None, length) => test_perrorln_and_val!("Fail in array def, expect some expr after comma";
                                    return lexer.push_expect("Some expression", index + 1 + current_len + length, current_len + 1 + length)),
                            }
                        }
                    }
                }
            }
        }

        perrorln!("Failed in prim expr parse, not start with left paren or left bracket");
        return lexer.push_expect("identifier or literal or array def", index, 0);
    }
} 

macro_rules! expr_to_primary {
    ($inner: expr) => (Expression(PostfixExpression{ prim: $inner, postfixs: Vec::new() }));
}
macro_rules! expr_ident { 
    ($name: expr) => (expr_to_primary!(PrimaryExpression::make_ident($name.to_owned(), StringPosition::new()))) 
}
macro_rules! expr_str_lit { 
    ($val: expr) => (expr_to_primary!(PrimaryExpression::make_str_lit($val.to_owned(), StringPosition::new()))) 
}
macro_rules! expr_char_lit { 
    ($val: expr) => (expr_to_primary!(PrimaryExpression::make_char_lit($val, StringPosition::new()))) 
}
macro_rules! expr_num_lit { 
    ($val: expr) => (expr_to_primary!(PrimaryExpression::make_num_lit($val, StringPosition::new()))) 
}
macro_rules! expr_bool_lit { 
    ($val: expr) => (expr_to_primary!(PrimaryExpression::make_bool_lit($val, StringPosition::new()))) 
}
macro_rules! expr_paren_expr { 
    ($expr: expr) => (expr_to_primary!(PrimaryExpression::make_paren($expr, StringPosition::new()))) 
}
macro_rules! expr_array_def { 
    ($($exprs: expr, )*) => (expr_to_primary!(PrimaryExpression::make_array_def(vec![$($exprs, )*], StringPosition::new()))) 
}
macro_rules! expr_array_dup_def { 
    ($expr1: expr, $expr2: expr) => (expr_to_primary!(PrimaryExpression::make_array_dup_def($expr1, $expr2, StringPosition::new()))); 
}

#[cfg(test)]
mod tests {

    #[test]
    fn ast_expr_prim_parse() {
        use super::PrimaryExpression;
        use super::PrimaryExpressionBase;
        use syntax::ast_item::IASTItem;
        use common::StringPosition;
        use message::MessageEmitter;
        use lexical::Lexer;
        use syntax::Expression;
        use lexical::NumericLiteralValue;
        use syntax::ast_item::expression::postfix::Postfix;
        use syntax::ast_item::expression::postfix::PostfixExpression;

        macro_rules! parse {
            ($program: expr) => (PrimaryExpression::parse(&mut Lexer::new_test($program, MessageEmitter::new()), 0))
        }

        // Case 1
        assert_eq!(
            expr_to_primary!(parse!("[1, 2, 3f128, 0u64]").0.unwrap()), 
            expr_array_def!{
                expr_num_lit!(NumericLiteralValue::I32(1)),
                expr_num_lit!(NumericLiteralValue::I32(2)), 
                expr_num_lit!(NumericLiteralValue::I32(0)),
                expr_num_lit!(NumericLiteralValue::U64(0)),
            }
        );

        // Case 2
                       // 0123456 78  9 ABCDE FGH I    JKLMN
        let res = parse!("[[(1)], [abc, (3)], [4, defg, [6]]]");
        match res.0.unwrap() {
            PrimaryExpression(PrimaryExpressionBase::ArrayDef(exprs), _pos) => {
                assert_eq!(exprs[0], expr_array_def!{ expr_paren_expr!(expr_num_lit!(NumericLiteralValue::I32(1))), });
                assert_eq!(exprs[1], 
                    expr_array_def!{
                        expr_ident!("abc"),
                        expr_paren_expr!(expr_num_lit!(NumericLiteralValue::I32(3))),
                    }
                );
                assert_eq!(exprs[2], 
                    expr_array_def!{
                        expr_num_lit!(NumericLiteralValue::I32(4)),
                        expr_ident!("defg"),
                        expr_array_def!{
                            expr_num_lit!(NumericLiteralValue::I32(6)),
                        },
                    }
                );
            }
            other => panic!("Not expected, but {:?}", other),
        }

        // Case 3
        assert_eq!(
            expr_to_primary!(parse!("[abc, 123, \"456\", '\\u0065', false, (a)]").0.unwrap()),
            expr_array_def!{
                expr_ident!("abc"),
                expr_num_lit!(NumericLiteralValue::I32(123)),
                expr_str_lit!("456"),
                expr_char_lit!('\u{0065}'),
                expr_bool_lit!(false),
                expr_paren_expr!(expr_ident!("a")),
            }
        );        
        
        // Case 4
        assert_eq!(
            expr_to_primary!(parse!("[abc, 123f, \"456\\u\", '\\u00', false, (a)]").0.unwrap()),
            expr_array_def!{
                expr_ident!("abc"),
                expr_num_lit!(NumericLiteralValue::I32(0)),
                expr_str_lit!("<invalid>"),
                expr_char_lit!('\u{FFFE}'),
                expr_bool_lit!(false),
                expr_paren_expr!(expr_ident!("a")),
            }
        );
    }
}