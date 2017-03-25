
// PrimaryExpression = 
//     fIdentifier | fLiteral 
//     | fLeftParen fRighParen                                            // `()`, unit type and unit literal
//     | fLeftParen Expression fRightParen
//     | fLeftParen Expression [fComma Expression]+ fRightParen           // var tuple = (1, "abc", 'd') 
//     | fLeftBracket [Expression [fComma Expression]*] fRightBracket     // var array = [1, 2, 3, a, b, c]
//     | fLeftBracket Expression fSemiColon Expression fRightBracket      // var array = [false; 100]

use std::fmt;

use codepos::StringPosition;
use util::format_vector_debug;
use util::format_vector_display;
use message::Message;
use message::MessageCollection;

use lexical::Lexer;
use lexical::SeperatorKind;
use lexical::KeywordKind;
use lexical::LitValue;

use super::super::ast_item::ISyntaxItem;
use super::super::expression::d3::D3Expression;

#[derive(Eq, PartialEq, Clone)]
pub enum PrimaryExpression {
    Ident(String, StringPosition),
    Lit(LitValue, StringPosition),
    Unit(StringPosition),                                                   // Position for '(', ')'
    ParenExpr(Box<D3Expression>, StringPosition),                           // Position for '(', ')'
    TupleDef(Vec<D3Expression>, StringPosition),                            // Position for '(', ')'
    ArrayDef(Vec<D3Expression>, StringPosition),                            // Position for '[', ']'
    ArrayDupDef(Box<D3Expression>, Box<D3Expression>, [StringPosition; 2]), // Position for '[', ']' and ';'
}

impl fmt::Debug for PrimaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            PrimaryExpression::Ident(ref name, ref pos) => write!(f, "Prim{{ {} @ {:?} }}", name, pos),
            PrimaryExpression::Lit(ref val, ref pos) => write!(f, "Prim{{ {} @ {:?} }}", val, pos),
            PrimaryExpression::Unit(ref pos) => write!(f, "Prim{{ () @ {:?} }}", pos),
            PrimaryExpression::ParenExpr(ref expr, ref pos) => write!(f, "Prim{{ ({:?}) @ {:?} }}", expr, pos),
            PrimaryExpression::TupleDef(ref exprs, ref pos) => write!(f, "Prim{{ ({}) @ {:?} }}", format_vector_debug(exprs, ", "), pos),
            PrimaryExpression::ArrayDef(ref exprs, pos) => write!(f, "Prim{{ [{}] @ {:?} }}", format_vector_debug(exprs, ", "), pos),
            PrimaryExpression::ArrayDupDef(ref expr1, ref expr2, ref pos) => write!(f, "Prim{{ [{:?} ; @ {:?} {:?}] @ {:?} }}", expr1, pos[1], expr2, pos[0]),
        }
    }
}
impl fmt::Display for PrimaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            PrimaryExpression::Ident(ref name, ref _pos) => write!(f, "{}", name),
            PrimaryExpression::Lit(ref val, ref _pos) => write!(f, "{}", val),
            PrimaryExpression::Unit(ref _pos) => write!(f, "()"),
            PrimaryExpression::ParenExpr(ref expr, ref _pos) => write!(f, "({})", expr),
            PrimaryExpression::TupleDef(ref exprs, ref _pos) => write!(f, "({})", format_vector_display(exprs, ", ")),
            PrimaryExpression::ArrayDef(ref exprs, ref _pos) => write!(f, "[{}]", format_vector_display(exprs, ", ")),
            PrimaryExpression::ArrayDupDef(ref expr1, ref expr2, ref _pos) => write!(f, "[{}; {}]", expr1, expr2),
        }
    }
}

impl PrimaryExpression {

    pub fn make_paren(expr: D3Expression, pos: StringPosition) -> PrimaryExpression {
        PrimaryExpression::ParenExpr(Box::new(expr), pos)
    }
    pub fn make_array_dup_def(expr1: D3Expression, expr2: D3Expression, pos: [StringPosition; 2]) -> PrimaryExpression {
        PrimaryExpression::ArrayDupDef(Box::new(expr1), Box::new(expr2), pos)
    }
}

impl ISyntaxItem for PrimaryExpression {
    
    fn pos_all(&self) -> StringPosition {
        match *self {
            PrimaryExpression::Ident(ref _name, pos) => pos,
            PrimaryExpression::Lit(ref _val, pos) => pos,
            PrimaryExpression::Unit(pos) => pos,
            PrimaryExpression::ParenExpr(ref _expr, pos) => pos,
            PrimaryExpression::TupleDef(ref _exprs, pos) => pos, 
            PrimaryExpression::ArrayDef(ref _exprs, pos) => pos,
            PrimaryExpression::ArrayDupDef(ref _expr1, ref _expr2, pos) => pos[0],
        }
    }
    
    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        lexer.nth(index).is_ident()
        || lexer.nth(index).is_lit()
        || lexer.nth(index).is_seperator(SeperatorKind::LeftParenthenes)
        || lexer.nth(index).is_seperator(SeperatorKind::LeftBracket)
    }

    fn parse(lexer: &mut Lexer, messages: &mut MessageCollection, index: usize) -> (Option<PrimaryExpression>, usize) {
        let log_enable = false;

        if lexer.nth(index).is_lit() {
            return (Some(PrimaryExpression::Lit(lexer.nth(index).get_lit_val().unwrap(), lexer.pos(index))), 1);
        }
        match lexer.nth(index).get_identifier() {
            Some(ident) => return (Some(PrimaryExpression::Ident(ident.clone(), lexer.pos(index))), 1), 
            None => (),
        }
        if lexer.nth(index).is_keyword(KeywordKind::This) {
            return (Some(PrimaryExpression::Ident("this".to_owned(), lexer.pos(index))), 1);
        }

        test_condition_perrorln!{ log_enable, "parsing primary not literal or identifier" }
        if lexer.nth(index).is_seperator(SeperatorKind::LeftParenthenes) {
            if lexer.nth(index + 1).is_seperator(SeperatorKind::RightParenthenes) {
                return (Some(PrimaryExpression::Unit(
                    StringPosition::merge(lexer.pos(index), lexer.pos(index + 1))
                )), 2);
            }

            let mut current_len = 1;
            let mut exprs = Vec::new();
            loop {
                match D3Expression::parse(lexer, messages, index + current_len) {
                    (None, length) => {
                        test_condition_perrorln!{ log_enable, "parsing paren expression get expression failed" }
                        return (None, current_len + length);
                    }
                    (Some(expr), expr_len) => {
                        current_len += expr_len;
                        exprs.push(expr);
                    }
                }
                if lexer.nth(index + current_len).is_seperator(SeperatorKind::RightParenthenes) {
                    current_len += 1;
                    break; // Finished
                } else if lexer.nth(index + current_len).is_seperator(SeperatorKind::Comma) 
                    && lexer.nth(index + current_len + 1).is_seperator(SeperatorKind::RightParenthenes) {
                    current_len += 2; 
                    break; // Finished
                } else if lexer.nth(index + current_len).is_seperator(SeperatorKind::Comma) {
                    current_len += 1;
                    continue;
                } else {
                    return push_unexpect!(lexer, messages, "Right paren", index + current_len, current_len);
                }
            }

            let pos = StringPosition::merge(lexer.pos(index), lexer.pos(index + current_len - 1));
            if exprs.len() == 0 {
                unreachable!()
            } else if exprs.len() == 1 {
                return (Some(PrimaryExpression::make_paren(exprs.into_iter().last().unwrap(), pos)), current_len);
            } else {
                return (Some(PrimaryExpression::TupleDef(exprs, pos)), current_len);
            }
        }

        if lexer.nth(index).is_seperator(SeperatorKind::LeftBracket) {
             // Empty array literal, accept in syntax parse, currently denied in codegen
            if lexer.nth(index + 1).is_seperator(SeperatorKind::RightBracket) {
                return (
                    Some(PrimaryExpression::ArrayDef(
                        Vec::new(), 
                        StringPosition::merge(lexer.pos(index), lexer.pos(index + 1)),
                    )), 
                    2
                );
            }
            match D3Expression::parse(lexer, messages, index + 1) {
                (None, length) => {
                    test_condition_perrorln!{ log_enable, "parsing array (dup) def failed, parse expr1 return none" }
                    return (None, length);  // recover by find paired right bracket
                }
                (Some(expr1), expr1_len) => {
                    test_condition_perrorln!{ log_enable, "parsing array (dup) def get expr1: {} with length {} and next is {:?}", expr1, expr1_len, lexer.nth(index + 1 + expr1_len), }
                    if lexer.nth(index + 1 + expr1_len).is_seperator(SeperatorKind::SemiColon) {
                        let semicolon_pos = lexer.pos(index + 1 + expr1_len);
                        match D3Expression::parse(lexer, messages, index + 2 + expr1_len) {
                            (None, length) => {
                                test_condition_perrorln!{ log_enable, "parsing array dup def failed, parse expr2 failed" }
                                return (None, expr1_len + 2 + length);
                            } 
                            (Some(expr2), expr2_len) => {
                                if lexer.nth(index + 2 + expr1_len + expr2_len).is_seperator(SeperatorKind::RightBracket) {
                                    test_condition_perrorln!{ log_enable, "parsing array dup def succeed, expr1: {}, expr2: {}", expr1, expr2, } 
                                    return (
                                        Some(PrimaryExpression::make_array_dup_def(expr1, expr2, [
                                            StringPosition::merge(lexer.pos(index), lexer.pos(index + expr1_len + expr2_len + 2)),
                                            semicolon_pos,
                                        ])),
                                        expr1_len + expr2_len + 3
                                    );
                                } else {
                                    test_condition_perrorln!{ log_enable, "parsing array dup def failed, not followed right bracket" }
                                    return push_unexpect!(lexer, messages, "Right bracket after array dup def", index + 3 + expr1_len + expr2_len, expr1_len + expr2_len + 1);
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
                                Some(PrimaryExpression::ArrayDef(
                                    exprs, 
                                    StringPosition::merge(lexer.pos(index), lexer.pos(index + current_len))
                                )), 
                                current_len + 1
                            );
                        }
                        if lexer.nth(index + current_len).is_seperator(SeperatorKind::Comma)  // Accept [1, 2, 3, abc, ] 
                            && lexer.nth(index + current_len + 1).is_seperator(SeperatorKind::RightBracket) {
                            test_condition_perrorln!{ log_enable, "parsing array def succeed, exprs: {:?}", exprs, }
                            return (
                                Some(PrimaryExpression::ArrayDef(
                                    exprs, 
                                    StringPosition::merge(lexer.pos(index), lexer.pos(index + 1 + current_len))
                                )), 
                                current_len + 2
                            );
                        }
                        if lexer.nth(index + current_len).is_seperator(SeperatorKind::Comma) {
                            current_len += 1;
                            match D3Expression::parse(lexer, messages, index + current_len) {
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
        let _dummy = log_enable;
        return push_unexpect!(lexer, messages, ["identifier", "literal", "array def", ], index, 0);
    }
} 
