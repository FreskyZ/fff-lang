
// PostfixExpression = 
//     PrimaryExpression 
//     PrimaryExpression [
//             fLeftBracket [Expression [fComma Expression]*] fRightBracket
//             | fLeftParen [Expression [fComma Expression]*] fRightParen
//             | fDot fIdentifier
//             | fAs Type
//         ]*

use std::fmt;

use common::From2;
use common::Position;
use common::StringPosition;
use message::Message;

use lexical::Lexer;
use lexical::IToken;
use lexical::SeperatorKind;
use lexical::KeywordKind;

use syntax::ast_item::IASTItem;
use syntax::ast_item::expression::d3::D3Expression;
use syntax::SMType;

use super::primary::PrimaryExpression;

#[derive(Eq, PartialEq, Clone)]
pub enum Postfix {
    Subscription(Vec<D3Expression>),
    FunctionCall(Vec<D3Expression>),
    MemberAccess(String, StringPosition),
    TypeCast(SMType),
}
impl fmt::Debug for Postfix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Postfix::Subscription(ref exprs) => {
                let mut buf = exprs.iter().fold(".operator[](".to_owned(), 
                    |mut buf, expr| { buf.push_str(&format!("{:?}, ", expr)); buf });
                buf.push_str(")");
                buf 
            }
            Postfix::FunctionCall(ref exprs) => {
                let mut buf = exprs.iter().fold(".operator()(".to_owned(), 
                    |mut buf, expr| { buf.push_str(&format!("{:?}, ", expr)); buf });
                buf.push_str(")");
                buf 
            }
            Postfix::MemberAccess(ref name, ref pos) => format!(".operator->({:?} @ {:?})", name, pos),
            Postfix::TypeCast(ref ty) => format!(".operator {:?}()", ty),
        })
    }
}
impl fmt::Display for Postfix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Postfix::Subscription(ref exprs) => { 
                let mut buf = exprs.iter().fold(".operator[](".to_owned(), 
                    |mut buf, expr| { buf.push_str(&format!("{}, ", expr)); buf });
                buf.push_str(")");
                buf
            },
            Postfix::FunctionCall(ref exprs) => {
                let mut buf = exprs.iter().fold(".operator()(".to_owned(), 
                    |mut buf, expr| { buf.push_str(&format!("{}, ", expr)); buf });
                buf.push_str(")");
                buf
            }
            Postfix::MemberAccess(ref name, ref pos) => format!(".operator->({})", name),
            Postfix::TypeCast(ref ty) => format!(".operator {}()", ty),
        })
    }
}

#[derive(Eq, PartialEq, Clone)]
pub struct PostfixExpression {
    pub prim: PrimaryExpression,
    pub postfixs: Vec<Postfix>,
}

impl fmt::Debug for PostfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}{}", 
            self.prim,
            self.postfixs.iter().fold(String::new(), |mut buf, postfix| { buf.push_str(&format!("{:?}", postfix)); buf })
        )
    }
}
impl fmt::Display for PostfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", 
            self.prim,
            self.postfixs.iter().fold(String::new(), |mut buf, postfix| { buf.push_str(&format!("{}", postfix)); buf })
        )
    }
}

impl PostfixExpression {

    pub fn pos_prim(&self) -> StringPosition { self.prim.pos_all() }
}

impl IASTItem for PostfixExpression {
    
    fn pos_all(&self) -> StringPosition { 
        StringPosition::from2(
            self.prim.pos_all().start_pos, 
            self.prim.pos_all().end_pos,    // TODO: finish it
        )
    }

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<PostfixExpression>, usize) {
        let log_enable = false;

        let (primary, primary_len) = match PrimaryExpression::parse(lexer, index) {
            (Some(prim), prim_len) => (prim, prim_len),
            (None, prim_len) => return (None, prim_len), // no recover
        };
        test_condition_perrorln!{ log_enable, "parsing postfix, primary is {}", primary, }

        let mut postfixs = Vec::new();
        let mut current_len = primary_len;
        'postfix: loop {
            // function call and subscription both accept expression list, merge the processor and it is the end seperator
            let mut expect_end_sep = SeperatorKind::Add;
            if lexer.nth(index + current_len).is_seperator(SeperatorKind::Dot) {
                match lexer.nth(index + current_len + 1).get_identifier() {
                    Some(ident) => {
                        test_condition_perrorln!{ log_enable, "get one postfix, member access {:?}", ident, }
                        postfixs.push(Postfix::MemberAccess(
                            ident.clone(), 
                            StringPosition::from2(
                                lexer.pos(index + current_len).start_pos,
                                lexer.pos(index + current_len + 1).end_pos
                            )
                        ));
                        current_len += 2;
                        continue 'postfix;
                    }
                    None => {
                        test_condition_perrorln!{ log_enable, "get postfix failed, member access not followed ident" }
                        return lexer.push_expect("member identifier", index + current_len + 1, current_len + 1);
                    }
                }
            } else if lexer.nth(index + current_len).is_keyword(KeywordKind::As) {
                match SMType::parse(lexer, index + current_len + 1) {
                    (Some(ty), ty_len) => {
                        test_condition_perrorln!{ log_enable, "get one postfix, type cast as {:?}", ty, }
                        current_len += ty_len + 1;
                        postfixs.push(Postfix::TypeCast(ty));
                        continue 'postfix;
                    }
                    (None, length) => {
                        test_condition_perrorln!{ log_enable, "get postfix failed, type cast not followed typedef" }
                        return (None, current_len + 1 + length);  
                    } 
                }
            } else if lexer.nth(index + current_len).is_seperator(SeperatorKind::LeftParenthenes) {
                if lexer.nth(index + current_len + 1).is_seperator(SeperatorKind::RightParenthenes) {
                    test_condition_perrorln!{ log_enable, "get one postfix, none parameter function call" }
                    current_len += 2;
                    postfixs.push(Postfix::FunctionCall(Vec::new()));
                    continue 'postfix; // no param function call
                }
                expect_end_sep = SeperatorKind::RightParenthenes;
            } else if lexer.nth(index + current_len).is_seperator(SeperatorKind::LeftBracket) {
                expect_end_sep = SeperatorKind::RightBracket;
            } else {
                break; // other final token, break
            }

            // Get the expression list
            test_condition_perrorln!{ log_enable, "parsing postfix, start processing expression list of {}", 
                if expect_end_sep == SeperatorKind::RightParenthenes { "function call".to_owned() } else { "subscription".to_owned() }, }
            current_len += 1; 
            match D3Expression::parse(lexer,  index + current_len) {
                (None, length) => { 
                    test_condition_perrorln!{ log_enable, "parsing postfix's expression list, expression parse failed" }
                    return (None, current_len + length);
                }
                (Some(expr1), expr1_len) => {
                    let mut exprs_len = expr1_len;
                    let mut exprs = vec![expr1];
                    'expr: loop { 
                        match expect_end_sep {
                            SeperatorKind::RightParenthenes 
                                if lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::RightParenthenes) 
                                    || (lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::Comma)
                                        && lexer.nth(index + current_len + exprs_len + 1).is_seperator(SeperatorKind::RightParenthenes))  => {
                                test_condition_perrorln!{ log_enable, "parsing postfix function call's expression list finished" }
                                current_len += exprs_len + if lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::Comma) { 2 } else { 1 };
                                postfixs.push(Postfix::FunctionCall(exprs));
                                continue 'postfix;
                            }
                            SeperatorKind::RightBracket 
                                if lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::RightBracket)
                                    || (lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::Comma)
                                        && lexer.nth(index + current_len + exprs_len + 1).is_seperator(SeperatorKind::RightBracket)) => {
                                test_condition_perrorln!{ log_enable, "parsing postfix subscription's expression list finished" }
                                current_len += exprs_len + if lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::Comma) { 2 } else { 1 };
                                // first recoverable error here!!!
                                if exprs.is_empty() {
                                    lexer.push(Message::EmptySubscription{ pos: Position::new() }); // TODO: Position infrastructure for syntax
                                }
                                postfixs.push(Postfix::Subscription(exprs));
                                continue 'postfix;
                            }
                            SeperatorKind::RightParenthenes | SeperatorKind::RightBracket => (),
                            _ => unreachable!()
                        }
                        if lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::Comma) {
                            exprs_len += 1;
                            match D3Expression::parse(lexer, index + current_len + exprs_len) {
                                (Some(expr), expr_len) => {
                                    test_condition_perrorln!{ log_enable, "parsing postfix's expression list, get expression: {}", expr, }
                                    exprs_len += expr_len;
                                    exprs.push(expr);
                                }
                                (None, length) => {
                                    test_condition_perrorln!{ log_enable, "parsing postfix's expression list failed, get none expression" }
                                    return (None, current_len + exprs_len + length);
                                }
                            }
                        }
                    }
                }
            }
        }

        test_condition_perrorln!{ log_enable, "parsing postfixs finished, get postfixes: {:?}", postfixs, }
        (Some(PostfixExpression{ prim: primary, postfixs: postfixs }), current_len)
    }
}
