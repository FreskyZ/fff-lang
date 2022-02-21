
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
use common::StringPosition;
use common::format_vector_display;
use common::format_vector_debug;
use message::SyntaxMessage as Message;

use lexical::Lexer;
use lexical::SeperatorKind;
use lexical::KeywordKind;

use syntax::ast_item::IASTItem;
use syntax::expression::d3::D3Expression;
use syntax::SMType;

use super::primary::PrimaryExpression;

#[derive(Eq, PartialEq, Clone)]
pub enum Postfix {
    Subscription(Vec<D3Expression>, StringPosition),  // '[', ']' position
    FunctionCall(Vec<D3Expression>, StringPosition),  // '(', ')' position
    MemberAccess(String, StringPosition),             // '.xxx' position
    TypeCast(SMType, StringPosition),                 // 'as' position
}
impl fmt::Debug for Postfix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Postfix::Subscription(ref exprs, ref pos) => write!(f, ".operator[] @ {:?} ({})", pos, format_vector_debug(exprs, ", ")),
            Postfix::FunctionCall(ref exprs, ref pos) => write!(f, ".operator() @ {:?} ({})", pos, format_vector_debug(exprs, ", ")),
            Postfix::MemberAccess(ref name, ref pos) => write!(f, ".{} @ {:?}", name, pos),
            Postfix::TypeCast(ref ty, ref pos) => write!(f, ".operator {:?}() @ {:?}", ty, pos),
        }
    }
}
impl fmt::Display for Postfix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Postfix::Subscription(ref exprs, ref _pos) => write!(f, ".operator[]({})", format_vector_display(exprs, ", ")),
            Postfix::FunctionCall(ref exprs, ref _pos) => write!(f, ".operator()({})", format_vector_display(exprs, ", ")),
            Postfix::MemberAccess(ref name, ref _pos) => write!(f, ".{}", name),
            Postfix::TypeCast(ref ty, ref _pos) => write!(f, ".operator {}()", ty),
        }
    }
}
impl Postfix {
    pub fn pos(&self) -> StringPosition {
        match *self {
            Postfix::Subscription(ref _exprs, ref pos) => *pos,
            Postfix::FunctionCall(ref _exprs, ref pos) => *pos,
            Postfix::MemberAccess(ref _name, ref pos) => *pos,
            Postfix::TypeCast(ref ty, ref pos) => StringPosition::from2(pos.start_pos, ty.pos_all().end_pos),
        }
    }
}

#[derive(Eq, PartialEq, Clone)]
pub struct PostfixExpression {
    pub prim: PrimaryExpression,
    pub postfixs: Vec<Postfix>,
}

impl fmt::Debug for PostfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Post{{ {:?}{} }}", self.prim, format_vector_debug(&self.postfixs, ""))
    }
}
impl fmt::Display for PostfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.prim, format_vector_display(&self.postfixs, ""))
    }
}

impl PostfixExpression {

    pub fn pos_prim(&self) -> StringPosition { self.prim.pos_all() }
}

impl IASTItem for PostfixExpression {
    
    fn pos_all(&self) -> StringPosition { 
        StringPosition::from2(
            self.prim.pos_all().start_pos, 
            match self.postfixs.iter().last() {
                Some(&Postfix::Subscription(ref _exprs, ref pos)) | Some(&Postfix::FunctionCall(ref _exprs, ref pos)) => pos.end_pos,
                Some(&Postfix::MemberAccess(ref _name, ref pos)) => pos.end_pos,
                Some(&Postfix::TypeCast(ref ty, ref _pos)) => ty.pos_all().end_pos,
                None => self.prim.pos_all().end_pos,
            }
        )
    }

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        PrimaryExpression::is_first_final(lexer, index)
    }
    
    #[allow(unused_assignments)]
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
                        postfixs.push(Postfix::TypeCast(ty, lexer.pos(index + current_len)));
                        current_len += ty_len + 1;
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
                    postfixs.push(Postfix::FunctionCall(
                        Vec::new(), 
                        StringPosition::from2(lexer.pos(index + current_len).start_pos, lexer.pos(index + current_len + 1).end_pos)
                    ));
                    current_len += 2;
                    continue 'postfix; // no param function call
                }
                if lexer.nth(index + current_len + 1).is_seperator(SeperatorKind::Comma) 
                    && lexer.nth(index + current_len + 2).is_seperator(SeperatorKind::RightParenthenes) {
                        let pos1 = StringPosition::from2(lexer.pos(index + current_len).start_pos, lexer.pos(index + current_len + 2).end_pos);
                        let pos2 = lexer.pos(index + current_len + 1).start_pos;
                        lexer.push(Message::SingleCommaInFunctionCall{ call_pos: pos1, comma_pos: pos2 });
                        postfixs.push(Postfix::FunctionCall(
                            Vec::new(),
                            pos1,
                        ));
                        current_len += 3;
                        continue 'postfix;
                }
                expect_end_sep = SeperatorKind::RightParenthenes;
            } else if lexer.nth(index + current_len).is_seperator(SeperatorKind::LeftBracket) {
                if lexer.nth(index + current_len + 1).is_seperator(SeperatorKind::RightBracket) {
                    // first recoverable error here!!!, it is recoverable because later with type infer it can be used
                    let pos = lexer.pos(index + current_len);
                    lexer.push(Message::EmptySubscription{ pos: pos });
                    postfixs.push(Postfix::Subscription(
                        Vec::new(),
                        StringPosition::from2(lexer.pos(index + current_len).start_pos, lexer.pos(index + current_len + 1).end_pos),
                    ));
                    current_len += 2;
                    continue 'postfix;
                }
                if lexer.nth(index + current_len + 1).is_seperator(SeperatorKind::Comma) 
                    && lexer.nth(index + current_len + 2).is_seperator(SeperatorKind::RightBracket) {
                        let pos1 = StringPosition::from2(lexer.pos(index + current_len).start_pos, lexer.pos(index + current_len + 2).end_pos);
                        let pos2 = lexer.pos(index + current_len + 1).start_pos;
                        lexer.push(Message::SingleCommaInSubscription{ sub_pos: pos1, comma_pos: pos2 });
                        postfixs.push(Postfix::Subscription(
                            Vec::new(),
                            pos1,
                        ));
                        current_len += 3;
                        continue 'postfix;
                }
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
                    loop { 
                        match expect_end_sep {
                            SeperatorKind::RightParenthenes 
                                if lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::RightParenthenes) 
                                    || (lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::Comma)
                                        && lexer.nth(index + current_len + exprs_len + 1).is_seperator(SeperatorKind::RightParenthenes))  => {
                                test_condition_perrorln!{ log_enable, "parsing postfix function call's expression list finished" }
                                let pos_left_paren = lexer.pos(index + current_len - 1);
                                current_len += exprs_len + if lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::Comma) { 2 } else { 1 };
                                let pos_right_paren = lexer.pos(index + current_len - 1);
                                postfixs.push(Postfix::FunctionCall(
                                    exprs,
                                    StringPosition::from2(pos_left_paren.start_pos, pos_right_paren.end_pos)
                                ));
                                continue 'postfix;
                            }
                            SeperatorKind::RightBracket 
                                if lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::RightBracket)
                                    || (lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::Comma)
                                        && lexer.nth(index + current_len + exprs_len + 1).is_seperator(SeperatorKind::RightBracket)) => {
                                test_condition_perrorln!{ log_enable, "parsing postfix subscription's expression list finished" }
                                let pos_left_bracket = lexer.pos(index + current_len - 1);
                                current_len += exprs_len + if lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::Comma) { 2 } else { 1 };
                                let pos_right_bracket = lexer.pos(index + current_len - 1);
                                postfixs.push(Postfix::Subscription(
                                    exprs,
                                    StringPosition::from2(pos_left_bracket.start_pos, pos_right_bracket.end_pos)
                                ));
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
        let _dummy = log_enable;
        (Some(PostfixExpression{ prim: primary, postfixs: postfixs }), current_len)
    }
}
