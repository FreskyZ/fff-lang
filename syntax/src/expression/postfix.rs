
// PostfixExpression = 
//     PrimaryExpression 
//     PrimaryExpression [
//             fLeftBracket [Expression [fComma Expression]*] fRightBracket
//             | fLeftParen [Expression [fComma Expression]*] fRightParen
//             | fDot fIdentifier
//             | fAs Type
//         ]*

use std::fmt;

use codepos::StringPosition;
use message::SyntaxMessage;
use message::Message;
use message::MessageCollection;

use lexical::Lexer;
use lexical::SeperatorKind;
use lexical::KeywordKind;

use super::super::ISyntaxItem;
use super::super::ISyntaxItemFormat;
use super::super::expression::d3::D3Expression;
use super::super::SMType;

use super::primary::PrimaryExpression;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Postfix {
    Subscription(Vec<D3Expression>, StringPosition),  // '[', ']' position
    FunctionCall(Vec<D3Expression>, StringPosition),  // '(', ')' position
    MemberAccess(String, StringPosition),             // '.xxx' position
    TypeCast(SMType, StringPosition),                 // 'as' position
}
impl Postfix {
    pub fn pos(&self) -> StringPosition {
        match *self {
            Postfix::Subscription(ref _exprs, ref pos) => *pos,
            Postfix::FunctionCall(ref _exprs, ref pos) => *pos,
            Postfix::MemberAccess(ref _name, ref pos) => *pos,
            Postfix::TypeCast(ref ty, ref pos) => StringPosition::merge(*pos, ty.pos_all()),
        }
    }
}

#[derive(Eq, PartialEq, Clone)]
pub struct PostfixExpression {
    pub prim: PrimaryExpression,
    pub postfixs: Vec<Postfix>,
}
impl ISyntaxItemFormat for PostfixExpression {
    fn format(&self, indent: u32) -> String {
        if self.postfixs.len() == 0 {
            format!("{}", self.prim.format(indent))
        } else {
            format!("{}PostfixExpr:\n{}{}", 
                PostfixExpression::indent_str(indent),
                self.prim.format(indent + 1),
                self.postfixs.iter().fold(String::new(), |mut buf, postfix| { buf.push_str("\n"); buf.push_str(&match postfix {
                    &Postfix::Subscription(ref exprs, ref strpos) => 
                        format!("{}Subscription\n{}Bracket at <{:?}>{}", 
                            PostfixExpression::indent_str(indent + 1), PostfixExpression::indent_str(indent + 2),
                            strpos, exprs.iter().fold(String::new(), |mut buf, expr| { buf.push_str("\n"); buf.push_str(&expr.format(indent + 2)); buf })),
                    &Postfix::FunctionCall(ref exprs, ref strpos) => 
                        format!("{}FunctionCall\n{}Paren at <{:?}>{}", 
                            PostfixExpression::indent_str(indent + 1), PostfixExpression::indent_str(indent + 2),
                            strpos, exprs.iter().fold(String::new(), |mut buf, expr| { buf.push_str("\n"); buf.push_str(&expr.format(indent + 2)); buf })),
                    &Postfix::MemberAccess(ref name, ref strpos) => 
                        format!("{}AccessMember: {} <{:?}>", PostfixExpression::indent_str(indent + 1), name, strpos),
                    &Postfix::TypeCast(ref type_use, ref strpos) => 
                        format!("{}CastAs\n{}As at <{:?}>\n{}", 
                            PostfixExpression::indent_str(indent + 1), PostfixExpression::indent_str(indent + 2), strpos, type_use.format(indent + 2)),
                }); buf }),
            )
        }
    }
}
impl fmt::Debug for PostfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n{}", self.format(0))
    }
}

impl PostfixExpression {

    pub fn pos_prim(&self) -> StringPosition { self.prim.pos_all() }
}

impl ISyntaxItem for PostfixExpression {
    
    fn pos_all(&self) -> StringPosition { 
        StringPosition::merge(
            self.prim.pos_all(), 
            match self.postfixs.iter().last() {
                Some(&Postfix::Subscription(ref _exprs, ref pos)) | Some(&Postfix::FunctionCall(ref _exprs, ref pos)) => *pos,
                Some(&Postfix::MemberAccess(ref _name, ref pos)) => *pos,
                Some(&Postfix::TypeCast(ref ty, ref _pos)) => ty.pos_all(),
                None => self.prim.pos_all(),
            }
        )
    }

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        PrimaryExpression::is_first_final(lexer, index)
    }
    
    #[allow(unused_assignments)]
    fn parse(lexer: &mut Lexer, messages: &mut MessageCollection, index: usize) -> (Option<PostfixExpression>, usize) {
        
        #[cfg(feature = "trace_postfix_expr_parse")]
        macro_rules! trace_to_stderr { ($($arg:tt)*) => ({ perror!("[PostfixExpr]"); perrorln!($($arg)*); }) }
        #[cfg(not(feature = "trace_postfix_expr_parse"))]
        macro_rules! trace_to_stderr { ($($arg:tt)*) => () }

        let (primary, primary_len) = match PrimaryExpression::parse(lexer, messages, index) {
            (Some(prim), prim_len) => (prim, prim_len),
            (None, prim_len) => return (None, prim_len), // no recover
        };
        trace_to_stderr!("parsing postfix, primary is {}", primary);

        let mut postfixs = Vec::new();
        let mut current_len = primary_len;
        'postfix: loop {
            // function call and subscription both accept expression list, merge the processor and it is the end seperator
            let mut expect_end_sep = SeperatorKind::Add;
            if lexer.nth(index + current_len).is_seperator(SeperatorKind::Dot) {
                match lexer.nth(index + current_len + 1).get_identifier() {
                    Some(ident) => {
                        trace_to_stderr!{ "get one postfix, member access {:?}", ident, }
                        postfixs.push(Postfix::MemberAccess(
                            ident.clone(), 
                            StringPosition::merge(
                                lexer.pos(index + current_len),
                                lexer.pos(index + current_len + 1)
                            )
                        ));
                        current_len += 2;
                        continue 'postfix;
                    }
                    None => {
                        trace_to_stderr!{ "get postfix failed, member access not followed ident" }
                        return push_unexpect!(lexer, messages, "member identifier", index + current_len + 1, current_len + 1);
                    }
                }
            } else if lexer.nth(index + current_len).is_keyword(KeywordKind::As) {
                match SMType::parse(lexer, messages, index + current_len + 1) {
                    (Some(ty), ty_len) => {
                        trace_to_stderr!{ "get one postfix, type cast as {:?}", ty, }
                        postfixs.push(Postfix::TypeCast(ty, lexer.pos(index + current_len)));
                        current_len += ty_len + 1;
                        continue 'postfix;
                    }
                    (None, length) => {
                        trace_to_stderr!{ "get postfix failed, type cast not followed typedef" }
                        return (None, current_len + 1 + length);  
                    } 
                }
            } else if lexer.nth(index + current_len).is_seperator(SeperatorKind::LeftParenthenes) {
                if lexer.nth(index + current_len + 1).is_seperator(SeperatorKind::RightParenthenes) {
                    trace_to_stderr!{ "get one postfix, none parameter function call" }
                    postfixs.push(Postfix::FunctionCall(
                        Vec::new(), 
                        StringPosition::merge(lexer.pos(index + current_len), lexer.pos(index + current_len + 1))
                    ));
                    current_len += 2;
                    continue 'postfix; // no param function call
                }
                if lexer.nth(index + current_len + 1).is_seperator(SeperatorKind::Comma) 
                    && lexer.nth(index + current_len + 2).is_seperator(SeperatorKind::RightParenthenes) {
                        let pos1 = StringPosition::merge(lexer.pos(index + current_len), lexer.pos(index + current_len + 2));
                        let pos2 = lexer.pos(index + current_len + 1).start_pos();
                        messages.push(SyntaxMessage::SingleCommaInFunctionCall{ call_pos: pos1, comma_pos: pos2 });
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
                    messages.push(SyntaxMessage::EmptySubscription{ pos: pos });
                    postfixs.push(Postfix::Subscription(
                        Vec::new(),
                        StringPosition::merge(lexer.pos(index + current_len), lexer.pos(index + current_len + 1)),
                    ));
                    current_len += 2;
                    continue 'postfix;
                }
                if lexer.nth(index + current_len + 1).is_seperator(SeperatorKind::Comma) 
                    && lexer.nth(index + current_len + 2).is_seperator(SeperatorKind::RightBracket) {
                        let pos1 = StringPosition::merge(lexer.pos(index + current_len), lexer.pos(index + current_len + 2));
                        let pos2 = lexer.pos(index + current_len + 1).start_pos();
                        messages.push(SyntaxMessage::SingleCommaInSubscription{ sub_pos: pos1, comma_pos: pos2 });
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
            trace_to_stderr!{ "parsing postfix, start processing expression list of {}", 
                if expect_end_sep == SeperatorKind::RightParenthenes { "function call".to_owned() } else { "subscription".to_owned() }, }
            current_len += 1; 
            match D3Expression::parse(lexer, messages, index + current_len) {
                (None, length) => { 
                    trace_to_stderr!{ "parsing postfix's expression list, expression parse failed" }
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
                                trace_to_stderr!{ "parsing postfix function call's expression list finished" }
                                let pos_left_paren = lexer.pos(index + current_len - 1);
                                current_len += exprs_len + if lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::Comma) { 2 } else { 1 };
                                let pos_right_paren = lexer.pos(index + current_len - 1);
                                postfixs.push(Postfix::FunctionCall(
                                    exprs,
                                    StringPosition::merge(pos_left_paren, pos_right_paren)
                                ));
                                continue 'postfix;
                            }
                            SeperatorKind::RightBracket 
                                if lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::RightBracket)
                                    || (lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::Comma)
                                        && lexer.nth(index + current_len + exprs_len + 1).is_seperator(SeperatorKind::RightBracket)) => {
                                trace_to_stderr!{ "parsing postfix subscription's expression list finished" }
                                let pos_left_bracket = lexer.pos(index + current_len - 1);
                                current_len += exprs_len + if lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::Comma) { 2 } else { 1 };
                                let pos_right_bracket = lexer.pos(index + current_len - 1);
                                postfixs.push(Postfix::Subscription(
                                    exprs,
                                    StringPosition::merge(pos_left_bracket, pos_right_bracket)
                                ));
                                continue 'postfix;
                            }
                            SeperatorKind::RightParenthenes | SeperatorKind::RightBracket => (),
                            _ => unreachable!()
                        }
                        if lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::Comma) {
                            exprs_len += 1;
                            match D3Expression::parse(lexer, messages, index + current_len + exprs_len) {
                                (Some(expr), expr_len) => {
                                    trace_to_stderr!{ "parsing postfix's expression list, get expression: {}", expr, }
                                    exprs_len += expr_len;
                                    exprs.push(expr);
                                }
                                (None, length) => {
                                    trace_to_stderr!{ "parsing postfix's expression list failed, get none expression" }
                                    return (None, current_len + exprs_len + length);
                                }
                            }
                        }
                    }
                }
            }
        }

        trace_to_stderr!{ "parsing postfixs finished, get postfixes: {:?}", postfixs, }
        (Some(PostfixExpression{ prim: primary, postfixs: postfixs }), current_len)
    }
}
