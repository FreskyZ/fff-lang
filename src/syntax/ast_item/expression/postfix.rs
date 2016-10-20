
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
use syntax::Expression;
use syntax::SMType;

use super::primary::PrimaryExpression;

#[derive(Eq, PartialEq)]
pub enum Postfix {
    Subscription(Vec<Expression>),
    FunctionCall(Vec<Expression>),
    MemberAccess(String, StringPosition),
    TypeCast(SMType),
}
impl fmt::Debug for Postfix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Postfix::Subscription(ref exprs) => {
                let mut buf = exprs.iter().fold(".operator()(".to_owned(), 
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
            Postfix::MemberAccess(ref name, ref pos) => format!("operator->({:?} @ {:?})", name, pos),
            Postfix::TypeCast(ref ty) => format!("operator as({:?})", ty),
        })
    }
}
impl fmt::Display for Postfix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Postfix::Subscription(ref exprs) => { 
                let mut buf = exprs.iter().fold(".operator()(".to_owned(), 
                    |mut buf, expr| { buf.push_str(&format!("{:?}, ", expr)); buf });
                buf.push_str(")");
                buf
            },
            Postfix::FunctionCall(ref exprs) => {
                let mut buf = exprs.iter().fold(".operator[](".to_owned(), 
                    |mut buf, expr| { buf.push_str(&format!("{:?}, ", expr)); buf });
                buf.push_str(")");
                buf
            }
            Postfix::MemberAccess(ref name, ref pos) => format!("operator->({:?})", name),
            Postfix::TypeCast(ref ty) => format!("operator as({})", ty),
        })
    }
}

#[derive(Eq, PartialEq)]
pub struct PostfixExpression {
    pub prim: PrimaryExpression,
    pub postfixs: Vec<Postfix>,
}

impl fmt::Debug for PostfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}{:?}", 
            self.prim,
            self.postfixs.iter().fold(String::new(), |mut buf, postfix| { buf.push_str(&format!("{:?}", postfix)); buf })
        )
    }
}
impl fmt::Display for PostfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}{}", 
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

        let (primary, primary_len) = match PrimaryExpression::parse(lexer, index) {
            (Some(prim), prim_len) => (prim, prim_len),
            (None, prim_len) => return (None, prim_len), // no recover
        };
        test_perrorln!("parsing postfix, primary is {:?}", primary, );

        let mut postfixs = Vec::new();
        let mut current_len = primary_len;
        'postfix: loop {
            // function call and subscription both accept expression list, merge the processor and it is the end seperator
            let mut expect_end_sep = SeperatorKind::Add;
            if lexer.nth(index + current_len).is_seperator(SeperatorKind::Dot) {
                match lexer.nth(index + current_len + 1).get_identifier() {
                    Some(ident) => {
                        current_len += 2;
                        postfixs.push(Postfix::MemberAccess(ident.clone(), lexer.pos(index + current_len + 1)));
                        continue 'postfix;
                    }
                    None => return lexer.push_expect("member identifier", index + current_len + 1, current_len + 1),
                }
            } else if lexer.nth(index + current_len).is_keyword(KeywordKind::As) {
                match SMType::parse(lexer, index + current_len + 1) {
                    (Some(ty), ty_len) => {
                        current_len += ty_len + 1;
                        postfixs.push(Postfix::TypeCast(ty));
                        continue 'postfix;
                    }
                    (None, length) => return lexer.push_expect("typedef", index + current_len + 1, current_len + 1 + length),
                }
            } else if lexer.nth(index + current_len).is_seperator(SeperatorKind::LeftParenthenes) {
                if lexer.nth(index + current_len + 1).is_seperator(SeperatorKind::RightParenthenes) {
                    continue 'postfix; // no param function call
                }
                expect_end_sep = SeperatorKind::RightParenthenes;
            } else if lexer.nth(index + current_len).is_seperator(SeperatorKind::LeftBracket) {
                expect_end_sep = SeperatorKind::RightBracket;
            } else {
                break; // other final token, break
            }

            // Get the expression list
            current_len += 1; 
            match Expression::parse(lexer,  index + current_len) {
                (None, length) => return lexer.push_expect("some expression", index + current_len, current_len + length),
                (Some(expr1), expr1_len) => {
                    let mut exprs_len = expr1_len;
                    let mut exprs = vec![expr1];
                    'expr: loop { 
                        match expect_end_sep {
                            SeperatorKind::RightParenthenes if lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::RightParenthenes) => {
                                current_len += exprs_len + 1;
                                postfixs.push(Postfix::FunctionCall(exprs));
                                continue 'postfix;
                            }
                            SeperatorKind::RightBracket if lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::RightBracket)  => {
                                current_len += exprs_len + 1;
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
                            match Expression::parse(lexer, index + current_len + exprs_len) {
                                (Some(expr), expr_len) => {
                                    exprs_len += expr_len + 1;
                                    exprs.push(expr);
                                }
                                (None, length) => return lexer.push_expect("some expr", index + current_len + exprs_len, current_len + exprs_len + length),
                            }
                        }
                    }
                }
            }
        }

        (Some(PostfixExpression{ prim: primary, postfixs: postfixs }), current_len)
    }
}

macro_rules! expr_to_postfix {
    ($prim: expr) => (Expression(PostfixExpression{ prim: $prim, postfixs: Vec::new() }));
    ($prim: expr, $($posts: expr)*) => (
        Expression::Postfix(PostfixExpression{ prim: $prim, postfixs: vec![$($posts, )*]})
    )
}
macro_rules! expr_post_call { ($($exprs: expr, )*) => (Postfix::FunctionCall(vec![$($exprs, )*])) }
macro_rules! expr_post_sub { ($($exprs: expr, )*) => (Postfix::Subscription(vec![$($exprs, )*])) }
macro_rules! expr_post_member { ($name: expr) => (Postfix::MemberAccess($name.to_owned())) }
macro_rules! expr_post_cast { ($ty: expr) => (Postfix::TypeCast($ty)) }

#[cfg(test)]
mod tests {

    #[test]
    fn ast_expr_postfix_parse() {
        use super::PostfixExpression;
        use message::MessageEmitter;
        use lexical::Lexer;
        //use syntax::Expression;
        use syntax::ast_item::IASTItem;
        //use syntax::ast_item::expression::primary::PrimaryExpression;

        // should be
        // temp1 = function call, hij, klm, 123,
        // member access, abc, defg,
        // subscription, top, temp1 
        // function call, top, opq, 456
        // function call, top, empty
        // type cast, top as [i32]
        // member access, top, rst
        // subscription, top, uvw, xyz, ABC
        //                                1  23   45  67  8 9  ABCD  E F  GHI J  KL  MNO  PQ  R S  T U  V
        let lexer = &mut Lexer::new_test("abc.defg[hij(klm, 123)](opq, 456)() as [i32].rst[uvw, xyz, ABC]", MessageEmitter::new());
        let result = PostfixExpression::parse(lexer, 0);
        perrorln!("{:?}", PostfixExpression::parse(lexer, 0));
        perrorln!("Messages: {:?}", lexer.messages());
    }
}