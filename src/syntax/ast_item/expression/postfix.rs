
// PostfixExpression = 
//     PrimaryExpression 
//     PrimaryExpression [
//             fLeftBracket [Expression [fComma Expression]*] fRightBracket
//             | fLeftParen [Expression [fComma Expression]*] fRightParen
//             | fDot fIdentifier
//             | fAs Type
//         ]*

use common::Position;
use message::Message;

use lexical::Lexer;
use lexical::IToken;
use lexical::SeperatorKind;
use lexical::KeywordKind;

use syntax::ast_item::IASTItem;
use syntax::Expression;
use syntax::SMType;

use super::primary::PrimaryExpression;

#[derive(Debug, Eq, PartialEq)]
pub enum Postfix {
    Subscription(Vec<Expression>),
    FunctionCall(Vec<Expression>),
    MemberAccess(String),
    TypeCast(SMType),
}

#[derive(Debug, Eq, PartialEq)]
pub struct PostfixExpression {
    pub prim: PrimaryExpression,
    pub postfixs: Vec<Postfix>,
}

impl PostfixExpression {

}

impl IASTItem for PostfixExpression {

    fn symbol_len(&self) -> usize {
        self.prim.symbol_len() + self.postfixs.iter().fold(0, |counter, ref postfix| {
            counter + match postfix {
                //                                    1 for '[', ']' or '(', ')', and over added ','
                &&Postfix::Subscription(ref indexers) => indexers.iter().fold(0, |counter, ref indexer| counter + indexer.symbol_len() + 1) + 1,
                &&Postfix::FunctionCall(ref params) => params.iter().fold(0, |counter, ref param| counter + param.symbol_len() + 1) + 1, 
                &&Postfix::MemberAccess(ref ident) => 2,
                &&Postfix::TypeCast(ref ty) => 1 + ty.symbol_len(),
            }
        }) 
    }

    fn parse(lexer: &mut Lexer, index: usize) -> Option<PostfixExpression> {

        let primary = match PrimaryExpression::parse(lexer, index) {
            Some(prim) => prim,
            None => return lexer.push_expect_symbol("primary expression", index),
        };
        test_perrorln!("parsing postfix, primary is {:?}", primary, );

        let mut postfixs = Vec::new();
        let mut current_len = primary.symbol_len();
        'postfix: loop {
            // function call and subscription both accept expression list, merge the processor and it is the end seperator
            let mut expect_end_sep = SeperatorKind::Add;
            if lexer.nth(index + current_len).is_seperator(SeperatorKind::Dot) {
                match lexer.nth(index + current_len + 1).get_identifier() {
                    Some(ident) => {
                        current_len += 2;
                        postfixs.push(Postfix::MemberAccess(ident.clone()));
                        continue 'postfix;
                    }
                    None => return lexer.push_expect_symbol("member identifier", index + current_len + 1),
                }
            } else if lexer.nth(index + current_len).is_keyword(KeywordKind::As) {
                match SMType::parse(lexer, index + current_len + 1) {
                    Some(ty) => {
                        current_len += ty.symbol_len() + 1;
                        postfixs.push(Postfix::TypeCast(ty));
                        continue 'postfix;
                    }
                    None => return lexer.push_expect_symbol("typedef", index + current_len + 1),
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
                None => return lexer.push_expect_symbol("some expression", index + current_len),
                Some(expr1) => {
                    let mut exprs_len = expr1.symbol_len();
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
                                Some(expr) => {
                                    exprs_len += expr.symbol_len() + 1;
                                    exprs.push(expr);
                                }
                                None => return lexer.push_expect_symbol("some expr", index + current_len + exprs_len),
                            }
                        }
                    }
                }
            }
        }

        Some(PostfixExpression{ prim: primary, postfixs: postfixs })
    }
}

macro_rules! expr_to_postfix {
    ($prim: expr) => (Expression::Postfix(PostfixExpression{ prim: $prim, postfixs: Vec::new() }));
    ($prim: expr, $($posts: expr)*) => (Expression::Postfix(PostfixExpression{ prim: $prim, postfixs: vec![$($posts, )*] }))
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
        perrorln!("Messages: {:?}", lexer.emitter());
    }
}