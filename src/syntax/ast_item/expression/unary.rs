
// UnaryExpression = PostfixExpression | UnaryOperator UnaryExpression

use std::fmt;

use common::From2;
use common::StringPosition;

use lexical::Lexer;
use lexical::IToken;
use lexical::SeperatorKind;

use syntax::ast_item::IASTItem;
use syntax::ast_item::expression::postfix::PostfixExpression;

#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOperator {
    Increment(StringPosition), 
    Decrement(StringPosition),
    BitNot(StringPosition), 
    LogicalNot(StringPosition),
}
#[derive(Eq, PartialEq)]
pub struct UnaryExpression {
    pub post: PostfixExpression,
    pub unaries: Vec<UnaryOperator>,  // if it is [LogicalNot, BitNot], that is `!~abc`
}

impl fmt::Debug for UnaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{:?}", 
            self.unaries.iter().fold(
                String::new(), 
                |mut buf, ref unary| {
                    match unary {
                        &&UnaryOperator::BitNot(ref pos) => buf.push_str(&format!("~ @ {}", pos)),
                        &&UnaryOperator::LogicalNot(ref pos) => buf.push_str(&format!("! @ {}", pos)),
                        &&UnaryOperator::Increment(ref pos) => buf.push_str(&format!("++ @ {}", pos)),
                        &&UnaryOperator::Decrement(ref pos) => buf.push_str(&format!("-- @ {}", pos)),
                    }
                    buf
                }
            ),
            self.post
        )
    }
}
impl fmt::Display for UnaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", 
            self.unaries.iter().fold(
                String::new(), 
                |mut buf, ref unary| {
                    match unary {
                        &&UnaryOperator::BitNot(ref pos) => buf.push_str("~"),
                        &&UnaryOperator::LogicalNot(ref pos) => buf.push_str("!"),
                        &&UnaryOperator::Increment(ref pos) => buf.push_str("++"),
                        &&UnaryOperator::Decrement(ref pos) => buf.push_str("--"),
                    }
                    buf
                }
            ),
            self.post
        )
    }
}

impl IASTItem for UnaryExpression {

    fn pos_all(&self) -> StringPosition {
        match self.unaries.iter().last() {
            Some(&UnaryOperator::BitNot(ref pos))
            | Some(&UnaryOperator::LogicalNot(ref pos))
            | Some(&UnaryOperator::Increment(ref pos))
            | Some(&UnaryOperator::Decrement(ref pos)) => StringPosition::from2(pos.start_pos, self.post.pos_all().end_pos),
            None => self.post.pos_all(),
        }
    }

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<UnaryExpression>, usize) {

        let mut current_len = 0;
        let mut unaries = Vec::new();
        loop {
            match lexer.nth(index + current_len).get_seperator() {
                Some(&SeperatorKind::Increase) => {
                    unaries.push(UnaryOperator::Increment(lexer.pos(index + current_len)));
                    current_len += 1;
                }
                Some(&SeperatorKind::Decrease) => {
                    unaries.push(UnaryOperator::Decrement(lexer.pos(index + current_len)));
                    current_len += 1;
                }
                Some(&SeperatorKind::BitNot) => {
                    unaries.push(UnaryOperator::BitNot(lexer.pos(index + current_len)));   
                    current_len += 1;
                }
                Some(&SeperatorKind::LogicalNot) => {
                    unaries.push(UnaryOperator::LogicalNot(lexer.pos(index + current_len)));
                    current_len += 1;
                }
                Some(_) => {
                    return lexer.push_expects(vec!["unary expression", "literal", "identifier", "array def"], index + current_len, current_len);
                }
                None => match PostfixExpression::parse(lexer, index + current_len) {
                    (Some(postfix), postfix_len) => return (Some(UnaryExpression{ post: postfix, unaries: unaries }), current_len + postfix_len),
                    (None, length) => return (None, current_len + length),
                }
            } 
        }
    }
}