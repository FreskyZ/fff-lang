
// UnaryExpression = PostfixExpression | UnaryOperator UnaryExpression

use std::fmt;

use codepos::StringPosition;
use message::MessageCollection;

use lexical::Lexer;
use lexical::SeperatorKind;
use lexical::SeperatorCategory;

use super::super::ISyntaxItem;
use super::super::ISyntaxItemFormat;
use super::super::expression::postfix::PostfixExpression;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct UnaryOperator {
    pub op: SeperatorKind,
    pub pos: StringPosition,
}
impl UnaryOperator {
    pub fn new(op: SeperatorKind, pos: StringPosition) -> UnaryOperator {
        UnaryOperator{ op: op, pos: pos }
    }
}

#[derive(Eq, PartialEq, Clone)]
pub struct UnaryExpression {
    pub post: PostfixExpression,
    pub unaries: Vec<UnaryOperator>,  // if it is [LogicalNot, BitNot], that is `!~abc`
}
impl ISyntaxItemFormat for UnaryExpression {
    fn format(&self, indent: u32) -> String {
        if self.unaries.len() == 0 {
            format!("{}", self.post.format(indent))
        } else {
            format!("{}UnaryExpr:\n{}{}", 
                UnaryExpression::indent_str(indent), 
                self.post.format(indent + 1),
                self.unaries.iter().fold(String::new(),
                    |mut buf, &UnaryOperator{ ref op, ref pos }| { buf.push('\n'); buf.push_str(&format!("{:?}<{:?}>", op, pos)); buf }
                ),
            )
        }
    }
}
impl fmt::Debug for UnaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n{}", self.format(0))
    }
}

impl ISyntaxItem for UnaryExpression {

    fn pos_all(&self) -> StringPosition {
        match self.unaries.iter().last() {
            Some(&UnaryOperator{ op: ref _op, ref pos }) => StringPosition::merge(*pos, self.post.pos_all()),
            None => self.post.pos_all(),
        }
    }

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        PostfixExpression::is_first_final(lexer, index)
        || match lexer.nth(index).get_seperator() {
            Some(ref sep) => sep.is_category(SeperatorCategory::Unary),
            None => false,
        }
    }

    fn parse(lexer: &mut Lexer, messages: &mut MessageCollection, index: usize) -> (Option<UnaryExpression>, usize) {

        let mut current_len = 0;
        let mut unaries = Vec::new();
        loop {
            match lexer.nth(index + current_len).get_seperator() {
                Some(ref sep) if sep.is_category(SeperatorCategory::Unary) => {
                    unaries.push(UnaryOperator::new(sep.clone(), lexer.pos(index + current_len)));
                    current_len += 1;
                }
                Some(_) | None => match PostfixExpression::parse(lexer, messages, index + current_len) {
                    (Some(postfix), postfix_len) => return (Some(UnaryExpression{ post: postfix, unaries: unaries }), current_len + postfix_len),
                    (None, length) => return (None, current_len + length),
                }
            } 
        }
    }
}