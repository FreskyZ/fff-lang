
// UnaryExpression = PostfixExpression | UnaryOperator UnaryExpression

use std::fmt;

use codepos::StringPosition;

use lexical::Lexer;
use lexical::SeperatorKind;
use lexical::SeperatorCategory;

use syntax::ast_item::IASTItem;
use syntax::expression::postfix::PostfixExpression;

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

impl fmt::Debug for UnaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}{}",
            self.post,
            self.unaries.iter().fold(
                String::new(), 
                |mut buf, ref unary| {
                    match unary.op {
                        SeperatorKind::BitNot => buf.push_str(&format!(".operator~() @ {:?}", unary.pos)),
                        SeperatorKind::LogicalNot => buf.push_str(&format!(".operator!() @ {:?}", unary.pos)),
                        SeperatorKind::Increase => buf.push_str(&format!(".operator++() @ {:?}", unary.pos)),
                        SeperatorKind::Decrease => buf.push_str(&format!(".operator--() @ {:?}", unary.pos)),
                        SeperatorKind::Sub => buf.push_str(&format!(".operator-() @ {:?}", unary.pos)),
                        _ => unreachable!(),
                    }
                    buf
                }
            ),
        )
    }
}
impl fmt::Display for UnaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", 
            self.post,
            self.unaries.iter().fold(
                String::new(), 
                |mut buf, ref unary| {
                    match unary.op {
                        SeperatorKind::BitNot => buf.push_str(".operator~()"),
                        SeperatorKind::LogicalNot => buf.push_str(".operator!()"),
                        SeperatorKind::Increase => buf.push_str(".operator++()"),
                        SeperatorKind::Decrease => buf.push_str(".operator--()"),
                        SeperatorKind::Sub => buf.push_str(".operator-()"),
                        _ => unreachable!(),
                    }
                    buf
                }
            ),
        )
    }
}

impl IASTItem for UnaryExpression {

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

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<UnaryExpression>, usize) {

        let mut current_len = 0;
        let mut unaries = Vec::new();
        loop {
            match lexer.nth(index + current_len).get_seperator() {
                Some(ref sep) if sep.is_category(SeperatorCategory::Unary) => {
                    unaries.push(UnaryOperator::new(sep.clone(), lexer.pos(index + current_len)));
                    current_len += 1;
                }
                Some(_) | None => match PostfixExpression::parse(lexer, index + current_len) {
                    (Some(postfix), postfix_len) => return (Some(UnaryExpression{ post: postfix, unaries: unaries }), current_len + postfix_len),
                    (None, length) => return (None, current_len + length),
                }
            } 
        }
    }
}