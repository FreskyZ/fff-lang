
// PostfixExpression = 
//     PrimaryExpression 
//     PrimaryExpression [
//             fLeftBracket Expression fRightBracket
//             | fLeftParen [Expression [fComma Expression]*] fRightParen
//             | fDot fIdentifier
//             | fAs Type
//         ]*

use lexical::Lexer;

use syntax::ast_item::IASTItem;
use syntax::Expression;
use syntax::SMType;

use super::primary::PrimaryExpression;

pub enum PostfixExpression {
    Primary(PrimaryExpression),
    Subscription(Box<PostfixExpression>, Expression),
    FunctionCall(Box<PostfixExpression>, Vec<Expression>),
    MemberAccess(Box<PostfixExpression>, String),
    TypeCast(Box<PostfixExpression>, SMType),
}

impl PostfixExpression {

    pub fn make_subscription(post: PostfixExpression, expr: Expression) -> PostfixExpression {
        PostfixExpression::Subscription(Box::new(post), expr)
    }
    pub fn make_function_call(post: PostfixExpression, exprs: Vec<Expression>) -> PostfixExpression {
        PostfixExpression::FunctionCall(Box::new(post), exprs)
    }
    pub fn make_member_access(post: PostfixExpression, ident: String) -> PostfixExpression {
        PostfixExpression::MemberAccess(Box::new(post), ident)
    } 
    pub fn make_type_cast(post: PostfixExpression, ty: SMType) -> PostfixExpression {
        PostfixExpression::TypeCast(Box::new(post), ty)
    }
}

impl IASTItem for PostfixExpression {

    fn symbol_len(&self) -> usize {
        match *self {
            PostfixExpression::Primary(ref prim) => prim.symbol_len(),
            PostfixExpression::Subscription(ref boxed_post, ref indexer) => boxed_post.as_ref().symbol_len() + indexer.symbol_len() + 2,
            PostfixExpression::FunctionCall(ref boxed_post, ref params) =>
                boxed_post.as_ref().symbol_len() + 2 + params.iter().fold(0, |counter, ref param| counter + param.symbol_len() + 1),
            PostfixExpression::MemberAccess(ref boxed_post, ref ident) => boxed_post.as_ref().symbol_len() + 2,
            PostfixExpression::TypeCast(ref boxed_post, ref ty) => boxed_post.as_ref().symbol_len() + 1 + ty.symbol_len(),
        }
    }

    fn parse(lexer: &mut Lexer, index: usize) -> Option<PostfixExpression> {

        None

    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn ast_expr_postfix_parse() {

    }
}