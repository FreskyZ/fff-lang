
// ExpressionStatement = Expression [AssignOperator Expression] fSemiColon

use std::fmt;

use codepos::StringPosition;
use message::Message;
use message::MessageCollection;

use lexical::Lexer;
use lexical::SeperatorKind;
use lexical::SeperatorCategory;

use super::super::ast_item::ISyntaxItem;
use super::super::Expression;

#[derive(Eq, PartialEq)]
pub struct ExpressionStatement {
    pub left_expr: Expression,
    pub op: Option<SeperatorKind>,
    pub right_expr: Option<Expression>,
    pub pos: [StringPosition; 2],       // position for assign op and ';'
}

fn format_assign_op(op: &SeperatorKind) -> String {
    match *op {
        SeperatorKind::Assign => format!(".operator="),
        SeperatorKind::AddAssign => format!(".operator+="),
        SeperatorKind::SubAssign => format!(".operator-="),
        SeperatorKind::MulAssign => format!(".operator*="),
        SeperatorKind::DivAssign => format!(".operator/="),
        SeperatorKind::RemAssign => format!(".operator%="),
        SeperatorKind::BitAndAssign => format!(".operator&="),
        SeperatorKind::BitOrAssign => format!(".operator|="),
        SeperatorKind::BitXorAssign => format!(".operator^="),
        _ => unreachable!(),
    }
}

impl ExpressionStatement {
}
impl fmt::Debug for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}{}; @ {:?}",
            self.left_expr,
            match (&self.op, &self.right_expr) { 
                (&Some(ref op), &Some(ref expr)) => format!("{:?}({:?}) @ {:?}", format_assign_op(op), expr, self.pos[0]), 
                (ref _other_op, ref _other_expr) => String::new() 
            },
            self.pos[1]
        )
    }
}
impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{};",
            self.left_expr,
            match (&self.op, &self.right_expr) { 
                (&Some(ref op), &Some(ref expr)) => format!("{}({})", format_assign_op(op), expr), 
                (_, _) => String::new() 
            },
        )
    }
}
impl ISyntaxItem for ExpressionStatement {

    fn pos_all(&self) -> StringPosition { StringPosition::merge(self.left_expr.pos_all(), self.pos[1]) }

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        Expression::is_first_final(lexer, index)
    }

    fn parse(lexer: &mut Lexer, messages: &mut MessageCollection, index: usize) -> (Option<ExpressionStatement>, usize) {

        let (left_expr, mut current_len) = match Expression::parse(lexer, messages, index) {
            (Some(expr), expr_len) => (expr, expr_len),
            (None, length) => return (None, length),
        };

        if lexer.nth(index + current_len).is_seperator(SeperatorKind::SemiColon) {
            // if !left_expr.is_function_call() { // process in codegen not here
            //     messages.push(SyntaxMessage::NotFunctionCallIndependentExpressionStatement{ pos: left_expr.pos_all() });
            // }
            return (Some(ExpressionStatement{
                left_expr: left_expr,
                op: None,
                right_expr: None,
                pos: [StringPosition::new(), lexer.pos(index + current_len)]
            }), current_len + 1);
        }

        let (assign_op, assign_op_pos) = match lexer.nth(index + current_len).get_seperator() {
            Some(ref assign_op) if assign_op.is_category(SeperatorCategory::Assign) => {
                current_len += 1;
                (assign_op.clone(), lexer.pos(index + current_len - 1))
            },
            Some(_) | None => return push_unexpect!(lexer, messages, ["assignment operator", "semicolon", ], index + current_len, current_len),
        };
        
        match Expression::parse(lexer, messages, index + current_len) {
            (Some(right_expr), right_expr_len) => {
                current_len += right_expr_len;
                if lexer.nth(index + current_len).is_seperator(SeperatorKind::SemiColon) {
                    return (Some(ExpressionStatement{
                        left_expr: left_expr,
                        op: Some(assign_op),
                        right_expr: Some(right_expr),
                        pos: [assign_op_pos, lexer.pos(index + current_len)]
                    }), current_len + 1);
                } else {
                    return push_unexpect!(lexer, messages, "semicolon", index + current_len, current_len);
                }
            }
            (None, length) => return (None, current_len + length),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::ExpressionStatement;
    use codepos::StringPosition;
    use lexical::SeperatorKind;
    use super::super::super::ast_item::ISyntaxItem;
    use super::super::super::Expression;

    #[test]
    fn ast_stmt_expr() {

        macro_rules! test_case {
            ($program: expr, $len: expr, $expect: expr) => (
                let (result, len) = ExpressionStatement::with_test_str_ret_size($program);
                assert_eq!(result, Some($expect));
                assert_eq!(len, $len);
            )
        }

        //           12345678 90123456789 0123
        test_case!{ "writeln(\"helloworld\");", 5,
            ExpressionStatement {
                left_expr: Expression::with_test_str("writeln(\"helloworld\")"),
                op: None,
                right_expr: None,
                pos: [StringPosition::new(), make_str_pos!(1, 22, 1, 22)]
            } 
        }
        //           1234567890
        test_case!{ "1 + 1 = 2;", 6,
            ExpressionStatement {
                left_expr: Expression::with_test_str("1 + 1"),
                op: Some(SeperatorKind::Assign),
                right_expr: Some(Expression::with_test_str("        2")),
                pos: [make_str_pos!(1, 7, 1, 7), make_str_pos!(1, 10, 1, 10)]
            }
        }
        //           1234567890
        test_case!{ "1 + 1+= 2;", 6,
            ExpressionStatement {
                left_expr: Expression::with_test_str("1 + 1"),
                op: Some(SeperatorKind::AddAssign),
                right_expr: Some(Expression::with_test_str("       2")),
                pos: [make_str_pos!(1, 6, 1, 7), make_str_pos!(1, 10, 1, 10)]
            }
        }
    }
}