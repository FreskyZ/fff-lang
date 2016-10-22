
// JumpStatement = ReturnStatement | BreakStatement | ContinueStatement
// ReturnStatemt = fReturn [Expression] fSemiColon
// BreakStatement = fBreak [Expression] fSemiColon        
// ContinueStatement = fContinue [Expression] fSemiColon 

use std::fmt;

use common::From2;
use common::StringPosition;

use lexical::Lexer;
use lexical::IToken;
use lexical::SeperatorKind;
use lexical::KeywordKind;

use syntax::ast_item::IASTItem;
use syntax::Expression;

#[derive(Eq, PartialEq)]
pub enum JumpStatementType {
    Return, 
    Break,
    Continue,
}
impl fmt::Debug for JumpStatementType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            JumpStatementType::Return => write!(f, "return"),
            JumpStatementType::Break => write!(f, "break"),
            JumpStatementType::Continue => write!(f, "continue"),
        }
    }
}
impl_display_by_debug!{ JumpStatementType }

#[derive(Eq, PartialEq)]
pub struct JumpStatement {
    pub id: usize,
    pub ty: JumpStatementType,
    pub expr: Option<Expression>,
    pub pos: [StringPosition; 2],
}

impl fmt::Debug for JumpStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}>{} @ {:?}{}; @ {:?}", 
            self.id, self.ty, self.pos[0], 
            match self.expr { Some(ref expr) => format!(" {:?} ", expr), None => String::new() }, 
            self.pos[1]
        )
    }
}
impl fmt::Display for JumpStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}>{}{};", 
            self.id, self.ty,
            match self.expr { Some(ref expr) => format!(" {} ", expr), None => String::new() }, 
        )
    }
}

impl IASTItem for JumpStatement {

    fn pos_all(&self) -> StringPosition { StringPosition::from2(self.pos[0].start_pos, self.pos[1].end_pos) } 

    // It's also speciall..., index is for index of break|continue|return for distinguish them
    fn parse(lexer: &mut Lexer, index: usize) -> (Option<JumpStatement>, usize) {

        let ty = match lexer.nth(index).get_keyword() {
            Some(&KeywordKind::Return) => JumpStatementType::Return,
            Some(&KeywordKind::Continue) => JumpStatementType::Continue,
            Some(&KeywordKind::Break) => JumpStatementType::Break,
            _ => return (None, 0), 
        };

        if lexer.nth(index + 1).is_seperator(SeperatorKind::SemiColon) {
            return (Some(JumpStatement{
                id: 0,
                ty: ty,
                expr: None,
                pos: [lexer.pos(index), lexer.pos(index + 1)],
            }), 2);
        }
        match Expression::parse(lexer, index + 1) {
            (None, length) => return (None, length),
            (Some(expr), expr_len) => {
                if lexer.nth(index + 1 + expr_len).is_seperator(SeperatorKind::SemiColon) {
                    return (Some(JumpStatement{
                        id: 0,
                        ty: ty,
                        expr: Some(expr),
                        pos: [lexer.pos(index), lexer.pos(index + 1 + expr_len)],
                    }), 2 + expr_len);
                } else {
                    return lexer.push_expect("semicolon", index + expr_len + 1, expr_len + 1);
                }
            } 
        }
    }
}

#[cfg(test)]
mod tests {
    use super::JumpStatement;
    use super::JumpStatementType;
    use common::StringPosition;
    use lexical::Lexer;
    use syntax::ast_item::IASTItem;
    use syntax::Expression;
    use syntax::ExpressionBase;
    use syntax::ExpressionOperator;
    use lexical::SeperatorKind;
    use lexical::NumericLiteralValue;

    #[test]
    fn ast_stmt_jump() {

        macro_rules! test_case {
            ($program: expr, $len: expr, $expect: expr) => (
                let lexer = &mut Lexer::new_test2($program);
                let (result, len) = JumpStatement::parse(lexer, 0);
                assert_eq!(result, Some($expect));
                assert_eq!(len, $len);
            )
        }

        test_case!{ "continue;", 2,
            JumpStatement{ 
                id: 0, 
                ty: JumpStatementType::Continue, 
                expr: None, 
                pos: [make_str_pos!(1, 1, 1, 8), make_str_pos!(1, 9, 1, 9)] 
            }
        }
        test_case!{ "break   ;", 2,
            JumpStatement{ 
                id: 0, 
                ty: JumpStatementType::Break, 
                expr: None, 
                pos: [make_str_pos!(1, 1, 1, 5), make_str_pos!(1, 9, 1, 9)] 
            }
        }
        test_case!{ "return;", 2,
            JumpStatement{ 
                id: 0, 
                ty: JumpStatementType::Return, 
                expr: None, 
                pos: [make_str_pos!(1, 1, 1, 6), make_str_pos!(1, 7, 1, 7)] 
            }
        }
        
        //           123456789 012345 67
        test_case!{ "continue \"inner\";", 3,
            JumpStatement{ 
                id: 0, 
                ty: JumpStatementType::Continue, 
                expr: Some(Expression::new_test(
                    ExpressionBase::StringLiteral("inner".to_owned()), 
                    make_str_pos!(1, 10, 1, 16),
                    Vec::new(),
                    make_str_pos!(1, 10, 1, 16),
                )), 
                pos: [make_str_pos!(1, 1, 1, 8), make_str_pos!(1, 17, 1, 17)] 
            }
        }
        //           123456 7890123 45
        test_case!{ "break \"outter\";", 3,
            JumpStatement{ 
                id: 0, 
                ty: JumpStatementType::Break, 
                expr: Some(Expression::new_test(
                    ExpressionBase::StringLiteral("outter".to_owned()), 
                    make_str_pos!(1, 7, 1, 14),
                    Vec::new(),
                    make_str_pos!(1, 7, 1, 14),
                )), 
                pos: [make_str_pos!(1, 1, 1, 5), make_str_pos!(1, 15, 1, 15)] 
            }
        }
        //           1234567890123
        test_case!{ "return 1 + 1;", 5,
            JumpStatement{ 
                id: 0, 
                ty: JumpStatementType::Return, 
                expr: Some(Expression::new_test(
                    ExpressionBase::NumericLiteral(NumericLiteralValue::I32(1)), 
                    make_str_pos!(1, 8, 1, 8),
                    vec![
                        ExpressionOperator::Binary(
                            SeperatorKind::Add,
                            make_str_pos!(1, 10, 1, 10),
                            Expression::new_test(
                                ExpressionBase::NumericLiteral(NumericLiteralValue::I32(1)),
                                make_str_pos!(1, 12, 1, 12),
                                Vec::new(),
                                make_str_pos!(1, 12, 1, 12),
                            ),
                        ),
                    ],
                    make_str_pos!(1, 8, 1, 12)
                )),
                pos: [make_str_pos!(1, 1, 1, 6), make_str_pos!(1, 13, 1, 13)] 
            }
        }
    }
}