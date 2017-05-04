///! fff-lang
///! 
///! syntax/expr_stmt
///! ExprStatement = BinaryExpr [AssignOperator BinaryExpr] fSemiColon

use std::fmt;

use codepos::StringPosition;
use message::Message;
use message::MessageCollection;

use lexical::Token;
use lexical::TokenStream;
use lexical::SeperatorKind;
use lexical::SeperatorCategory;

use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;
use super::super::BinaryExpr;

#[cfg_attr(test, derive(Eq, PartialEq))]
struct SimpleExprStatement {
    expr: BinaryExpr, 
    all_strpos: StringPosition, // include semicolon
}
#[cfg_attr(test, derive(Eq, PartialEq))]
struct AssignExprStatement {
    left: BinaryExpr,
    right: BinaryExpr,
    assign_op: SeperatorKind,
    assign_op_strpos: StringPosition,
    all_strpos: StringPosition,
}
#[cfg_attr(test, derive(Eq, PartialEq))]
enum ActualExprStatement {
    Simple(SimpleExprStatement),
    Assign(AssignExprStatement),
}
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct ExprStatement(ActualExprStatement);

impl ISyntaxItemFormat for ExprStatement {
    fn format(&self, indent: u32) -> String {
        match self.0 {
            ActualExprStatement::Simple(SimpleExprStatement{ ref expr, ref all_strpos }) => 
                format!("{}ExprStmt <{:?}>\n{}", ExprStatement::indent_str(indent), all_strpos, expr.format(indent + 1)),
            ActualExprStatement::Assign(AssignExprStatement{ ref left, ref right, ref assign_op, ref assign_op_strpos, ref all_strpos }) =>
                format!("{}ExprStmt <{:?}>\n{}{:?} <{:?}>\n{}\n{}",
                    ExprStatement::indent_str(indent), all_strpos,
                    ExprStatement::indent_str(indent + 1), assign_op, assign_op_strpos,
                    left.format(indent + 1),
                    right.format(indent + 1), 
                ),
        }
    }
}
impl fmt::Debug for ExprStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl ExprStatement {
    
    pub fn new_simple(all_strpos: StringPosition, expr: BinaryExpr) -> ExprStatement { 
        ExprStatement(ActualExprStatement::Simple(SimpleExprStatement{ expr, all_strpos })) 
    }
    pub fn new_assign(all_strpos: StringPosition, 
        assign_op: SeperatorKind, assign_op_strpos: StringPosition, left: BinaryExpr, right: BinaryExpr) -> ExprStatement {
        ExprStatement(ActualExprStatement::Assign(AssignExprStatement{
            left: left, right: right, assign_op: assign_op, assign_op_strpos: assign_op_strpos, all_strpos: all_strpos
        }))
    }

    pub fn is_simple(&self) -> bool { match self.0 { ActualExprStatement::Simple(_) => true, ActualExprStatement::Assign(_) => false } }
    pub fn is_assign(&self) -> bool { match self.0 { ActualExprStatement::Simple(_) => false, ActualExprStatement::Assign(_) => true } }

    pub fn get_all_strpos(&self) -> StringPosition {
        match self.0 {
            ActualExprStatement::Simple(ref simple) => simple.all_strpos,
            ActualExprStatement::Assign(ref assign) => assign.all_strpos,
        }
    }

    pub fn get_expr(&self) -> Option<&BinaryExpr> {
        match self.0 {
            ActualExprStatement::Simple(ref simple) => Some(&simple.expr),
            ActualExprStatement::Assign(_) => None,
        }
    }
    pub fn get_left_expr(&self) -> Option<&BinaryExpr> {
        match self.0 {
            ActualExprStatement::Simple(_) => None,
            ActualExprStatement::Assign(ref assign) => Some(&assign.left),
        }
    }
    pub fn get_right_expr(&self) -> Option<&BinaryExpr> {
        match self.0 {
            ActualExprStatement::Simple(_) => None,
            ActualExprStatement::Assign(ref assign) => Some(&assign.right),
        }
    }
    pub fn get_assign_op(&self) -> Option<&SeperatorKind> {
        match self.0 {
            ActualExprStatement::Simple(_) => None,
            ActualExprStatement::Assign(ref assign) => Some(&assign.assign_op),
        }
    }
    pub fn get_assign_op_strpos(&self) -> StringPosition {
        match self.0 {
            ActualExprStatement::Simple(_) => StringPosition::new(),
            ActualExprStatement::Assign(ref assign) => assign.assign_op_strpos,
        }
    }
}
impl ISyntaxItemGrammar for ExprStatement {
    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool {
        BinaryExpr::is_first_final(tokens, index)
    }
}
impl ISyntaxItemParse for ExprStatement {

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<ExprStatement>, usize) {

        let (left_expr, mut current_length) = match BinaryExpr::parse(tokens, messages, index) {
            (Some(expr), expr_len) => (expr, expr_len),
            (None, length) => return (None, length),
        };

        let (assign_op, assign_op_strpos) =  match tokens.nth(index + current_length) {
            &Token::Sep(SeperatorKind::SemiColon) => {
                return (Some(ExprStatement::new_simple(
                    StringPosition::merge(tokens.pos(index), tokens.pos(index + current_length)),
                    left_expr
                )), current_length + 1);
            }
            &Token::Sep(assign_op) if assign_op.is_category(SeperatorCategory::Assign) => {
                current_length += 1;
                (assign_op, tokens.pos(index + current_length - 1))
            }
            _ => return push_unexpect!(tokens, messages, ["assignment operator", "semicolon", ], index + current_length, current_length),
        };
        
        match BinaryExpr::parse(tokens, messages, index + current_length) {
            (Some(right_expr), right_expr_len) => {
                current_length += right_expr_len;
                if tokens.nth(index + current_length) == &Token::Sep(SeperatorKind::SemiColon) {
                    return (Some(ExprStatement::new_assign(
                        StringPosition::merge(tokens.pos(index), tokens.pos(index + current_length)),
                        assign_op, assign_op_strpos,
                        left_expr, right_expr,
                    )), current_length + 1);
                } else {
                    return push_unexpect!(tokens, messages, "semicolon", index + current_length, current_length);
                }
            }
            (None, length) => return (None, current_length + length),
        }
    }
}

#[cfg(test)] #[test]
fn ast_stmt_expr() {
    use super::super::ISyntaxItemWithStr;
    use super::super::BinaryExpr;
    use super::super::PostfixExpr;
    use super::super::PrimaryExpr;
    use lexical::LitValue;

    //                                                 0         1          2
    //                                                 12345678 90123456789 012
    assert_eq!{ ExprStatement::with_test_str_ret_size("writeln(\"helloworld\");"), (
        Some(ExprStatement::new_simple(
            make_strpos!(1, 1, 1, 22),
            BinaryExpr::new_postfix(
                PostfixExpr::new_function_call(
                    PostfixExpr::new_primary(PrimaryExpr::new_ident("writeln".to_owned(), make_strpos!(1, 1, 1, 7))),
                    make_strpos!(1, 8, 1, 21), vec![
                    BinaryExpr::new_lit(LitValue::from("helloworld"), make_strpos!(1, 9, 1, 20))
                ])
            )
        )),
        5
    )}

    //                                                 123456789012
    assert_eq!{ ExprStatement::with_test_str_ret_size("1 + 1 <<= 2;"), ( // to show I have 3 char seperator available
        Some(ExprStatement::new_assign(
            make_strpos!(1, 1, 1, 12),
            SeperatorKind::ShiftLeftAssign, make_strpos!(1, 7, 1, 9),
            BinaryExpr::new_binary(
                BinaryExpr::new_lit(LitValue::from(1), make_strpos!(1, 1, 1, 1)),
                SeperatorKind::Add, make_strpos!(1, 3, 1, 3),
                BinaryExpr::new_lit(LitValue::from(1), make_strpos!(1, 5, 1, 5)),
            ),
            BinaryExpr::new_lit(LitValue::from(2), make_strpos!(1, 11, 1, 11))
        )),
        6
    )}
}