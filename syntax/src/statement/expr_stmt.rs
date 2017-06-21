///! fff-lang
///! 
///! syntax/expr_stmt
///! ExprStatement = Expr [AssignOperator Expr] fSemiColon

// TODO: update to public

use std::fmt;

use codemap::Span;
use lexical::Token;
use lexical::SeperatorKind;
use lexical::SeperatorCategory;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;
use super::super::Expr;

#[cfg_attr(test, derive(Eq, PartialEq))]
struct SimpleExprStatement {
    expr: Expr, 
    all_strpos: Span, // include semicolon
}
#[cfg_attr(test, derive(Eq, PartialEq))]
struct AssignExprStatement {
    left: Expr,
    right: Expr,
    assign_op: SeperatorKind,
    assign_op_strpos: Span,
    all_strpos: Span,
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
    
    pub fn new_simple(all_strpos: Span, expr: Expr) -> ExprStatement { 
        ExprStatement(ActualExprStatement::Simple(SimpleExprStatement{ expr, all_strpos })) 
    }
    pub fn new_assign(all_strpos: Span, 
        assign_op: SeperatorKind, assign_op_strpos: Span, left: Expr, right: Expr) -> ExprStatement {
        ExprStatement(ActualExprStatement::Assign(AssignExprStatement{
            left: left, right: right, assign_op: assign_op, assign_op_strpos: assign_op_strpos, all_strpos: all_strpos
        }))
    }

    pub fn is_simple(&self) -> bool { match self.0 { ActualExprStatement::Simple(_) => true, ActualExprStatement::Assign(_) => false } }
    pub fn is_assign(&self) -> bool { match self.0 { ActualExprStatement::Simple(_) => false, ActualExprStatement::Assign(_) => true } }

    pub fn get_all_strpos(&self) -> Span {
        match self.0 {
            ActualExprStatement::Simple(ref simple) => simple.all_strpos,
            ActualExprStatement::Assign(ref assign) => assign.all_strpos,
        }
    }

    pub fn get_expr(&self) -> Option<&Expr> {
        match self.0 {
            ActualExprStatement::Simple(ref simple) => Some(&simple.expr),
            ActualExprStatement::Assign(_) => None,
        }
    }
    pub fn get_left_expr(&self) -> Option<&Expr> {
        match self.0 {
            ActualExprStatement::Simple(_) => None,
            ActualExprStatement::Assign(ref assign) => Some(&assign.left),
        }
    }
    pub fn get_right_expr(&self) -> Option<&Expr> {
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
    pub fn get_assign_op_strpos(&self) -> Span {
        match self.0 {
            ActualExprStatement::Simple(_) => Span::default(),
            ActualExprStatement::Assign(ref assign) => assign.assign_op_strpos,
        }
    }
}
impl ISyntaxItemGrammar for ExprStatement {
    fn is_first_final(sess: &ParseSession) -> bool { Expr::is_first_final(sess) }
}
impl ISyntaxItemParse for ExprStatement {
    type Target = ExprStatement;

    fn parse(sess: &mut ParseSession) -> ParseResult<ExprStatement> {

        let starting_strpos = sess.pos;
        let left_expr = Expr::parse(sess)?;

        let (assign_op, assign_op_strpos) = match (sess.tk, sess.pos) {
            (&Token::Sep(SeperatorKind::SemiColon), ref semi_colon_strpos) => {
                sess.move_next();
                return Ok(ExprStatement::new_simple(starting_strpos.merge(semi_colon_strpos), left_expr));
            }
            (&Token::Sep(assign_op), assign_op_strpos) if assign_op.is_category(SeperatorCategory::Assign) => {
                sess.move_next();
                (assign_op, assign_op_strpos)
            }
            _ => return sess.push_unexpect("assignment operator, semicolon"),
        };

        let right_expr = Expr::parse(sess)?;
        let ending_strpos = sess.pos;
        sess.expect_sep(SeperatorKind::SemiColon)?;
        return Ok(ExprStatement::new_assign(starting_strpos.merge(&ending_strpos), assign_op, assign_op_strpos, left_expr, right_expr));
    }
}

#[cfg(test)] #[test]
fn expr_stmt_parse() {
    use codemap::SymbolCollection;
    use lexical::LitValue;
    use super::super::LitExpr;
    use super::super::IdentExpr;
    use super::super::Expr;
    use super::super::BinaryExpr;
    use super::super::FnCallExpr;
    use super::super::ExprList;
    use super::super::ISyntaxItemWithStr;

    //                                                 0         1          2
    //                                                 12345678 90123456789 012
    assert_eq!{ ExprStatement::with_test_input_ret_size("writeln(\"helloworld\");", &mut make_symbols!["writeln", "helloworld"]), (
        Some(ExprStatement::new_simple(
            make_span!(0, 21),
            Expr::FnCall(FnCallExpr::new(
                IdentExpr::new(make_id!(1), make_span!(0, 6)),
                make_span!(7, 20), ExprList::new(vec![
                    Expr::Lit(LitExpr::new(LitValue::new_str_lit(make_id!(2)), make_span!(8, 19)))
                ])
            ))
        )),
        5
    )}

    //                                                 123456789012
    assert_eq!{ ExprStatement::with_test_str_ret_size("1 + 1 <<= 2;"), ( // to show I have 3 char seperator available
        Some(ExprStatement::new_assign(
            make_span!(0, 11),
            SeperatorKind::ShiftLeftAssign, make_span!(6, 8),
            Expr::Binary(BinaryExpr::new(
                Expr::Lit(LitExpr::new(LitValue::from(1), make_span!(0, 0))),
                SeperatorKind::Add, make_span!(2, 2),
                Expr::Lit(LitExpr::new(LitValue::from(1), make_span!(4, 4))),
            )),
            Expr::Lit(LitExpr::new(LitValue::from(2), make_span!(10, 10)))
        )),
        6
    )}
}