///! fff-lang
///!
///! syntax/stmt
///! stmt = const_decl | var_decl | if_stmt | while_stmt | for_stmt | loop_stmt 
///!        | simple_expr_stmt | assign_expr_stmt | continue_stmt | break_stmt | fn_def | type_def
 
use std::fmt;

use super::FnDef;
use super::TypeDef;
use super::Formatter;
use super::ParseResult;
use super::ParseSession;
use super::ISyntaxItemParse;
use super::ISyntaxItemFormat;
use super::ISyntaxItemGrammar;

mod block_stmt;
mod expr_stmt;
mod for_stmt;
mod if_stmt;
mod jump_stmt;
mod loop_stmt;
mod ret_stmt;
mod while_stmt;
mod var_decl;

pub use self::var_decl::VarDeclStatement;
pub use self::jump_stmt::BreakStatement;
pub use self::jump_stmt::ContinueStatement;
pub use self::ret_stmt::ReturnStatement;
pub use self::expr_stmt::SimpleExprStatement;
pub use self::expr_stmt::AssignExprStatement;
pub use self::loop_stmt::LoopStatement;
pub use self::while_stmt::WhileStatement;
pub use self::for_stmt::ForStatement;
pub use self::if_stmt::IfClause;
pub use self::if_stmt::ElseIfClause;
pub use self::if_stmt::ElseClause;
pub use self::if_stmt::IfStatement;
pub use self::block_stmt::BlockStatement;

macro_rules! dispatch_statement_impl {
    ($this: expr, $inner: ident, $b: block) => (
        match $this {
            &Statement::Type(ref $inner) => $b,
            &Statement::Fn(ref $inner) => $b,
            &Statement::VarDecl(ref $inner) => $b,
            &Statement::Break(ref $inner) => $b,
            &Statement::Continue(ref $inner) => $b,
            &Statement::Return(ref $inner) => $b,
            &Statement::SimpleExpr(ref $inner) => $b,
            &Statement::AssignExpr(ref $inner) => $b,
            &Statement::If(ref $inner) => $b,
            &Statement::While(ref $inner) => $b,
            &Statement::For(ref $inner) => $b,
            &Statement::Loop(ref $inner) => $b,
            &Statement::Block(ref $inner) => $b,
        }
    )
}

// 37 byte
#[cfg_attr(test, derive(Eq, PartialEq))]
pub enum Statement {
    Type(TypeDef),                    // type
    Fn(FnDef),                        // fn
    Block(BlockStatement),            // {
    Break(BreakStatement),            // break 
    Continue(ContinueStatement),      // continue
    SimpleExpr(SimpleExprStatement),  // same as before
    AssignExpr(AssignExprStatement),  // _, (unary seperator, literal, identifier, left paren, left bracket)
    For(ForStatement),                // for
    If(IfStatement),                  // if
    Loop(LoopStatement),              // loop
    Return(ReturnStatement),          // return
    VarDecl(VarDeclStatement),        // const, var
    While(WhileStatement),            // while
}
impl ISyntaxItemFormat for Statement {
    fn format(&self, f: Formatter) -> String { dispatch_statement_impl!(self, inner, { f.apply(inner) }) }
}
impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(Formatter::default())) }
}
impl ISyntaxItemGrammar for Statement {

    fn is_first_final(sess: &ParseSession) -> bool {
        FnDef::is_first_final(sess)
        || TypeDef::is_first_final(sess)
        || VarDeclStatement::is_first_final(sess)
        || BreakStatement::is_first_final(sess)
        || ContinueStatement::is_first_final(sess)
        || ReturnStatement::is_first_final(sess)
        || AssignExprStatement::is_first_final(sess)
        || IfStatement::is_first_final(sess)
        || WhileStatement::is_first_final(sess)
        || ForStatement::is_first_final(sess)
        || LoopStatement::is_first_final(sess)
        || BlockStatement::is_first_final(sess)
    }
}
impl ISyntaxItemParse for Statement {
    type Target = Statement;

    fn parse(sess: &mut ParseSession) -> ParseResult<Statement> {

        if VarDeclStatement::is_first_final(sess) { 
            return Ok(Statement::VarDecl(VarDeclStatement::parse(sess)?));
        } else if BreakStatement::is_first_final(sess) {
            return Ok(Statement::Break(BreakStatement::parse(sess)?));
        } else if ContinueStatement::is_first_final(sess) {
            return Ok(Statement::Continue(ContinueStatement::parse(sess)?));
        } else if ReturnStatement::is_first_final(sess) {
            return Ok(Statement::Return(ReturnStatement::parse(sess)?));
        } else if LoopStatement::is_first_final(sess) {
            return Ok(Statement::Loop(LoopStatement::parse(sess)?));
        } else if WhileStatement::is_first_final(sess) {
            return Ok(Statement::While(WhileStatement::parse(sess)?));
        } else if ForStatement::is_first_final(sess) {
            return Ok(Statement::For(ForStatement::parse(sess)?));
        } else if IfStatement::is_first_final(sess) {
            return Ok(Statement::If(IfStatement::parse(sess)?));
        } else if BlockStatement::is_first_final(sess) {
            return Ok(Statement::Block(BlockStatement::parse(sess)?));
        } else if AssignExprStatement::is_first_final(sess) {
            return AssignExprStatement::parse(sess);
        } else if TypeDef::is_first_final(sess) {
            return Ok(Statement::Type(TypeDef::parse(sess)?));
        } else if FnDef::is_first_final(sess) {
            return Ok(Statement::Fn(FnDef::parse(sess)?));
        } else {
            return sess.push_unexpect("statement");
        }
    }
}

#[cfg(test)] #[test]
fn stmt_parse() {

}