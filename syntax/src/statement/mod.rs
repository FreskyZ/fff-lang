///! fff-lang
///!
///! syntax/stmt
// Statement = 
//     ConstDecl | VarDecl 
//     | IfStatement | WhileStatement | ForStatement | LoopStatement 
//     | AssignStatement
//     | JumpStatement
 
use std::fmt;

use super::ParseSession;
use super::ParseResult;
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
pub use self::expr_stmt::ExprStatement;
pub use self::loop_stmt::LoopStatement;
pub use self::while_stmt::WhileStatement;
pub use self::for_stmt::ForStatement;
pub use self::if_stmt::IfConditionBody;
pub use self::if_stmt::IfStatement;
pub use self::block_stmt::BlockStatement;

macro_rules! dispatch_statement_impl {
    ($this: expr, $inner: ident, $b: block) => (
        match $this {
            &Statement::VarDecl(ref $inner) => $b,
            &Statement::Break(ref $inner) => $b,
            &Statement::Continue(ref $inner) => $b,
            &Statement::Return(ref $inner) => $b,
            &Statement::Expr(ref $inner) => $b,
            &Statement::If(ref $inner) => $b,
            &Statement::While(ref $inner) => $b,
            &Statement::For(ref $inner) => $b,
            &Statement::Loop(ref $inner) => $b,
            &Statement::Block(ref $inner) => $b,
        }
    )
}
// TODO: Change to
// pub struct LabeledStmt<TStmt> { // to be considered
//     pub name: LabelDef,
//     pub stmt: TStmt,
// }
// pub enum Stmt {
//     Block(BlockStmt),
//     Break(ContinueStmt),
//     Continue(BreakStmt),
//     Expr(ExprStmt),
//     If(IfStmt),
//     For(ForStmt),
//     While(WhileStmt),
//     Loop(LoopStmt),
//     Return(ReturnStmt),
//     VarDecl(VarDeclStmt),
//     Labeled(LabeledStmt),
// }

#[cfg_attr(test, derive(Eq, PartialEq))]
pub enum Statement {
    Block(BlockStatement),            // {
    Break(BreakStatement),            // break 
    Continue(ContinueStatement),      // continue
    Expr(ExprStatement),        // _, (unary seperator, literal, identifier, left paren, left bracket)
    For(ForStatement),                // for
    If(IfStatement),                  // if
    Loop(LoopStatement),              // loop
    Return(ReturnStatement),          // return
    VarDecl(VarDeclStatement),        // const, var
    While(WhileStatement),            // while
}
impl ISyntaxItemFormat for Statement {
    fn format(&self, indent: u32) -> String { dispatch_statement_impl!(self, inner, { inner.format(indent) }) }
}
impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(0)) }
}
impl ISyntaxItemGrammar for Statement {

    fn is_first_final(sess: &ParseSession) -> bool {
        VarDeclStatement::is_first_final(sess)
        || BreakStatement::is_first_final(sess)
        || ContinueStatement::is_first_final(sess)
        || ReturnStatement::is_first_final(sess)
        || ExprStatement::is_first_final(sess)
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
        } else if ExprStatement::is_first_final(sess) {
            return Ok(Statement::Expr(ExprStatement::parse(sess)?));
        } else {
            return sess.push_unexpect("statement");
        }
    }
}

#[cfg(test)] #[test]
fn stmt_parse() {

}

// TODO: 
// apply ParseSession
// try design and apply ISyntaxItemResolve and ResolveSession