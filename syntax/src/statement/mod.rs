///! fff-lang
///!
///! syntax/stmt
// Statement = 
//     ConstDecl | VarDecl 
//     | IfStatement | WhileStatement | ForStatement | LoopStatement 
//     | AssignStatement
//     | JumpStatement
 
use std::fmt;

use message::Message;
use message::MessageCollection;
use lexical::TokenStream;

#[cfg(feature = "parse_sess")] use super::ParseSession;
#[cfg(feature = "parse_sess")] use super::ParseResult;
#[cfg(feature = "parse_sess")] use super::ISyntaxItemParseX;
#[cfg(feature = "parse_sess")] use super::ISyntaxItemGrammarX;
use super::ISyntaxItemParse;
use super::ISyntaxItemFormat;
use super::ISyntaxItemGrammar;

mod var_decl;
mod expr_stmt;
mod if_stmt;
mod for_stmt;
mod jump_stmt;
mod while_stmt;
mod loop_stmt;
mod ret_stmt;
mod block_stmt;

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

    fn is_first_final(lexer: &mut TokenStream, index: usize) -> bool {
        VarDeclStatement::is_first_final(lexer, index)
        || BreakStatement::is_first_final(lexer, index)
        || ContinueStatement::is_first_final(lexer, index)
        || ReturnStatement::is_first_final(lexer, index)
        || ExprStatement::is_first_final(lexer, index)
        || IfStatement::is_first_final(lexer, index)
        || WhileStatement::is_first_final(lexer, index)
        || ForStatement::is_first_final(lexer, index)
        || LoopStatement::is_first_final(lexer, index)
        || BlockStatement::is_first_final(lexer, index)
    }
}
#[cfg(feature = "parse_sess")]
impl ISyntaxItemGrammarX for Statement {
    
    fn is_first_finalx(sess: &ParseSession) -> bool {
        VarDeclStatement::is_first_finalx(sess)
        || BreakStatement::is_first_finalx(sess)
        || ContinueStatement::is_first_finalx(sess)
        || ReturnStatement::is_first_finalx(sess)
        || ExprStatement::is_first_finalx(sess)
        || IfStatement::is_first_finalx(sess)
        || WhileStatement::is_first_finalx(sess)
        || ForStatement::is_first_finalx(sess)
        || LoopStatement::is_first_finalx(sess)
        || BlockStatement::is_first_finalx(sess)
    }
}
impl ISyntaxItemParse for Statement {
    
    fn parse(lexer: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<Statement>, usize) {

        if VarDeclStatement::is_first_final(lexer, index) { 
            return match VarDeclStatement::parse(lexer, messages, index) {
                (Some(var_decl), var_decl_len) => (Some(Statement::VarDecl(var_decl)), var_decl_len),
                (None, length) => (None, length),
            };
        } else if BreakStatement::is_first_final(lexer, index) {
            return match BreakStatement::parse(lexer, messages, index) {
                (Some(break_stmt), break_stmt_len) => (Some(Statement::Break(break_stmt)), break_stmt_len),
                (None, length) => (None, length),
            };
        } else if ContinueStatement::is_first_final(lexer, index) {
            return match ContinueStatement::parse(lexer, messages, index) {
                (Some(cont_stmt), cont_stmt_len) => (Some(Statement::Continue(cont_stmt)), cont_stmt_len),
                (None, length) => (None, length),
            };
        } else if ReturnStatement::is_first_final(lexer, index) {
            return match ReturnStatement::parse(lexer, messages, index) {
                (Some(ret_stmt), ret_stmt_len) => (Some(Statement::Return(ret_stmt)), ret_stmt_len),
                (None, length) => (None, length), 
            };
        } else if LoopStatement::is_first_final(lexer, index) {
            return match LoopStatement::parse(lexer, messages, index) {
                (Some(loop_stmt), loop_stmt_len) => (Some(Statement::Loop(loop_stmt)), loop_stmt_len),
                (None, length) => (None, length),
            };
        } else if WhileStatement::is_first_final(lexer, index) {
            return match WhileStatement::parse(lexer, messages, index) {
                (Some(while_stmt), while_stmt_len) => (Some(Statement::While(while_stmt)), while_stmt_len),
                (None, length) => (None, length),
            };
        } else if ForStatement::is_first_final(lexer, index) {
            return match ForStatement::parse(lexer, messages, index) {
                (Some(for_stmt), for_stmt_len) => (Some(Statement::For(for_stmt)), for_stmt_len),
                (None, length) => (None, length),
            };
        } else if IfStatement::is_first_final(lexer, index) {
            return match IfStatement::parse(lexer, messages, index) {
                (Some(if_stmt), if_stmt_len) => (Some(Statement::If(if_stmt)), if_stmt_len),
                (None, length) => (None, length),
            };
        } else if BlockStatement::is_first_final(lexer, index) {
            return match BlockStatement::parse(lexer, messages, index) {
                (Some(block), block_len) => (Some(Statement::Block(block)), block_len),
                (None, length) => (None, length),
            };
        } else if ExprStatement::is_first_final(lexer, index) {
            return match ExprStatement::parse(lexer, messages, index) {
                (Some(expr_stmt), expr_stmt_len) => (Some(Statement::Expr(expr_stmt)), expr_stmt_len),
                (None, length) => (None, length),
            }
        } else {
            return push_unexpect!(lexer, messages, "statement", 0, index);
        }
    }
}
#[cfg(feature = "parse_sess")]
impl ISyntaxItemParseX for Statement {

    fn parsex(sess: &mut ParseSession) -> ParseResult<Statement> {

        if VarDeclStatement::is_first_finalx(sess) { 
            return Ok(Statement::VarDecl(VarDeclStatement::parsex(sess)?));
        } else if BreakStatement::is_first_finalx(sess) {
            return Ok(Statement::Break(BreakStatement::parsex(sess)?));
        } else if ContinueStatement::is_first_finalx(sess) {
            return Ok(Statement::Continue(ContinueStatement::parsex(sess)?));
        } else if ReturnStatement::is_first_finalx(sess) {
            return Ok(Statement::Return(ReturnStatement::parsex(sess)?));
        } else if LoopStatement::is_first_finalx(sess) {
            return Ok(Statement::Loop(LoopStatement::parsex(sess)?));
        } else if WhileStatement::is_first_finalx(sess) {
            return Ok(Statement::While(WhileStatement::parsex(sess)?));
        } else if ForStatement::is_first_finalx(sess) {
            return Ok(Statement::For(ForStatement::parsex(sess)?));
        } else if IfStatement::is_first_finalx(sess) {
            return Ok(Statement::If(IfStatement::parsex(sess)?));
        } else if BlockStatement::is_first_finalx(sess) {
            return Ok(Statement::Block(BlockStatement::parsex(sess)?));
        } else if ExprStatement::is_first_finalx(sess) {
            return Ok(Statement::Expr(ExprStatement::parsex(sess)?));
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