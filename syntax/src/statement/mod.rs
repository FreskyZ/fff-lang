
// Statement = 
//     ConstDecl | VarDecl 
//     | IfStatement | WhileStatement | ForStatement | LoopStatement 
//     | AssignStatement
//     | JumpStatement
// TODO: loop label to be identifier
 
use std::fmt;

use codepos::StringPosition;
use message::Message;
use message::MessageCollection;

use lexical::TokenStream;

use super::ISyntaxItem;
use super::Block;

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

#[cfg_attr(test, derive(Eq, PartialEq))]
pub enum Statement {
    VarDecl(VarDeclStatement),        // const, var
    Break(BreakStatement),            // break 
    Continue(ContinueStatement),      // continue
    Return(ReturnStatement),          // return
    Expression(ExprStatement),  // _, (unary seperator, literal, identifier, left paren, left bracket)
    If(IfStatement),                  // if
    While(WhileStatement),            // while
    For(ForStatement),                // for
    Loop(LoopStatement),              // loop
    Block(BlockStatement),            // {
}
impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Statement::VarDecl(ref inner) => write!(f, "{:?}", inner),
            Statement::Break(ref inner) => write!(f, "{:?}", inner),
            Statement::Continue(ref inner) => write!(f, "{:?}", inner),
            Statement::Return(ref inner) => write!(f, "{:?}", inner),
            Statement::Expression(ref inner) => write!(f, "{:?}", inner),
            Statement::If(ref inner) => write!(f, "{:?}", inner),
            Statement::While(ref inner) => write!(f, "{:?}", inner),
            Statement::For(ref inner) => write!(f, "{:?}", inner),
            Statement::Loop(ref inner) => write!(f, "{:?}", inner),
            Statement::Block(ref inner) => write!(f, "{:?}", inner),
        }
    }
}
impl ISyntaxItem for Statement {

    fn pos_all(&self) -> StringPosition {
        match *self {  
            Statement::VarDecl(ref inner) => inner.pos_all(),
            Statement::Break(ref inner) => inner.pos_all(),
            Statement::Continue(ref inner) => inner.pos_all(),
            Statement::Return(ref inner) => inner.pos_all(),
            Statement::Expression(ref inner) => inner.pos_all(),
            Statement::If(ref inner) => inner.pos_all(),
            Statement::While(ref inner) => inner.pos_all(),
            Statement::For(ref inner) => inner.pos_all(),
            Statement::Loop(ref inner) => inner.pos_all(),
            Statement::Block(ref block) => block.pos_all(),
        }
    }
    
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
        || Block::is_first_final(lexer, index)
    }
    
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
                (Some(expr_stmt), expr_stmt_len) => (Some(Statement::Expression(expr_stmt)), expr_stmt_len),
                (None, length) => (None, length),
            }
        } else {
            return push_unexpect!(lexer, messages, "statement", 0, index);
        }
    }
}

#[cfg(test)] #[test] #[ignore]
fn stmt_parse() {

}

// TODO: 
// Add ISyntaxItemFormat to all types, 
// impl Debug for ISyntaxItemFormat
// Remove ISyntaxItem::pos_all, add ISyntaxItemGetPosition
// try set Eq and PartialEq as test only
// For and While add label support
// move is_first_final into ISyntaxItemGrammar
// move parse into ISyntaxItemParse and apply ParseSession
// try design and apply ISyntaxItemResolve and ResolveSession