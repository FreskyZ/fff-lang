
// Statement = 
//     ConstDecl | VarDecl 
//     | IfStatement | WhileStatement | ForStatement | LoopStatement 
//     | AssignStatement
//     | JumpStatement
 

use std::fmt;

use common::StringPosition;

use lexical::Lexer;
use lexical::IToken;
use lexical::KeywordKind;

use syntax::ast_item::IASTItem;

mod var_decl;
mod expr_stmt;
mod if_stmt;
mod for_stmt;
mod jump_stmt;
mod while_stmt;
mod loop_stmt;

pub use self::var_decl::VarDeclStatement;
pub use self::jump_stmt::JumpStatementType;
pub use self::jump_stmt::JumpStatement;
pub use self::expr_stmt::ExpressionStatement;
pub use self::loop_stmt::LoopStatement;
pub use self::while_stmt::WhileStatement;
pub use self::for_stmt::ForStatement;
pub use self::if_stmt::ElseIfBranch;
pub use self::if_stmt::IfStatement;

#[derive(Eq, PartialEq)]
pub enum Statement {
    VarDecl(VarDeclStatement),        // const, var
    Jump(JumpStatement),              // break, continue, return
    Expression(ExpressionStatement),  // _, (unary seperator, literal, identifier, left paren, left bracket)
    If(IfStatement),                  // if
    While(WhileStatement),            // while
    For(ForStatement),                // for
    Loop(LoopStatement),              // loop
}
impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Statement::VarDecl(ref inner) => write!(f, "{:?}", inner),
            Statement::Jump(ref inner) => write!(f, "{:?}", inner),
            Statement::Expression(ref inner) => write!(f, "{:?}", inner),
            Statement::If(ref inner) => write!(f, "{:?}", inner),
            Statement::While(ref inner) => write!(f, "{:?}", inner),
            Statement::For(ref inner) => write!(f, "{:?}", inner),
            Statement::Loop(ref inner) => write!(f, "{:?}", inner),
        }
    }
}
impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Statement::VarDecl(ref inner) => write!(f, "{}", inner),
            Statement::Jump(ref inner) => write!(f, "{}", inner),
            Statement::Expression(ref inner) => write!(f, "{}", inner),
            Statement::If(ref inner) => write!(f, "{}", inner),
            Statement::While(ref inner) => write!(f, "{}", inner),
            Statement::For(ref inner) => write!(f, "{}", inner),
            Statement::Loop(ref inner) => write!(f, "{}", inner),
        }
    }
}

impl Statement {

    pub fn get_id(&self) -> usize {
        match *self {
            Statement::VarDecl(ref inner) => inner.id,
            Statement::Jump(ref inner) => inner.id,
            Statement::Expression(ref inner) => inner.id,
            Statement::If(ref inner) => inner.id,
            Statement::While(ref inner) => inner.id,
            Statement::For(ref inner) => inner.id,
            Statement::Loop(ref inner) => inner.id,
        }
    }

    pub fn set_id(&mut self, id: usize) {
        match *self {
            Statement::VarDecl(ref mut inner) => inner.id = id,
            Statement::Jump(ref mut inner) => inner.id = id,
            Statement::Expression(ref mut inner) => inner.id = id,
            Statement::If(ref mut inner) => inner.id = id,
            Statement::While(ref mut inner) => inner.id = id,
            Statement::For(ref mut inner) => inner.id = id,
            Statement::Loop(ref mut inner) => inner.id = id,
        }
    }
}

impl IASTItem for Statement {

    fn pos_all(&self) -> StringPosition {
        match *self {  
            Statement::VarDecl(ref inner) => inner.pos_all(),
            Statement::Jump(ref inner) => inner.pos_all(),
            Statement::Expression(ref inner) => inner.pos_all(),
            Statement::If(ref inner) => inner.pos_all(),
            Statement::While(ref inner) => inner.pos_all(),
            Statement::For(ref inner) => inner.pos_all(),
            Statement::Loop(ref inner) => inner.pos_all(),
        }
    }
    
    fn parse(lexer: &mut Lexer, index: usize) -> (Option<Statement>, usize) {

        match lexer.nth(index).get_keyword() {
            Some(&KeywordKind::Const) 
            | Some(&KeywordKind::Var) => match VarDeclStatement::parse(lexer, index) {
                (Some(var_decl), var_decl_len) => (Some(Statement::VarDecl(var_decl)), var_decl_len),
                (None, length) => (None, length),
            },
            Some(&KeywordKind::Break) 
            | Some(&KeywordKind::Continue) 
            | Some(&KeywordKind::Return) => match JumpStatement::parse(lexer, index) {
                (Some(jump_stmt), jump_stmt_len) => (Some(Statement::Jump(jump_stmt)), jump_stmt_len),
                (None, length) => (None, length), 
            },
            Some(&KeywordKind::Loop) => match LoopStatement::parse(lexer, index) {
                (Some(loop_stmt), loop_stmt_len) => (Some(Statement::Loop(loop_stmt)), loop_stmt_len),
                (None, length) => (None, length),
            },
            Some(&KeywordKind::While) => match WhileStatement::parse(lexer, index) {
                (Some(while_stmt), while_stmt_len) => (Some(Statement::While(while_stmt)), while_stmt_len),
                (None, length) => (None, length),
            },
            Some(&KeywordKind::For) => match ForStatement::parse(lexer, index) {
                (Some(for_stmt), for_stmt_len) => (Some(Statement::For(for_stmt)), for_stmt_len),
                (None, length) => (None, length),
            },
            Some(&KeywordKind::If) => match IfStatement::parse(lexer, index) {
                (Some(if_stmt), if_stmt_len) => (Some(Statement::If(if_stmt)), if_stmt_len),
                (None, length) => (None, length),
            },
            _ => match ExpressionStatement::parse(lexer, index) {
                (Some(expr_stmt), expr_stmt_len) => (Some(Statement::Expression(expr_stmt)), expr_stmt_len),
                (None, length) => (None, length),
            }
        }
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn ast_stmt_all() {

    }
}