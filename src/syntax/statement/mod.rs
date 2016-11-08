
// Statement = 
//     ConstDecl | VarDecl 
//     | IfStatement | WhileStatement | ForStatement | LoopStatement 
//     | AssignStatement
//     | JumpStatement
 
use std::fmt;
use std::cmp;

use common::StringPosition;

use lexical::Lexer;

use syntax::ast_item::IASTItem;
use syntax::Block;

mod var_decl;
mod expr_stmt;
mod if_stmt;
mod for_stmt;
mod jump_stmt;
mod while_stmt;
mod loop_stmt;

pub use self::var_decl::VarDeclStatement;
pub use self::jump_stmt::BreakStatement;
pub use self::jump_stmt::ContinueStatement;
pub use self::jump_stmt::ReturnStatement;
pub use self::expr_stmt::ExpressionStatement;
pub use self::loop_stmt::LoopStatement;
pub use self::while_stmt::WhileStatement;
pub use self::for_stmt::ForStatement;
pub use self::if_stmt::ElseIfBranch;
pub use self::if_stmt::IfStatement;

pub enum Statement {
    VarDecl(VarDeclStatement),        // const, var
    Break(BreakStatement),            // break 
    Continue(ContinueStatement),      // continue
    Return(ReturnStatement),          // return
    Expression(ExpressionStatement),  // _, (unary seperator, literal, identifier, left paren, left bracket)
    If(IfStatement),                  // if
    While(WhileStatement),            // while
    For(ForStatement),                // for
    Loop(LoopStatement),              // loop
    Block(Block, usize),              // {
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
            Statement::Block(ref block, ref index) => write!(f, "<{}>{:?}", index, block),
        }
    }
}
impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Statement::VarDecl(ref inner) => write!(f, "{}", inner),
            Statement::Break(ref inner) => write!(f, "{}", inner),
            Statement::Continue(ref inner) => write!(f, "{}", inner),
            Statement::Return(ref inner) => write!(f, "{}", inner),
            Statement::Expression(ref inner) => write!(f, "{}", inner),
            Statement::If(ref inner) => write!(f, "{}", inner),
            Statement::While(ref inner) => write!(f, "{}", inner),
            Statement::For(ref inner) => write!(f, "{}", inner),
            Statement::Loop(ref inner) => write!(f, "{}", inner),
            Statement::Block(ref block, ref index) => write!(f, "<{}>{}", index, block),
        }
    }
}

impl cmp::PartialEq<Statement> for Statement {
    fn eq(&self, other: &Statement) -> bool { self.get_id() == other.get_id() }
    fn ne(&self, other: &Statement) -> bool { self.get_id() != other.get_id() }
}
impl cmp::Eq for Statement { 
}

impl Statement {

    pub fn get_id(&self) -> usize {
        match *self {
            Statement::VarDecl(ref inner) => inner.id,
            Statement::Break(ref inner) => inner.id,
            Statement::Continue(ref inner) => inner.id,
            Statement::Return(ref inner) => inner.id,
            Statement::Expression(ref inner) => inner.id,
            Statement::If(ref inner) => inner.id,
            Statement::While(ref inner) => inner.id,
            Statement::For(ref inner) => inner.id,
            Statement::Loop(ref inner) => inner.id,
            Statement::Block(ref _block, ref index) => *index, 
        }
    }

    pub fn set_id(&mut self, id: usize) {
        match *self {
            Statement::VarDecl(ref mut inner) => inner.id = id,
            Statement::Break(ref mut inner) => inner.id = id,
            Statement::Return(ref mut inner) => inner.id = id,
            Statement::Continue(ref mut inner) => inner.id = id,
            Statement::Expression(ref mut inner) => inner.id = id,
            Statement::If(ref mut inner) => inner.id = id,
            Statement::While(ref mut inner) => inner.id = id,
            Statement::For(ref mut inner) => inner.id = id,
            Statement::Loop(ref mut inner) => inner.id = id,
            Statement::Block(ref _block, ref mut index) => *index = id,
        }
    }
}

impl IASTItem for Statement {

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
            Statement::Block(ref block, ref _index) => block.pos_all(),
        }
    }
    
    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        VarDeclStatement::is_first_final(lexer, index)
        || BreakStatement::is_first_final(lexer, index)
        || ContinueStatement::is_first_final(lexer, index)
        || ReturnStatement::is_first_final(lexer, index)
        || ExpressionStatement::is_first_final(lexer, index)
        || IfStatement::is_first_final(lexer, index)
        || WhileStatement::is_first_final(lexer, index)
        || ForStatement::is_first_final(lexer, index)
        || LoopStatement::is_first_final(lexer, index)
        || Block::is_first_final(lexer, index)
    }
    
    fn parse(lexer: &mut Lexer, index: usize) -> (Option<Statement>, usize) {

        if VarDeclStatement::is_first_final(lexer, index) { 
            return match VarDeclStatement::parse(lexer, index) {
                (Some(var_decl), var_decl_len) => (Some(Statement::VarDecl(var_decl)), var_decl_len),
                (None, length) => (None, length),
            };
        } else if BreakStatement::is_first_final(lexer, index) {
            return match BreakStatement::parse(lexer, index) {
                (Some(break_stmt), break_stmt_len) => (Some(Statement::Break(break_stmt)), break_stmt_len),
                (None, length) => (None, length),
            };
        } else if ContinueStatement::is_first_final(lexer, index) {
            return match ContinueStatement::parse(lexer, index) {
                (Some(cont_stmt), cont_stmt_len) => (Some(Statement::Continue(cont_stmt)), cont_stmt_len),
                (None, length) => (None, length),
            };
        } else if ReturnStatement::is_first_final(lexer, index) {
            return match ReturnStatement::parse(lexer, index) {
                (Some(ret_stmt), ret_stmt_len) => (Some(Statement::Return(ret_stmt)), ret_stmt_len),
                (None, length) => (None, length), 
            };
        } else if LoopStatement::is_first_final(lexer, index) {
            return match LoopStatement::parse(lexer, index) {
                (Some(loop_stmt), loop_stmt_len) => (Some(Statement::Loop(loop_stmt)), loop_stmt_len),
                (None, length) => (None, length),
            };
        } else if WhileStatement::is_first_final(lexer, index) {
            return match WhileStatement::parse(lexer, index) {
                (Some(while_stmt), while_stmt_len) => (Some(Statement::While(while_stmt)), while_stmt_len),
                (None, length) => (None, length),
            };
        } else if ForStatement::is_first_final(lexer, index) {
            return match ForStatement::parse(lexer, index) {
                (Some(for_stmt), for_stmt_len) => (Some(Statement::For(for_stmt)), for_stmt_len),
                (None, length) => (None, length),
            };
        } else if IfStatement::is_first_final(lexer, index) {
            return match IfStatement::parse(lexer, index) {
                (Some(if_stmt), if_stmt_len) => (Some(Statement::If(if_stmt)), if_stmt_len),
                (None, length) => (None, length),
            };
        } else if Block::is_first_final(lexer, index) {
            return match Block::parse(lexer, index) {
                (Some(block), block_len) => (Some(Statement::Block(block, 0)), block_len),
                (None, length) => (None, length),
            };
        } else if ExpressionStatement::is_first_final(lexer, index) {
            return match ExpressionStatement::parse(lexer, index) {
                (Some(expr_stmt), expr_stmt_len) => (Some(Statement::Expression(expr_stmt)), expr_stmt_len),
                (None, length) => (None, length),
            }
        } else {
            return lexer.push_expect("statement", 0, index);
        }
    }
}

#[cfg(test)]
mod tests {
    
    #[test]
    #[ignore]
    fn ast_stmt_all() {

    }
}