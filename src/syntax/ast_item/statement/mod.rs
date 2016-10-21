
// Statement = 
//     ConstDecl | VarDecl 
//     | IfStatement | WhileStatement | ForStatement 
//     | AssignStatement | OpAssignStatement
//     | ContinueStatement | BreakStatement | ReturnStatement

// IfStatement = fIf Expression Block [fElse fIf Expression Block]* [ fElse Block ]  // if ...}   
// WhileStatement = fWhile Expression Block                                                                 // while ...}
// // furthuer on this: `continue 0;` for current block, `continue 1;` and `continue 2;` and more for outter level, include break
// ContinueStatement = fContinue fSemiColon                                                                                        // continue;
// BreakStatement = fBreak fSemiColon                                                                                              // break;
// ForStatement = fFor fIdentifier fIn Expression fRange Expression Block  // for i in 1..5                  // for ...}
// AssignStatement = Expression AssignOperator Expression fSemiColon                                                               // ... = ...;
// ReturnStatement = fReturn [Expression] fSemiColon                                                                               // return ...;

use common::StringPosition;

use lexical::Lexer;
// use lexical::IToken;
// use lexical::SeperatorKind;

use syntax::ast_item::IASTItem;
use syntax::Expression;
use syntax::Block;

mod var_decl;
mod assign_stmt;
mod if_stmt;
mod for_stmt;
mod jump_stmt;
mod while_stmt;
use self::var_decl::VarDeclStatement;

#[derive(Debug, Eq, PartialEq)]
pub struct ElseIfBranch {
    pub cond_expr: Expression,
    pub body: Block,
    pub pos: [StringPosition; 2],   // position for if and else
}

#[derive(Debug, Eq, PartialEq)]
pub struct IfStatement {
    pub id: usize,
    pub if_expr: Expression,
    pub if_pos: StringPosition,
    pub if_body: Block,
    pub elseifs: Vec<ElseIfBranch>,
    pub else_body: Block,
    pub else_pos: StringPosition,
}

#[derive(Debug, Eq, PartialEq)]
pub struct WhileStatement {
    pub id: usize,
    pub cond_expr: Expression,
    pub body: Block,
    pub while_pos: StringPosition,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ForStatement {
    pub id: usize,
    pub iter_name: String, 
    pub expr_low: Expression,
    pub expr_high: Expression,
    pub body: Block,
    pub pos: [StringPosition; 4]  // position for 'for', iter_name, 'in', range operator, 
}

#[derive(Debug, Eq, PartialEq)]
pub struct AssignmentStatement {
    pub id: usize,
    pub left_expr: Expression,
    pub right_expr: Expression,
    pub pos: [StringPosition; 2],    // position for '=' and ';'
}

#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
    VarDecl(VarDeclStatement),
    If(IfStatement), 
    While(WhileStatement),
    For(ForStatement),
    Return(StringPosition, usize),
    Break(StringPosition, usize),
    Continue(StringPosition, usize),
    Assignment(AssignmentStatement),
}

impl IASTItem for Statement {

    fn pos_all(&self) -> StringPosition {  
        StringPosition::new()
    }
    
    fn parse(lexer: &mut Lexer, index: usize) -> (Option<Statement>, usize) {

        (None, 0)
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn ast_stmt_() {

    }
}