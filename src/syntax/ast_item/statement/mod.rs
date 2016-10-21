
// Statement = 
//     ConstDecl | VarDecl 
//     | IfStatement | WhileStatement | ForStatement 
//     | AssignStatement | OpAssignStatement
//     | ContinueStatement | BreakStatement | ReturnStatement

// ConstDecl = fConst Type fIdentifier fAssign Expression fSemiColon                                                                // const ...;
// VarDecl = fVar Type fIdentifier fAssign Expression fSemiColon                                                                    // var ...;
// IfStatement = fIf Expression Block [fElse fIf  Expression Block]* [ fElse Block ]  // if ...}   
// WhileStatement = fWhile Expression Block                                                                 // while ...}
// // furthuer on this: `continue 0;` for current block, `continue 1;` and `continue 2;` and more for outter level
// ContinueStatement = fContinue fSemiColon                                                                                        // continue;
// BreakStatement = fBreak fSemiColon                                                                                              // break;
// ForStatement = fFor fLeftParen fIdentifier fIn Expression fRange Expression fRightParen Block  // for i in 1..5                  // for ...}
// AssignStatement = fIdentifier AssignOperator Expression fSemiColon                                                               // ... = ...;
// ReturnStatement = fReturn [Expression] fSemiColon                                                                               // return ...;

use common::StringPosition;

use lexical::Lexer;
// use lexical::IToken;
// use lexical::SeperatorKind;

use syntax::ast_item::IASTItem;
use syntax::Expression;

#[derive(Debug, Eq, PartialEq)]
pub struct Statement {
    pub exprs: Vec<Expression>,
}

impl IASTItem for Statement {

    fn pos_all(&self) -> StringPosition {  
        StringPosition::new()
    }
    
    fn parse(lexer: &mut Lexer, index: usize) -> (Option<Statement>, usize) {

        (None, 0)
    }
}

