
// VM Instruction design

use lexical::NumLitValue;
use syntax::SMType;

pub enum LiteralValue {
    StringLiteral(String),
    CharLiteral(char),
    BooleanLiteral(bool),
    NumericLiteral(NumLitValue),
    EmptyArray,
}

pub enum Operand {
    Literal(LiteralValue),
    Identifier(String),
}

pub enum Operator {

}

pub enum VMCode {
    ScopeEnter, 
    RegisterLocal(String, SMType),
    ScopeLeave,

    LoadLiteral(LiteralValue),      // load literal to rax,
    LoadLocal(String),              // read local with this name to  
    StoreLocal(String),             // store rax to rbp[n],

    PerformOperator( Operator),     // Perform operator on stack top

    Goto(usize),
    GotoIf(bool, usize),
}