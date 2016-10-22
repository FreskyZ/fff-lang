
// generate vm code from AST

// flatten AST to flattened AST code, perform name resolve and type check while flattening, scope concept is removed in this step
// currently directly execute flattened AST code

// vm is a register based virtual machine
// everything here is on call stack, or a reference on call stack to a managed heap
// call stack frame is traditional 
// RBP[0], return address(actually points to previous stack frame, RIP),
// RBP[n], local variables, formed by various stack items
// stack item is one local variable, has value and type

use lexical::NumericLiteralValue;

pub enum LiteralValue {
    StringLiteral(String),
    CharLiteral(char),
    BooleanLiteral(bool),
    NumericLiteral(NumericLiteralValue),
}

pub enum VMCode{
    LoadLiteral(LiteralValue),  // load literal to rax,
    StoreLocal(usize),          // store rax to rbp[n],
     
}