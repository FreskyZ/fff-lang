
// VM Instruction design

#[derive(Debug, PartialEq)]
pub enum LiteralValue {
    IntegralLiteral(u64),  // integral, char, bool
    FloatingLiteral(f64),  // f32, f64
    StringLiteral(String),  
}
impl Eq for LiteralValue {
}

#[derive(Debug, Eq, PartialEq)]
pub enum Operand {
    Literal(LiteralValue),
    Identifier(String),
}

pub enum UnaryOperator {

}
pub enum BinaryOperator {

}

pub enum Register {
    RAX,
    EAX,
    AX,
    AH,
    AL,

    RIP,
    RBP,
}

pub enum AddrOrReg {
    Stack(usize), // actually negative, but all negative
    Heap(usize),
    Reg(Register),
}

pub enum Code {
    
    Mov(AddrOrReg, AddrOrReg),                    // Move from 0 to 1
    UnOp(AddrOrReg, UnaryOperator),               // UnOp on 0 and return to rax
    BinOp(AddrOrReg, AddrOrReg, BinaryOperator),  // BinOp on 0 and 1 and return to rax

    Goto(usize),
    GotoIf(bool, usize),
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub struct CodeID(usize); // CodeID are always valid

pub struct CodeCollection {
    codes: Vec<Code>,
}

impl CodeCollection {
    
    pub fn new() -> CodeCollection {
        CodeCollection{ codes: Vec::new() }
    }

    pub fn push(&mut self, code: Code) -> CodeID {
        let ret_val = self.codes.len();
        self.codes.push(code);
        return CodeID(ret_val);
    }
}