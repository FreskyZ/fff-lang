
// VM Instruction design

#[derive(Debug, PartialEq)]
pub enum LitValue {
    Int(u64),  // integral, char, bool
    Float(f64),  // f32, f64
    Str(String),  
}
impl Eq for LiteralValue {
}

pub enum Register {
    RAX,
    EAX,
    AX,
    AH, // I don't know what it can do, just leave it here
    AL,
    RIP,
    RBP,
}

pub enum Operand {
    Lit(LitValue),
    Stack(usize), // actually negative, but all negative
    Heap(usize),
    Reg(Register),
}

pub enum UnaryOperator {

}
pub enum BinaryOperator {

}

pub enum Code {
    
    Mov(Operand, Operand),                    // Move from 0 to 1
    UnOp(Operand, UnaryOperator),               // UnOp on 0 and return to rax
    BinOp(Operand, Operand, BinaryOperator),  // BinOp on 0 and 1 and return to rax

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