
// VM Instruction design

#[derive(Debug, PartialEq)]
pub enum LitValue {
    Int(u64),    // integral, char, bool
    Float(f64),  // f32, f64
}
impl Eq for LitValue {
}

pub enum Register {
    RAX,
    // EAX,
    // AX,
    // AH, // I don't know what it can do, just leave it here
    // AL,
    RIP,
    RBP,
}

pub enum Operand {
    Lit(LitValue),
    Stack(usize), // actually negative, but all negative
    Heap(usize),
    Reg(Register),
}

pub enum Code {
    
    LoadStr(String, usize),                   // Special for Str literal, load to stack offset
                                              // because before string is constructed, you cannot move the rust string literal to the stack 
                                              // and call `?string_new` internal fn

    Goto(usize),
    GotoIf(bool, usize),

    Halt,                                     // Special Halt at head for vm exit
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub struct CodeID(usize); // CodeID are always valid

pub struct CodeCollection {
    codes: Vec<Code>,
}

impl CodeCollection {
    
    pub fn new() -> CodeCollection {
        CodeCollection{ codes: vec![Code::Halt] }
    }

    pub fn emit(&mut self, code: Code) -> CodeID {
        let ret_val = self.codes.len();
        self.codes.push(code);
        return CodeID(ret_val);
    }
}