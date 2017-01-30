
// VM Instruction design

use std::fmt;

use util::format_vector_debug;

use lexical::LitValue;
use lexical::SeperatorKind;

use codegen::ItemID;

#[derive(Eq, PartialEq, Clone)]
pub enum Operand {
    Unknown,
    Lit(LitValue),
    Stack(usize), // [rbp - n]
    Register,     // act as register rax, every operation return at stacktop, only store moves it some where
} 
impl fmt::Debug for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Operand::Unknown => write!(f, "<unknown>"),
            Operand::Lit(ref lit) => write!(f, "{}", lit),
            Operand::Stack(ref offset) => write!(f, "[rbp - {}]", offset),
            Operand::Register => write!(f, "rax"),
        }
    }
}

#[derive(Clone)]
pub enum Code {
    PlaceHolder,

    Call(ItemID, Vec<Operand>),
    FieldAccess(Operand, usize),               // operand, field id
    Store(usize, Operand),                  

    Return(Operand),                           // return; is return ();
    Goto(usize),
    GotoIf(Operand, bool, usize),
}
impl fmt::Debug for Code {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Code::PlaceHolder => 
                write!(f, "placeholder"),
            Code::Call(ref id, ref params) => 
                write!(f, "Call {:?}, {}", id, format_vector_debug(params, ", ")),
            Code::FieldAccess(ref operand, ref field_id) => 
                write!(f, "FieldAccess {:?} {}", operand, field_id),
            Code::Store(ref target, ref src) =>
                write!(f, "Store {:?} {:?}", target, src),
            Code::Return(ref op) => 
                write!(f, "Return {:?}", op),
            Code::Goto(ref id) => 
                write!(f, "Goto {:?}", id),
            Code::GotoIf(ref op, true, ref id) =>
                write!(f, "Goto {:?} if {:?} is true", id, op),
            Code::GotoIf(ref op, false, ref id) => 
                write!(f, "Goto {:?} if {:?} is false", id, op),
        }
    }
}

pub struct CodeCollection {
    pub codes: Vec<Code>,
}

impl CodeCollection {
    
    pub fn new() -> CodeCollection {
        CodeCollection{ codes: Vec::new() }
    }

    pub fn emit(&mut self, code: Code) -> usize {
        let ret_val = self.codes.len();
        self.codes.push(code);
        return ret_val;
    }
    pub fn emit_silent(&mut self, code: Code) {
        self.codes.push(code);
    }

    pub fn next_id(&self) -> usize {
        self.codes.len()
    }
    pub fn dummy_id() -> usize {
        !0
    }

    pub fn refill(&mut self, id: usize, code: Code) {
        self.codes[id] = code;
    }
    pub fn refill_addr(&mut self, gotoid: usize, target_id: usize) {
        match self.codes[gotoid] {
            Code::Goto(ref mut id) => *id = target_id,
            Code::GotoIf(ref _op, ref _bool, ref mut id) => *id = target_id,
            _ => unreachable!(),
        }
    }

    pub fn as_slice(&self) -> &[Code] {
        self.codes.as_slice()
    }

    pub fn dump(&self) -> String {
        format_vector_debug(&self.codes.iter().enumerate().collect(), "\n")
    }
}