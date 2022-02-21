
// Runtime value, slot content in runtime heap and stack

use crate::common::format_vector_debug;
use crate::lexical::LitValue;
use crate::lexical::NumLitValue;
use crate::codegen::Operand;

#[derive(Debug, PartialEq, Clone)]
pub enum RuntimeValue {
    Nothing,           // dummy
    Unit,
    Int(u64), 
    Float(f64),
    Char(char),
    Bool(bool),
    Str(String),
    Array(Vec<RuntimeValue>), // items
    ArrayRef(usize),          // heap index
    Tuple(Vec<RuntimeValue>), // 
    PrevRIP(usize),
    PrevRBP(usize),
}
impl Eq for RuntimeValue{
}
impl PartialEq<bool> for RuntimeValue {
    fn eq(&self, rhs: &bool) -> bool {
        match *self {
            RuntimeValue::Bool(ref value) => *value == *rhs,
            _ => unreachable!(),
        }
    }    
    fn ne(&self, rhs: &bool) -> bool {
        match *self {
            RuntimeValue::Bool(ref value) => *value != *rhs,
            _ => unreachable!(),
        }
    }
}
impl RuntimeValue {

    pub fn from_lit(lit: &LitValue) -> RuntimeValue {
        match lit {
            &LitValue::Unit => RuntimeValue::Unit,
            &LitValue::Str(Some(ref value)) => RuntimeValue::Str(value.clone()),   // none should be unreachable
            &LitValue::Char(Some(ref value)) => RuntimeValue::Char(*value), 
            &LitValue::Bool(ref value) => RuntimeValue::Bool(*value),
            &LitValue::Num(Some(NumLitValue::I8(ref value))) => RuntimeValue::Int(*value as u64),
            &LitValue::Num(Some(NumLitValue::U8(ref value))) => RuntimeValue::Int(*value as u64),
            &LitValue::Num(Some(NumLitValue::I16(ref value))) => RuntimeValue::Int(*value as u64),
            &LitValue::Num(Some(NumLitValue::U16(ref value))) => RuntimeValue::Int(*value as u64),
            &LitValue::Num(Some(NumLitValue::I32(ref value))) => RuntimeValue::Int(*value as u64),
            &LitValue::Num(Some(NumLitValue::U32(ref value))) => RuntimeValue::Int(*value as u64),
            &LitValue::Num(Some(NumLitValue::I64(ref value))) => RuntimeValue::Int(*value as u64),
            &LitValue::Num(Some(NumLitValue::U64(ref value))) => RuntimeValue::Int(*value as u64),
            &LitValue::Num(Some(NumLitValue::F32(ref value))) => RuntimeValue::Float(*value as f64),
            &LitValue::Num(Some(NumLitValue::F64(ref value))) => RuntimeValue::Float(*value as f64),
            _ => unreachable!(),
        }
    }

    pub fn get_str_lit(&self) -> &String {
        match *self {
            RuntimeValue::Str(ref value) => value,
            _ => unreachable!(),
        }
    }

    pub fn get_field(&self, field_id: usize) -> RuntimeValue {
        match *self {
            RuntimeValue::Tuple(ref values) => values[field_id].clone(),
            _ => unreachable!()
        }
    } 

    pub fn unwrap_stored_register(&self) -> usize {
        match *self {
            RuntimeValue::PrevRBP(ref value) => *value,
            RuntimeValue::PrevRIP(ref value) => *value,
            _ => unreachable!()
        }
    }
}

pub struct Runtime {
    pub rip: usize,
    pub rbp: usize,
    pub rsp: usize,
    pub rax: RuntimeValue,
    pub stack: Vec<RuntimeValue>,
    pub heap: Vec<RuntimeValue>,
}

impl Runtime {

    pub fn new() -> Runtime {
        Runtime{
            rip: 0,
            rbp: 0,
            rsp: 0,
            rax: RuntimeValue::Nothing,
            stack: Vec::new(),
            heap: Vec::new(),
        }
    }

    pub fn index(&self, idx: &Operand) -> RuntimeValue {
        match idx {
            &Operand::Unknown => unreachable!(),
            &Operand::Lit(ref lit_value) => RuntimeValue::from_lit(lit_value),
            &Operand::Stack(ref index) => self.stack[self.rbp + *index].clone(), // Currently +, future -
            &Operand::Register => self.rax.clone(),
        }
    }

    pub fn index_mut(&mut self, idx: &Operand) -> &mut RuntimeValue {
        match idx {
            &Operand::Unknown => unreachable!(),
            &Operand::Lit(ref _lit_value) => unreachable!(), // literal is not modifiable lvalue
            &Operand::Stack(ref index) => &mut self.stack[self.rbp + *index], // Currently +, future -
            &Operand::Register => &mut self.rax,
        }
    }

    pub fn reserve_stack(&mut self, local_size: usize) {
        // perrorln!("[DEBUG][3][vm/runtime.rs|Runtime::reserve_stack][FnBegin] Param local_size = {}", local_size);
        for _ in 0..local_size {
            self.stack.push(RuntimeValue::Nothing);
        }
    }

    // return heap object index
    pub fn allocate_heap(&mut self, value: RuntimeValue) -> usize {
        let ret_val = self.heap.len();
        self.heap.push(value);
        return ret_val;
    }

    pub fn dump(&self) -> String {
        format!("\n   RIP: {}\n   RBP: {}\n   RSP: {}\n   RAX: {:?}\n   Stack:\n      {}\n   Heap:\n      {}\n", 
            self.rip, self.rbp, self.rsp, self.rax, format_vector_debug(&self.stack, "\n      "), format_vector_debug(&self.heap, "\n      "))
    }
}