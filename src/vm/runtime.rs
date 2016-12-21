
// Runtime value, slot content in runtime heap and stack

use common::format_vector_debug;

use lexical::LitValue;
use lexical::NumLitValue;

use codegen::Type;
use codegen::TypeCollection;
use codegen::Operand;

#[derive(Debug, PartialEq, Clone)]
pub enum RuntimeValue {
    Nothing,           // dummy
    Unit,
    Int(u64),   // typeid, value
    Float(f64),
    Char(char),
    Bool(bool),
    Str(String),
    Array(Vec<RuntimeValue>), // items
    ArrayRef(usize),          // heap index
    Tuple(Vec<RuntimeValue>), // 
    StoredRegister(usize),
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

    pub fn from_type(ty: &Type, _types: &TypeCollection) -> RuntimeValue {
        // match ty {
        //     &Type::Base(ref typename) => match typename.as_ref() {
        //         "unit" => RuntimeValue::Unit,
        //         "i8" => RuntimeValue::Int(0),
        //         "u8" => RuntimeValue::Int(0),
        //         "i16" => RuntimeValue::Int(0),
        //         "u16" => RuntimeValue::Int(0),
        //         "i32" => RuntimeValue::Int(0),
        //         "u32" => RuntimeValue::Int(0),
        //         "i64" => RuntimeValue::Int(0),
        //         "u64" => RuntimeValue::Int(0),
        //         "f32" => RuntimeValue::F32(0f32),
        //         "f64" => RuntimeValue::F64(0f64),
        //         "char" => RuntimeValue::Char('\0'),
        //         "bool" => RuntimeValue::Bool(false),
        //         "string" => RuntimeValue::StrRef(heap.allocate_string()),
        //         "auto" => RuntimeValue::Auto,
        //         _ => unreachable!()
        //     },
        //     &Type::Array(ref inner) => RuntimeValue::ArrayRef(*inner, heap.allocate_array(*inner)),
        //     &Type::Tuple(ref item_types) => {
        //         let values = Vec::new();
        //         for _ty in item_types {
        //             // values.push(RuntimeValue::with_type(&types.items[*ty], types, heap));
        //         }
        //         RuntimeValue::Tuple(item_types.clone(), values)
        //     }
        // }
        RuntimeValue::Unit
    }

    pub fn get_field(&self, field_id: usize) -> RuntimeValue {
        match *self {
            RuntimeValue::Tuple(ref values) => values[field_id].clone(),
            _ => unreachable!()
        }
    } 

    pub fn unwrap_stored_register(&self) -> usize {
        match *self {
            RuntimeValue::StoredRegister(ref value) => *value,
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

    pub fn reserve_stack(&mut self, local_size: usize) {
        for _ in 0..local_size {
            self.stack.push(RuntimeValue::Nothing);
        }
    }

    pub fn operand_to_value(&mut self, operand: &Operand) -> RuntimeValue { // copy
        match operand {
            &Operand::Unknown => unreachable!(),
            &Operand::Lit(ref lit_value) => RuntimeValue::from_lit(lit_value),
            &Operand::Stack(ref index) => self.stack[*index].clone(),
            &Operand::Register => self.rax.clone(),
        }
    }

    // return heap object index
    pub fn allocate_heap(&mut self, value: RuntimeValue) -> usize {
        let ret_val = self.heap.len();
        self.heap.push(value);
        return ret_val;
    }

    pub fn dump(&self) -> String {
        format!("\n   RIP: {}\n    RBP: {}\n    RSP: {}\n    RAX: {:?}\n    Stack:\n {}\n    Heap: {}\n", 
            self.rip, self.rbp, self.rsp, self.rax, format_vector_debug(&self.stack, "\n        "), format_vector_debug(&self.heap, "\n        "))
    }
}