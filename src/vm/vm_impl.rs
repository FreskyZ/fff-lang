
use message::RuntimeMessage;
use message::MessageEmitter;

use lexical::LitValue;

use codegen::Operand;
use codegen::Code;
use codegen::Program;
use codegen::FnCollection;
use codegen::Type;
use codegen::ItemID;
use codegen::TypeCollection;

#[derive(Debug, PartialEq)]
enum RuntimeValue {
    Nothing,           // dummy
    Auto,              // auto, assign to any other but any operation are invalid
    Unit,
    Int(usize, u64),   // typeid, value
    F32(f32),
    F64(f64),
    Char(char),
    Bool(bool),
    Str(String),
    Array(usize, Vec<RuntimeValue>), // item type, items
    StrRef(usize),                   // heap index
    ArrayRef(usize, usize),          // item type, heap index
    Tuple(Vec<usize>, Vec<RuntimeValue>), // items type
}
impl Eq for RuntimeValue{
}
impl RuntimeValue {
    fn with_type(ty: &Type, types: &TypeCollection, heap: &mut RuntimeHeap) -> RuntimeValue {
        match ty {
            &Type::Base(ref typename) => match typename.as_ref() {
                "unit" => RuntimeValue::Unit,
                "i8" => RuntimeValue::Int(1, 0),
                "u8" => RuntimeValue::Int(2, 0),
                "i16" => RuntimeValue::Int(3, 0),
                "u16" => RuntimeValue::Int(4, 0),
                "i32" => RuntimeValue::Int(5, 0),
                "u32" => RuntimeValue::Int(6, 0),
                "i64" => RuntimeValue::Int(7, 0),
                "u64" => RuntimeValue::Int(8, 0),
                "f32" => RuntimeValue::F32(0f32),
                "f64" => RuntimeValue::F64(0f64),
                "char" => RuntimeValue::Char('\0'),
                "bool" => RuntimeValue::Bool(false),
                "string" => RuntimeValue::StrRef(heap.allocate_string()),
                "auto" => RuntimeValue::Auto,
                _ => unreachable!()
            },
            &Type::Array(ref inner) => RuntimeValue::ArrayRef(*inner, heap.allocate_array(*inner)),
            &Type::Tuple(ref item_types) => {
                let mut values = Vec::new();
                for _ty in item_types {
                    // values.push(RuntimeValue::with_type(&types.items[*ty], types, heap));
                }
                RuntimeValue::Tuple(item_types.clone(), values)
            }
        }
    }
}

struct RuntimeHeap {
    objs: Vec<RuntimeValue>,
}
impl RuntimeHeap {
    fn new() -> RuntimeHeap {
        RuntimeHeap{
            objs: vec![RuntimeValue::Nothing], // nothing at nullptr
        }
    }
    fn allocate_array(&mut self, item_ty: usize) -> usize {
        let ret_val = self.objs.len();
        self.objs.push(RuntimeValue::Array(item_ty, Vec::new()));
        return ret_val;
    }
    fn allocate_string(&mut self) -> usize {
        self.objs.push(RuntimeValue::Str(String::new()));
        self.objs.len() - 1
    }
}

// execution state in a function
struct ExecutionContext {
    fn_index: usize,
    stack: Vec<RuntimeValue>,
    rax: RuntimeValue,
    rip: usize,
}
impl ExecutionContext {

    fn new(fn_index: usize) -> ExecutionContext {
        ExecutionContext{
            fn_index: fn_index,
            stack: Vec::new(),
            rax: RuntimeValue::Nothing,
            rip: 0,
        }
    }

    fn push_var_decl(&mut self, ty: &Type, types: &TypeCollection, heap: &mut RuntimeHeap) {
        self.stack.push(RuntimeValue::with_type(ty, types, heap));
    } 

    fn operand_as_bool(&self, operand: &Operand) -> Result<bool, RuntimeMessage> {
        match operand {
            &Operand::Lit(LitValue::Bool(value)) => Ok(value),
            &Operand::Lit(_) => Err(RuntimeMessage::ConvertNonBoolToBool),
            &Operand::Stack(local_offset) => match self.stack[local_offset] {
                RuntimeValue::Bool(value) => Ok(value),
                _ => Err(RuntimeMessage::ConvertNonBoolToBool),
            },
            &Operand::Register => match self.rax {
                RuntimeValue::Bool(value) => Ok(value),
                _ => Err(RuntimeMessage::ConvertNonBoolToBool),
            },
            &Operand::Unknown => unreachable!()
        }
    }
}

enum CircleResult{
    Continue,
    Err(RuntimeMessage),
    Exit,
}

pub struct VirtualMachine {
    fns: FnCollection, 
    types: TypeCollection, 
    codes: Vec<Code>,
    ctxts: Vec<ExecutionContext>,
    heap: RuntimeHeap, // heap are shared
}
impl VirtualMachine {
    pub fn new(program: Program) -> VirtualMachine {
        VirtualMachine{
            fns: program.fns,
            types: program.types,
            codes: program.codes,
            ctxts: Vec::new(),
            heap: RuntimeHeap::new(),
        }
    }

    fn find_main(&self) -> Option<usize> { // main fn index
        for (index, f) in self.fns.iter().enumerate() {
            if f.name.fmt(&self.types) == "main" && f.args.len() == 0 && f.ret_type == ItemID::new(0) { // currently only support fn main() { ... }
                return Some(index);
            }
        }
        return None;
    }

    fn circle(&mut self) -> CircleResult {

        let ctxts_len = self.ctxts.len();
        let cur_ctxt = &mut self.ctxts[ctxts_len - 1]; // if it's empty it's internal error
        let codes = &self.codes;
        let cur_code = &codes[cur_ctxt.rip];               // if rip out of bound it's internal error

        match cur_code {
            &Code::Goto(ref new_rip) => {
                cur_ctxt.rip = *new_rip;
            }
            &Code::GotoIf(ref operand, ref target, ref new_rip) => {
                match cur_ctxt.operand_as_bool(operand) {
                    Ok(value) => if value == *target { cur_ctxt.rip = *new_rip }, // else nothing
                    Err(msg) => return CircleResult::Err(msg),
                }
            }

            &Code::Unary(ref _operand, ref _op) => {
                
            }
            _ => ()
        }

        CircleResult::Exit
    }

    pub fn execute(&mut self) -> Option<RuntimeMessage> {

        let main_fn_index = match self.find_main() {
            Some(index) => index,
            None => return Some(RuntimeMessage::CannotFindMain),
        };

        self.ctxts.push(ExecutionContext::new(main_fn_index));
        
        loop {
            match self.circle() {
                CircleResult::Continue => continue,
                CircleResult::Err(msg) => return Some(msg),
                CircleResult::Exit => return None,
            }
        }
    }
}

#[cfg(test)] #[test]
fn vm_nomain() {

    let fns = FnCollection::new();
    let types = TypeCollection::new();
    let mut vm = VirtualMachine::new(Program{ fns: fns, types: types, msgs: MessageEmitter::new(), codes: Vec::new() });

    match vm.execute() {
        Some(exception) => assert_eq!(exception, RuntimeMessage::CannotFindMain),
        None => panic!("Unexpectedly succeed"),
    }
}