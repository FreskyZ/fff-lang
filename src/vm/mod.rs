
// virtual machine
// mainly primitive type implementations
// vmcode design is in mod codegen

use codegen::Code;
use codegen::Program;
use codegen::FnCollection;
use codegen::TypeCollection;

enum RuntimeValue {
    Int(usize, u64),   // typeid, value
    Float(usize, f64), // typeid, value
    Char(char),
    Bool(bool),
    Str(String),
    Array(Vec<RuntimeValue>),
    Tuple(Vec<RuntimeValue>),
    ScopeBarrier,
}

// execution state in a function
struct ExecutionContext {
    stack: Vec<RuntimeValue>,
    rax: RuntimeValue,
    rip: usize,
}

struct VirtualMachine {
    fns: FnCollection, 
    types: TypeCollection, 
}
impl VirtualMachine {
    fn new(program: Program) -> VirtualMachine {
        VirtualMachine{
            fns: program.fns,
            types: program.types,
        }
    }
}

pub fn run(vm_program: Program) {

    let _vm = VirtualMachine::new(vm_program);
}