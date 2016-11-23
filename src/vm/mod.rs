
// virtual machine
// mainly primitive type implementations
// vmcode design is in mod codegen

use codegen::Code;

pub enum RuntimeValue {
    I32(i32),
    U32(u32),
    F32(f32),
    F64(f64),
    Char(char),
    SMString(String),
    Array(Vec<RuntimeValue>),
    ScopeBarrier,
}

// execution state in a function
pub struct ExecutionState {
    insts: Vec<Code>,
    regs: usize,
    local_vars: Vec<RuntimeValue>,
    eval_stack: Vec<RuntimeValue>,
}

pub struct VirtualMachine {
    // funcs: Vec<FunctionDef>,
    states: Vec<ExecutionState>,
}

impl VirtualMachine {

    fn new() -> VirtualMachine { 
        VirtualMachine{ 
            // funcs: Vec::new(),
            states: Vec::new(),
        } 
    }

    // fn load_program(&mut self, mut program: Program) {

    //     self.funcs.append(&mut program.0);
    // }

    fn execute_step(&mut self) {

    }

    fn execute_to_end(&mut self) {

    }
}