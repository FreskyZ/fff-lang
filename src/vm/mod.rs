
// virtual machine
// mainly primitive type implementations
// vmcode design is in mod codegen

mod vm_impl;
mod runtime;
mod builtin_impl;

use message::RuntimeMessage;
use codegen::Program;

// Runtime exception will report immediately and abort
pub fn run(vm_program: Program) -> Option<RuntimeMessage> {
    use vm::vm_impl::VirtualMachine;

    let mut vm = VirtualMachine::new(vm_program);
    vm.execute()
}

// At first program is inputed, fns and types, fns include all codes
// then a vm is constructed, these information are loaded
// then the vm starts to run
// An execution context is prepared for main fn, reference to codes, types and fns are prepared,
// rax is init to nothing, rip is set to 0, and a circle starts
// for each circle, 
// retrive current code, if rip over bound, that's an error about no return, because no ret fns have emitted an return (), 
//     move forward rip, execute the code 
// for normal binary and unary and call member and cast, load the varids and perform operation and leave the result at rax, 
//     if no such binary or unary for the operand, a compile error is pretended by an runtime error is throw
// for var decl and scope barrier, operation on vars same as codegen
// for assign, simple store
// for goto and gotoif, simple rip control
// for callglobal, for simpler, prepare new execution context and directly copy the local vars to the next execution context vars
// for return, check this execution context return type, throw compile(pretended by run) time error if needed
//     copy the value to previous execution context rax and pop exection context
// when execution context stack in vm are empty, at any code, the process of the program is ended