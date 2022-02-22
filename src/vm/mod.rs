///! fff-lang
///!
///! vm, virtual machine
///! mainly primitive type implementations
///! vmcode design is in mod codegen

// Virtual machine implementation
// Stack = [StackFrame]*
// StackFrame = RIP RBP [Local]*
//                      RBP points to Local[0]

mod runtime;
mod builtin_impl;

use message::Message;
use message::MessageCollection;

use lexical::LitValue;

use codegen::Operand;
use codegen::Code;
use codegen::Program;
use codegen::FnCollection;
use codegen::Type;
use codegen::ItemID;
use codegen::TypeCollection;

use self::runtime::RuntimeValue;
use self::runtime::Runtime;
use self::builtin_impl::dispatch_builtin;

enum CircleResult{
    Continue,
    Err(Message),
    Exit,
}

pub struct VirtualMachine {
    fns: FnCollection, 
    types: TypeCollection, 
    codes: Vec<Code>,
}
impl VirtualMachine {
    
    pub fn new(program: Program) -> VirtualMachine {
        VirtualMachine{
            fns: program.fns,
            types: program.types,
            codes: program.codes,
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

    fn circle(&mut self, rt: &mut Runtime) -> CircleResult {

        if rt.rip == !1 {
            return CircleResult::Exit;  // normal exit
        }
        perrorln!("Current Code: ({}, {:?})", rt.rip, self.codes[rt.rip]);
        perrorln!("Current State: {}", rt.dump());

        match &self.codes[rt.rip] {
            &Code::PlaceHolder => unreachable!(),

            &Code::Goto(ref new_rip) => {
                rt.rip = *new_rip;
                CircleResult::Continue
            }
            &Code::GotoIf(ref operand, ref target, ref new_rip) => {
                rt.rip = if rt.index(operand) == *target { *new_rip } else { rt.rip + 1 };
                CircleResult::Continue
            }

            &Code::Call(ref fnid, ref operands) => {
                let thefn = self.fns.get_by_idx(fnid.as_option().unwrap());
                if thefn.is_internal() {
                    let fn_sign = thefn.get_sign();
                    rt.rax = dispatch_builtin(fn_sign, operands, &self.types, rt);
                    rt.rip += 1;
                } else {
                    rt.stack.push(RuntimeValue::PrevRIP(rt.rip + 1));
                    rt.stack.push(RuntimeValue::PrevRBP(rt.rbp));
                    let new_rbp = rt.stack.len(); // Use new rbp because if set rbp here and use previous function Operand::Stack, they are invalid
                    perrorln!("[DEBUG][4][vm/vm_impl.rs|VirtualMachine::circle][branch Code::Call(fnid: {:?}, operands: {:?})]", fnid, operands);
                    rt.reserve_stack(thefn.local_size);
                    for (index, operand) in operands.iter().enumerate() {
                        perrorln!("[DEBUG][4][vm/vm_impl.rs|VirtualMachine::circle][branch Code::Call] Iterating operand, current operand: ({}, {:?}), stack index to be set: {}, value to be set: {:?}", index, operand, rt.rbp + index + 1, rt.index(operand));
                        rt.stack[new_rbp + index + 1] = rt.index(operand); // TODO FUTURE: this `+1` is also because of that feature in VarCollection offset
                    }
                    rt.rbp = new_rbp;
                    rt.rip = thefn.code_ptr.unwrap();
                }
                CircleResult::Continue
            }
            &Code::Return(ref operand) => {
                // But not complex at all
                rt.rax = rt.index(operand).clone();

                let pop_num = rt.stack.len() - rt.rbp + 2;
                rt.rip = rt.stack[rt.rbp - 2].unwrap_stored_register();
                rt.rbp = rt.stack[rt.rbp - 1].unwrap_stored_register(); 
                for _ in 0..pop_num {
                    let _ = rt.stack.pop();
                }

                CircleResult::Continue
            }

            &Code::FieldAccess(ref operand, ref field_id) => {
                rt.rax = rt.index(operand).get_field(*field_id);
                rt.rip += 1;
                CircleResult::Continue
            }

            &Code::Store(ref local_id, ref operand) => {

                *rt.index_mut(&Operand::Stack(*local_id)) = rt.index(operand).clone();
                rt.rip += 1;
                CircleResult::Continue
            }
        }
    }

    pub fn execute(&mut self, messages: &mut MessageCollection) {

        let main_fn_index = match self.find_main() {
            Some(index) => index,
            None => {
                messages.push(Message::new_simple("cannot find main entrance"));
                return;
            }
        };

        let mut rt = Runtime::new();
        rt.rip = self.fns[main_fn_index].code_ptr.unwrap(); // should be unwrap able
        rt.stack.push(RuntimeValue::PrevRIP(!1));    // prev rip
        rt.stack.push(RuntimeValue::PrevRBP(0));     // prev rbp is not important
        rt.rbp = 2;
        perrorln!("[DEBUG][3][vm/vm_impl.rs|VirtualMachine::execute] main fn local_size = {}", self.fns[main_fn_index].local_size);
        rt.reserve_stack(self.fns[main_fn_index].local_size);
        perrorln!("[DEBUG][3][vm/vm_impl.rs|VirtualMachine::execute] after reserve stack, rt.rbp = {}, rt.stack.len() = {}", rt.rbp, rt.stack.len());

        loop {
            match self.circle(&mut rt) {
                CircleResult::Continue => continue,
                CircleResult::Exit => return,
                CircleResult::Err(msg) => {
                    messages.push(msg);
                    return;
                }
            }
        }
    }
}

#[cfg(test)] #[test]
fn vm_nomain() {

    let fns = FnCollection::new();
    let types = TypeCollection::new();
    let messages = &mut MessageCollection::new();
    let mut vm = VirtualMachine::new(Program{ fns: fns, types: types, msgs: MessageCollection::new(), codes: Vec::new() });

    vm.execute(messages);
    match messages.is_empty() {
        false => (),
        true => panic!("Unexpectedly succeed"),
    }
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