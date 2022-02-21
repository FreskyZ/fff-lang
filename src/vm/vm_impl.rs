
// Virtual machine implementation
// Stack = [StackFrame]*
// StackFrame = RIP RBP [Local]*
//                      RBP points to Local[0]

use crate::message::RuntimeMessage;
use crate::message::MessageEmitter;

use crate::lexical::LitValue;

use crate::codegen::Operand;
use crate::codegen::Code;
use crate::codegen::Program;
use crate::codegen::FnCollection;
use crate::codegen::Type;
use crate::codegen::ItemID;
use crate::codegen::TypeCollection;

use super::runtime::RuntimeValue;
use super::runtime::Runtime;
use super::builtin_impl::dispatch_builtin;

enum CircleResult{
    Continue,
    Err(RuntimeMessage),
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
        //perrorln!("Current Code: ({}, {:?})", rt.rip, self.codes[rt.rip]);
        //perrorln!("Current State: {}", rt.dump());

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
                    //perrorln!("[DEBUG][4][vm/vm_impl.rs|VirtualMachine::circle][branch Code::Call(fnid: {:?}, operands: {:?})]", fnid, operands);
                    rt.reserve_stack(thefn.local_size);
                    for (index, operand) in operands.iter().enumerate() {
                        //perrorln!("[DEBUG][4][vm/vm_impl.rs|VirtualMachine::circle][branch Code::Call] Iterating operand, current operand: ({}, {:?}), stack index to be set: {}, value to be set: {:?}", index, operand, rt.rbp + index + 1, rt.index(operand));
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

    pub fn execute(&mut self) -> Option<RuntimeMessage> {

        let main_fn_index = match self.find_main() {
            Some(index) => index,
            None => return Some(RuntimeMessage::CannotFindMain),
        };

        let mut rt = Runtime::new();
        rt.rip = self.fns[main_fn_index].code_ptr.unwrap(); // should be unwrap able
        rt.stack.push(RuntimeValue::PrevRIP(!1));    // prev rip
        rt.stack.push(RuntimeValue::PrevRBP(0));     // prev rbp is not important
        rt.rbp = 2;
        //perrorln!("[DEBUG][3][vm/vm_impl.rs|VirtualMachine::execute] main fn local_size = {}", self.fns[main_fn_index].local_size);
        rt.reserve_stack(self.fns[main_fn_index].local_size);
        //perrorln!("[DEBUG][3][vm/vm_impl.rs|VirtualMachine::execute] after reserve stack, rt.rbp = {}, rt.stack.len() = {}", rt.rbp, rt.stack.len());

        loop {
            match self.circle(&mut rt) {
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