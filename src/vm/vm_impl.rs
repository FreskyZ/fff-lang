
// Virtual machine implementation
// Stack = [StackFrame]*
// StackFrame = RIP RBP [Local]*
//                      RBP points to Local[0]

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
        perrorln!("Current code: {:?}", self.codes[rt.rip]);
        perrorln!("Current State: {}", rt.dump());

        match &self.codes[rt.rip] {
            &Code::PlaceHolder => unreachable!(),

            &Code::Goto(ref new_rip) => {
                rt.rip = *new_rip;
            }
            &Code::GotoIf(ref operand, ref target, ref new_rip) => {
                let rt_value = rt.operand_to_value(operand);
                if rt_value == *target {
                    rt.rip = *new_rip;
                }
            }

            &Code::Call(ref fnid, ref operands) => {
                let mut rt_values = Vec::new();
                for operand in operands {
                    rt_values.push(rt.operand_to_value(operand));
                }

                let thefn = self.fns.get_by_idx(fnid.as_option().unwrap());
                if thefn.is_internal() {
                    let fn_sign = thefn.get_sign();
                    rt.rax = dispatch_builtin(fn_sign, rt_values, &self.types, rt);
                } else {
                    rt.stack.push(RuntimeValue::StoredRegister(rt.rip + 1));
                    rt.stack.push(RuntimeValue::StoredRegister(rt.rbp));
                    rt.rbp = rt.stack.len();
                    rt.reserve_stack(thefn.local_size);
                    for (index, rt_value) in rt_values.into_iter().enumerate() {
                        rt.stack[rt.rbp + index + 1] = rt_value; // TODO FUTURE: this `+1` is also because of that feature in VarCollection offset
                    }
                    rt.rip = thefn.code_ptr.unwrap();
                }
            }
            &Code::CallMember(ref operand, ref fnid, ref operands) => {
                let mut rt_values = Vec::new();
                rt_values.push(rt.operand_to_value(operand));
                for operand in operands {
                    rt_values.push(rt.operand_to_value(operand));
                }

                let thefn = self.fns.get_by_idx(fnid.as_option().unwrap());
                if thefn.is_internal() {
                    let fn_sign = thefn.get_sign();
                    rt.rax = dispatch_builtin(fn_sign, rt_values, &self.types, rt);
                } else {
                    unreachable!() // yeah
                }
            }

            &Code::FieldAccess(ref operand, ref field_id) => {
                let rt_value = rt.operand_to_value(operand);
                rt.rax = rt_value.get_field(*field_id);
            }

            // TODO: change to store to local
            &Code::Store(ref _op1, ref _ops2) => {

                rt.rip += 1;
            }

            &Code::Return(ref operand) => {
                // But not complex at all
                rt.rax = rt.operand_to_value(operand);
                rt.rip = rt.stack[rt.rbp - 2].unwrap_stored_register();
                rt.rbp = rt.stack[rt.rbp - 1].unwrap_stored_register(); 
            }
        }

        CircleResult::Continue // TODO: after circle finished, this should be Err or unreachable
    }

    pub fn execute(&mut self) -> Option<RuntimeMessage> {

        let main_fn_index = match self.find_main() {
            Some(index) => index,
            None => return Some(RuntimeMessage::CannotFindMain),
        };

        let mut rt = Runtime::new();
        rt.rip = self.fns[main_fn_index].code_ptr.unwrap(); // should be unwrap able
        rt.stack.push(RuntimeValue::StoredRegister(!1));    // prev rip
        rt.stack.push(RuntimeValue::StoredRegister(0));     // prev rbp is not important
        rt.reserve_stack(self.fns[main_fn_index].local_size);

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