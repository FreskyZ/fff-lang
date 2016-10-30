
// Generater
// Input program, dispatch statement to control flow statement generater or expression statement generater
// Recording generate process and provide utilities
// Get result and assemble to codegened program

use common::StringPosition;
use message::MessageEmitter;
use message::CodegenMessage;

use syntax::Program as SyntaxProgram;
use syntax::FunctionDef;

use codegen::VMCode;
use codegen::function_gener::FunctionGenerater;

pub struct CodeGenerater {
    msgs: MessageEmitter,
    funcs: Vec<FunctionDef>,
    codes: Vec<VMCode>,
}

impl CodeGenerater {

    pub fn new(program: SyntaxProgram) -> CodeGenerater {
        CodeGenerater{
            msgs: MessageEmitter::new(),
            funcs: program.functions,
            codes: Vec::new(),
        }
    }

    fn validate_func_names(&mut self) {

        let mut name_and_poss = Vec::<(String, Vec<StringPosition>)>::new(); // function name and their 'fn' positions, vec<pos> are for 3 or more same name
        for func in &self.funcs {
            let mut found_same = false;
            for &mut (ref exist_name, ref mut their_pos) in &mut name_and_poss {
                if exist_name == &func.name {
                    their_pos.push(func.pos_fn());
                    found_same = true;
                    break;
                }
            }
            if !found_same {
                name_and_poss.push((func.name.clone(), Vec::new()));
            }
        }

        for (name, their_pos) in name_and_poss {
            if their_pos.len() > 1 {
                self.msgs.push(CodegenMessage::FunctionHasSameName{ name: name, poss: their_pos });
            }
        }
    }

    pub fn generate(&mut self) {

        self.validate_func_names();
        
        for func in &self.funcs {
            let mut func_gener = FunctionGenerater::new(func);
            func_gener.generate(); 
        }
    }
}

#[cfg(test)]
mod tests {

    fn gen_program() {

        // Function name same error 

    }
}