
// Generater
// Input program, dispatch statement to control flow statement generater or expression statement generater
// Recording generate process and provide utilities
// Get result and assemble to codegened program

use common::StringPosition;
use message::MessageEmitter;
use message::CodegenMessage;

use syntax::Program as SyntaxProgram;
use syntax::FunctionDef;
// use syntax::Statement;

use codegen::VMCode;
// use codegen::expr_stmt::expr_stmt_expand;

pub struct CodeGenerater {
    msgs: MessageEmitter,
    funcs: Vec<FunctionDef>,
    codes: Vec<VMCode>,

    cur_func_index: usize, // current generating
}

// Constructer
impl CodeGenerater {

    pub fn new(program: SyntaxProgram) -> CodeGenerater {
        CodeGenerater{
            msgs: MessageEmitter::new(),
            funcs: program.functions,
            codes: Vec::new(),
            cur_func_index: 0,
        }
    }
}

// Function level check and dispatch
impl CodeGenerater {

    pub fn current_syn_func(&self) -> &FunctionDef {
        &self.funcs[self.cur_func_index]
    }

    fn validate_arg(&mut self) {

    }

    fn generate_function(&mut self, _index: usize) {

        self.validate_arg();

        // for stmt in &mut self.current_syn_func().body.stmts {
        //     match stmt {
        //         &mut Statement::Expression(ref _expr_stmt) => {
        //             // let mut codes = expr_stmt_expand(self, expr_stmt);
        //             // self.codes.append(&mut codes);
        //         }
        //         _other => {
        //             // currently nothing
        //         }
        //     }
        // }
    } 
}

// Program level check and dispatch
impl CodeGenerater {

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
        
        for index in 0..self.funcs.len() {
            self.generate_function(index);
        }
    }
}

#[cfg(test)]
mod tests {

    fn gen_program() {

        // Function name same error 

    }
}