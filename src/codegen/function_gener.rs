
// Function generater, provide function current generate state

use syntax::FunctionDef;
use syntax::Argument;
use syntax::Statement;

use codegen::vm_code::VMCode;
use codegen::expr_stmt::expr_stmt_expand;

pub struct FunctionGenerater<'a> {
    syn_func: &'a FunctionDef,
    codes: Vec<VMCode>, 
}

impl<'a> FunctionGenerater<'a> {

    pub fn new(func: &'a FunctionDef) -> FunctionGenerater {
        FunctionGenerater{
            syn_func: func,
            codes: Vec::new(),
        }
    }

    #[cfg(test)]
    pub fn get_syn_func(&self) -> &'a FunctionDef {
        &self.syn_func
    }

    // validate arg type exist and arg name not conflict
    fn validate_arg(&self) {

    }

    pub fn generate(&mut self) {

        // check name
        self.validate_arg();

        for stmt in &self.syn_func.body.stmts {
            match stmt {
                &Statement::Expression(ref expr_stmt) => {
                    let mut codes = expr_stmt_expand(self, expr_stmt);
                    self.codes.append(&mut codes);
                }
                _other => {
                    // currently nothing
                }
            }
        }
    }
}