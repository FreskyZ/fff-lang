
// Function generater, provide function current generate state

use syntax::FunctionDef as SyntaxFunctionDef; 
use syntax::Argument as SyntaxArgument;
// use syntax::Statement;

// use codegen::VMCode;
// use codegen::expr_stmt::expr_stmt_expand;
use codegen::Type;

pub struct Argument {
    pub name: String,
    pub ty: Type,
}

impl Argument {
    
    fn new_for_proto(_sync_arg: SyntaxArgument) -> Option<Argument> {
        None
    }
}

pub struct FunctionDef {
    pub name: String,
    pub args: Vec<Argument>,
    pub ret_type: Type,
}

impl FunctionDef {

    pub fn new_for_proto(_syn_func: SyntaxFunctionDef) -> Option<FunctionDef> {
        None
    }
}

// impl<'a> FunctionGenerater<'a> {

//     pub fn new(func: &'a FunctionDef) -> FunctionGenerater {
//         FunctionGenerater{
//             syn_func: func,
//             codes: Vec::new(),
//         }
//     }

//     #[cfg(test)]
//     pub fn get_syn_func(&self) -> &'a FunctionDef {
//         &self.syn_func
//     }

//     // validate arg type exist and arg name not conflict
//     fn validate_arg(&self) {

//     }

//     pub fn generate(&mut self) {

//         // check name
//         self.validate_arg();

//         // for stmt in &self.syn_func.body.stmts {
//         //     match stmt {
//         //         &Statement::Expression(ref expr_stmt) => {
//         //             let mut codes = expr_stmt_expand(self, expr_stmt);
//         //             self.codes.append(&mut codes);
//         //         }
//         //         _other => {
//         //             // currently nothing
//         //         }
//         //     }
//         // }
//     }
// }