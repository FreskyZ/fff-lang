
// Function generater, provide function current generate state

use common::StringPosition;
use message::CodegenMessage;

use syntax::FunctionDef as SyntaxFunctionDef; 
use syntax::Argument as SyntaxArgument;
use syntax::Block as SyntaxBlock;
// use syntax::Statement;

use codegen::VMCode;
// use codegen::expr_stmt::expr_stmt_expand;
use codegen::Type;

pub struct FuncArg {
    pub name: String,
    pub ty: Type,
}

impl FuncArg {
    
    fn new_proto(sync_arg: SyntaxArgument) -> Option<FuncArg> {
        None
    }
}

pub struct FuncSign {       
    pub name: String,
    pub has_this: bool,      // same as args.iter().first().name == "this"
    pub args: Vec<FuncArg>,
    pub ret_type: Type,
}

pub struct Function {
    pub sign: FuncSign,
    pub imp: Option<VMCode>, // None for build in
}

pub struct FunctionCollection {
    funcs: Vec<Function>,
}

impl FunctionCollection {
    
    pub fn new() -> FunctionCollection {
        let mut ret_val = FunctionCollection{ funcs: Vec::new() };
        ret_val.add_prim_funcs();
        ret_val
    }

    fn add_prim_funcs(&mut self) {

    }
}

impl FunctionCollection {

    // Check function names and emit message, then ignore them and keep parse
    fn validate_func_names(&self, syn_funcs: &Vec<SyntaxFunctionDef>) {

        let mut name_and_poss = Vec::<(String, Vec<StringPosition>)>::new(); // function name and their 'fn' positions, vec<pos> are for 3 or more same name
        for func in syn_funcs {
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
                // self.msgs.push(CodegenMessage::FunctionHasSameName{ name: name, poss: their_pos });
            }
        }
    }

    pub fn generate(&mut self, syn_funcs: Vec<SyntaxFunctionDef>) {

    }
}

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