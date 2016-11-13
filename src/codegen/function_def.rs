
// Function generater, provide function current generate state

use common::StringPosition;
use message::CodegenMessage;
use message::MessageEmitter;

use syntax::FunctionDef as SyntaxFunctionDef; 
use syntax::Argument as SyntaxArgument;
use syntax::Block as SyntaxBlock;
// use syntax::Statement;

use codegen::VMCode;
// use codegen::expr_stmt::expr_stmt_expand;
use codegen::TypeID;
use codegen::TypeCollection;

pub type FunctionID = usize;

pub struct FuncArg {
    pub name: String,
    pub ty: Option<TypeID>, // may be invalid, but the name should be remained for furthure check
    pub pos: [StringPosition; 2],
}
impl FuncArg {
    
    // Always construct, ty maybe none for invalid type
    fn new(syn_arg: SyntaxArgument, types: &mut TypeCollection, messages: &mut MessageEmitter) -> FuncArg {

        let pos1 = syn_arg.ty.pos();
        let pos2 = syn_arg.pos_name;
        let ty = types.try_get_id(syn_arg.ty, messages);   // message emitted for none
        FuncArg{ name: syn_arg.name, ty: ty, pos: [pos1, pos2] }  
    }
}

pub struct FuncSign {       
    pub name: String,
    pub args: Vec<FuncArg>,
    pub ret_type: Option<TypeID>, // None for invalid
    pub pos: [StringPosition; 2], // pos_fn and pos_name
}

pub struct Function {
    pub id: FunctionID,
    pub sign: FuncSign,
    pub imp: Option<VMCode>, // None for build in
}
impl Function {

    // Check arg name existence, if exist, ignore the next ones
    fn new_args(
        syn_args: Vec<SyntaxArgument>, fn_pos: StringPosition, fn_name: &str, 
        types: &mut TypeCollection, messages: &mut MessageEmitter) -> Vec<FuncArg> {

        let mut args = Vec::<FuncArg>::new();
        'new_arg: for syn_arg in syn_args {
            'exist_args: for arg in &args {
                if arg.name == syn_arg.name {
                    messages.push(CodegenMessage::FunctionArgumentNameConfilict{ 
                        func_pos: fn_pos,
                        func_name: fn_name.to_owned(),
                        name: arg.name.clone(),
                        pos1: arg.pos[1].clone(),
                        pos2: syn_arg.pos_name,
                    });
                    continue 'new_arg; // ignore same name arg
                }
            }
            
            // No more errors
            args.push(FuncArg::new(syn_arg, types, messages));
        }

        args
    }

    fn new(syn_func: SyntaxFunctionDef, types: &mut TypeCollection, messages: &mut MessageEmitter) -> Function {

        let args = Function::new_args(syn_func.args, syn_func.pos2[0], &syn_func.name, types, messages);
        let id = syn_func.id;
        let name = syn_func.name;
        let ret_type = types.try_get_id(syn_func.ret_type, messages);
        let pos = syn_func.pos2;

        Function{ id: id, sign: FuncSign{ name: name, args: args, ret_type: ret_type, pos: pos }, imp: None }
    }
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

#[cfg(test)]
mod tests {

    #[test]
    fn gen_func_arg() {

    }

    #[test]
    fn gen_func_func() {

    }
}