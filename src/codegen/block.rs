
// Block

use syntax::Block as SyntaxBlock;
use syntax::Statement;

use codegen::TypeID;
use codegen::VarCollection;
use codegen::session::GenerationSession;

pub struct Block {
    pub fn_id: usize,
    pub block: SyntaxBlock,
}

impl Block {
    
    pub fn new(id: usize, block: SyntaxBlock) -> Block {
        Block{ fn_id: id, block: block }
    }

    fn fn_id_to_vars(&self, sess: &mut GenerationSession) -> VarCollection {

        let mut ret_val = VarCollection::new();

        let fn_decl = match sess.fn_idx_to_decl(self.fn_id) {
            Some(ref fn_decl) => fn_decl,
            None => return ret_val,
        };

        for arg in &fn_decl.args {
            ret_val.try_push(Var{ name: arg.name.clone(), id: arg.id, is_const: false });
        }

        return ret_val;
    }

    // All next generation steps dispatcher
    pub fn generate(&mut self, _sess: &mut GenerationSession) {

    }
}

#[cfg(test)]
#[test]
fn gen_block_fn_id_to_vars() {
    use codegen::FnDecl;
    use syntax::FunctionDef as SyntaxFunctionDef;
    use codegen::VarOrScope;
    use codegen::Var;

    let gen_vars = |param_str: &str| -> VarCollection {
        let program1 = "fn main(".to_owned();
        let program2 = ") { writeln(\"helloworld\"); }";
        let syn_fn = SyntaxFunctionDef::from_str_with_id(&(program1 + param_str + program2), 0, 42);
        let mut sess = GenerationSession::new();
        let (_fndecl, id, block) = FnDecl::new(syn_fn, &mut sess);
        let block = Block::new(id, block);
        block.fn_id_to_vars(&mut sess)
    };

    let vars = gen_vars("i32 a, u32 b");
    assert_eq!(vars.len(), 2);
    assert_eq!(vars.index(0), &VarOrScope::Some(Var::new("a".to_owned(), TypeID::Some(5), false )));
    assert_eq!(vars.index(1), &VarOrScope::Some(Var::new("b".to_owned(), TypeID::Some(6), false )));
}


#[cfg(test)]
#[test]
fn gen_block_all() {

}