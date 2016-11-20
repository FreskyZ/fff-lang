
// Block

use syntax::Block as SyntaxBlock;
// use syntax::Statement;

use codegen::TypeID;
use codegen::Var;
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

        match sess.fns.find_by_idx(self.fn_id) {
            None => return ret_val,
            Some(ref fn_decl) => {

                for arg in &fn_decl.args {
                    let id = ret_val.try_push(Var::new(arg.name.clone(), arg.ty, false), &mut sess.msgs);
                    perrorln!("push returned id is {:?}", id);
                }

                return ret_val;
            }
        };
    }

    // All next generation steps dispatcher
    pub fn generate(&mut self, _sess: &mut GenerationSession) {

    }
}

#[cfg(test)]
#[test]
fn gen_block_fn_id_to_vars() {
    // use codegen::FnImpl;
    use syntax::FunctionDef as SyntaxFunctionDef;
    use codegen::VarOrScope;
    use codegen::Var;

    let gen_vars = |param_str: &str| -> VarCollection {
        let program1 = "fn main(".to_owned();
        let program2 = ") { writeln(\"helloworld\"); }";
        let syn_fn = SyntaxFunctionDef::from_str(&(program1 + param_str + program2), 0);
        let mut sess = GenerationSession::new();
        let (id, block) = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs);
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