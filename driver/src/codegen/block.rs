
// Block

use common::From2;
use common::StringPosition;

use syntax::Block as SyntaxBlock;
// use syntax::Statement;

use codegen::ItemID;
use codegen::var_def::Var;
use codegen::var_def::VarCollection;
use codegen::statement::StatementGenerator;
use codegen::session::GenerationSession;
use codegen::Code;

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
        let fn_decl = sess.fns.get_by_idx(self.fn_id);
        for arg in &fn_decl.args {
            let _varid = ret_val.try_push(
                Var::new(arg.name.clone(), arg.typeid, false, arg.pos), 
                &sess.types, &mut sess.msgs
            );
        }
        return ret_val;
    }

    // All next generation steps dispatcher
    pub fn generate(self, sess: &mut GenerationSession) {

        let code_ptr = Some(sess.codes.next_id());
        sess.vars = self.fn_id_to_vars(sess);
        sess.loops.ret_type = sess.fns.get_by_idx(self.fn_id).ret_type;
        StatementGenerator::generate(self.block, sess);
        sess.fns.get_by_idx_mut(self.fn_id).code_ptr = code_ptr;
        sess.fns.get_by_idx_mut(self.fn_id).local_size = sess.vars.get_max_offset();
    }
}

#[cfg(test)] #[test]
fn gen_block_prepare_vars() {
    
    use syntax::FunctionDef as SyntaxFunctionDef;
    use codegen::var_def::VarOrScope;
    use codegen::var_def::Var;

    let gen_vars = |param_str: &str| -> VarCollection {
        //              12345678
        let program1 = "fn main(".to_owned();
        let program2 = ") { writeln(\"helloworld\"); }";
        let syn_fn = SyntaxFunctionDef::from_str(&(program1 + param_str + program2), 0);
        let mut sess = GenerationSession::new();
        let (id, block) = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);
        let block = Block::new(id, block);
        block.fn_id_to_vars(&mut sess)
    };
    //                   901234567890
    let vars = gen_vars("i32 a, u32 b");
    assert_eq!(vars.len(), 2);
    assert_eq!(vars.index(0), &VarOrScope::Some(Var::new_test("a", ItemID::new(5), false, make_str_pos!(1, 9, 1, 13), 1)));  // 4)));
    assert_eq!(vars.index(1), &VarOrScope::Some(Var::new_test("b", ItemID::new(6), false, make_str_pos!(1, 16, 1, 20), 2))); // 8)));
    assert_eq!(vars.get_type(vars.find_by_name("a")), ItemID::new(5));
    assert_eq!(vars.get_type(vars.find_by_name("b")), ItemID::new(6));

    //                   901234567890
    let vars = gen_vars("i32 a, u32 b, string a, ");
    assert_eq!(vars.len(), 2);
    assert_eq!(vars.index(0), &VarOrScope::Some(Var::new_test("a", ItemID::new(5), false, make_str_pos!(1, 9, 1, 13), 1)));  // 4)));
    assert_eq!(vars.index(1), &VarOrScope::Some(Var::new_test("b", ItemID::new(6), false, make_str_pos!(1, 16, 1, 20), 2))); // 8)));
    assert_eq!(vars.get_type(vars.find_by_name("a")), ItemID::new(5));
    assert_eq!(vars.get_type(vars.find_by_name("b")), ItemID::new(6));
}
