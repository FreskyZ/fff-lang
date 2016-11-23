
// Block

use common::From2;
use common::StringPosition;

use syntax::Block as SyntaxBlock;
// use syntax::Statement;

use codegen::TypeID;
use codegen::Var;
use codegen::VarCollection;
use codegen::StatementGenerator;
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
        let fn_decl = sess.fns.find_by_idx(self.fn_id);
        for arg in &fn_decl.args {
            ret_val.try_push(
                Var::new(arg.name.clone(), arg.ty, false, StringPosition::from2(arg.pos[0].start_pos, arg.pos[1].end_pos)), 
                &sess.types, &mut sess.msgs
            );
        }
        return ret_val;
    }

    // All next generation steps dispatcher
    pub fn generate(self, sess: &mut GenerationSession) {

        let vars = self.fn_id_to_vars(sess);
        sess.vars = vars;
        StatementGenerator::generate(self.block, sess);

    }
}

#[cfg(test)]
#[test]
fn gen_block_fn_id_to_vars() {
    
    use syntax::FunctionDef as SyntaxFunctionDef;
    use codegen::VarOrScope;
    use codegen::Var;

    let gen_vars = |param_str: &str| -> VarCollection {
        //              12345678
        let program1 = "fn main(".to_owned();
        let program2 = ") { writeln(\"helloworld\"); }";
        let syn_fn = SyntaxFunctionDef::from_str(&(program1 + param_str + program2), 0);
        let mut sess = GenerationSession::new();
        let (id, block) = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs);
        let block = Block::new(id, block);
        block.fn_id_to_vars(&mut sess)
    };
    //                   901234567890
    let vars = gen_vars("i32 a, u32 b");
    assert_eq!(vars.len(), 2);
    assert_eq!(vars.index(0), &VarOrScope::Some(Var::new_test("a", TypeID::Some(5), false, make_str_pos!(1, 9, 1, 13), 4)));
    assert_eq!(vars.index(1), &VarOrScope::Some(Var::new_test("b", TypeID::Some(6), false, make_str_pos!(1, 16, 1, 20), 8)));

    //                   901234567890
    let vars = gen_vars("i32 a, u32 b, string a, ");
    assert_eq!(vars.len(), 2);
    assert_eq!(vars.index(0), &VarOrScope::Some(Var::new_test("a", TypeID::Some(5), false, make_str_pos!(1, 9, 1, 13), 4)));
    assert_eq!(vars.index(1), &VarOrScope::Some(Var::new_test("b", TypeID::Some(6), false, make_str_pos!(1, 16, 1, 20), 8)));
}
