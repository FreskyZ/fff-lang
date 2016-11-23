
// Statement generation

// use message::MessageEmitter;

use syntax::Block as SyntaxBlock;

// use codegen::TypeDeclCollection;
// use codegen::FnCollection;
// use codegen::VarCollection;
// use codegen::CodeCollection;
use codegen::session::GenerationSession;

pub trait IStatementDispatcher<T>{
    fn generate(this: &T, sess: &mut GenerationSession);
}

// Just a static dispatcher
pub struct StatementGenerator {
}

impl IStatementDispatcher<SyntaxBlock> for StatementGenerator {

    fn generate(block: &SyntaxBlock, sess: &mut GenerationSession) {

    }
} 


#[cfg(test)]
#[test]
fn gen_stmt() {
}