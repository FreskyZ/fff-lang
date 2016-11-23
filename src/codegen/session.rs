
// GenerationSession, collection of collections, beatifuler interfaces

use message::Message;
use message::MessageEmitter;

use syntax::Program as SyntaxProgram;

use codegen::TypeDeclCollection;
use codegen::FnCollection;
use codegen::CodeCollection;
use codegen::VarCollection;
use codegen::Block;

pub struct GenerationSession {
    pub msgs: MessageEmitter,
    pub types: TypeDeclCollection,
    pub fns: FnCollection,
    pub codes: CodeCollection,
    pub vars: VarCollection,
}

impl GenerationSession {

    pub fn new() -> GenerationSession {
        GenerationSession{
            msgs: MessageEmitter::new(),
            types: TypeDeclCollection::new(),
            fns: FnCollection::new(),
            codes: CodeCollection::new(),
            vars: VarCollection::new(),
        }
    } 
    
    // Messages interface
    pub fn push_message<T: Into<Message>>(&mut self, msg: T) {
        self.msgs.push(msg);
    }
    pub fn messages(&self) -> &MessageEmitter {
        &self.msgs
    }
    
    // Dispatch program items to session, return Program for vm use
    pub fn dispatch(program: SyntaxProgram) -> CodeCollection {

        let mut sess = GenerationSession::new();

        // such that function can declare in any order
        let mut blocks = Vec::new();
        for func in program.functions {
            let (its_id, its_block) = sess.fns.push_decl(func, &mut sess.types, &mut sess.msgs);
            blocks.push(Block::new(its_id, its_block));
        }
        sess.fns.check_sign_eq(&mut sess.types, &mut sess.msgs);
        
        for mut block in blocks {
            block.generate(&mut sess);
        }

        sess.codes
    }
}
