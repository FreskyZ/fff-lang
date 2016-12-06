
// GenerationSession, collection of collections, beatifuler interfaces

use message::Message;
use message::MessageEmitter;

use syntax::Program as SyntaxProgram;

use codegen::type_def::TypeCollection;
use codegen::fn_def::FnCollection;
use codegen::CodeCollection;
use codegen::var_def::VarCollection;
use codegen::block::Block;
use codegen::loop_def::LoopCollection;

pub struct GenerationSession {
    pub msgs: MessageEmitter,
    pub types: TypeCollection,
    pub fns: FnCollection,
    pub codes: CodeCollection,
    pub vars: VarCollection,
    pub loops: LoopCollection,
}

impl GenerationSession {

    pub fn new() -> GenerationSession {
        GenerationSession{
            msgs: MessageEmitter::new(),
            types: TypeCollection::new(),
            fns: FnCollection::new(),
            codes: CodeCollection::new(),
            vars: VarCollection::new(),
            loops: LoopCollection::new(),
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
            let (its_id, its_block) = sess.fns.push_decl(func, &mut sess.types, &mut sess.msgs, &mut sess.vars);
            blocks.push(Block::new(its_id, its_block));
        }
        sess.fns.check_sign_eq(&mut sess.types, &mut sess.msgs);
        
        for block in blocks {
            block.generate(&mut sess);
        }

        sess.codes
    }
}


#[cfg(test)] #[test] // #[ignore]
fn gen_program_inter() {
    use std::io::stdin;

    loop {
        let mut buf = String::new();

        perrorln!("Input:");
        match stdin().read_line(&mut buf) {
            Ok(_) => (),
            Err(_) => break,
        }

        if buf != "break\r\n" {
            match SyntaxProgram::from_str(&buf) {
                Some(program) => {
                    let codes = GenerationSession::dispatch(program); 
                    perrorln!("Code: {}", codes.dump());
                }
                None => {
                    perrorln!("Unexpectedly failed");
                }
            }
        } else {
            break;
        }
    }
}