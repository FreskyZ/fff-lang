
// GenerationSession, collection of collections, beatifuler interfaces

use message::Message;
use message::MessageEmitter;

use syntax::SMType as SyntaxType;
use syntax::Program as SyntaxProgram;

use codegen::TypeID;
use codegen::TypeDeclCollection;
use codegen::FnID;
use codegen::FnImpl;
use codegen::FnCollection;
use codegen::VMCodeCollection;
use codegen::Block;

pub struct GenerationSession {
    pub msgs: MessageEmitter,
    pub types: TypeDeclCollection,
    pub fns: FnCollection,
    pub codes: VMCodeCollection,
}
pub struct Program {
    pub msgs: MessageEmitter,
    pub codes: VMCodeCollection,
    pub types: TypeDeclCollection,
    pub funcs: FnCollection,
}

impl GenerationSession {

    pub fn new() -> GenerationSession {
        GenerationSession{
            msgs: MessageEmitter::new(),
            types: TypeDeclCollection::new(),
            fns: FnCollection::new(),
            codes: VMCodeCollection::new(),
        }
    } 
    
    // Messages interface
    pub fn push_message<T: Into<Message>>(&mut self, msg: T) {
        self.msgs.push(msg);
    }
    pub fn messages(&self) -> &MessageEmitter {
        &self.msgs
    }

    // TypeCollection interface 
    pub fn type_usage_to_id(&mut self, ty: SyntaxType) -> TypeID {
        self.types.try_get_id(ty, &mut self.msgs)
    }

    // FunctionCollection interface
    pub fn fn_idx_to_decl(&self, id: usize) -> Option<&FnImpl> {
        self.fns.find_by_idx(id)
    }
    pub fn fn_id_to_decl(&self, id: FnID) -> Option<&FnImpl> {
        self.fns.find_by_id(id)
    }
    
    // Dispatch program items to session, return Program for vm use
    pub fn dispatch(program: SyntaxProgram) -> Program {
        let mut sess = GenerationSession::new();

        // Future: aliases(using)
        // Future: typedefs

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

        // Seperate function id, function sign with SyntaxBlock and generate next
        // for i in 0..sess.funcs.len() {
        //     sess.funcs.index_mut(i).generate_code(&mut sess);
        // }

        Program{ codes: sess.codes, types: sess.types, msgs: sess.msgs, funcs: sess.fns }
    }
}

impl Program {
}