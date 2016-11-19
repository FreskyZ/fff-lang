
// GenerationSession, collection of collections, beatifuler interfaces

use message::Message;
use message::MessageEmitter;

use syntax::SMType as SyntaxType;
use syntax::Program as SyntaxProgram;

use codegen::TypeID;
use codegen::TypeDeclCollection;
use codegen::FnID;
use codegen::FnDecl;
use codegen::FnDeclCollection;
use codegen::VMCodeCollection;
use codegen::Block;

pub struct GenerationSession {
    msgs: MessageEmitter,
    types: TypeDeclCollection,
    fndecls: FnDeclCollection,
    codes: VMCodeCollection,
}
pub struct Program {
    pub msgs: MessageEmitter,
    pub codes: VMCodeCollection,
    pub types: TypeDeclCollection,
    pub funcs: FnDeclCollection,
}

impl GenerationSession {

    pub fn new() -> GenerationSession {
        GenerationSession{
            msgs: MessageEmitter::new(),
            types: TypeDeclCollection::new(),
            fndecls: FnDeclCollection::new(),
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
    pub fn fn_idx_to_decl(&self, id: usize) -> Option<&FnDecl> {
        self.fndecls.index(id)
    }
    pub fn fn_id_to_decl(&self, id: FnID) -> Option<&FnDecl> {
        match id {
            FnID::Some(id) => self.fndecls.index(id),
            FnID::Invalid => None,
        }
    }
    
    // Dispatch program items to session, return Program for vm use
    pub fn dispatch(program: SyntaxProgram) -> Program {
        let mut sess = GenerationSession::new();

        // Future: aliases(using)
        // Future: typedefs

        // such that function can declare in any order
        let mut blocks = Vec::new();
        for func in program.functions {
            let (fn_decl, its_id, its_block) = FnDecl::new(func, &mut sess);
            sess.fndecls.push(fn_decl);
            blocks.push(Block::new(its_id, its_block));
        }
        
        for mut block in blocks {
            block.generate(&mut sess);
        }

        // Seperate function id, function sign with SyntaxBlock and generate next
        // for i in 0..sess.funcs.len() {
        //     sess.funcs.index_mut(i).generate_code(&mut sess);
        // }

        Program{ codes: sess.codes, types: sess.types, msgs: sess.msgs, funcs: sess.fndecls }
    }
}

impl Program {
}