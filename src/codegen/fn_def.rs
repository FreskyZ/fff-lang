
// FnDecl generater, provide FnDecl current generate state

use std::fmt;
use std::cmp;
use std::ops;
use std::slice;
use std::collections::HashMap;

use common::StringPosition;
use message::CodegenMessage;
use message::MessageEmitter;

use syntax::FunctionDef as SyntaxFunctionDef; 
use syntax::Argument as SyntaxArgument;
use syntax::Block as SyntaxBlock;

use codegen::type_def::TypeID;
use codegen::type_def::TypeCollection;
use codegen::var_def::VarCollection;
use codegen::vm_code::CodeCollection;
use codegen::session::GenerationSession;

#[derive(Eq, PartialEq, Clone, Copy)]
pub enum FnID {
    Some(usize),
    Invalid,
}
impl fmt::Debug for FnID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &FnID::Some(ref val) => write!(f, "fn[{}]", val),
            &FnID::Invalid => write!(f, "fn[-]"),
        }
    }
}
impl FnID {
    pub fn is_invalid(&self) -> bool {
        match self {
            &FnID::Some(_) => false,
            &FnID::Invalid => true,
        }
    }
    pub fn is_valid(&self) -> bool {
        match self {
            &FnID::Some(_) => true,
            &FnID::Invalid => false,
        }
    }
}

pub struct FnArg {
    pub name: String,
    pub ty: TypeID, // may be invalid, but the name should be remained for furthure check
    pub pos: [StringPosition; 2],
}
impl fmt::Debug for FnArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} @ {:?} {:?} @ {:?}", self.name, self.pos[0], self.ty, self.pos[1])
    }
}
impl FnArg {
    
    // Always construct, ty maybe none for invalid type
    fn new(syn_arg: SyntaxArgument, types: &mut TypeCollection, messages: &mut MessageEmitter) -> FnArg {

        let pos1 = syn_arg.ty.pos();
        let pos2 = syn_arg.pos_name;
        let ty = types.get_id_by_smtype(syn_arg.ty, messages);   // message emitted for none
        FnArg{ name: syn_arg.name, ty: ty, pos: [pos1, pos2] }  
    }

    fn new_internal(name: &str, ty: usize) -> FnArg {
        FnArg{ name: name.to_owned(), ty: TypeID::Some(ty), pos: [StringPosition::new(), StringPosition::new()] }
    }

    // ty is None
    pub fn is_valid(&self) -> bool {
        self.ty.is_valid()
    }
}

pub struct FnImpl { // Not Fn because Fn is used in std
    pub id: usize,
    pub name: String, 
    pub args: Vec<FnArg>,
    pub ret_type: TypeID,
    pub pos: [StringPosition; 3],  // pos_fn and pos_name and pos_ret_type
    pub valid: bool,               // buf for all args valid and ret_type valid and later not sign collission with other
    pub code_ptr: Option<usize>,   // start pointer in codes, none for internal
}
impl cmp::PartialEq<FnImpl> for FnImpl {
    fn eq(&self, rhs: &FnImpl) -> bool { self.id == rhs.id }
    fn ne(&self, rhs: &FnImpl) -> bool { self.id != rhs.id }
}
impl cmp::Eq for FnImpl{
}
impl FnImpl {

    // From Vec<Argument> to Vec<FnArg>
    // returned bool for all arg type valid and no redefinition
    fn new_args(syn_args: Vec<SyntaxArgument>, fn_pos: StringPosition, fn_name: &str, 
        types: &mut TypeCollection, messages: &mut MessageEmitter) -> (Vec<FnArg>, bool) {

        let mut args = Vec::<FnArg>::new();
        let mut valid = true;
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
                    valid = false;
                    continue 'new_arg; // ignore same name arg
                }
            }
            
            // No more errors
            let arg = FnArg::new(syn_arg, types, messages);
            valid = valid && arg.is_valid();
            args.push(arg);
        }

        (args, valid)
    }
    // As it consumes the SyntaxFnDef but not process the block, return it
    fn new(syn_fn: SyntaxFunctionDef, id: usize, types: &mut TypeCollection, messages: &mut MessageEmitter) -> (FnImpl, SyntaxBlock) {
        
        let (args, mut valid) = FnImpl::new_args(syn_fn.args, syn_fn.pos2[0], &syn_fn.name, types, messages);
        let pos_ret_type = syn_fn.ret_type.pos();
        let ret_type = types.get_id_by_smtype(syn_fn.ret_type, messages);
        valid = valid && ret_type.is_valid();
        (FnImpl{ 
            id: id, 
            name: syn_fn.name, 
            pos: [syn_fn.pos2[0], syn_fn.pos2[1], pos_ret_type], 
            args: args, 
            ret_type: ret_type, 
            valid: valid,
            code_ptr: None,
        }, syn_fn.body)
    }

    pub fn is_valid(&self) -> bool {
        self.valid
    }
    fn sign_eq(&self, name: &str, args: &Vec<TypeID>) -> bool {
        self.is_valid()
        && self.name == name
        // && self.ret_type == ret_type // Can not overload by ret type
        && self.args.len() == args.len()
        && {
            let mut ret_val = true;
            for (ref arg1, ref arg2) in self.args.iter().zip(args.iter()) {
                if arg1.ty != **arg2 {
                    ret_val = false;
                }
            }  
            ret_val
        }
    }

    fn format_display_sign(&self, types: &TypeCollection) -> String {

        let mut buf = self.name.clone();

        buf += "(";
        let args_len = self.args.len();
        for (index, arg) in self.args.iter().enumerate() {
            buf += &types.format_display_by_id(arg.ty);
            if index != args_len - 1 {
                buf += ", ";
            }
        }
        buf += ")";
        buf
    }
}

pub struct FnCollection {
    fns: Vec<FnImpl>,
}
impl ops::Index<usize> for FnCollection {
    type Output = FnImpl;
    fn index(&self, idx: usize) -> &FnImpl {
        &self.fns[idx]
    }
}
impl FnCollection {
    
    pub fn new() -> FnCollection {
        let mut ret_val = FnCollection{ fns: Vec::new() };
        ret_val.add_internal_fns();
        ret_val
    }

    fn add_internal(&mut self, name: &str, ret_type: usize, args: Vec<FnArg>) {
        let id = self.fns.len();
        self.fns.push(FnImpl{
            id: id,
            name: name.to_owned(),
            args: args, 
            ret_type: TypeID::Some(ret_type),
            pos: [StringPosition::new(), StringPosition::new(), StringPosition::new()],
            valid: true,
            code_ptr: None,
        })
    }

    fn add_internal_fns(&mut self) {
        self.add_internal("write", 0, vec![
            FnArg::new_internal("arg1", 13),
        ]);
        self.add_internal("writeln", 0, vec![
            FnArg::new_internal("arg1", 13)
        ]);
        self.add_internal("read_i32", 5, Vec::new());
    }

    // If same signature, still push the fndecl and return index, but push message and when require ID by signature, return invalid
    pub fn push_decl(&mut self, syn_fn: SyntaxFunctionDef, types: &mut TypeCollection, msgs: &mut MessageEmitter, _vars: &mut VarCollection) -> (usize, SyntaxBlock) {

        let (newfn, syn_block) = FnImpl::new(syn_fn, self.fns.len(), types, msgs);
        let ret_val = newfn.id;
        self.fns.push(newfn);
        return (ret_val, syn_block);
    }
    pub fn check_sign_eq(&mut self, types: &mut TypeCollection, msgs: &mut MessageEmitter) {

        let mut sign_map_pos = HashMap::<String, Vec<usize>>::new();
        for fcn in &mut self.fns {

            let sign_display = fcn.format_display_sign(types);
            let mut should_insert = false;
            match sign_map_pos.get_mut(&sign_display) {
                Some(poss) => { 
                    poss.push(fcn.id);
                },
                None => {
                    should_insert = true;
                }
            }
            if should_insert {
                sign_map_pos.insert(sign_display, vec![fcn.id]);
            }
        }

        for (sign, ids) in sign_map_pos {
            if ids.len() > 1 {
                let mut poss = Vec::new();
                for id in ids {
                    self.fns[id].valid = false;
                    poss.push(self.fns[id].pos[0]);
                }
                msgs.push(CodegenMessage::FunctionRedefinition{
                    sign: sign,
                    fnposs: poss,
                });
            }
        }
    }

    // Here if find recorded sign collision return invalid
    pub fn find_by_sign(&self, name: &str, args: &Vec<TypeID>) -> FnID {
        
        for (index, fcn) in self.fns.iter().enumerate() {
            if fcn.is_valid() && fcn.sign_eq(name, args) {
                return FnID::Some(index);
            }
        }
        return FnID::Invalid;
    }
    // Will return None if is invalid
    pub fn find_by_id(&self, idx: FnID) -> Option<&FnImpl> {
        match idx {
            FnID::Some(id) => match self.fns[id].valid {
                true => Some(&self.fns[id]),
                false => None,
            },
            FnID::Invalid => None,
        }
    }
    // It mainly used internal and ignore not valid
    pub fn find_by_idx(&self, idx: usize) -> &FnImpl {
        &self.fns[idx]
    }
    pub fn find_by_idx_mut(&mut self, idx: usize) -> &mut FnImpl {
        &mut self.fns[idx]
    }

    pub fn iter<'a>(&'a self) -> slice::Iter<'a, FnImpl> {
        self.fns.iter()
    }

    pub fn len(&self) -> usize { self.fns.len() }
    pub fn dump(&self, types: &TypeCollection) -> String {
        let mut buf = "Fns:\n".to_owned();
        for i in 0..self.fns.len() {
            buf += &format!("    {} {{ code_ptr: {:?} }}\n", self.fns[i].format_display_sign(types), self.fns[i].code_ptr);
        }
        buf
    }
}

#[cfg(test)]
use syntax::Argument;

#[cfg(test)]
#[test]
fn gen_fn_arg() {

    let mut sess = GenerationSession::new();

    let arg = Argument::from_str("[i32] a", 0);
    let arg = FnArg::new(arg, &mut sess.types, &mut sess.msgs);
    assert_eq!(arg.name, "a");
    assert_eq!(arg.ty, TypeID::Some(15)); // TODO future: after `auto` primitive type removed, this is 14
    assert_eq!(arg.pos, [make_str_pos!(1, 1, 1, 5), make_str_pos!(1, 7, 1, 7)]);
    let expect_messages = &mut MessageEmitter::new();
    assert_eq!(sess.messages(), expect_messages);

    let arg = Argument::from_str("int argc", 0);
    let arg = FnArg::new(arg, &mut sess.types, &mut sess.msgs);
    assert_eq!(arg.name, "argc");
    assert_eq!(arg.ty, TypeID::Invalid);
    assert_eq!(arg.pos, [make_str_pos!(1, 1, 1, 3), make_str_pos!(1, 5, 1, 8)]);
    let expect_messages = &mut MessageEmitter::new();
    expect_messages.push(CodegenMessage::TypeNotExist{ name: "int".to_owned(), pos: make_str_pos!(1, 1, 1, 3) });
    assert_eq!(sess.messages(), expect_messages);
}

#[cfg(test)]
#[test]
fn gen_fn_sign_eq() {
    
    let left_getter = || FnImpl{
        id: 0,
        name: "main".to_owned(),
        ret_type: TypeID::Some(5),
        args: vec![ 
            FnArg{ name: "argc".to_owned(), ty: TypeID::Some(1), pos: [StringPosition::new(), StringPosition::new()] },
            FnArg{ name: "argv".to_owned(), ty: TypeID::Some(2), pos: [StringPosition::new(), StringPosition::new()] },
        ],
        pos: [StringPosition::new(), StringPosition::new(), StringPosition::new()],
        valid: true,
        code_ptr: None,
    };

    // Normal equal
    let left = &mut left_getter();
    assert!(left.sign_eq("main", &vec![TypeID::Some(1), TypeID::Some(2)]));

    // Name not Equal
    let left = &mut left_getter();
    assert!(!left.sign_eq("another", &vec![TypeID::Some(1), TypeID::Some(2)]));

    // Something is invalid
    let left = &mut left_getter();
    left.valid = false;
    assert!(!left.sign_eq("main", &vec![TypeID::Some(1), TypeID::Some(2)]));

    // One of the arg is invalid
    let left = &mut left_getter();
    assert!(!left.sign_eq("main", &vec![TypeID::Invalid, TypeID::Some(2)]));

    // One of the arg name is not same
    let left = &mut left_getter();
    left.args[1].name = "some other".to_owned();
    assert!(left.sign_eq("main", &vec![TypeID::Some(1), TypeID::Some(2)]));

    // One of the arg type is not same
    let left = &mut left_getter();
    left.args[1].ty = TypeID::Some(8);
    assert!(!left.sign_eq("main", &vec![TypeID::Some(1), TypeID::Some(2)]));
}

#[cfg(test)]
#[test]
fn gen_fn_args() {

    // Normal
    let syn_args = vec![ // 12345678901234567890123
        Argument::from_str("fn main(i32 a", 3),
        Argument::from_str("fn main(i32 a, string b", 6),
    ];
    let fn_pos = make_str_pos!(1, 1, 1, 2);
    let fn_name = "main";
    let sess = &mut GenerationSession::new();
    let (args, valid) = FnImpl::new_args(syn_args, fn_pos, fn_name, &mut sess.types, &mut sess.msgs);
    assert_eq!(valid, true);
    assert_eq!(args[0].name, "a");
    assert_eq!(args[0].ty, TypeID::Some(5));
    assert_eq!(args[0].pos, [make_str_pos!(1, 9, 1, 11), make_str_pos!(1, 13, 1, 13)]);
    assert_eq!(args[1].name, "b");
    assert_eq!(args[1].ty, TypeID::Some(13));
    assert_eq!(args[1].pos, [make_str_pos!(1, 16, 1, 21), make_str_pos!(1, 23, 1, 23)]);
    let expect_message = MessageEmitter::new();
    assert_eq!(sess.messages(), &expect_message);

    // 1 arg type invalid
    let syn_args = vec![ 
        //                  12345678901234567890123456789012345678 
        Argument::from_str("fn main(i32 a, [intttt] b, [[u8]] cde)", 3),
        //                  0        1         2         3
        Argument::from_str("fn main(i32 a, [intttt] b, [[u8]] cde)", 6),
        //                  0  1   23   45 67     8 90 123 45 6  7
        Argument::from_str("fn main(i32 a, [intttt] b, [[u8]] cde)", 11),
    ];  
    let fn_pos = make_str_pos!(1, 1, 1, 2);
    let fn_name = "main";
    let sess = &mut GenerationSession::new();
    let (args, valid) = FnImpl::new_args(syn_args, fn_pos, fn_name, &mut sess.types, &mut sess.msgs);
    assert_eq!(valid, false);
    assert_eq!(args[0].name, "a");
    assert_eq!(args[0].ty, TypeID::Some(5));
    assert_eq!(args[0].pos, [make_str_pos!(1, 9, 1, 11), make_str_pos!(1, 13, 1, 13)]);
    assert_eq!(args[1].name, "b");
    assert_eq!(args[1].ty, TypeID::Invalid);
    assert_eq!(args[1].pos, [make_str_pos!(1, 16, 1, 23), make_str_pos!(1, 25, 1, 25)]);
    assert_eq!(args[2].name, "cde");
    assert_eq!(args[2].ty, TypeID::Some(16)); // [u8] is 14, [[u8]] is 15  // TODO future: after primitive type auto is removed, this is 15
    assert_eq!(args[2].pos, [make_str_pos!(1, 28, 1, 33), make_str_pos!(1, 35, 1, 37)]);
    let mut expect_message = MessageEmitter::new();
    expect_message.push(CodegenMessage::TypeNotExist{ name: "intttt".to_owned(), pos: make_str_pos!(1, 17, 1, 22) });
    assert_eq!(sess.messages(), &expect_message);

    // 2 name collision
    let syn_args = vec![ 
        //                  12345678901234567890123456789012345678901234567
        Argument::from_str("fn main(i32 a, u32 bc, i32 a, i32 bc, string d)", 3),
        //                  0        1         2         3         4
        Argument::from_str("fn main(i32 a, u32 bc, i32 a, i32 bc, string d)", 6),
        //                  0  1   23   45 6   7 8 9   01 2   3 4 5      67
        Argument::from_str("fn main(i32 a, u32 bc, i32 a, i32 bc, string d)", 9),
        Argument::from_str("fn main(i32 a, u32 bc, i32 a, i32 bc, string d)", 12),
        Argument::from_str("fn main(i32 a, u32 bc, i32 a, i32 bc, string d)", 15),
    ];
    let fn_pos = make_str_pos!(1, 1, 1, 2);
    let fn_name = "main";
    let sess = &mut GenerationSession::new();
    let (args, valid) = FnImpl::new_args(syn_args, fn_pos, fn_name, &mut sess.types, &mut sess.msgs);
    assert_eq!(valid, false);
    assert_eq!(args[0].name, "a");
    assert_eq!(args[0].ty, TypeID::Some(5));
    assert_eq!(args[0].pos, [make_str_pos!(1, 9, 1, 11), make_str_pos!(1, 13, 1, 13)]);
    assert_eq!(args[1].name, "bc");
    assert_eq!(args[1].ty, TypeID::Some(6));
    assert_eq!(args[1].pos, [make_str_pos!(1, 16, 1, 18), make_str_pos!(1, 20, 1, 21)]);
    assert_eq!(args[2].name, "d");
    assert_eq!(args[2].ty, TypeID::Some(13));
    assert_eq!(args[2].pos, [make_str_pos!(1, 39, 1, 44), make_str_pos!(1, 46, 1, 46)]);
    let mut expect_message = MessageEmitter::new();
    expect_message.push(CodegenMessage::FunctionArgumentNameConfilict{ 
        func_pos: make_str_pos!(1, 1, 1, 2), func_name: "main".to_owned(),
        name: "a".to_owned(), pos1: make_str_pos!(1, 13, 1, 13), pos2: make_str_pos!(1, 28, 1, 28),
    });
    expect_message.push(CodegenMessage::FunctionArgumentNameConfilict{ 
        func_pos: make_str_pos!(1, 1, 1, 2), func_name: "main".to_owned(),
        name: "bc".to_owned(), pos1: make_str_pos!(1, 20, 1, 21), pos2: make_str_pos!(1, 35, 1, 36),
    });
    assert_eq!(sess.messages(), &expect_message);

    // Nothing    
    let syn_args = Vec::new();
    let fn_pos = make_str_pos!(1, 1, 1, 2);
    let fn_name = "main";
    let sess = &mut GenerationSession::new();
    let (args, valid) = FnImpl::new_args(syn_args, fn_pos, fn_name, &mut sess.types, &mut sess.msgs);
    assert_eq!(valid, true);
    assert_eq!(args.len(), 0);
    let expect_message = MessageEmitter::new();
    assert_eq!(sess.messages(), &expect_message);
}

#[cfg(test)]
#[test]
fn gen_fn_decl() {
    // Because no branch, so only one test

    //             0        1         2          3          4
    //             12345678901234567890123456 78901234567 89012
    let program = "fn main() -> () { writeln(\"helloworld\"); }";
    //             1  2   34 5  67 8 9      A B            CD E
    let sess = &mut GenerationSession::new();
    let syn_fn = SyntaxFunctionDef::from_str(program, 0);
    let (id, block) = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);
    {
        let fndecl = sess.fns.find_by_idx(id);
        assert_eq!(fndecl.id, 0);
        assert_eq!(fndecl.name, "main");
        assert_eq!(fndecl.args.len(), 0);
        assert_eq!(fndecl.pos, [make_str_pos!(1, 1, 1, 2), make_str_pos!(1, 4, 1, 7), make_str_pos!(1, 14, 1, 15)]);
        assert_eq!(fndecl.ret_type, TypeID::Some(0));
        assert_eq!(id, 0);
        assert_eq!(block.stmts.len(), 1);
    }

    let program = "fn some(i32 a, u32 b, [string] c) -> [u8] {}";
    let syn_fn = SyntaxFunctionDef::from_str(program, 0);
    let _ = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);
    let program = " fn some(i32 a, u32 b, [string] c) -> [u8] {}";
    let syn_fn = SyntaxFunctionDef::from_str(program, 0);
    let _ = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);
    let program = "  fn some(i32 a, u32 b, string c) -> u8 {}";
    let syn_fn = SyntaxFunctionDef::from_str(program, 0);
    let _ = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);
    let program = "   fn some(i32 a, u32 b, [string] c) -> [u8] {}";
    let syn_fn = SyntaxFunctionDef::from_str(program, 0);
    let _ = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);
    let program = "    fn some(i32 a, u32 b) -> [u8] {}";
    let syn_fn = SyntaxFunctionDef::from_str(program, 0);
    let _ = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);

    let program = "     fn some(i32 a, u32 b, string c) -> [u8] {}";
    let syn_fn = SyntaxFunctionDef::from_str(program, 0);
    let _ = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);

    sess.fns.check_sign_eq(&mut sess.types, &mut sess.msgs);
    let expect_messsage1 = &mut MessageEmitter::new();
    expect_messsage1.push(CodegenMessage::FunctionRedefinition{
        sign: "some(i32, u32, [string])".to_owned(),
        fnposs: vec![make_str_pos!(1, 1, 1, 2), make_str_pos!(1, 2, 1, 3), make_str_pos!(1, 4, 1, 5)]
    });
    expect_messsage1.push(CodegenMessage::FunctionRedefinition{
        sign: "some(i32, u32, string)".to_owned(),
        fnposs: vec![make_str_pos!(1, 3, 1, 4), make_str_pos!(1, 6, 1, 7)]
    });
    let expect_messsage2 = &mut MessageEmitter::new();
    expect_messsage2.push(CodegenMessage::FunctionRedefinition{
        sign: "some(i32, u32, string)".to_owned(),
        fnposs: vec![make_str_pos!(1, 3, 1, 4), make_str_pos!(1, 6, 1, 7)]
    });
    expect_messsage2.push(CodegenMessage::FunctionRedefinition{
        sign: "some(i32, u32, [string])".to_owned(),
        fnposs: vec![make_str_pos!(1, 1, 1, 2), make_str_pos!(1, 2, 1, 3), make_str_pos!(1, 4, 1, 5)]
    });
    if !(sess.messages() == expect_messsage1 || sess.messages() == expect_messsage2) {
        panic!("assertion failed, left: `{:?}`, right: `{:?}`", sess.messages(), expect_messsage1);
    }
}