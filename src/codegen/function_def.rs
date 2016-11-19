
// FnDecl generater, provide FnDecl current generate state

use std::fmt;
use std::cmp;
use std::collections::HashMap;

use common::StringPosition;
use message::CodegenMessage;
use message::MessageEmitter;

use syntax::FunctionDef as SyntaxFunctionDef; 
use syntax::Argument as SyntaxArgument;
use syntax::Block as SyntaxBlock;

// use codegen::VMCode;
// use codegen::expr_stmt::expr_stmt_expand;
use codegen::TypeID;
use codegen::session::GenerationSession;

#[derive(Eq, PartialEq, Clone, Copy)]
pub enum FnID {
    Some(usize),
    Invalid,
}
impl fmt::Debug for FnID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &FnID::Some(ref val) => write!(f, "type[{}]", val),
            &FnID::Invalid => write!(f, "type[-]"),
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
    fn new(syn_arg: SyntaxArgument, sess: &mut GenerationSession) -> FnArg {

        let pos1 = syn_arg.ty.pos();
        let pos2 = syn_arg.pos_name;
        let ty = sess.type_usage_to_id(syn_arg.ty);   // message emitted for none
        FnArg{ name: syn_arg.name, ty: ty, pos: [pos1, pos2] }  
    }

    // ty is None
    pub fn is_valid(&self) -> bool {
        self.ty.is_valid()
    }
}

pub struct FnDecl {
    pub id: usize,
    pub name: String, 
    pub args: Vec<FnArg>,
    pub ret_type: TypeID,
    pub pos: [StringPosition; 3],  // pos_fn and pos_name and pos_ret_type
    valid: bool,                   // buf for all args valid and ret_type valid
}
impl cmp::PartialEq<FnDecl> for FnDecl {
    fn eq(&self, rhs: &FnDecl) -> bool { self.id == rhs.id }
    fn ne(&self, rhs: &FnDecl) -> bool { self.id != rhs.id }
}
impl cmp::Eq for FnDecl{
}
impl FnDecl {

    // From Vec<Argument> to Vec<FnArg>
    // returned bool for all arg type valid and no redefinition
    fn new_args(syn_args: Vec<SyntaxArgument>, fn_pos: StringPosition, fn_name: &str, sess: &mut GenerationSession) -> (Vec<FnArg>, bool) {

        let mut args = Vec::<FnArg>::new();
        let mut valid = true;
        'new_arg: for syn_arg in syn_args {
            'exist_args: for arg in &args {
                if arg.name == syn_arg.name {
                    sess.push_message(CodegenMessage::FunctionArgumentNameConfilict{ 
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
            let arg = FnArg::new(syn_arg, sess);
            valid = valid && arg.is_valid();
            args.push(arg);
        }

        (args, valid)
    }
    // As it consumes the SyntaxFnDef but not process the block, return it
    pub fn new(syn_fn: SyntaxFunctionDef, sess: &mut GenerationSession) -> (FnDecl, usize, SyntaxBlock) {
        
        let (args, mut valid) = FnDecl::new_args(syn_fn.args, syn_fn.pos2[0], &syn_fn.name, sess);
        let pos_ret_type = syn_fn.ret_type.pos();
        let ret_type = sess.type_usage_to_id(syn_fn.ret_type);
        valid = valid && ret_type.is_valid();
        (FnDecl{ 
            id: syn_fn.id, name: syn_fn.name, pos: [syn_fn.pos2[0], syn_fn.pos2[1], pos_ret_type], 
            args: args, ret_type: ret_type, valid: valid
        }, syn_fn.id, syn_fn.body)
    }

    pub fn is_valid(&self) -> bool {
        self.valid
    }
    fn sign_eq(&self, rhs: &FnDecl) -> bool {
        self.is_valid() && rhs.is_valid()
        && self.name == rhs.name
        && self.ret_type == rhs.ret_type
        && self.args.len() == rhs.args.len()
        && {
            let mut ret_val = true;
            for (ref arg1, ref arg2) in self.args.iter().zip(rhs.args.iter()) {
                if arg1.ty != arg2.ty {
                    ret_val = false;
                }
            }  
            ret_val
        }
    }
}

pub struct FnDeclCollection {
    fns: HashMap<usize, FnDecl>,
}
impl FnDeclCollection {
    
    pub fn new() -> FnDeclCollection {
        FnDeclCollection{ fns: HashMap::new() }
    }

    pub fn push(&mut self, func: FnDecl) { self.fns.insert(func.id, func); }
    pub fn len(&self) -> usize { self.fns.len() }
    pub fn index(&self, idx: usize) -> Option<&FnDecl> { self.fns.get(&idx) }
}

#[cfg(test)]
use syntax::Argument;

#[cfg(test)]
#[test]
fn gen_fn_arg() {

    let sess = &mut GenerationSession::new();

    let arg = Argument::from_str("[i32] a", 0);
    let arg = FnArg::new(arg, sess);
    assert_eq!(arg.name, "a");
    assert_eq!(arg.ty, TypeID::Some(14));
    assert_eq!(arg.pos, [make_str_pos!(1, 1, 1, 5), make_str_pos!(1, 7, 1, 7)]);
    let expect_messages = &mut MessageEmitter::new();
    assert_eq!(sess.messages(), expect_messages);

    let arg = Argument::from_str("int argc", 0);
    let arg = FnArg::new(arg, sess);
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
    
    let left_getter = || FnDecl{
        id: 0,
        name: "main".to_owned(),
        ret_type: TypeID::Some(5),
        args: vec![ 
            FnArg{ name: "argc".to_owned(), ty: TypeID::Some(1), pos: [StringPosition::new(), StringPosition::new()] },
            FnArg{ name: "argv".to_owned(), ty: TypeID::Some(2), pos: [StringPosition::new(), StringPosition::new()] },
        ],
        pos: [StringPosition::new(), StringPosition::new(), StringPosition::new()],
        valid: true,
    };
    let right_getter = || FnDecl{
        id: 0,
        name: "main".to_owned(),
        ret_type: TypeID::Some(5),
        args: vec![ 
            FnArg{ name: "argc".to_owned(), ty: TypeID::Some(1), pos: [StringPosition::new(), StringPosition::new()] },
            FnArg{ name: "argv".to_owned(), ty: TypeID::Some(2), pos: [StringPosition::new(), StringPosition::new()] },
        ],
        pos: [StringPosition::new(), StringPosition::new(), StringPosition::new()],
        valid: true,
    };

    // Normal equal
    let left = &mut left_getter();
    let right = &mut right_getter();
    assert!(left.sign_eq(right));
    assert!(right.sign_eq(left));

    // Name not Equal
    let left = &mut left_getter();
    let right = &mut right_getter();
    left.name = "another".to_owned();
    assert!(!left.sign_eq(right));
    assert!(!right.sign_eq(left));

    // Something is invalid
    let left = &mut left_getter();
    let right = &mut right_getter();
    right.valid = false;
    assert!(!left.sign_eq(right));
    assert!(!right.sign_eq(left));

    // One of the arg is invalid
    let left = &mut left_getter();
    let right = &mut right_getter();
    left.args[0].ty = TypeID::Invalid;
    assert!(!left.sign_eq(right));
    assert!(!right.sign_eq(left));

    // One of the arg name is not same
    let left = &mut left_getter();
    let right = &mut right_getter();
    right.args[1].name = "some other".to_owned();
    assert!(left.sign_eq(right));
    assert!(right.sign_eq(left));

    // One of the arg type is not same
    let left = &mut left_getter();
    let right = &mut right_getter();
    left.args[1].ty = TypeID::Some(8);
    assert!(!left.sign_eq(right));
    assert!(!right.sign_eq(left));
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
    let (args, valid) = FnDecl::new_args(syn_args, fn_pos, fn_name, sess);
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
    let (args, valid) = FnDecl::new_args(syn_args, fn_pos, fn_name, sess);
    assert_eq!(valid, false);
    assert_eq!(args[0].name, "a");
    assert_eq!(args[0].ty, TypeID::Some(5));
    assert_eq!(args[0].pos, [make_str_pos!(1, 9, 1, 11), make_str_pos!(1, 13, 1, 13)]);
    assert_eq!(args[1].name, "b");
    assert_eq!(args[1].ty, TypeID::Invalid);
    assert_eq!(args[1].pos, [make_str_pos!(1, 16, 1, 23), make_str_pos!(1, 25, 1, 25)]);
    assert_eq!(args[2].name, "cde");
    assert_eq!(args[2].ty, TypeID::Some(15)); // [u8] is 14, [[u8]] is 15
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
    let (args, valid) = FnDecl::new_args(syn_args, fn_pos, fn_name, sess);
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
    let (args, valid) = FnDecl::new_args(syn_args, fn_pos, fn_name, sess);
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
    let mut syn_fn = SyntaxFunctionDef::from_str(program, 0);
    syn_fn.id = 42;
    let (fndecl, id, block) = FnDecl::new(syn_fn, sess);
    assert_eq!(fndecl.id, 42);
    assert_eq!(fndecl.name, "main");
    assert_eq!(fndecl.args.len(), 0);
    assert_eq!(fndecl.pos, [make_str_pos!(1, 1, 1, 2), make_str_pos!(1, 4, 1, 7), make_str_pos!(1, 14, 1, 15)]);
    assert_eq!(fndecl.ret_type, TypeID::Some(0));
    assert_eq!(id, 42);
    assert_eq!(block.stmts.len(), 1);
}