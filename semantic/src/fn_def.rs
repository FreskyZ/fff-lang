
// FnDecl generater, provide FnDecl current generate state

use std::fmt;
use std::cmp;
use std::ops;
use std::slice;
use std::collections::HashMap;

use codepos::Span;
use message::CodegenMessage;
use message::MessageCollection;

use lexical::SeperatorKind;

use syntax::ISyntaxItem;
use syntax::FunctionDef as SyntaxFunctionDef; 
use syntax::Argument as SyntaxArgument;
use syntax::Block as SyntaxBlock;

use codegen::ItemID;
use codegen::type_def::Type;
use codegen::type_def::TypeCollection;
use codegen::var_def::VarCollection;
use codegen::vm_code::CodeCollection;
use codegen::session::GenerationSession;

pub struct FnArg {
    pub name: String,
    pub typeid: ItemID,           // may be invalid, but the name should be remained for furthure check
    pub pos: Span,      // position start from type end to name
}
impl fmt::Debug for FnArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: type[{:?}] @ {:?}", self.name, self.typeid, self.pos)
    }
}
impl FnArg {
    
    // Always construct, ty maybe none for invalid type
    fn new(syn_arg: SyntaxArgument, types: &mut TypeCollection, messages: &mut MessageCollection, fns: &mut FnCollection) -> FnArg {

        let pos1 = syn_arg.ty.pos();
        let pos2 = syn_arg.pos_name;
        let ty = types.get_id_by_smtype(syn_arg.ty, messages, fns);   // message emitted for none
        FnArg{ name: syn_arg.name, typeid: ty, pos: Span::merge(pos1, pos2) }  
    }
    pub fn new_internal(name: &str, typeid: usize) -> FnArg {
        FnArg{ name: name.to_owned(), typeid: ItemID::new(typeid), pos: Span::new() }
    }

    // ty is None
    pub fn is_valid(&self) -> bool {
        self.typeid.is_valid()
    }

    pub fn fmt_display(&self, types: &TypeCollection) -> String {
        format!("{} {}, ", types.fmt_by_id(self.typeid), self.name)
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum FnName{
    Ident(String), 
    Operator(SeperatorKind),
    Cast(usize), // cast to the typeid
}
impl FnName {
    pub fn fmt(&self, types: &TypeCollection) -> String {
        match *self {
            FnName::Ident(ref name) => format!("{}", name),
            FnName::Operator(ref sep) => match sep {
                &SeperatorKind::Mul => format!("operator*"),
                &SeperatorKind::Div => format!("operator/"),
                &SeperatorKind::Rem => format!("operator%"),
                &SeperatorKind::Add => format!("operator+"),
                &SeperatorKind::Sub => format!("operator-"),
                &SeperatorKind::ShiftLeft => format!("operator<<"),
                &SeperatorKind::ShiftRight => format!("operator>>"),
                &SeperatorKind::Equal => format!("operator=="),
                &SeperatorKind::NotEqual => format!("operator!="),
                &SeperatorKind::Great => format!("operator>"),
                &SeperatorKind::Less => format!("operator<"),
                &SeperatorKind::GreatEqual => format!("operator>="),
                &SeperatorKind::LessEqual => format!("operator<="),
                &SeperatorKind::BitAnd => format!("operator&"),
                &SeperatorKind::BitOr => format!("operator|"),
                &SeperatorKind::BitXor => format!("operator^"),
                &SeperatorKind::LogicalAnd => format!("operator&&"),
                &SeperatorKind::LogicalOr => format!("operator||"),
                &SeperatorKind::Increase => format!("operator++"),
                &SeperatorKind::Decrease => format!("operator--"),
                &SeperatorKind::BitNot => format!("operator~"),
                &SeperatorKind::LogicalNot => format!("operator!"),
                _ => unreachable!()
            },
            FnName::Cast(ref typeid) => format!("as {}", types.fmt_by_id(ItemID::new(*typeid))),
        }
    }
}
impl From<SeperatorKind> for FnName {
    fn from(sep: SeperatorKind) -> FnName { FnName::Operator(sep) }
}
impl From<usize> for FnName {
    fn from(typeid: usize) -> FnName { FnName::Cast(typeid) }
}
impl From<String> for FnName {
    fn from(name: String) -> FnName { FnName::Ident(name) }
}
impl<'a> From<&'a str> for FnName {
    fn from(name: &'a str) -> FnName { FnName::Ident(name.to_owned()) }
}

pub struct FnImpl { // Not Fn because Fn is used in std
    pub id: usize,
    pub name: FnName, 
    pub args: Vec<FnArg>,
    pub ret_type: ItemID,
    pub pos: [Span; 3],  // pos_fn and pos_name and pos_ret_type
    pub valid: bool,               // buf for all args valid and ret_type valid and later not sign collission with other
    pub code_ptr: Option<usize>,   // start pointer in codes, none for internal
    pub local_size: usize,
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
    fn new_args(syn_args: Vec<SyntaxArgument>, fn_pos: Span, fn_name: &str, 
        types: &mut TypeCollection, messages: &mut MessageCollection, fns: &mut FnCollection) -> (Vec<FnArg>, bool) {

        let mut args = Vec::<FnArg>::new();
        let mut valid = true;
        'new_arg: for syn_arg in syn_args {
            'exist_args: for arg in &args {
                if arg.name == syn_arg.name {
                    messages.push(CodegenMessage::FunctionArgumentNameConfilict{ 
                        func_pos: fn_pos,
                        func_name: fn_name.to_owned(),
                        name: arg.name.clone(),
                        pos1: arg.pos.clone(),
                        pos2: syn_arg.pub_pos_all(),
                    });
                    valid = false;
                    continue 'new_arg; // ignore same name arg
                }
            }
            
            // No more errors
            let arg = FnArg::new(syn_arg, types, messages, fns);
            valid = valid && arg.is_valid();
            args.push(arg);
        }

        (args, valid)
    }
    // As it consumes the SyntaxFnDef but not process the block, return it
    fn new(syn_fn: SyntaxFunctionDef, types: &mut TypeCollection, messages: &mut MessageCollection, fns: &mut FnCollection) -> (FnImpl, SyntaxBlock) {
        
        let (args, mut valid) = FnImpl::new_args(syn_fn.args, syn_fn.pos2[0], &syn_fn.name, types, messages, fns);
        let pos_ret_type = syn_fn.ret_type.pos();
        let ret_type = types.get_id_by_smtype(syn_fn.ret_type, messages, fns);
        valid = valid && ret_type.is_valid();
        (FnImpl{ 
            id: 0, 
            name: FnName::Ident(syn_fn.name), 
            pos: [syn_fn.pos2[0], syn_fn.pos2[1], pos_ret_type], 
            args: args, 
            ret_type: ret_type, 
            valid: valid,
            code_ptr: None,
            local_size: 0,
        }, syn_fn.body)
    }

    pub fn is_internal(&self) -> bool { self.code_ptr.is_none() }

    pub fn is_valid(&self) -> bool {
        self.valid
    }

    pub fn sign_eq<T: Into<FnName>>(&self, name: T, args: &Vec<ItemID>) -> bool {
        self.is_valid()
        && self.name == name.into()
        && self.args.len() == args.len()
        && {
            let mut ret_val = true;
            for (ref arg1, ref arg2) in self.args.iter().zip(args.iter()) {
                if arg1.typeid != **arg2 {
                    ret_val = false;
                }
            }  
            ret_val
        }
    }

    pub fn get_sign(&self) -> (FnName, Vec<ItemID>) { // pure signature
        (self.name.clone(), self.args.iter().map(|ref arg| arg.typeid).collect())
    }

    fn fmt_display(&self, types: &TypeCollection) -> String {

        let mut buf = self.name.fmt(types);

        buf += "(";
        let args_len = self.args.len();
        for (index, arg) in self.args.iter().enumerate() {
            buf += &types.fmt_by_id(arg.typeid);
            if index != args_len - 1 {
                buf += ", ";
            }
        }
        buf += &format!(") -> {}", types.fmt_by_id(self.ret_type));
        buf
    }
    fn fmt_display_sign(&self, types: &TypeCollection) -> String {
        
        let mut buf = self.name.fmt(types);

        buf += "(";
        let args_len = self.args.len();
        for (index, arg) in self.args.iter().enumerate() {
            buf += &types.fmt_by_id(arg.typeid);
            if index != args_len - 1 {
                buf += ", ";
            }
        }
        buf += ")";
        buf
    }
    pub fn fmt_display_sign_temp<T: Into<FnName>>(name: T, args: &Vec<ItemID>, types: &TypeCollection) -> String {

        let mut buf = name.into().fmt(types);

        buf += "(";
        let args_len = args.len();
        for (index, arg_typeid) in args.iter().enumerate() {
            buf += &types.fmt_by_id(*arg_typeid);
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
        ret_val.push_builtin_global_fn();
        ret_val
    }

    pub fn push_builtin_fn<T: Into<FnName>>(&mut self, name: T, ret_type: usize, args: Vec<FnArg>) {
        let new_id = self.fns.len();
        self.fns.push(FnImpl{
            id: new_id,
            name: name.into(),
            args: args,
            ret_type: ItemID::new(ret_type),
            pos: [Span::new(); 3],
            valid: true,
            code_ptr: None,
            local_size: 0,
        });
    }
    fn push_builtin_global_fn(&mut self) {
        self.push_builtin_fn("write", 0, vec![
            FnArg::new_internal("arg1", 13),
        ]);
        self.push_builtin_fn("writeln", 0, vec![
            FnArg::new_internal("arg1", 13)
        ]);
        self.push_builtin_fn("read_i32", 5, Vec::new());
        self.push_builtin_fn("read_u64", 8, Vec::new());
    }

    // If same signature, still push the fndecl and return index, but push message and when require ID by signature, return invalid
    pub fn push_decl(&mut self, syn_fn: SyntaxFunctionDef, types: &mut TypeCollection, msgs: &mut MessageCollection, _vars: &mut VarCollection) -> (usize, SyntaxBlock) {
        // BIG BUG HERE
        // This method read in syntax function def and generate semantic function def
        // In the process, get new fn id according to current fns collection and set the id field in the newfn
        // I chose to give the id previous to the fn constructor to avoid make `newfn` here to be mut, just because of a bit of lazy
        // It works well for very long time
        // **BUT** after template type instantiation(although currently only builtin) are added
        // they will push instantiated member functions to this collection
        // this makes the previous recorded new id to be invalid
        // this is a problem that the borrower checker cannot find, because the length field is primitive integral type and is auto copied when recorded
        // so even if the length changed, it cannot notify here
        // Then the error propagates to expression generation where the generater cannot find the identifiers declared in function arguments
        // It's a long way to find the bug here, thanks to gen_fn_decl2 test's 2nd test case's guess on FnName not equal
        // 2016/12/20 - Fresky Han
        let (mut newfn, syn_block) = FnImpl::new(syn_fn, types, msgs, self);
        let ret_val = self.fns.len();
        newfn.id = ret_val;
        self.fns.push(newfn);
        return (ret_val, syn_block);
    }
    // check equal sign after push all decl, for occassions like, A A B A B, to report 2 messages for A and B, not 3 messages for 3 collisions
    pub fn check_sign_eq(&mut self, types: &TypeCollection, msgs: &mut MessageCollection) {

        let mut sign_map_pos = HashMap::<String, Vec<usize>>::new();
        for fcn in &mut self.fns {

            let sign_display = fcn.fmt_display_sign(types);
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
    pub fn find_by_sign<T: Into<FnName> + Clone>(&mut self, name: T, args: &Vec<ItemID>) -> ItemID {
        
        for (index, fcn) in self.fns.iter().enumerate() {
            if fcn.is_valid() && fcn.sign_eq(name.clone(), args) {
                return ItemID::new(index);
            }
        }

        ItemID::new_invalid()
    }
    // Will return None if is invalid
    pub fn find_by_id(&self, id: ItemID) -> Option<&FnImpl> {
        match id.into_option() {
            Some(id) => match self.fns[id].valid {
                true => Some(&self.fns[id]),
                false => None,
            },
            None => None,
        }
    }
    // It mainly used internal and ignore not valid
    pub fn get_by_idx(&self, idx: usize) -> &FnImpl {
        &self.fns[idx]
    }
    pub fn get_by_idx_mut(&mut self, idx: usize) -> &mut FnImpl {
        &mut self.fns[idx]
    }

    // Act like Vec<FnImpl>
    pub fn len(&self) -> usize { self.fns.len() }
    pub fn iter<'a>(&'a self) -> slice::Iter<'a, FnImpl> {
        self.fns.iter()
    }

    pub fn dump(&self, types: &TypeCollection) -> String {
        let mut buf = "Fns:\n".to_owned();
        for i in 0..self.fns.len() {
            match self.fns[i].code_ptr {
                Some(ref code_ptr) =>
                    buf += &format!("    <{}> {} {{ local size = {}, code ptr = {} }};\n", i, self.fns[i].fmt_display(types), self.fns[i].local_size, code_ptr),
                None => 
                    buf += &format!("    <{}> {};\n", i, self.fns[i].fmt_display(types)),
            }
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

    let arg = Argument::with_test_str("[i32] a");
    let arg = FnArg::new(arg, &mut sess.types, &mut sess.msgs, &mut sess.fns);
    assert_eq!(arg.name, "a");
    assert_eq!(arg.typeid, ItemID::new(14)); 
    assert_eq!(arg.pos, make_span!(0, 6));
    let expect_messages = &mut MessageCollection::new();
    assert_eq!(&sess.msgs, expect_messages);

    let arg = Argument::with_test_str("int argc");
    let arg = FnArg::new(arg, &mut sess.types, &mut sess.msgs, &mut sess.fns);
    assert_eq!(arg.name, "argc");
    assert_eq!(arg.typeid, ItemID::new_invalid());
    assert_eq!(arg.pos, make_span!(0, 7));
    let expect_messages = &mut MessageCollection::new();
    expect_messages.push(CodegenMessage::TypeNotExist{ name: "int".to_owned(), pos: make_span!(0, 2) });
    assert_eq!(&sess.msgs, expect_messages);
}

#[cfg(test)]
#[test]
fn gen_fn_sign_eq() {
    
    let left_getter = || FnImpl{
        id: 0,
        name: FnName::Ident("main".to_owned()),
        ret_type: ItemID::new(5),
        args: vec![ 
            FnArg{ name: "argc".to_owned(), typeid: ItemID::new(1), pos: Span::new() },
            FnArg{ name: "argv".to_owned(), typeid: ItemID::new(2), pos: Span::new() },
        ],
        pos: [Span::new(), Span::new(), Span::new()],
        valid: true,
        code_ptr: None,
        local_size: 0,
    };

    // Normal equal
    let left = &mut left_getter();
    assert!(left.sign_eq("main", &vec![ItemID::new(1), ItemID::new(2)]));

    // Name not Equal
    let left = &mut left_getter();
    assert!(!left.sign_eq("another", &vec![ItemID::new(1), ItemID::new(2)]));

    // Something is invalid
    let left = &mut left_getter();
    left.valid = false;
    assert!(!left.sign_eq("main", &vec![ItemID::new(1), ItemID::new(2)]));

    // One of the arg is invalid
    let left = &mut left_getter();
    assert!(!left.sign_eq("main", &vec![ItemID::new_invalid(), ItemID::new(2)]));

    // One of the arg name is not same
    let left = &mut left_getter();
    left.args[1].name = "some other".to_owned();
    assert!(left.sign_eq("main", &vec![ItemID::new(1), ItemID::new(2)]));

    // One of the arg type is not same
    let left = &mut left_getter();
    left.args[1].typeid = ItemID::new(8);
    assert!(!left.sign_eq("main", &vec![ItemID::new(1), ItemID::new(2)]));
}

#[cfg(test)]
#[test]
fn gen_fn_args() {

    // Normal
    let syn_args = vec![ // 12345678901234567890123
        Argument::with_test_str_and_index("fn main(i32 a", 3),
        Argument::with_test_str_and_index("fn main(i32 a, string b", 6),
    ];
    let fn_pos = make_span!(0, 1);
    let fn_name = "main";
    let sess = &mut GenerationSession::new();
    let (args, valid) = FnImpl::new_args(syn_args, fn_pos, fn_name, &mut sess.types, &mut sess.msgs, &mut sess.fns);
    assert_eq!(valid, true);
    assert_eq!(args[0].name, "a");
    assert_eq!(args[0].typeid, ItemID::new(5));
    assert_eq!(args[0].pos, make_span!(8, 12));
    assert_eq!(args[1].name, "b");
    assert_eq!(args[1].typeid, ItemID::new(13));
    assert_eq!(args[1].pos, make_span!(15, 22));
    let expect_message = MessageCollection::new();
    assert_eq!(&sess.msgs, &expect_message);

    // 1 arg type invalid
    let syn_args = vec![ 
        //                  12345678901234567890123456789012345678 
        Argument::with_test_str_and_index("fn main(i32 a, [intttt] b, [[u8]] cde)", 3),
        //                  0        1         2         3
        Argument::with_test_str_and_index("fn main(i32 a, [intttt] b, [[u8]] cde)", 6),
        //                  0  1   23   45 67     8 90 123 45 6  7
        Argument::with_test_str_and_index("fn main(i32 a, [intttt] b, [[u8]] cde)", 11),
    ];  
    let fn_pos = make_span!(0, 1);
    let fn_name = "main";
    let sess = &mut GenerationSession::new();
    let (args, valid) = FnImpl::new_args(syn_args, fn_pos, fn_name, &mut sess.types, &mut sess.msgs, &mut sess.fns);
    assert_eq!(valid, false);
    assert_eq!(args[0].name, "a");
    assert_eq!(args[0].typeid, ItemID::new(5));
    assert_eq!(args[0].pos, make_span!(8, 12));
    assert_eq!(args[1].name, "b");
    assert_eq!(args[1].typeid, ItemID::new_invalid());
    assert_eq!(args[1].pos, make_span!(15, 24));
    assert_eq!(args[2].name, "cde");
    assert_eq!(args[2].typeid, ItemID::new(15)); // [u8] is 14, [[u8]] is 15 
    assert_eq!(args[2].pos, make_span!(27, 36));
    let mut expect_message = MessageCollection::new();
    expect_message.push(CodegenMessage::TypeNotExist{ name: "intttt".to_owned(), pos: make_span!(16, 21) });
    assert_eq!(&sess.msgs, &expect_message);

    // 2 name collision
    let syn_args = vec![ 
        //                  12345678901234567890123456789012345678901234567
        Argument::with_test_str_and_index("fn main(i32 a, u32 bc, i32 a, i32 bc, string d)", 3),
        //                  0        1         2         3         4
        Argument::with_test_str_and_index("fn main(i32 a, u32 bc, i32 a, i32 bc, string d)", 6),
        //                  0  1   23   45 6   7 8 9   01 2   3 4 5      67
        Argument::with_test_str_and_index("fn main(i32 a, u32 bc, i32 a, i32 bc, string d)", 9),
        Argument::with_test_str_and_index("fn main(i32 a, u32 bc, i32 a, i32 bc, string d)", 12),
        Argument::with_test_str_and_index("fn main(i32 a, u32 bc, i32 a, i32 bc, string d)", 15),
    ];
    let fn_pos = make_span!(0, 1);
    let fn_name = "main";
    let sess = &mut GenerationSession::new();
    let (args, valid) = FnImpl::new_args(syn_args, fn_pos, fn_name, &mut sess.types, &mut sess.msgs, &mut sess.fns);
    assert_eq!(valid, false);
    assert_eq!(args[0].name, "a");
    assert_eq!(args[0].typeid, ItemID::new(5));
    assert_eq!(args[0].pos, make_span!(8, 12));
    assert_eq!(args[1].name, "bc");
    assert_eq!(args[1].typeid, ItemID::new(6));
    assert_eq!(args[1].pos, make_span!(15, 20));
    assert_eq!(args[2].name, "d");
    assert_eq!(args[2].typeid, ItemID::new(13));
    assert_eq!(args[2].pos, make_span!(38, 45));
    let mut expect_message = MessageCollection::new();
    expect_message.push(CodegenMessage::FunctionArgumentNameConfilict{ 
        func_pos: make_span!(0, 1), func_name: "main".to_owned(),
        name: "a".to_owned(), pos1: make_span!(8, 12), pos2: make_span!(23, 27),
    });
    expect_message.push(CodegenMessage::FunctionArgumentNameConfilict{ 
        func_pos: make_span!(0, 1), func_name: "main".to_owned(),
        name: "bc".to_owned(), pos1: make_span!(15, 20), pos2: make_span!(30, 35),
    });
    assert_eq!(&sess.msgs, &expect_message);

    // Nothing    
    let syn_args = Vec::new();
    let fn_pos = make_span!(0, 1);
    let fn_name = "main";
    let sess = &mut GenerationSession::new();
    let (args, valid) = FnImpl::new_args(syn_args, fn_pos, fn_name, &mut sess.types, &mut sess.msgs, &mut sess.fns);
    assert_eq!(valid, true);
    assert_eq!(args.len(), 0);
    let expect_message = MessageCollection::new();
    assert_eq!(&sess.msgs, &expect_message);
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
    let syn_fn = SyntaxFunctionDef::with_test_str(program);
    let (id, block) = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);
    {
        let fndecl = sess.fns.get_by_idx(id);
        // assert_eq!(fndecl.id, 0);  // no need to test id, builtin methods may vary during development
        assert_eq!(fndecl.name.fmt(&sess.types), "main");
        assert_eq!(fndecl.args.len(), 0);
        assert_eq!(fndecl.pos, [make_span!(0, 1), make_span!(3, 6), make_span!(13, 14)]);
        assert_eq!(fndecl.ret_type, ItemID::new(0));
        // assert_eq!(id, 0);
        assert_eq!(block.stmts.len(), 1);
    }

    let program = "fn some(i32 a, u32 b, [string] c) -> [u8] {}";
    let syn_fn = SyntaxFunctionDef::with_test_str(program);
    let _ = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);
    let program = " fn some(i32 a, u32 b, [string] c) -> [u8] {}";
    let syn_fn = SyntaxFunctionDef::with_test_str(program);
    let _ = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);
    let program = "  fn some(i32 a, u32 b, string c) -> u8 {}";
    let syn_fn = SyntaxFunctionDef::with_test_str(program);
    let _ = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);
    let program = "   fn some(i32 a, u32 b, [string] c) -> [u8] {}";
    let syn_fn = SyntaxFunctionDef::with_test_str(program);
    let _ = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);
    let program = "    fn some(i32 a, u32 b) -> [u8] {}";
    let syn_fn = SyntaxFunctionDef::with_test_str(program);
    let _ = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);

    let program = "     fn some(i32 a, u32 b, string c) -> [u8] {}";
    let syn_fn = SyntaxFunctionDef::with_test_str(program);
    let _ = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);

    sess.fns.check_sign_eq(&mut sess.types, &mut sess.msgs);
    let expect_messsage1 = &mut MessageCollection::new();
    expect_messsage1.push(CodegenMessage::FunctionRedefinition{
        sign: "some(i32, u32, [string])".to_owned(),
        fnposs: vec![make_span!(0, 1), make_span!(1, 2), make_span!(3, 4)]
    });
    expect_messsage1.push(CodegenMessage::FunctionRedefinition{
        sign: "some(i32, u32, string)".to_owned(),
        fnposs: vec![make_span!(2, 3), make_span!(5, 6)]
    });
    let expect_messsage2 = &mut MessageCollection::new();
    expect_messsage2.push(CodegenMessage::FunctionRedefinition{
        sign: "some(i32, u32, string)".to_owned(),
        fnposs: vec![make_span!(2, 3), make_span!(5, 6)]
    });
    expect_messsage2.push(CodegenMessage::FunctionRedefinition{
        sign: "some(i32, u32, [string])".to_owned(),
        fnposs: vec![make_span!(0, 1), make_span!(1, 2), make_span!(3, 4)] 
    });
    if !(&sess.msgs == expect_messsage1 || &sess.msgs == expect_messsage2) {
        panic!("assertion failed, left: `{:?}`, right: `{:?}`", &sess.msgs, expect_messsage1);
    }
}

#[cfg(test)] #[test]
fn gen_fn_decl2() {
    {
    let program = "fn main(i32 a) -> () { ++a; }";
    let sess = &mut GenerationSession::new();
    let syn_fn = SyntaxFunctionDef::with_test_str(program);
    let (fnid, _syn_block) = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);
    let thefn = sess.fns.get_by_idx(fnid);
    assert_eq!(thefn.args[0].name, "a");
    assert_eq!(thefn.args[0].typeid, ItemID::new(5));
    }
    {
    let program = "fn main(i32 a) -> [u32] { ++a; }";
    let sess = &mut GenerationSession::new();
    let syn_fn = SyntaxFunctionDef::with_test_str(program);
    let (fnid, _syn_block) = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);
    let thefn = sess.fns.get_by_idx(fnid);
    assert_eq!(thefn.name, FnName::Ident("main".to_owned()));
    assert_eq!(thefn.args[0].name, "a");
    assert_eq!(thefn.args[0].typeid, ItemID::new(5));
    }
}