///! fff-lang
///!
///! semantic/fndef

use std::fmt;
use std::cmp;
use std::ops;
use std::slice;
use std::collections::HashMap;

use codepos::StringPosition;
use message::CodegenMessage;
use message::MessageCollection;

use lexical::SeperatorKind;

use syntax;
use syntax::ISyntaxItemWithStr;

use super::super::SymbolID;
use super::super::ItemID;
use super::super::TypeUse;
use super::super::ISemanticItemFormat;
use super::super::ISemanticItemFromSyntaxItem;
use super::super::ResolveSession;
use super::super::type_def::Type;
use super::super::type_def::TypeCollection;
use super::super::var_def::VarCollection;
use super::super::vm_code::CodeCollection;
use super::super::session::GenerationSession;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct FnParam {
    pub name: SymbolID,
    pub decltype: TypeUse,      // should enum to typeid
    pub all_strpos: StringPosition,
}
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct FnDef {
    pub name: SymbolID,               // previously there is FnName, now identifier is interned, operator is interned `operator+` etc. cast is interned `operator as i32` etc.
    pub name_strpos: StringPosition,
    pub params: Vec<FnParam>,
    pub ret_type: TypeUse,            // if implicit `()`, it is next char of the right paren
    pub body: syntax::Block,          // at first phase it is directly syntax::Block because no need of semantic::Block
    pub valid: bool,                  // buf for all args valid and ret_type valid and later not sign collission with other
    pub all_strpos: StringPosition,

    // consider later:
    pub code_ptr: Option<usize>,   // start pointer in codes, none for internal
    pub local_size: usize,
}
impl ISemanticItemFormat for FnDef {
    fn format(&self, indent: u32) -> String {
        let mut retval = String::new();
        retval.push_str(&format!("{}FnDef <{:?}>", FnDef::indent_str(indent), self.all_strpos));
        retval.push_str(&format!("\n{}Name {:?} <{:?}>", FnDef::indent_str(indent + 1), self.name, self.name_strpos));

        for &FnParam{ ref decltype, ref name, ref all_strpos } in &self.params {
            retval.push_str(&format!("\n{}Param {:?} <{:?}>\n{}", FnDef::indent_str(indent + 1), name, all_strpos, decltype.format(indent + 2)));
        }
        retval.push_str(&format!("\n{}", ret_type.format(indent + 1)));
        retval.push_str(&format!("\n{}", self.body.format(indent + 1)));
        return retval;
    }
}
impl fmt::Debug for FnDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(0)) }
}
impl FnDef {

    // Check redefinition, **assume first definition is valid**, so remove collision
    fn check_params_redef(params: Vec<FnParam>, sess: &mut ResolveSession) -> Vec<FnParam> {

        let mut name_and_params = HashMap::<SymbolID, Vec<FnParam>>::new();
        for param in params {
            if let Some(poss) = name_and_params.get_mut(param.name) {
                poss.push(param);
                continue;
            }
            name_and_params.insert(param.name, vec![param]);
        }

        let mut retval = Vec::new();
        for (name, params) in name_and_params {
            if params.len() > 1 {
                // sess.push_message(Message::new_by_str("param redefinition in fn #1, name #8 is defined in position <0>..., <0>..., <0>..."));
                continue;
            }
            retval.push(params[0]);
        }
        return retval;
    }
}
impl ISemanticItemFromSyntaxItem<syntax::FnDef> for FnDef {

    fn from_syntax_item(rfndef: syntax::FnDef, sess: &mut ResolveSession) -> FnDef {

        let name = sess.symbols.intern(rfndef.name);
        let name_strpos = rfndef.name_strpos;
        let params_paren_strpos = rfndef.params_paren_strpos;

        let mut params = Vec::new();
        for syntax::FnParam{ name: param_name, name_strpos: param_name_strpos, decltype } in rfndef.params {
            let param_name = sess.symbols.intern(param_name);
            let decltype = TypeUse::from_syntax_item(decltype, sess);
            let all_strpos = StringPosition::merge(param_name_strpos, decltype.all_strpos);
            params.push(FnParam{ name: param_name, decltype, all_strpos });
        }   
        let params = FnDef::check_params_redef(params, sess);

        let ret_type = match rfndef.ret_type {
            Some(ret_type) => TypeUse::from_syntax_item(ret_type),
            None => TypeUse::from_syntax_item(syntax::TypeUseF::new_unit(StringPosition::double(params_paren_strpos.end_pos().next_col()))),
        };
        let body = rfndef.body;
        FnDef{ name, name_strpos, params, params_paren_strpos, ret_type, body, code_ptr: None, local_size: 0 }
    }
}

impl FnDef {

    pub fn is_internal(&self) -> bool { self.code_ptr.is_none() }

    pub fn is_valid(&self) -> bool {
        self.valid
    }

    // pub fn sign_eq<T: Into<FnName>>(&self, name: T, args: &Vec<ItemID>) -> bool {
    //     self.is_valid()
    //     && self.name == name.into()
    //     && self.args.len() == args.len()
    //     && {
    //         let mut ret_val = true;
    //         for (ref arg1, ref arg2) in self.args.iter().zip(args.iter()) {
    //             if arg1.typeid != **arg2 {
    //                 ret_val = false;
    //             }
    //         }  
    //         ret_val
    //     }
    // }
    // pub fn get_sign(&self) -> (FnName, Vec<ItemID>) { // pure signature
    //     (self.name.clone(), self.args.iter().map(|ref arg| arg.typeid).collect())
    // }

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
    // pub fn fmt_display_sign_temp<T: Into<FnName>>(name: T, args: &Vec<ItemID>, types: &TypeCollection) -> String {

    //     let mut buf = name.into().fmt(types);

    //     buf += "(";
    //     let args_len = args.len();
    //     for (index, arg_typeid) in args.iter().enumerate() {
    //         buf += &types.fmt_by_id(*arg_typeid);
    //         if index != args_len - 1 {
    //             buf += ", ";
    //         }
    //     }
    //     buf += ")";
    //     buf
    // }
}

pub struct FnCollection {
    fns: Vec<FnDef>,
}
impl FnCollection {
    
    pub fn new() -> FnCollection {
        let mut ret_val = FnCollection{ fns: Vec::new() };
        ret_val.push_builtin_global_fn();
        ret_val
    }

    pub fn push_builtin_fn<T: Into<FnName>>(&mut self, name: T, ret_type: usize, args: Vec<FnParam>) {
        let new_id = self.fns.len();
        self.fns.push(FnDef{
            id: new_id,
            name: name.into(),
            args: args,
            ret_type: ItemID::new(ret_type),
            pos: [StringPosition::new(); 3],
            valid: true,
            code_ptr: None,
            local_size: 0,
        });
    }
    fn push_builtin_global_fn(&mut self) {
        self.push_builtin_fn("write", 0, vec![
            FnParam::new_internal("arg1", 13),
        ]);
        self.push_builtin_fn("writeln", 0, vec![
            FnParam::new_internal("arg1", 13)
        ]);
        self.push_builtin_fn("read_i32", 5, Vec::new());
        self.push_builtin_fn("read_u64", 8, Vec::new());
    }

    // If same signature, still push the fndecl and return index, but push message and when require ID by signature, return invalid
    pub fn push_decl(&mut self, syn_fn: syntax::FnDef, types: &mut TypeCollection, msgs: &mut MessageCollection, _vars: &mut VarCollection) -> (usize, syntax::Block) {
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
        let (mut newfn, syn_block) = FnDef::new(syn_fn, types, msgs, self);
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
    pub fn find_by_id(&self, id: ItemID) -> Option<&FnDef> {
        match id.into_option() {
            Some(id) => match self.fns[id].valid {
                true => Some(&self.fns[id]),
                false => None,
            },
            None => None,
        }
    }
    // It mainly used internal and ignore not valid
    pub fn get_by_idx(&self, idx: usize) -> &FnDef {
        &self.fns[idx]
    }
    pub fn get_by_idx_mut(&mut self, idx: usize) -> &mut FnDef {
        &mut self.fns[idx]
    }

    // Act like Vec<FnDef>
    pub fn len(&self) -> usize { self.fns.len() }
    pub fn iter<'a>(&'a self) -> slice::Iter<'a, FnDef> {
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

#[cfg(test)] #[test]
fn gen_fn_arg() {
    use syntax::TypeUseF;

    let mut sess = GenerationSession::new();

    let arg = syntax::FnParam::new("a".to_owned(), make_strpos!(1, 1, 1, 1), TypeUseF::new_simple_test("i32", make_strpos!(1, 1, 1, 1))); // ("a: [i32]");
    let arg = FnParam::new(arg, &mut sess.types, &mut sess.msgs, &mut sess.fns);
    assert_eq!(arg.name, "a");
    assert_eq!(arg.typeid, ItemID::new(14)); 
    assert_eq!(arg.pos, make_str_pos!(1, 1, 1, 7));
    let expect_messages = &mut MessageCollection::new();
    assert_eq!(&sess.msgs, expect_messages);

    let arg = syntax::FnParam::new("argc".to_owned(), make_strpos!(1, 1, 1, 1), TypeUseF::new_simple("i32".to_owned(), make_strpos!(1, 1, 1, 1)));
    // let arg = syntax::FnParam::with_test_str("argc: int");
    let arg = FnParam::new(arg, &mut sess.types, &mut sess.msgs, &mut sess.fns);
    assert_eq!(arg.name, "argc");
    assert_eq!(arg.typeid, ItemID::new_invalid());
    assert_eq!(arg.pos, make_str_pos!(1, 1, 1, 8));
    let expect_messages = &mut MessageCollection::new();
    expect_messages.push(CodegenMessage::TypeNotExist{ name: "int".to_owned(), pos: make_str_pos!(1, 1, 1, 3) });
    assert_eq!(&sess.msgs, expect_messages);
}

#[cfg(test)] #[test]
fn gen_fn_sign_eq() {
    
    let left_getter = || FnDef{
        id: 0,
        name: FnName::Ident("main".to_owned()),
        ret_type: ItemID::new(5),
        args: vec![ 
            FnParam{ name: "argc".to_owned(), typeid: ItemID::new(1), pos: StringPosition::new() },
            FnParam{ name: "argv".to_owned(), typeid: ItemID::new(2), pos: StringPosition::new() },
        ],
        pos: [StringPosition::new(), StringPosition::new(), StringPosition::new()],
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
    use syntax::ISyntaxItemWithStr;

    // Normal
    let syn_args = vec![ // 12345678901234567890123
        // syntax::FnParam::with_test_str_and_index("fn main(i32 a", 3),
        // syntax::FnParam::with_test_str_and_index("fn main(i32 a, string b", 6),
    ];
    let fn_pos = make_str_pos!(1, 1, 1, 2);
    let fn_name = "main";
    let sess = &mut GenerationSession::new();
    let (args, valid) = FnDef::new_args(syn_args, fn_pos, fn_name, &mut sess.types, &mut sess.msgs, &mut sess.fns);
    assert_eq!(valid, true);
    assert_eq!(args[0].name, "a");
    assert_eq!(args[0].typeid, ItemID::new(5));
    assert_eq!(args[0].pos, make_str_pos!(1, 9, 1, 13));
    assert_eq!(args[1].name, "b");
    assert_eq!(args[1].typeid, ItemID::new(13));
    assert_eq!(args[1].pos, make_str_pos!(1, 16, 1, 23));
    let expect_message = MessageCollection::new();
    assert_eq!(&sess.msgs, &expect_message);

    // 1 arg type invalid
    let syn_args = vec![ 
        //                  12345678901234567890123456789012345678 
        // syntax::FnParam::with_test_str_and_index("fn main(i32 a, [intttt] b, [[u8]] cde)", 3),
        //                  0        1         2         3
        // syntax::FnParam::with_test_str_and_index("fn main(i32 a, [intttt] b, [[u8]] cde)", 6),
        //                  0  1   23   45 67     8 90 123 45 6  7
        // syntax::FnParam::with_test_str_and_index("fn main(i32 a, [intttt] b, [[u8]] cde)", 11),
    ];  
    let fn_pos = make_str_pos!(1, 1, 1, 2);
    let fn_name = "main";
    let sess = &mut GenerationSession::new();
    let (args, valid) = FnDef::new_args(syn_args, fn_pos, fn_name, &mut sess.types, &mut sess.msgs, &mut sess.fns);
    assert_eq!(valid, false);
    assert_eq!(args[0].name, "a");
    assert_eq!(args[0].typeid, ItemID::new(5));
    assert_eq!(args[0].pos, make_str_pos!(1, 9, 1, 13));
    assert_eq!(args[1].name, "b");
    assert_eq!(args[1].typeid, ItemID::new_invalid());
    assert_eq!(args[1].pos, make_str_pos!(1, 16, 1, 25));
    assert_eq!(args[2].name, "cde");
    assert_eq!(args[2].typeid, ItemID::new(15)); // [u8] is 14, [[u8]] is 15 
    assert_eq!(args[2].pos, make_str_pos!(1, 28, 1, 37));
    let mut expect_message = MessageCollection::new();
    expect_message.push(CodegenMessage::TypeNotExist{ name: "intttt".to_owned(), pos: make_str_pos!(1, 17, 1, 22) });
    assert_eq!(&sess.msgs, &expect_message);

    // 2 name collision
    let syn_args = vec![ 
        //                  12345678901234567890123456789012345678901234567
        // syntax::FnParam::with_test_str_and_index("fn main(i32 a, u32 bc, i32 a, i32 bc, string d)", 3),
        //                  0        1         2         3         4
        // syntax::FnParam::with_test_str_and_index("fn main(i32 a, u32 bc, i32 a, i32 bc, string d)", 6),
        //                  0  1   23   45 6   7 8 9   01 2   3 4 5      67
        // syntax::FnParam::with_test_str_and_index("fn main(i32 a, u32 bc, i32 a, i32 bc, string d)", 9),
        // syntax::FnParam::with_test_str_and_index("fn main(i32 a, u32 bc, i32 a, i32 bc, string d)", 12),
        // syntax::FnParam::with_test_str_and_index("fn main(i32 a, u32 bc, i32 a, i32 bc, string d)", 15),
    ];
    let fn_pos = make_str_pos!(1, 1, 1, 2);
    let fn_name = "main";
    let sess = &mut GenerationSession::new();
    let (args, valid) = FnDef::new_args(syn_args, fn_pos, fn_name, &mut sess.types, &mut sess.msgs, &mut sess.fns);
    assert_eq!(valid, false);
    assert_eq!(args[0].name, "a");
    assert_eq!(args[0].typeid, ItemID::new(5));
    assert_eq!(args[0].pos, make_str_pos!(1, 9, 1, 13));
    assert_eq!(args[1].name, "bc");
    assert_eq!(args[1].typeid, ItemID::new(6));
    assert_eq!(args[1].pos, make_str_pos!(1, 16, 1, 21));
    assert_eq!(args[2].name, "d");
    assert_eq!(args[2].typeid, ItemID::new(13));
    assert_eq!(args[2].pos, make_str_pos!(1, 39, 1, 46));
    let mut expect_message = MessageCollection::new();
    expect_message.push(CodegenMessage::FunctionArgumentNameConfilict{ 
        func_pos: make_str_pos!(1, 1, 1, 2), func_name: "main".to_owned(),
        name: "a".to_owned(), pos1: make_str_pos!(1, 9, 1, 13), pos2: make_str_pos!(1, 24, 1, 28),
    });
    expect_message.push(CodegenMessage::FunctionArgumentNameConfilict{ 
        func_pos: make_str_pos!(1, 1, 1, 2), func_name: "main".to_owned(),
        name: "bc".to_owned(), pos1: make_str_pos!(1, 16, 1, 21), pos2: make_str_pos!(1, 31, 1, 36),
    });
    assert_eq!(&sess.msgs, &expect_message);

    // Nothing    
    let syn_args = Vec::new();
    let fn_pos = make_str_pos!(1, 1, 1, 2);
    let fn_name = "main";
    let sess = &mut GenerationSession::new();
    let (args, valid) = FnDef::new_args(syn_args, fn_pos, fn_name, &mut sess.types, &mut sess.msgs, &mut sess.fns);
    assert_eq!(valid, true);
    assert_eq!(args.len(), 0);
    let expect_message = MessageCollection::new();
    assert_eq!(&sess.msgs, &expect_message);
}

#[cfg(test)]
#[test]
fn gen_fn_decl() {
    use syntax::ISyntaxItemWithStr;
    // Because no branch, so only one test

    //             0        1         2          3          4
    //             12345678901234567890123456 78901234567 89012
    let program = "fn main() -> () { writeln(\"helloworld\"); }";
    //             1  2   34 5  67 8 9      A B            CD E
    let sess = &mut GenerationSession::new();
    let syn_fn = syntax::FnDef::with_test_str(program);
    let (id, block) = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);
    {
        let fndecl = sess.fns.get_by_idx(id);
        // assert_eq!(fndecl.id, 0);  // no need to test id, builtin methods may vary during development
        assert_eq!(fndecl.name.fmt(&sess.types), "main");
        assert_eq!(fndecl.args.len(), 0);
        assert_eq!(fndecl.pos, [make_str_pos!(1, 1, 1, 2), make_str_pos!(1, 4, 1, 7), make_str_pos!(1, 14, 1, 15)]);
        assert_eq!(fndecl.ret_type, ItemID::new(0));
        // assert_eq!(id, 0);
        assert_eq!(block.get_statements().len(), 1);
    }

    let program = "fn some(i32 a, u32 b, [string] c) -> [u8] {}";
    let syn_fn = syntax::FnDef::with_test_str(program);
    let _ = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);
    let program = " fn some(i32 a, u32 b, [string] c) -> [u8] {}";
    let syn_fn = syntax::FnDef::with_test_str(program);
    let _ = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);
    let program = "  fn some(i32 a, u32 b, string c) -> u8 {}";
    let syn_fn = syntax::FnDef::with_test_str(program);
    let _ = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);
    let program = "   fn some(i32 a, u32 b, [string] c) -> [u8] {}";
    let syn_fn = syntax::FnDef::with_test_str(program);
    let _ = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);
    let program = "    fn some(i32 a, u32 b) -> [u8] {}";
    let syn_fn = syntax::FnDef::with_test_str(program);
    let _ = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);

    let program = "     fn some(i32 a, u32 b, string c) -> [u8] {}";
    let syn_fn = syntax::FnDef::with_test_str(program);
    let _ = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);

    sess.fns.check_sign_eq(&mut sess.types, &mut sess.msgs);
    let expect_messsage1 = &mut MessageCollection::new();
    expect_messsage1.push(CodegenMessage::FunctionRedefinition{
        sign: "some(i32, u32, [string])".to_owned(),
        fnposs: vec![make_str_pos!(1, 1, 1, 2), make_str_pos!(1, 2, 1, 3), make_str_pos!(1, 4, 1, 5)]
    });
    expect_messsage1.push(CodegenMessage::FunctionRedefinition{
        sign: "some(i32, u32, string)".to_owned(),
        fnposs: vec![make_str_pos!(1, 3, 1, 4), make_str_pos!(1, 6, 1, 7)]
    });
    let expect_messsage2 = &mut MessageCollection::new();
    expect_messsage2.push(CodegenMessage::FunctionRedefinition{
        sign: "some(i32, u32, string)".to_owned(),
        fnposs: vec![make_str_pos!(1, 3, 1, 4), make_str_pos!(1, 6, 1, 7)]
    });
    expect_messsage2.push(CodegenMessage::FunctionRedefinition{
        sign: "some(i32, u32, [string])".to_owned(),
        fnposs: vec![make_str_pos!(1, 1, 1, 2), make_str_pos!(1, 2, 1, 3), make_str_pos!(1, 4, 1, 5)] 
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
    let syn_fn = syntax::FnDef::with_test_str(program);
    let (fnid, _syn_block) = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);
    let thefn = sess.fns.get_by_idx(fnid);
    assert_eq!(thefn.args[0].name, "a");
    assert_eq!(thefn.args[0].typeid, ItemID::new(5));
    }
    {
    let program = "fn main(i32 a) -> [u32] { ++a; }";
    let sess = &mut GenerationSession::new();
    let syn_fn = syntax::FnDef::with_test_str(program);
    let (fnid, _syn_block) = sess.fns.push_decl(syn_fn, &mut sess.types, &mut sess.msgs, &mut sess.vars);
    let thefn = sess.fns.get_by_idx(fnid);
    assert_eq!(thefn.name, FnName::Ident("main".to_owned()));
    assert_eq!(thefn.args[0].name, "a");
    assert_eq!(thefn.args[0].typeid, ItemID::new(5));
    }
}