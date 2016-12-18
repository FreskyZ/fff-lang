
// Variable, const or var
// Variable collection, mainly for variable shadowing
// ATTENTION: (2016-12-15) Currently no type size so codegen::Type will assume all size to be 1 and 
//     so offset mechanism is used in varcollection to support future sized type

use std::fmt;

use common::StringPosition;

use message::MessageEmitter;
use message::CodegenMessage;

use codegen::ItemID;
use codegen::type_def::TypeCollection;

#[derive(Eq, PartialEq, Clone, Copy)]
pub enum VarID {
    Some(usize),
    Invalid,
}
impl fmt::Debug for VarID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &VarID::Some(ref val) => write!(f, "vars[{}]", val),
            &VarID::Invalid => write!(f, "vars[<invalid>]"),
        }
    }
}
impl VarID {
    pub fn is_invalid(&self) -> bool {
        match self {
            &VarID::Some(_) => false,
            &VarID::Invalid => true,
        }
    }
    pub fn is_valid(&self) -> bool {
        match self {
            &VarID::Some(_) => true,
            &VarID::Invalid => false,
        }
    }
}

// Variable collection
#[derive(Debug, Eq, PartialEq)]
pub struct Var {
    pub name: String, 
    pub ty: ItemID,
    pub is_const: bool,
    pub def_pos: StringPosition,
    offset: usize, // rbp offset, 0 for some error, while any error won't go into vm
}
impl Var {
    
    pub fn new(name: String, ty: ItemID, is_const: bool, def_pos: StringPosition) -> Var {
        Var{ name: name, ty: ty, is_const: is_const, def_pos: def_pos, offset: 0 }
    }

    #[cfg(test)]
    pub fn new_test(name: &str, ty: ItemID, is_const: bool, def_pos: StringPosition, offset: usize) -> Var {
        Var{ name: name.to_owned(), ty: ty, is_const: is_const, def_pos: def_pos, offset: offset }
    } 
}

#[derive(Debug, Eq, PartialEq)]
pub enum VarOrScope {
    Some(Var),
    ScopeBarrier,
}
impl VarOrScope {
    fn is_barrier(&self) -> bool { match self { &VarOrScope::ScopeBarrier => true, _ => false } }
}

// At first, some variables are pushed and nothing are shadowed
// Then a new var pushed and found same name with previous one -> VariableDefined
// Then a new var pushed, nothing more happened
// Then a barrier pushed, a barrier pushed
// Then a new var pushed and found same name before ret iter to the barrier -> VariableDefined
// Then a new var pushed and found same names before iter to the barrier, shadow the variables before
// Then a barrier poped, all the variables shadow by this layer are not shadowed
// Many years later, name to id are required, just ret iter to find first not shadowed same name variable's id
pub struct VarCollection {
    items: Vec<VarOrScope>,  
    // no need to use buffered mask, rfind, that's exactly variable shadowing's logical meaning and best implementation method
    // next temp local var name, start from ?, which change into "?0", "?1", etc.
    next_temp: usize,
    // next local variable offset from ebp
    // Some means still working, None means meat some error and no need to continue working
    next_offset: Option<usize>, 
}
impl VarCollection {

    pub fn new() -> VarCollection {
        VarCollection{
            items: Vec::new(),
            next_temp: 0,
            next_offset: Some(0),
        }
    }

    // When meet var decl statment or compiler generated var decl statement
    // Add a variable definition, shadow previous scope item
    // First check var name existence before the last scope barrier, 
    //     if this name defined, ignore the item, push message and return invalid id, act like this var is not defined
    // Then check var type validation,
    //     if type is not valid, then the next_offset invalidated because furthur calculation is not necessary
    //     still push the var and return valid id for furthur reference for the name, but as type is invalid, expression type eval will be invalidated, too
    // If next_offset had been None before, just do not add the item_size to the next_offset, 
    //     if the item pass before check, push it, not set offset and return valid id
    pub fn try_push(&mut self, mut item: Var, types: &TypeCollection, messages: &mut MessageEmitter) -> VarID {

        for exist_item in self.items.iter().rev() {
            match exist_item {
                &VarOrScope::Some(ref exist_item) => {
                    if exist_item.name == item.name {
                        messages.push(CodegenMessage::VariableDefined{
                            name: item.name,
                            predef: exist_item.def_pos,
                            curdef: item.def_pos,
                        });
                        return VarID::Invalid;  // Ignore the item
                    }
                }
                &VarOrScope::ScopeBarrier => { 
                    break;
                }
            }
        }

        let ret_val = self.items.len();       
        match (types.find_by_id(item.ty), &mut self.next_offset) {
            (None, next_offset) => { // Invalid type, invalidate next_offset, not set item.offset
                *next_offset = None;
            }
            (Some(ty), &mut Some(ref mut next_offset)) => { // valid item type and valid next offset, set next_offset, set item.offset
                *next_offset += ty.get_size();
                item.offset = *next_offset;
            }
            (Some(_ty), &mut None) => {  // Valid item type but next offset is invalid, not set item.offset
            },
        }
        self.items.push(VarOrScope::Some(item));
        return VarID::Some(ret_val);
    }
    // When meet a scope barrier, or in other words, currently, meet a left brace
    // Add a scope barrier
    pub fn push_scope(&mut self) {
        self.items.push(VarOrScope::ScopeBarrier);
    }
    pub fn pop_scope(&mut self) {
        
        let mut pop_count = 1; // Add Scopebarrier here
        for item in self.items.iter().rev() {
            match item {
                &VarOrScope::Some(ref _item) => pop_count += 1,
                &VarOrScope::ScopeBarrier => break, 
            }
        }

        for _ in 0..pop_count {
            let _ = self.items.pop().unwrap();
        }

        match self.next_offset {
            None => (),
            Some(ref mut offset) => { 
                for item in self.items.iter().rev() { // Skip barriers
                    match item {
                        &VarOrScope::ScopeBarrier => (),
                        &VarOrScope::Some(ref item) => {
                            *offset = item.offset;     // is just the needed next_offset
                            break;
                        }
                    }
                }
            }
        }
    }

    pub fn get_type(&self, id: VarID) -> ItemID {
        match id {
            VarID::Invalid => ItemID::new_invalid(),
            VarID::Some(id) => match self.items[id] {
                VarOrScope::Some(ref var) => var.ty,
                VarOrScope::ScopeBarrier => unreachable!(),
            }
        }
    }
    pub fn get_offset(&self, id: VarID) -> usize {
        match id {
            VarID::Invalid => 0,
            VarID::Some(id) => match self.items[id] {
                VarOrScope::Some(ref var) => var.offset,
                VarOrScope::ScopeBarrier => unreachable!(),
            }
        }
    }
    pub fn current_offset(&self) -> usize {
        self.next_offset.unwrap_or(0)
    }

    // When a name is referenced
    pub fn find_by_name(&self, rhs: &str) -> VarID {

        let len = self.items.len();
        for (index, item) in self.items.iter().rev().enumerate() {
            match item {
                &VarOrScope::Some(ref item) => {
                    if item.name == rhs {
                        return VarID::Some(len - 1 - index);
                    } 
                }
                &VarOrScope::ScopeBarrier => (),
            }
        }
        return VarID::Invalid;
    }
    pub fn find_by_id(&self, id: VarID) -> Option<&Var> { // scope do not have id

        match id {
            VarID::Some(id) => {
                 if id >= self.items.len() {
                     None
                 } else {
                     match self.items[id] {
                         VarOrScope::Some(ref var) => Some(var),
                         VarOrScope::ScopeBarrier => None,
                     }
                 }
            } 
            VarID::Invalid => None,
        }
    }

    pub fn push_temp(&mut self, ty: ItemID, is_const: bool, types: &TypeCollection, messages: &mut MessageEmitter) -> VarID {
        self.next_temp += 1; // do not worry about start from 0
        let next_temp = self.next_temp;
        self.try_push(Var::new("?".to_owned() + &format!("{}", next_temp), ty, is_const, StringPosition::new()), types, messages)  // They are not gonna to redefined
    }  

    pub fn len(&self) -> usize { self.items.len() }
    pub fn index(&self, idx: usize) -> &VarOrScope { &self.items[idx] }
}

#[cfg(test)]
macro_rules! new_var {
    ($name: expr, $id: expr, $is_const: expr, $pos: expr) => (Var::new($name.to_owned(), ItemID::new($id), $is_const, $pos))
}

#[cfg(test)]
#[test]
fn gen_vars_id() {

    let types = &TypeCollection::new();
    let mut vars = VarCollection::new();

    // Nothing
    assert_eq!(vars.len(), 0);
    assert_eq!(vars.items.len(), 0);

    // Normal no barrier no collision push
    let messages = &mut MessageEmitter::new();
    assert_eq!(vars.try_push(new_var!("1", 1, false, make_str_pos!(1, 1, 1, 1)), types, messages), VarID::Some(0));
    assert_eq!(vars.items.len(), 1);
    assert_eq!(vars.find_by_name("1"), VarID::Some(0));
    assert_eq!(vars.find_by_name("2"), VarID::Invalid);
    assert_eq!(messages, &MessageEmitter::new());

    let messages = &mut MessageEmitter::new();
    assert_eq!(vars.try_push(new_var!("3", 5, true, make_str_pos!(1, 2, 1, 2)), types, messages), VarID::Some(1));
    assert_eq!(vars.items.len(), 2);
    assert_eq!(vars.find_by_name("1"), VarID::Some(0));
    assert_eq!(vars.find_by_name("3"), VarID::Some(1));
    assert_eq!(messages, &MessageEmitter::new());

    // Normal no barrier but collision push
    let messages = &mut MessageEmitter::new();
    assert_eq!(vars.try_push(new_var!("1", 5, false, make_str_pos!(1, 3, 1, 3)), types, messages), VarID::Invalid);
    assert_eq!(vars.items.len(), 2);
    assert_eq!(vars.find_by_name("1"), VarID::Some(0));
    assert_eq!(vars.find_by_name("3"), VarID::Some(1));
    let expect_message = &mut MessageEmitter::new();
    expect_message.push(CodegenMessage::VariableDefined{ name: "1".to_owned(), predef: make_str_pos!(1, 1, 1, 1), curdef: make_str_pos!(1, 3, 1, 3) });
    assert_eq!(messages, expect_message);

    // Push barrier
    vars.push_scope();
    let messages = &mut MessageEmitter::new();
    assert_eq!(vars.try_push(new_var!("1", 13, false, make_str_pos!(1, 4, 1, 4)), types, messages), VarID::Some(3));
    assert_eq!(vars.items.len(), 4);
    assert_eq!(vars.find_by_name("1"), VarID::Some(3));  // 2 is the barrier
    assert_eq!(vars.find_by_name("3"), VarID::Some(1));
    assert_eq!(vars.find_by_name("42"), VarID::Invalid);
    assert_eq!(vars.find_by_name("1024"), VarID::Invalid);
    assert_eq!(messages, &MessageEmitter::new());

    let messages = &mut MessageEmitter::new();
    assert_eq!(vars.try_push(new_var!("42", 5, true, make_str_pos!(1, 5, 1, 5)), types, messages), VarID::Some(4));
    assert_eq!(vars.items.len(), 5);
    assert_eq!(vars.find_by_name("1"), VarID::Some(3));
    assert_eq!(vars.find_by_name("3"), VarID::Some(1));
    assert_eq!(vars.find_by_name("42"), VarID::Some(4));
    assert_eq!(vars.find_by_name("1024"), VarID::Invalid);
    assert_eq!(messages, &MessageEmitter::new());

    let messages = &mut MessageEmitter::new();
    assert_eq!(vars.try_push(new_var!("1", 16, false, make_str_pos!(1, 6, 1, 6)), types, messages), VarID::Invalid);
    assert_eq!(vars.items.len(), 5);
    assert_eq!(vars.find_by_name("1"), VarID::Some(3));
    assert_eq!(vars.find_by_name("3"), VarID::Some(1));
    assert_eq!(vars.find_by_name("42"), VarID::Some(4));
    assert_eq!(vars.find_by_name("1024"), VarID::Invalid);
    let expect_message = &mut MessageEmitter::new();
    expect_message.push(CodegenMessage::VariableDefined{ name: "1".to_owned(), predef: make_str_pos!(1, 4, 1, 4), curdef: make_str_pos!(1, 6, 1, 6) });
    assert_eq!(messages, expect_message);

    // Pop barrier
    vars.pop_scope();
    assert_eq!(vars.items.len(), 2);
    assert_eq!(vars.find_by_name("1"), VarID::Some(0));
    assert_eq!(vars.find_by_name("3"), VarID::Some(1));
    assert_eq!(vars.find_by_name("42"), VarID::Invalid);
    assert_eq!(vars.find_by_name("1024"), VarID::Invalid);
}

#[cfg(test)]
#[test]
fn gen_vars_offset() {

    macro_rules! test_case {
        ($vars: expr, $types: expr, $name: expr, $typeid: expr, $var_id: expr, $offset: expr) => ({
            assert_eq!($vars.try_push(Var::new($name.to_owned(), $typeid, false, StringPosition::new()), $types, &mut MessageEmitter::new()), $var_id, "try push return unexpceted");
            assert_eq!($vars.next_offset, Some($offset), "vars.next_offset unexpceted");
            if $var_id.is_valid() {
                assert_eq!($vars.find_by_name($name), $var_id, "find by name return unexpected");
                assert_eq!($vars.find_by_id($var_id).unwrap().offset, $offset, "find by id and unwrap's offset unexpceted");
            }
        });
        ($vars: expr, $types: expr, $name: expr, $typeid: expr, $var_id: expr) => ({
            assert_eq!($vars.try_push(Var::new($name.to_owned(), $typeid, false, StringPosition::new()), $types, &mut MessageEmitter::new()), $var_id, "try push return unexpceted");
            assert_eq!($vars.next_offset, None, "vars.next_offset unexpceted");
            if $var_id.is_valid() {
                assert_eq!($vars.find_by_name($name), $var_id, "find by name return unexpected");
                assert_eq!($vars.find_by_id($var_id).unwrap().offset, 0, "find by id and unwrap's offset unexpceted");
            }
        }) 
    }

    let types = &TypeCollection::new();
    let mut vars = VarCollection::new();

    //          vars, types, name,    var typeid,       var expect id,  var offset and next offset
    test_case!{ vars, types, "name1", ItemID::new(1),  VarID::Some(0), 1 }
    test_case!{ vars, types, "name2", ItemID::new(2),  VarID::Some(1), 2 }
    test_case!{ vars, types, "name3", ItemID::new(13), VarID::Some(2), 3 } // 26 }

    test_case!{ vars, types, "name1", ItemID::new(5),  VarID::Invalid, 3 } // 26 }
    vars.push_scope(); // this is var id 3
    test_case!{ vars, types, "name2", ItemID::new(5),  VarID::Some(4), 4 } // 30 }
    test_case!{ vars, types, "name1", ItemID::new(11), VarID::Some(5), 5 } // 34 }
    test_case!{ vars, types, "name5", ItemID::new(1),  VarID::Some(6), 6 } // 35 }
    vars.push_scope(); // this is var id 7
    test_case!{ vars, types, "name1", ItemID::new(7),  VarID::Some(8), 7 } // 43 }
    vars.pop_scope();
    assert_eq!{ vars.next_offset, Some(6) } //35) }
    vars.pop_scope();
    assert_eq!{ vars.next_offset, Some(3) } // 26) }

    vars.push_scope();
    test_case!{ vars, types, "name2", ItemID::new(5),  VarID::Some(4), 4 } // 30 }
    test_case!{ vars, types, "name not care", ItemID::new_invalid(), VarID::Some(5) }
    test_case!{ vars, types, "name not care again", ItemID::new(8), VarID::Some(6) }
    vars.pop_scope();
    assert_eq!{ vars.next_offset, None }
}