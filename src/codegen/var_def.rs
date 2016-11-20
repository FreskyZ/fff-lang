
// Variable, const or var
// Variable collection, mainly for variable shadowing

use std::fmt;

use common::StringPosition;

use message::MessageEmitter;
use message::CodegenMessage;

use codegen::TypeID;

// Variable collection
#[derive(Debug, Eq, PartialEq)]
pub struct Var {
    pub name: String, 
    pub ty: TypeID,
    pub is_const: bool,
    pub def_pos: StringPosition,
    is_shadowed: bool,
}
impl Var {
    
    pub fn new(name: String, ty: TypeID, is_const: bool, def_pos: StringPosition) -> Var {
        Var{ name: name, ty: ty, is_const: is_const, def_pos: def_pos, is_shadowed: false }
    }
}

// Scoped collection infrastructure
// Item ID are index in the collection
#[derive(Eq, PartialEq, Clone, Copy)]
pub enum VarID {
    Some(usize),
    Invalid,
}
impl fmt::Debug for VarID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &VarID::Some(ref val) => write!(f, "type[{}]", val),
            &VarID::Invalid => write!(f, "type[-]"),
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
    next_temp: usize, // next temp local var name, start from ?, which change into "?0", "?1", etc.
}
impl VarCollection {

    pub fn new() -> VarCollection {
        VarCollection{
            items: Vec::new(),
            next_temp: 0,
        }
    }

    // When meet var decl statment or compiler generated var decl statement
    // Add a variable definition, shadow previous scope item
    // return the ID if success, the previous item reference and the current item if failed
    pub fn try_push(&mut self, item: Var, messages: &mut MessageEmitter) -> VarID {

        for exist_item in self.items.iter().rev() {
            match exist_item {
                &VarOrScope::Some(ref exist_item) => {
                    if exist_item.name == item.name {
                        messages.push(CodegenMessage::VariableDefined{
                            name: item.name,
                            predef: exist_item.def_pos,
                            curdef: item.def_pos,
                        });
                        return VarID::Invalid;
                    }
                }
                &VarOrScope::ScopeBarrier => { 
                    break;
                }
            }
        }

        // Err returned and new masks set
        let ret_val = self.items.len();
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
            let _ = self.items.pop();
        }
    }

    // When a name is referenced
    pub fn find_by_name(&self, rhs: &str) -> VarID {

        let len = self.items.len() - 1;
        for (index, item) in self.items.iter().rev().enumerate() {
            match item {
                &VarOrScope::Some(ref item) => {
                    if item.name == rhs {
                        return VarID::Some(len - index);
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

    pub fn push_temp(&mut self, ty: TypeID, is_const: bool, messages: &mut MessageEmitter) -> VarID {
        self.next_temp += 1; // do not worry about start from 0
        let next_temp = self.next_temp;
        self.try_push(Var::new("?".to_owned() + &format!("{}", next_temp), ty, is_const, StringPosition::new()), messages)  // They are not gonna to redefined
    }  

    pub fn len(&self) -> usize { self.items.len() }
    pub fn index(&self, idx: usize) -> &VarOrScope { &self.items[idx] }
}

#[cfg(test)]
#[test]
fn gen_var_shadow() {

    macro_rules! new_var {
        ($name: expr, $id: expr, $is_const: expr, $pos: expr) => (Var::new($name.to_owned(), TypeID::Some($id), $is_const, $pos))
    }

    let mut vars = VarCollection::new();

    // Nothing
    assert_eq!(vars.len(), 0);
    assert_eq!(vars.items.len(), 0);

    // Normal no barrier no collision push
    let messages = &mut MessageEmitter::new();
    assert_eq!(vars.try_push(new_var!("1", 1, false, make_str_pos!(1, 1, 1, 1)), messages), VarID::Some(0));
    assert_eq!(vars.items.len(), 1);
    assert_eq!(vars.find_by_name("1"), VarID::Some(0));
    assert_eq!(vars.find_by_name("2"), VarID::Invalid);
    assert_eq!(messages, &MessageEmitter::new());

    let messages = &mut MessageEmitter::new();
    assert_eq!(vars.try_push(new_var!("3", 5, true, make_str_pos!(1, 2, 1, 2)), messages), VarID::Some(1));
    assert_eq!(vars.items.len(), 2);
    assert_eq!(vars.find_by_name("1"), VarID::Some(0));
    assert_eq!(vars.find_by_name("3"), VarID::Some(1));
    assert_eq!(messages, &MessageEmitter::new());

    // Normal no barrier but collision push
    let messages = &mut MessageEmitter::new();
    assert_eq!(vars.try_push(new_var!("1", 5, false, make_str_pos!(1, 3, 1, 3)), messages), VarID::Invalid);
    assert_eq!(vars.items.len(), 2);
    assert_eq!(vars.find_by_name("1"), VarID::Some(0));
    assert_eq!(vars.find_by_name("3"), VarID::Some(1));
    let expect_message = &mut MessageEmitter::new();
    expect_message.push(CodegenMessage::VariableDefined{ name: "1".to_owned(), predef: make_str_pos!(1, 1, 1, 1), curdef: make_str_pos!(1, 3, 1, 3) });
    assert_eq!(messages, expect_message);

    // Push barrier
    vars.push_scope();
    let messages = &mut MessageEmitter::new();
    assert_eq!(vars.try_push(new_var!("1", 13, false, make_str_pos!(1, 4, 1, 4)), messages), VarID::Some(3));
    assert_eq!(vars.items.len(), 4);
    assert_eq!(vars.find_by_name("1"), VarID::Some(3));  // 2 is the barrier
    assert_eq!(vars.find_by_name("3"), VarID::Some(1));
    assert_eq!(vars.find_by_name("42"), VarID::Invalid);
    assert_eq!(vars.find_by_name("1024"), VarID::Invalid);
    assert_eq!(messages, &MessageEmitter::new());

    let messages = &mut MessageEmitter::new();
    assert_eq!(vars.try_push(new_var!("42", 5, true, make_str_pos!(1, 5, 1, 5)), messages), VarID::Some(4));
    assert_eq!(vars.items.len(), 5);
    assert_eq!(vars.find_by_name("1"), VarID::Some(3));
    assert_eq!(vars.find_by_name("3"), VarID::Some(1));
    assert_eq!(vars.find_by_name("42"), VarID::Some(4));
    assert_eq!(vars.find_by_name("1024"), VarID::Invalid);
    assert_eq!(messages, &MessageEmitter::new());

    let messages = &mut MessageEmitter::new();
    assert_eq!(vars.try_push(new_var!("1", 16, false, make_str_pos!(1, 6, 1, 6)), messages), VarID::Invalid);
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