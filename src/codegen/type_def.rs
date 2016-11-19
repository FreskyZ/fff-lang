
// Type info

use std::cmp;
use std::fmt;

use common::format_vector_debug;
use message::CodegenMessage;
use message::MessageEmitter;

use syntax::SMType;

// use codegen::FnID;

#[derive(Eq, PartialEq, Clone, Copy)]
pub enum TypeID {
    Some(usize),
    Invalid,
}
impl fmt::Debug for TypeID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &TypeID::Some(ref val) => write!(f, "type[{}]", val),
            &TypeID::Invalid => write!(f, "type[-]"),
        }
    }
}
impl TypeID {
    pub fn is_invalid(&self) -> bool {
        match self {
            &TypeID::Some(_) => false,
            &TypeID::Invalid => true,
        }
    }
    pub fn is_valid(&self) -> bool {
        match self {
            &TypeID::Some(_) => true,
            &TypeID::Invalid => false,
        }
    }
}

// Type declare is type's name and type parameter
pub struct TypeDecl {
    pub id: usize,                // Here use usize for ID because they must be valid
    pub name: String,             // currently is only string, to be `Name` in the future
    pub type_params: Vec<usize>,  // for array and tuple
    // fields,                    
}
impl fmt::Debug for TypeDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.type_params.len() {
            0 => write!(f, "typeid = {}, {}", self.id, self.name),
            _ => write!(f, "typeid = {}, {}<{}>", self.id, self.name, format_vector_debug(&self.type_params, ", "))
        }
    }
}
impl cmp::PartialEq<TypeDecl> for TypeDecl {
    fn eq(&self, rhs: &TypeDecl) -> bool {
        self.id == rhs.id
    }
    fn ne(&self, rhs: &TypeDecl) -> bool {
        self.id != rhs.id
    }
}
impl cmp::Eq for TypeDecl {
}
impl TypeDecl {

    fn ident_eq(&self, name: &str, type_params: &Vec<usize>) -> bool {
        self.name == name
        && &self.type_params == type_params
    }
}

// Unimplemented because no typedefs in syntax, all types are primitive now
// // Member fields and functions
// pub struct TypeDef {
//     pub id: usize,
//     // pub fields: Vec<TypeField>, // primitive type do not have fields
//     pub funcs: Vec<FunctionID>, 
// }
// impl fmt::Debug for TypeDef {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         write!(f, "typeid = {}, funcs = {}", self.id, self.funcs)
//     }
// }
// impl cmp::PartialEq<TypeDef> for TypeDef {
//     fn eq(&self, rhs: &TypeDef) -> bool {
//         self.id == rhs.id
//     }
//     fn ne(&self, rhs: &TypeDef) -> bool {
//         self.id != rhs.id
//     }
// }
// impl cmp::Eq for TypeDef {
// }
// impl TypeDef {
// }

pub struct TypeDeclCollection {
    decls: Vec<TypeDecl>,
}
impl fmt::Debug for TypeDeclCollection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format_vector_debug(&self.decls, "\n"))
    }
}
impl TypeDeclCollection {

    pub fn new() -> TypeDeclCollection {

        let mut ret_val = TypeDeclCollection{
            decls: Vec::new(),
        };

        ret_val.add_prim_types();
        ret_val
    }

    fn add_prim(&mut self, id: usize, name: &str, _size: usize) {
        self.decls.push(TypeDecl{ id: id, name: name.to_owned(), type_params: Vec::new() });
    }
    // Do not tell primitive type to gener
    fn add_prim_types(&mut self) {

        self.add_prim(0, "unit", 0);       // unit type has size 0

        self.add_prim(1, "i8", 1);
        self.add_prim(2, "u8", 1);
        self.add_prim(3, "i16", 2);
        self.add_prim(4, "u16", 2);
        self.add_prim(5, "i32", 4);
        self.add_prim(6, "u32", 4);
        self.add_prim(7, "i64", 8);
        self.add_prim(8, "u64", 8);
        self.add_prim(9, "f32", 4);
        self.add_prim(10, "f64", 8);
        self.add_prim(11, "char", 4);      // UTF32 char
        self.add_prim(12, "bool", 1);      // 1 byte bool
        self.add_prim(13, "string", 24);   // special [char], which is sizeof pointer add capability and size
    }

    fn next_id(&self) -> usize {
        self.decls.len()
    }

    fn check_exist(&self, name: &str, type_params: &Vec<usize>) -> Option<usize> {
        for ty in &self.decls {
            if ty.ident_eq(name, type_params) {
                return Some(ty.id);
            }
        }
        return None;
    }

    // Because multi message may occur in long tuple type, need a message emitter
    // check base type existence, currently only primitive types, return the id of the primitive type
    // record processed array and tuple types, set their type params, return their type ids
    // position info are removed because messages are finally here, furthur errors will only report "variable with type" etc.
    fn get_id(&mut self, ty: SMType, messages: &mut MessageEmitter) -> Option<usize> {

        match ty {
            SMType::Unit(_pos) => self.check_exist("unit", &Vec::new()),
            SMType::Base(name, pos) => {
                match self.check_exist(&name, &Vec::new()){
                    Some(id) => Some(id),
                    None => {
                        messages.push(CodegenMessage::TypeNotExist{ name: name, pos: pos });
                        None
                    }
                }
            }
            SMType::Array(boxed_base, _pos) => {
                let ty_params = vec![match self.get_id(boxed_base.as_ref().clone(), messages) {
                    Some(inner_id) => inner_id,
                    None => return None,
                }];
                match self.check_exist("array", &ty_params) {
                    Some(id) => Some(id),
                    None => {
                        let this_id = self.next_id();
                        self.decls.push(TypeDecl{
                            id: this_id,
                            name: "array".to_owned(),
                            type_params: ty_params,
                            // size: 24,
                        });
                        Some(this_id)
                    }
                }
            }
            SMType::Tuple(smtypes, _pos) => {
                let mut ids = Vec::new();
                // let mut all_size = 0_usize;
                let mut has_failed = false;
                for smtype in smtypes {
                    match self.get_id(smtype, messages) {
                        Some(id) => {
                            // all_size += self.decls[id].size;
                            ids.push(id);
                        },
                        None => has_failed = true, // message emitted
                    }
                }
                if has_failed {
                    return None;    // if has failed, just return none
                }

                match self.check_exist("tuple", &ids) {
                    Some(id) => Some(id),
                    None => {
                        let this_id = self.next_id();
                        self.decls.push(TypeDecl{
                            id: this_id,
                            name: "tuple".to_owned(),
                            type_params: ids,
                            // size: all_size
                        });
                        Some(this_id)
                    }
                }
            }
        }
    }

    // wrap Option<usize> to TypeID
    pub fn try_get_id(&mut self, ty: SMType, messages: &mut MessageEmitter) -> TypeID {
        match self.get_id(ty, messages) {
            Some(id) => TypeID::Some(id),
            None => TypeID::Invalid,
        }
    }

    #[cfg(test)]
    pub fn id_to_type(&self, id: usize) -> &TypeDecl {
        &self.decls[id]
    }
    #[cfg(test)]
    pub fn ty_len(&self) -> usize {
        self.decls.len()
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn gen_type() {
        use super::TypeDeclCollection;
        use super::TypeID;
        use syntax::SMType;
        use message::CodegenMessage;
        use message::MessageEmitter;
        use common::StringPosition;

        macro_rules! check_content {
            ($types: expr, $id: expr, $name: expr, $params: expr, $size: expr) => (
                let ty = $types.id_to_type($id);
                assert_eq!(ty.id, $id);
                assert_eq!(ty.name, $name);
                assert_eq!(ty.type_params, $params);
                // assert_eq!(ty.size, $size);
            )
        }

        macro_rules! test_case {
            ($types: expr, $ty_str: expr, $expect: expr) => (
                match $types.try_get_id(SMType::from_str($ty_str, 0), &mut MessageEmitter::new()) {
                    TypeID::Some(id) => assert_eq!(id, $expect),
                    TypeID::Invalid => panic!("Unexpectedly return None"),
                }
            );
            
            ($types: expr, $ty_str: expr => $($msg: expr)*) => (
                let messages = &mut MessageEmitter::new();
                match $types.try_get_id(SMType::from_str($ty_str, 0), messages) {
                    TypeID::Some(id) => panic!("Unexpectedly success, result: {:?}", id),
                    TypeID::Invalid => (),
                }
                let expect_messages = &mut MessageEmitter::new();
                $(
                    expect_messages.push($msg);
                )*
                assert_eq!(messages, expect_messages);
            )
        }

        let mut types = TypeDeclCollection::new();

        {   // Initial content
            assert_eq!(types.ty_len(), 14);
            check_content!{ types, 0, "unit", Vec::new(), 0 }
            check_content!{ types, 1, "i8", Vec::new(), 1 }
            check_content!{ types, 2, "u8", Vec::new(), 1 }
            check_content!{ types, 3, "i16", Vec::new(), 2 }
            check_content!{ types, 4, "u16", Vec::new(), 2 }
            check_content!{ types, 5, "i32", Vec::new(), 4 }
            check_content!{ types, 6, "u32", Vec::new(), 4 }
            check_content!{ types, 7, "i64", Vec::new(), 8 }
            check_content!{ types, 8, "u64", Vec::new(), 8 }
            check_content!{ types, 9, "f32", Vec::new(), 4 }
            check_content!{ types, 10, "f64", Vec::new(), 8 }
            check_content!{ types, 11, "char", Vec::new(), 4 }
            check_content!{ types, 12, "bool", Vec::new(), 1 }
            check_content!{ types, 13, "string", Vec::new(), 24 }
        }

        // Unit
        test_case!{ types, "()", 0 }

        // Base
        test_case!{ types, "i32", 5 }
        test_case!{ types, "int" => 
            CodegenMessage::TypeNotExist{ name: "int".to_owned(), pos: make_str_pos!(1, 1, 1, 3) }
        }

        // Array only base
        test_case!{ types, "[u8]", 14 }
        // Array array
        test_case!{ types, "[[string]]", 16 }
        // Array array not exist
        test_case!{ types, "[[u1024]]" =>
            CodegenMessage::TypeNotExist{ name: "u1024".to_owned(), pos: make_str_pos!(1, 3, 1, 7) }
        }
        test_case!{ types, "[u8]", 14 }
        {
            check_content!{ types, 14, "array", vec![2], 24 }   // [u8]
            check_content!{ types, 15, "array", vec![13], 24 }  // [string]
            check_content!{ types, 16, "array", vec![15], 24 }  // [[string]]
        }

        // Tuple only base
        test_case!{ types, "(i32, u64, char)", 17 }
        // Tuple multiple, include array in tuple, tuple in tuple and tuple in array
        test_case!{ types, "(i32, [[string]], (u8, i32, [(string, bool)]))", 21 }
        // Tuple multiple, error
        //           0        1         2         3
        //           12345678901234567890123456789012345678
        test_case!{ types, "(i132, [ch], (u8, i32, [(str, bool)]))" => 
            CodegenMessage::TypeNotExist{ name: "i132".to_owned(), pos: make_str_pos!(1, 2, 1, 5) }
            CodegenMessage::TypeNotExist{ name: "ch".to_owned(), pos: make_str_pos!(1, 9, 1, 10) }
            CodegenMessage::TypeNotExist{ name: "str".to_owned(), pos: make_str_pos!(1, 26, 1, 28) }
        }
        {
            check_content!{ types, 17, "tuple", vec![5, 8, 11], 16 }   // (i32, u64, char), 4 + 8 + 4 = 16
            check_content!{ types, 18, "tuple", vec![13, 12], 25 }     // (string, bool),   24 + 1 = 25
            check_content!{ types, 19, "array", vec![18], 24 }         // [(string, bool)], 
            check_content!{ types, 20, "tuple", vec![2, 5, 19], 29 }   // (u8, i32, [(string, bool)]), 1 + 4 + 24
            check_content!{ types, 21, "tuple", vec![5, 16, 20], 57 }  // (i32, [[string]], (u8, i32, [(string, bool)])), 4 + 24 + 29 = 58
        }
    }
}