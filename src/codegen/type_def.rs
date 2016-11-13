
// Type info

use std::cmp;
use std::fmt;

use common::format_vector_debug;
use message::CodegenMessage;
use message::MessageEmitter;

use syntax::SMType;

pub type TypeID = usize;

#[derive(Clone)]
pub struct Type {
    pub id: TypeID,
    pub name: String,             // currently is only string, to be `Name` in the future
    pub type_params: Vec<TypeID>, // for array and tuple
    // fields,                    // primitive type do not have fields
    // funcs,                     // primitive type member function are checked at runtime 
    pub size: usize,
}
impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "type {}, typeid = {}, sizeof = {}", self.name, self.id, self.size)
    }
}
impl cmp::PartialEq<Type> for Type {
    fn eq(&self, rhs: &Type) -> bool {
        self.id == rhs.id
    }
    fn ne(&self, rhs: &Type) -> bool {
        self.id != rhs.id
    }
}
impl cmp::Eq for Type {
}
impl Type {

    fn ident_eq(&self, name: &str, type_params: &Vec<TypeID>) -> bool {
        self.name == name
        && &self.type_params == type_params
    }
}

// Will support free order of declaration
// Currently not concern recursive reference so that need delay resolve 
#[derive(Eq, PartialEq)]
pub struct TypeCollection {
    tys: Vec<Type>,  // TODO: change to Vec, id are index in vec
}
impl fmt::Debug for TypeCollection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format_vector_debug(&self.tys, "\n"))
    }
}

impl TypeCollection {

    pub fn new() -> TypeCollection {

        let mut ret_val = TypeCollection{
            tys: Vec::new(),
        };

        ret_val.add_prim_types();
        ret_val
    }

    fn add_prim(&mut self, id: TypeID, name: &str, size: usize) {
        self.tys.push(Type{ id: id, name: name.to_owned(), type_params: Vec::new(), size: size });
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

    fn next_id(&self) -> TypeID {
        self.tys.len()
    }

    fn check_exist(&self, name: &str, type_params: &Vec<TypeID>) -> Option<TypeID> {
        for ty in &self.tys {
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
    pub fn try_get_id(&mut self, ty: SMType, messages: &mut MessageEmitter) -> Option<TypeID> {

        match ty {
            SMType::Unit(_pos) => self.check_exist("unit", &Vec::new()),
            SMType::Base(name, pos) => {
                match self.check_exist(&name, &Vec::new()) {
                    Some(id) => Some(id),
                    None => {
                        messages.push(CodegenMessage::TypeNotExist{ name: name, pos: pos });
                        None
                    }
                }
            }
            SMType::Array(boxed_base, _pos) => {
                let ty_params = vec![match self.try_get_id(boxed_base.as_ref().clone(), messages) {
                    Some(inner_id) => inner_id,
                    None => return None, // message emitted
                }];
                match self.check_exist("array", &ty_params) {
                    Some(id) => Some(id),
                    None => {
                        let this_id = self.next_id();
                        self.tys.push(Type{
                            id: this_id,
                            name: "array".to_owned(),
                            type_params: ty_params,
                            size: 24,
                        });
                        Some(this_id)
                    }
                }
            }
            SMType::Tuple(smtypes, _pos) => {
                let mut ids = Vec::new();
                let mut all_size = 0_usize;
                let mut has_failed = false;
                for smtype in smtypes {
                    match self.try_get_id(smtype, messages) {
                        Some(id) =>{
                            all_size += self.tys[id].size;
                            ids.push(id);
                        }
                        None => has_failed = true, // message emitted
                    }
                }
                if has_failed {
                    // if has failed, just return none
                    return None;   
                }

                match self.check_exist("tuple", &ids) {
                    Some(id) => Some(id),
                    None => {
                        let this_id = self.next_id();
                        self.tys.push(Type{
                            id: this_id,
                            name: "tuple".to_owned(),
                            type_params: ids,
                            size: all_size
                        });
                        Some(this_id)
                    }
                }
            }
        }
    }

    #[cfg(test)]
    pub fn id_to_type(&self, id: TypeID) -> &Type {
        &self.tys[id]
    }
    #[cfg(test)]
    pub fn ty_len(&self) -> usize {
        self.tys.len()
    }

}

#[cfg(test)]
mod tests {

    #[test]
    fn gen_type() {
        use super::TypeCollection;
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
                assert_eq!(ty.size, $size);
            )
        }

        macro_rules! test_case {
            ($types: expr, $ty_str: expr, $expect: expr) => (
                match $types.try_get_id(SMType::from_str($ty_str, 0), &mut MessageEmitter::new()) {
                    Some(id) => assert_eq!(id, $expect),
                    None => panic!("Unexpectedly return None"),
                }
            );
            
            ($types: expr, $ty_str: expr => $($msg: expr)*) => (
                let messages = &mut MessageEmitter::new();
                match $types.try_get_id(SMType::from_str($ty_str, 0), messages) {
                    Some(id) => panic!("Unexpectedly success, result: {:?}", id),
                    None => (),
                }
                let expect_messages = &mut MessageEmitter::new();
                $(
                    expect_messages.push($msg);
                )*
                assert_eq!(messages, expect_messages);
            )
        }

        let mut types = TypeCollection::new();

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