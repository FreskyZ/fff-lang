
// Type info

use std::fmt;

use common::format_vector_debug;
use message::CodegenMessage;
use message::MessageEmitter;

use syntax::SMType;

#[derive(Eq, PartialEq, Clone)]
pub struct TypeField {
    pub name: String,
    pub ty: Type,
    pub offset: usize,
}
impl fmt::Debug for TypeField {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} {} @ {}", self.ty, self.name, self.offset)
    }
}

#[derive(Eq, PartialEq, Clone)]
pub struct TypeInfo {
    pub name: String, // currently is only string, to be `Name` in the future
    pub fields: Vec<TypeField>,
    pub size: usize,
}
impl fmt::Debug for TypeInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.fields.len() {
            0 => write!(f, "type {} {{ }}, sizeof = {}", self.name, self.size),
            _n => write!(f, "type {} {{\n{}\n}}, sizeof = {}", self.name, format_vector_debug(&self.fields, "\n"), self.size)
        }
    }
}

#[derive(Eq, PartialEq, Clone)]
pub enum Type {
    Unit,
    Base(Box<TypeInfo>),
    Array(Box<Type>),
    Tuple(Vec<Type>),
}
impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Unit => write!(f, "()"),
            Type::Base(ref boxed_base) => write!(f, "{:?}", boxed_base.as_ref()),
            Type::Array(ref boxed_base) => write!(f, "[{:?}]", boxed_base.as_ref()),
            Type::Tuple(ref types) => write!(f, "({})", format_vector_debug(types, "\n")) 
        }
    }
}
impl Type {
}

// Will support free order of declaration
// Currently not concern recursive reference so that need delay resolve 
#[derive(Eq, PartialEq)]
pub struct TypeCollection {
    infos: Vec<TypeInfo>,
}
impl fmt::Debug for TypeCollection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format_vector_debug(&self.infos, "\n"))
    }
}

macro_rules! make_simple_type {
    ($name: expr, $size: expr) => (
        TypeInfo{
            name: $name.to_owned(),
            fields: Vec::new(),
            size: $size
        }
    )
}

impl TypeCollection {

    pub fn new() -> TypeCollection {

        let mut ret_val = TypeCollection{
            infos: Vec::new(),
        };

        ret_val.add_prim_types();
        ret_val
    }

    // Do not tell primitive type to gener
    fn add_prim_types(&mut self) {
        self.infos.append(&mut vec![
            make_simple_type!("i8", 1),
            make_simple_type!("u8", 1),
            make_simple_type!("i16", 2),
            make_simple_type!("u16", 2),
            make_simple_type!("i32", 4),
            make_simple_type!("u32", 4),
            make_simple_type!("i64", 8),
            make_simple_type!("u64", 8),
            make_simple_type!("f32", 4),
            make_simple_type!("f64", 8),
            make_simple_type!("char", 4), // UTF32 char
            make_simple_type!("bool", 1), // 1 byte bool
            make_simple_type!("string", 24), // special [char], which is sizeof pointer add capability and size
        ])
    }

    // Because multi message may occur in long tuple type, need a message emitter
    // check every base existense, make proper Type to return, or else emit message
    pub fn try_to_type(&mut self, ty: SMType, messages: &mut MessageEmitter) -> Option<Type> {

        match ty {
            SMType::Unit(_pos) => Some(Type::Unit),
            SMType::Base(name, pos) => {
                for info in &self.infos {
                    if info.name == name {
                        return Some(Type::Base(Box::new(info.clone())));
                    }
                }
                messages.push(CodegenMessage::TypeNotExist{ name: name, pos: pos });
                return None;
            }
            SMType::Array(boxed_base, _pos) => {
                match self.try_to_type(boxed_base.as_ref().clone(), messages) {
                    Some(ty) => Some(Type::Array(Box::new(ty))),
                    None => None, // message emitted
                }
            }
            SMType::Tuple(smtypes, _pos) => {
                let mut types = Vec::new();
                let mut has_failed = false;
                for smtype in smtypes {
                    match self.try_to_type(smtype, messages) {
                        Some(ty) => types.push(ty),
                        None => has_failed = true, // message emitted
                    }
                }
                return if !has_failed { Some(Type::Tuple(types)) } else { None };
            }
        }
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn gen_type_try_push() {
        use super::Type;
        use super::TypeInfo;
        use super::TypeCollection;
        use syntax::SMType;
        use message::CodegenMessage;
        use message::MessageEmitter;
        use common::StringPosition;

        macro_rules! test_case {
            ($ty_str: expr, $expect: expr) => (
                let types = &mut TypeCollection::new();
                let smtype = SMType::from_str($ty_str, 0);
                let messages = &mut MessageEmitter::new();
                match types.try_to_type(smtype, messages) {
                    Some(ty) => assert_eq!(ty, $expect),
                    None => panic!("Unexpectedly return None"),
                }
            );
            
            ($ty_str: expr => $($msg: expr)*) => (
                let types = &mut TypeCollection::new();
                let smtype = SMType::from_str($ty_str, 0);
                let messages = &mut MessageEmitter::new();
                match types.try_to_type(smtype, messages) {
                    Some(ty) => panic!("Unexpectedly success, result: {:?}", ty),
                    None => (),
                }
                let expect_messages = &mut MessageEmitter::new();
                $(
                    expect_messages.push($msg);
                )*
                assert_eq!(messages, expect_messages);
            )
        }

        // Unit
        test_case!{ "()",
            Type::Unit
        }

        // Base normal
        test_case!{ "i32",
            Type::Base(Box::new(TypeInfo{
                name: "i32".to_owned(),
                fields: Vec::new(),
                size: 4,
            }))
        }
        // Base not exist
        test_case!{ "int" =>
            CodegenMessage::TypeNotExist{ name: "int".to_owned(), pos: make_str_pos!(1, 1, 1, 3) }
        }

        // Array only base
        test_case!{ "[u8]",
            Type::Array(Box::new(Type::Base(Box::new(TypeInfo{
                name: "u8".to_owned(),
                fields: Vec::new(),
                size: 1,
            }))))
        }
        // Array array
        test_case!{ "[[string]]",
            Type::Array(Box::new(Type::Array(Box::new(Type::Base(Box::new(TypeInfo{
                name: "string".to_owned(),
                fields: Vec::new(),
                size: 24,
            }))))))
        }
        // Array array not exist
        test_case!{ "[[u1024]]" =>
            CodegenMessage::TypeNotExist{ name: "u1024".to_owned(), pos: make_str_pos!(1, 3, 1, 7) }
        }

        // Tuple only base
        test_case!{ "(i32, u64, char)",
            Type::Tuple(vec![
                Type::Base(Box::new(TypeInfo{
                    name: "i32".to_owned(),
                    fields: Vec::new(),
                    size: 4,
                })),
                Type::Base(Box::new(TypeInfo{
                    name: "u64".to_owned(),
                    fields: Vec::new(),
                    size: 8,
                })),
                Type::Base(Box::new(TypeInfo{
                    name: "char".to_owned(),
                    fields: Vec::new(),
                    size: 4,
                })),
            ])
        }
        // Tuple multiple, include array in tuple, tuple in tuple and tuple in array
        test_case!{ "(i32, [char], (u8, i32, [(string, bool)]))",
            Type::Tuple(vec![
                Type::Base(Box::new(TypeInfo{
                    name: "i32".to_owned(),
                    fields: Vec::new(),
                    size: 4,
                })),
                Type::Array(Box::new(Type::Base(Box::new(TypeInfo{
                    name: "char".to_owned(),
                    fields: Vec::new(),
                    size: 4,
                })))),
                Type::Tuple(vec![
                    Type::Base(Box::new(TypeInfo{
                        name: "u8".to_owned(),
                        fields: Vec::new(),
                        size: 1,
                    })),
                    Type::Base(Box::new(TypeInfo{
                        name: "i32".to_owned(),
                        fields: Vec::new(),
                        size: 4,
                    })),
                    Type::Array(Box::new(Type::Tuple(vec![
                        Type::Base(Box::new(TypeInfo{
                            name: "string".to_owned(),
                            fields: Vec::new(),
                            size: 24,
                        })),
                        Type::Base(Box::new(TypeInfo{
                            name: "bool".to_owned(),
                            fields: Vec::new(),
                            size: 1,
                        })),
                    ]))),
                ]),
            ])            
        }
        // Tuple multiple, error
        //           0        1         2         3
        //           12345678901234567890123456789012345678
        test_case!{ "(i132, [ch], (u8, i32, [(str, bool)]))" => 
            CodegenMessage::TypeNotExist{ name: "i132".to_owned(), pos: make_str_pos!(1, 2, 1, 5) }
            CodegenMessage::TypeNotExist{ name: "ch".to_owned(), pos: make_str_pos!(1, 9, 1, 10) }
            CodegenMessage::TypeNotExist{ name: "str".to_owned(), pos: make_str_pos!(1, 26, 1, 28) }
        }
    }
}