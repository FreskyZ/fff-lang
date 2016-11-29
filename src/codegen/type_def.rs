
// Type info

use std::cmp;
use std::fmt;

use common::format_vector_debug;
use message::CodegenMessage;
use message::MessageEmitter;

use lexical::SeperatorKind;

use syntax::SMType;

use codegen::fn_def::FnID;

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
#[derive(Eq, PartialEq)]
pub enum Type {
    Base(String),
    Array(usize),
    Tuple(Vec<usize>),
}
impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Base(ref name) => write!(f, "{}", name),
            Type::Array(ref inner) => write!(f, "[{:?}]", inner),
            Type::Tuple(ref items) => write!(f, "({})", format_vector_debug(items, ", ")),
        }
    }
}

pub struct TypeCollection {
    items: Vec<Type>,
}
// New
impl TypeCollection {

    pub fn new() -> TypeCollection {

        TypeCollection{
            items: vec![
                Type::Base("unit".to_owned()),     // 0
                Type::Base("i8".to_owned()),       // 1
                Type::Base("u8".to_owned()),       // 2
                Type::Base("i16".to_owned()),      // 3
                Type::Base("u16".to_owned()),      // 4
                Type::Base("i32".to_owned()),      // 5
                Type::Base("u32".to_owned()),      // 6
                Type::Base("i64".to_owned()),      // 7
                Type::Base("u64".to_owned()),      // 8
                Type::Base("f32".to_owned()),      // 9
                Type::Base("f64".to_owned()),      // 10
                Type::Base("char".to_owned()),     // 11   // UTF32 char
                Type::Base("bool".to_owned()),     // 12   // 1 byte bool
                Type::Base("string".to_owned()),   // 13   // special [char]
            ]
        }
    }

    // Check primitive numeric type bin and un op existence, do not input anything rejected by before method and other ops not in ExpressionOperator
    /// Reserved
    fn check_prim_numeric_type_op(&self, id: TypeID, op: SeperatorKind) -> bool {

        macro_rules! check_prim_numeric_op_impl {
            (
                $input_id: expr, $input_op: expr,
                $([$op: pat, $($id: expr, )*])*
            ) => (
                match ($input_op, $input_id) {
                    $(
                        $(
                            ($op, $id) => true,
                        )*
                        ($op, _) => false,   
                    )*
                    (_, _) => unreachable!(),
                }
            )
        }

        let id = match id {
            TypeID::Some(id) if id >= 1 && id <= 12 => id,
            _ => unreachable!(),
        };
        check_prim_numeric_op_impl!{ id, op,
            //    operator kind,       i8, u8, i16, u16, i32, u32, i64, u64, f32, f64, char, bool, 
            [SeperatorKind::Add,        1,  2,   3,   4,   5,   6,   7,   8,   9,  10, ]
            [SeperatorKind::Sub,        1,  2,   3,   4,   5,   6,   7,   8,   9,  10, ]
            [SeperatorKind::Mul,        1,  2,   3,   4,   5,   6,   7,   8,   9,  10, ]
            [SeperatorKind::Div,        1,  2,   3,   4,   5,   6,   7,   8,   9,  10, ]
            [SeperatorKind::Rem,        1,  2,   3,   4,   5,   6,   7,   8,   9,  10, ]
            [SeperatorKind::ShiftLeft,  1,  2,   3,   4,   5,   6,   7,   8, ]
            [SeperatorKind::ShiftRight, 1,  2,   3,   4,   5,   6,   7,   8, ]
            [SeperatorKind::Equal,      1,  2,   3,   4,   5,   6,   7,   8,   9,  10,   11,   12, ]
            [SeperatorKind::NotEqual,   1,  2,   3,   4,   5,   6,   7,   8,   9,  10,   11,   12, ]
            [SeperatorKind::Great,      1,  2,   3,   4,   5,   6,   7,   8,   9,  10,   11, ]
            [SeperatorKind::Less,       1,  2,   3,   4,   5,   6,   7,   8,   9,  10,   11, ]
            [SeperatorKind::GreatEqual, 1,  2,   3,   4,   5,   6,   7,   8,   9,  10,   11, ]
            [SeperatorKind::LessEqual,  1,  2,   3,   4,   5,   6,   7,   8,   9,  10,   11, ]
            [SeperatorKind::BitAnd,     1,  2,   3,   4,   5,   6,   7,   8, ]
            [SeperatorKind::BitOr,      1,  2,   3,   4,   5,   6,   7,   8, ]
            [SeperatorKind::BitXor,     1,  2,   3,   4,   5,   6,   7,   8, ]
            [SeperatorKind::LogicalAnd,                                                        12, ]
            [SeperatorKind::LogicalOr,                                                         12, ]
            [SeperatorKind::BitNot,     1,  2,   3,   4,   5,   6,   7,   8, ]
            [SeperatorKind::LogicalNot,                                                        12, ]
            [SeperatorKind::Increase,   1,  2,   3,   4,   5,   6,   7,   8, ]
            [SeperatorKind::Decrease,   1,  2,   3,   4,   5,   6,   7,   8, ]
        }
    }
}
// Type usage to ID
impl TypeCollection {

    fn check_exist(&self, newtype: &Type) -> Option<usize> {
        for (index, ty) in self.items.iter().enumerate() {
            if newtype == ty {
                return Some(index);
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
            SMType::Unit(_pos) => Some(0),
            SMType::Base(name, pos) => {
                match self.check_exist(&Type::Base(name.clone())){
                    Some(id) => Some(id),
                    None => {
                        messages.push(CodegenMessage::TypeNotExist{ name: name, pos: pos });
                        None
                    }
                }
            }
            SMType::Array(boxed_base, _pos) => {
                match self.get_id(boxed_base.as_ref().clone(), messages) {
                    None => None, // message emitted
                    Some(id) => match self.check_exist(&Type::Array(id)) {
                        Some(id) => Some(id),
                        None => {
                            let ret_val = self.items.len();
                            self.items.push(Type::Array(id));
                            return Some(ret_val);
                        }
                    },
                }
            }
            SMType::Tuple(smtypes, _pos) => {
                let mut ids = Vec::new();
                let mut has_failed = false;
                for smtype in smtypes {
                    match self.get_id(smtype, messages) {
                        Some(id) => ids.push(id),
                        None => has_failed = true, // message emitted
                    }
                }
                if has_failed {
                    return None;    // if has failed, just return none
                }

                let newtype = Type::Tuple(ids);
                match self.check_exist(&newtype) {
                    Some(id) => Some(id),
                    None => {
                        let this_id = self.items.len();
                        self.items.push(newtype);
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
}
// Helper
impl TypeCollection {

    pub fn format_display_by_id(&self, id: TypeID) -> String {
        
        match id {
            TypeID::Invalid => "<error-type>".to_owned(),
            TypeID::Some(id) => {
                match self.items[id] {
                    Type::Base(ref name) => name.to_owned(),
                    Type::Array(ref inner_id) => format!("[{}]", self.format_display_by_id(TypeID::Some(*inner_id))),
                    Type::Tuple(ref ids) => {
                        let mut buf = "[".to_owned();
                        for id in ids {
                            buf += &self.format_display_by_id(TypeID::Some(*id));
                        }
                        buf += "]";
                        return buf;
                    }
                }
            }
        }
    }

    pub fn find_by_id(&self, id: TypeID) -> Option<&Type> {
        match id {
            TypeID::Invalid => None,
            TypeID::Some(id) => Some(&self.items[id]),
        }
    }

    #[cfg(test)]
    pub fn find_by_idx(&self, id: usize) -> &Type {
        &self.items[id]
    }
    #[cfg(test)]
    pub fn ty_len(&self) -> usize {
        self.items.len()
    }
}

#[cfg(test)] #[test]
fn gen_type_prim_op() {

    macro_rules! test_case { 
        ($id: expr, $op: expr, $res: expr) => (
            let types = &TypeCollection::new(); 
            assert_eq!(types.check_prim_numeric_type_op(TypeID::Some($id), $op), $res);
        ) 
    }

    test_case!(1, SeperatorKind::Add, true);
    test_case!(12, SeperatorKind::Sub, false);
}

#[cfg(test)] #[test]
fn gen_type() {
    use common::StringPosition;

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

    let mut types = TypeCollection::new();

    {   // Initial content
        assert_eq!(types.items.len(), 14);
        assert_eq!(types.items[0], Type::Base("unit".to_owned()));
        assert_eq!(types.items[1], Type::Base("i8".to_owned()));
        assert_eq!(types.items[2], Type::Base("u8".to_owned()));
        assert_eq!(types.items[3], Type::Base("i16".to_owned()));
        assert_eq!(types.items[4], Type::Base("u16".to_owned()));
        assert_eq!(types.items[5], Type::Base("i32".to_owned()));
        assert_eq!(types.items[6], Type::Base("u32".to_owned()));
        assert_eq!(types.items[7], Type::Base("i64".to_owned()));
        assert_eq!(types.items[8], Type::Base("u64".to_owned()));
        assert_eq!(types.items[9], Type::Base("f32".to_owned()));
        assert_eq!(types.items[10], Type::Base("f64".to_owned()));
        assert_eq!(types.items[11], Type::Base("char".to_owned()));
        assert_eq!(types.items[12], Type::Base("bool".to_owned()));
        assert_eq!(types.items[13], Type::Base("string".to_owned()));
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
        assert_eq!(types.items[14], Type::Array(2));   // [u8]
        assert_eq!(types.items[15], Type::Array(13));  // [string]
        assert_eq!(types.items[16], Type::Array(15));  // [[string]]
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
        assert_eq!(types.items[17], Type::Tuple(vec![5, 8, 11]));   // (i32, u64, char), 4 + 8 + 4 = 16
        assert_eq!(types.items[18], Type::Tuple(vec![13, 12]));     // (string, bool),   24 + 1 = 25
        assert_eq!(types.items[19], Type::Array(18));               // [(string, bool)],
        assert_eq!(types.items[20], Type::Tuple(vec![2, 5, 19]));   // (u8, i32, [(string, bool)]), 1 + 4 + 24
        assert_eq!(types.items[21], Type::Tuple(vec![5, 16, 20]));  // (i32, [[string]], (u8, i32, [(string, bool)])), 4 + 24 + 29 = 58
    }
}