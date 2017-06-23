
use std::cmp;
use std::fmt;

use util::format_vector_debug;
use message::CodegenMessage;
use message::MessageCollection;

use lexical::SeperatorKind;
use lexical::LitValue;
use lexical::NumLitValue;

use syntax;

use super::ItemID;
use super::FnCollection;
use super::FnArg;

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct TypeField {
    pub name: String,
    pub typeid: ItemID,
    pub offset: usize,
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
impl Type {
    pub fn get_size(&self) -> usize { 1 } // arbitraty implentation
}
impl Type {

    // Currently, `item0` => 0 `item1` => 1, `item2` => 2
    fn get_field_id_by_field_name(&self, field_name: &str) -> Option<usize> {
        match *self {
            Type::Base(_) | Type::Array(_) => None,
            Type::Tuple(ref _item_types) => {
                // Start with `item`
                let mut field_name_chars = field_name.chars();
                match (field_name_chars.next(), field_name_chars.next(), field_name_chars.next(), field_name_chars.next()) {
                    (Some('i'), Some('t'), Some('e'), Some('m')) => (),
                    _ => return None,
                }

                // follow with simple number
                let mut maybe_field_id = 0usize;
                let left_chars: Vec<_> = field_name_chars.collect();
                let left_chars_len = left_chars.len();
                if left_chars_len >= 8 {
                    return None;
                }
                const TENS: [u32; 9] = [1, 10, 100, 1000, 1_0000, 10_0000, 100_0000, 1000_0000, 10000_0000]; // no 10_0000_0000 for more convenient way to prevent arithmetic overflow panic
                for (index, ch) in left_chars.into_iter().enumerate() {
                    if ch > '9' || ch < '0' { return None; } 
                    maybe_field_id += TENS[left_chars_len - index - 1] as usize * (ch as usize - '0' as usize);
                }
                
                // return 
                return Some(maybe_field_id);
            }
        }
    }

    // Currently it returns value not ref because no where to ref
    pub fn find_field(&self, field_name: &str) -> Option<TypeField> {
        match self.get_field_id_by_field_name(field_name) {
            None => None,
            Some(field_id) => match *self {
                Type::Base(_) | Type::Array(_) => unreachable!(),
                Type::Tuple(ref item_types) => match field_id < item_types.len() {
                    true => Some(TypeField{
                        name: field_name.to_owned(),
                        typeid: ItemID::new(item_types[field_id]),
                        offset: field_id,
                    }),
                    false => None,
                },
            }
        }
    }
}

pub struct TypeCollection {
    types: Vec<Type>,
}

// Type usage to ID
impl TypeCollection {

    fn check_exist(&self, newtype: &Type) -> Option<usize> {
        for (index, ty) in self.types.iter().enumerate() {
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
    fn get_id_internal(&mut self, ty: SMType, messages: &mut MessageCollection, fns: &mut FnCollection) -> Option<usize> {

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
                match self.get_id_internal(boxed_base.as_ref().clone(), messages, fns) {
                    Some(item_typeid) => self.push_builtin_template_type(Type::Array(item_typeid), fns).into_option(),
                    None => None, // message emitted
                }
            }
            SMType::Tuple(smtypes, _pos) => {
                let mut item_typeids = Vec::new();
                let mut has_failed = false;
                for smtype in smtypes {
                    match self.get_id_internal(smtype, messages, fns) {
                        Some(id) => item_typeids.push(id),
                        None => has_failed = true, // message emitted
                    }
                }
                if has_failed {
                    return None;    // if has failed, just return none
                }

                self.push_builtin_template_type(Type::Tuple(item_typeids), fns).into_option()
            }
        }
    }

    pub fn get_id_by_lit(lit: &LitValue) -> ItemID {
        // pub enum LitValue {
        //     Unit,
        //     Str(Option<String>),
        //     Num(Option<NumLitValue>),
        //     Char(Option<char>),
        //     Bool(bool),
        // }
        // pub enum NumLitValue {
        //     I8(i8),
        //     U8(u8),
        //     I16(i16),
        //     U16(u16),
        //     I32(i32),
        //     U32(u32),
        //     I64(i64),
        //     U64(u64),
        //     F32(f32),
        //     F64(f64),
        // }
        match lit {
            &LitValue::Num(None) => ItemID::new_invalid(),
            &LitValue::Str(None) => ItemID::new_invalid(),
            &LitValue::Char(None) => ItemID::new_invalid(),

            &LitValue::Unit => ItemID::new(0),
            &LitValue::Str(_) => ItemID::new(13),
            &LitValue::Char(_) => ItemID::new(11),
            &LitValue::Bool(_) => ItemID::new(12),

            &LitValue::Num(Some(NumLitValue::I8(_))) => ItemID::new(1),
            &LitValue::Num(Some(NumLitValue::U8(_))) => ItemID::new(2),
            &LitValue::Num(Some(NumLitValue::I16(_))) => ItemID::new(3),
            &LitValue::Num(Some(NumLitValue::U16(_))) => ItemID::new(4),
            &LitValue::Num(Some(NumLitValue::I32(_))) => ItemID::new(5),
            &LitValue::Num(Some(NumLitValue::U32(_))) => ItemID::new(6),
            &LitValue::Num(Some(NumLitValue::I64(_))) => ItemID::new(7),
            &LitValue::Num(Some(NumLitValue::U64(_))) => ItemID::new(8),
            &LitValue::Num(Some(NumLitValue::F32(_))) => ItemID::new(9),
            &LitValue::Num(Some(NumLitValue::F64(_))) => ItemID::new(10),
        }
    }
    // wrap Option<usize> to ItemID
    pub fn get_id_by_smtype(&mut self, ty: SMType, messages: &mut MessageCollection, fns: &mut FnCollection) -> ItemID {
        match self.get_id_internal(ty, messages, fns) {
            Some(id) => ItemID::new(id),
            None => ItemID::new_invalid(),
        }
    }
}

macro_rules! push_builtin_fn {
    ($fns: expr, $name: expr, $ret_type: expr, $arg_types: expr) => (
        let mut args = Vec::new();
        for arg_type in &$arg_types {
            args.push(FnArg::new_internal("arg", *arg_type));
        }
        $fns.push_builtin_fn($name, $ret_type, args);
    );
    ($fns: expr, $name: expr, $ret_type: expr) => (
        $fns.push_builtin_fn($name, $ret_type, Vec::new());
    )
}

// New
impl TypeCollection {
    
    pub fn new() -> TypeCollection {
        TypeCollection{ types: vec![
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
        ]}
    }

    pub fn is_primitive_numeric(&self, typeid: ItemID) -> bool {
        match typeid.into_option() {
            Some(id) => 1 <= id && id <= 12,
            None => false,
        }
    }

    // push primitive type member fns
    pub fn push_builtin_types(&mut self, fns: &mut FnCollection) {

        macro_rules! push_integral_base_member {
            ($fns: expr, $typeid: expr) => (
                push_builtin_fn!($fns, SeperatorKind::Add, $typeid, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::Sub, $typeid, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::Mul, $typeid, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::Div, $typeid, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::Rem, $typeid, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::ShiftLeft, $typeid, [$typeid, 5]);
                push_builtin_fn!($fns, SeperatorKind::ShiftRight, $typeid, [$typeid, 5]);
                push_builtin_fn!($fns, SeperatorKind::BitAnd, $typeid, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::BitOr, $typeid, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::BitXor, $typeid, [$typeid, $typeid]);

                push_builtin_fn!($fns, SeperatorKind::Equal, 12, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::NotEqual, 12, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::GreatEqual, 12, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::LessEqual, 12, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::Great, 12, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::Less, 12, [$typeid, $typeid]);

                push_builtin_fn!($fns, SeperatorKind::BitNot, $typeid, [$typeid]);

                push_builtin_fn!($fns, "is_odd", 12, [$typeid]);
                push_builtin_fn!($fns, "to_string", 13, [$typeid]);

                // cast
                push_builtin_fn!($fns, 1, 1, [$typeid]);
                push_builtin_fn!($fns, 2, 2, [$typeid]);
                push_builtin_fn!($fns, 3, 3, [$typeid]);
                push_builtin_fn!($fns, 4, 4, [$typeid]);
                push_builtin_fn!($fns, 5, 5, [$typeid]);
                push_builtin_fn!($fns, 6, 6, [$typeid]);
                push_builtin_fn!($fns, 7, 7, [$typeid]);
                push_builtin_fn!($fns, 8, 8, [$typeid]);
                push_builtin_fn!($fns, 10, 10, [$typeid]);
            )
        }
        macro_rules! push_floating_base_member {
            ($fns: expr, $typeid: expr) => (
                push_builtin_fn!($fns, SeperatorKind::Add, $typeid, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::Sub, $typeid, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::Mul, $typeid, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::Div, $typeid, [$typeid, $typeid]);

                push_builtin_fn!($fns, SeperatorKind::Equal, 12, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::NotEqual, 12, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::GreatEqual, 12, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::LessEqual, 12, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::Great, 12, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::Less, 12, [$typeid, $typeid]);

                push_builtin_fn!($fns, "to_string", 13, [$typeid]);

                // cast
                push_builtin_fn!($fns, 8, 8, [$typeid]);
                push_builtin_fn!($fns, 9, 9, [$typeid]);
                push_builtin_fn!($fns, 10, 10, [$typeid]);
            )
        }
        macro_rules! push_char_base_member {
            ($fns: expr, $typeid: expr) => (
                push_builtin_fn!($fns, SeperatorKind::Equal, 12, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::NotEqual, 12, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::GreatEqual, 12, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::LessEqual, 12, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::Great, 12, [$typeid, $typeid]);
                push_builtin_fn!($fns, SeperatorKind::Less, 12, [$typeid, $typeid]);

                push_builtin_fn!($fns, 6, 6, [$typeid]); // char as u32
                
                push_builtin_fn!($fns, "to_string", 13, [$typeid]);
            )
        }
        
        // integral, floating
        push_integral_base_member!(fns, 1);
        push_integral_base_member!(fns, 2);
        push_integral_base_member!(fns, 3);
        push_integral_base_member!(fns, 4);
        push_integral_base_member!(fns, 5);
        push_integral_base_member!(fns, 6);
        push_integral_base_member!(fns, 7);
        push_integral_base_member!(fns, 8);
        push_floating_base_member!(fns, 9);
        push_floating_base_member!(fns, 10);
        push_char_base_member!(fns, 11);

        // bool
        push_builtin_fn!(fns, SeperatorKind::Equal, 12, [12, 12]);
        push_builtin_fn!(fns, SeperatorKind::NotEqual, 12, [12, 12]);
        push_builtin_fn!(fns, SeperatorKind::LogicalAnd, 12, [12, 12]);
        push_builtin_fn!(fns, SeperatorKind::LogicalOr, 12, [12, 12]);
        push_builtin_fn!(fns, SeperatorKind::LogicalNot, 12, [12]);

        // string
        push_builtin_fn!(fns, SeperatorKind::Add, 13, [13, 13]);
        push_builtin_fn!(fns, SeperatorKind::Equal, 12, [13, 13]);
        push_builtin_fn!(fns, SeperatorKind::NotEqual, 12, [13, 13]);
        push_builtin_fn!(fns, "length", 8, [13]);
        push_builtin_fn!(fns, "get_index", 11, [13, 5]);
        push_builtin_fn!(fns, "get_index", 11, [13, 8]);
    }
    
    // push instantiated builtin template type(currently array and tuple), push their member fns and ctors to the collection
    pub fn push_builtin_template_type(&mut self, ty: Type, fns: &mut FnCollection) -> ItemID {
        
        // if exist, just return
        for (index, item) in self.types.iter().enumerate() {
            if *item == ty {
                return ItemID::new(index);
            }
        }

        match ty {
            Type::Base(_) => unreachable!(),
            Type::Array(item_typeid) => {
                let array_typeid = self.types.len();
                self.types.push(Type::Array(item_typeid));

                push_builtin_fn!(fns, format!("?new_array_{}", item_typeid), array_typeid); // fn ?new_array_5() -> 14  // [i32]
                push_builtin_fn!(fns, "?new_array", array_typeid, [item_typeid, 5]);   // dup ctor 1
                push_builtin_fn!(fns, "?new_array", array_typeid, [item_typeid, 8]);   // dup ctor 2
                push_builtin_fn!(fns, "set_index", 0, [array_typeid, 5, item_typeid]);
                push_builtin_fn!(fns, "set_index", 0, [array_typeid, 8, item_typeid]);
                push_builtin_fn!(fns, "get_index", item_typeid, [array_typeid, 5]);
                push_builtin_fn!(fns, "get_index", item_typeid, [array_typeid, 8]);
                push_builtin_fn!(fns, "push", 0, [array_typeid, item_typeid]);              // fn 14::push(5) -> 0
                push_builtin_fn!(fns, "pop", item_typeid, [array_typeid]);
                push_builtin_fn!(fns, "length", 8, [array_typeid]);
                push_builtin_fn!(fns, SeperatorKind::Equal, 12, [array_typeid, array_typeid]);
                push_builtin_fn!(fns, SeperatorKind::NotEqual, 12, [array_typeid, array_typeid]);

                ItemID::new(array_typeid)
            }
            Type::Tuple(item_typeids) => {
                let tuple_typeid = self.types.len();
                self.types.push(Type::Tuple(item_typeids.clone()));

                push_builtin_fn!(fns, "?new_tuple", tuple_typeid, *item_typeids.as_slice());

                for (index, item_typeid) in item_typeids.iter().enumerate() {
                    push_builtin_fn!(fns, format!("set_item{}", index), 0, [tuple_typeid, *item_typeid]);
                    push_builtin_fn!(fns, format!("set_item{}", index), 0, [tuple_typeid, *item_typeid]);
                }
                push_builtin_fn!(fns, SeperatorKind::Equal, 12, [tuple_typeid, tuple_typeid]);
                push_builtin_fn!(fns, SeperatorKind::NotEqual, 12, [tuple_typeid, tuple_typeid]);

                ItemID::new(tuple_typeid)
            }
        }
    }
}
// Helper
impl TypeCollection {

    pub fn fmt_by_id(&self, id: ItemID) -> String {
        
        match id.as_option() {
            None => "<error-type>".to_owned(),
            Some(id) => {
                match self.types[id] {
                    Type::Base(ref name) => name.to_owned(),
                    Type::Array(ref inner_id) => format!("[{}]", self.fmt_by_id(ItemID::new(*inner_id))),
                    Type::Tuple(ref ids) => {
                        let mut buf = "(".to_owned();
                        for id in ids {
                            buf += &self.fmt_by_id(ItemID::new(*id));
                            buf += ", ";
                        }
                        buf += ")";
                        return buf;
                    }
                }
            }
        }
    }

    pub fn find_by_id(&self, id: ItemID) -> Option<&Type> {
        match id.as_option() {
            None => None,
            Some(id) => Some(&self.types[id]),
        }
    }
    pub fn get_by_idx(&self, id: usize) -> &Type {
        &self.types[id]
    }

    pub fn len(&self) -> usize {
        self.types.len()
    }
    pub fn dump(&self) -> String {
        let mut buf = "Types:\n".to_owned();
        for i in 0..self.types.len() {
            buf += &format!("    {}: {}\n", i, self.fmt_by_id(ItemID::new(i)));
        }
        buf
    }
}

#[cfg(test)] #[test]
fn gen_types_find_field() {
    
    assert_eq!(Type::Tuple(vec![1, 3, 7, 11]).find_field("item1"),
        Some(TypeField{ name: "item1".to_owned(), typeid: ItemID::new(3), offset: 1 })
    );
    assert_eq!(Type::Tuple(vec![1, 3, 7, 11]).find_field("item0"),
        Some(TypeField{ name: "item0".to_owned(), typeid: ItemID::new(1), offset: 0 })
    );
    assert_eq!(Type::Tuple(vec![1, 3, 7, 11]).find_field("item3"),
        Some(TypeField{ name: "item3".to_owned(), typeid: ItemID::new(11), offset: 3 })
    );
    assert_eq!(Type::Tuple(vec![1, 3, 7, 11]).find_field("item4"), None);
    assert_eq!(Type::Tuple(vec![1, 3, 7, 11]).find_field("itemxxx"), None);
    assert_eq!(Type::Tuple(vec![1, 3, 7, 11]).find_field("zzzfield"), None);

    assert_eq!(Type::Base("123".to_owned()).find_field("item0"), None);
    assert_eq!(Type::Array(5).find_field("itemxxx"), None);
}

#[cfg(test)] #[test]
fn gen_types_member_fn() {
    use super::session::GenerationSession;

    let mut sess = GenerationSession::new();

    assert_eq!(sess.fns.find_by_sign(SeperatorKind::Add, &vec![ItemID::new(5), ItemID::new(5)]).is_valid(), true);
    assert_eq!(sess.fns.find_by_sign(SeperatorKind::ShiftLeft, &vec![ItemID::new(8), ItemID::new(5)]).is_valid(), true);
    assert_eq!(sess.fns.find_by_sign("length", &vec![ItemID::new(13)]).is_valid(), true);
    assert_eq!(sess.fns.find_by_sign(12, &vec![ItemID::new(5)]).is_valid(), false); // proud about that

    sess.types.push_builtin_template_type(Type::Array(5), &mut sess.fns);
    sess.types.push_builtin_template_type(Type::Array(9), &mut sess.fns);
    sess.types.push_builtin_template_type(Type::Tuple(vec![14, 15]), &mut sess.fns);

    let messages = &mut MessageCollection::new();
    sess.fns.check_sign_eq(&sess.types, messages);
    if !messages.is_empty() {
        panic!("Messages not empty: {:?}", messages);
    }

    // perrorln!("{}", sess.fns.dump(&sess.types));
}

#[cfg(test)] #[test]
fn gen_types_by_smtype() {
    use codemap::Span;

    macro_rules! test_case {
        ($types: expr, $ty_str: expr, $expect: expr) => (
            match $types.get_id_by_smtype(SMType::with_test_str($ty_str), &mut MessageCollection::new(), &mut FnCollection::new()).as_option() {
                Some(id) => assert_eq!(id, $expect),
                None => panic!("Unexpectedly return None"),
            }
        );
        
        ($types: expr, $ty_str: expr => $($msg: expr)*) => (
            let messages = &mut MessageCollection::new();
            match $types.get_id_by_smtype(SMType::with_test_str($ty_str), messages, &mut FnCollection::new()).as_option() {
                Some(id) => panic!("Unexpectedly success, result: {:?}", id),
                None => (),
            }
            let expect_messages = &mut MessageCollection::new();
            $(
                expect_messages.push($msg);
            )*
            assert_eq!(messages, expect_messages);
        )
    }

    let mut types = TypeCollection::new();

    {   // Initial content
        assert_eq!(types.types.len(), 14);
        assert_eq!(types.types[0], Type::Base("unit".to_owned()));
        assert_eq!(types.types[1], Type::Base("i8".to_owned()));
        assert_eq!(types.types[2], Type::Base("u8".to_owned()));
        assert_eq!(types.types[3], Type::Base("i16".to_owned()));
        assert_eq!(types.types[4], Type::Base("u16".to_owned()));
        assert_eq!(types.types[5], Type::Base("i32".to_owned()));
        assert_eq!(types.types[6], Type::Base("u32".to_owned()));
        assert_eq!(types.types[7], Type::Base("i64".to_owned()));
        assert_eq!(types.types[8], Type::Base("u64".to_owned()));
        assert_eq!(types.types[9], Type::Base("f32".to_owned()));
        assert_eq!(types.types[10], Type::Base("f64".to_owned()));
        assert_eq!(types.types[11], Type::Base("char".to_owned()));
        assert_eq!(types.types[12], Type::Base("bool".to_owned()));
        assert_eq!(types.types[13], Type::Base("string".to_owned()));
    }

    // Unit
    test_case!{ types, "()", 0 }

    // Base
    test_case!{ types, "i32", 5 }
    test_case!{ types, "int" => 
        CodegenMessage::TypeNotExist{ name: "int".to_owned(), pos: make_span!(0, 2) }
    }

    // Array only base
    test_case!{ types, "[u8]", 14 }
    // Array array
    test_case!{ types, "[[string]]", 16 }
    // Array array not exist
    test_case!{ types, "[[u1024]]" =>
        CodegenMessage::TypeNotExist{ name: "u1024".to_owned(), pos: make_span!(2, 6) }
    }
    test_case!{ types, "[u8]", 14 }
    {
        assert_eq!(types.types[14], Type::Array(2));   // [u8]
        assert_eq!(types.types[15], Type::Array(13));  // [string]
        assert_eq!(types.types[16], Type::Array(15));  // [[string]]
    }

    // Tuple only base
    test_case!{ types, "(i32, u64, char)", 17 }
    // Tuple multiple, include array in tuple, tuple in tuple and tuple in array
    test_case!{ types, "(i32, [[string]], (u8, i32, [(string, bool)]))", 21 }
    // Tuple multiple, error
    //           0        1         2         3
    //           12345678901234567890123456789012345678
    test_case!{ types, "(i132, [ch], (u8, i32, [(str, bool)]))" => 
        CodegenMessage::TypeNotExist{ name: "i132".to_owned(), pos: make_span!(1, 4) }
        CodegenMessage::TypeNotExist{ name: "ch".to_owned(), pos: make_span!(8, 9) }
        CodegenMessage::TypeNotExist{ name: "str".to_owned(), pos: make_span!(25, 27) }
    }
    {
        assert_eq!(types.types[17], Type::Tuple(vec![5, 8, 11]));   // (i32, u64, char), 4 + 8 + 4 = 16
        assert_eq!(types.types[18], Type::Tuple(vec![13, 12]));     // (string, bool),   24 + 1 = 25
        assert_eq!(types.types[19], Type::Array(18));               // [(string, bool)],
        assert_eq!(types.types[20], Type::Tuple(vec![2, 5, 19]));   // (u8, i32, [(string, bool)]), 1 + 4 + 24
        assert_eq!(types.types[21], Type::Tuple(vec![5, 16, 20]));  // (i32, [[string]], (u8, i32, [(string, bool)])), 4 + 24 + 29 = 58
    }
}
