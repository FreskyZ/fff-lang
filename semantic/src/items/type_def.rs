///! fff-lang
///!
///! semantic/type_def
// all type defs are owned by type collection which is singleton owned by package
// but every scope have their type id collection

use codemap::SymbolID;

use syntax;

use super::TypeUse;
use super::super::FromSyntax;
use super::super::SharedDefScope;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct TypeFieldDef {
    pub name: SymbolID,
    pub typeuse: TypeUse,
}
impl FromSyntax<syntax::TypeFieldDef> for TypeFieldDef {
    fn from_syntax(node: syntax::TypeFieldDef, parent_scope: SharedDefScope) -> TypeFieldDef {
        TypeFieldDef{
            name: node.name.value,
            typeuse: FromSyntax::from_syntax(node.typeuse, parent_scope.clone()),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct TypeDef {
    pub name: SymbolID,
    pub fields: Vec<TypeFieldDef>,
}
impl FromSyntax<syntax::TypeDef> for TypeDef {
    fn from_syntax(node: syntax::TypeDef, parent_scope: SharedDefScope) -> TypeDef {
        TypeDef{
            name: node.name.value,
            fields: node.fields.into_iter().map(|field| FromSyntax::from_syntax(field, parent_scope.clone())).collect(),
        }
    }
}

// use codemap::Span;
// use codemap::SymbolID;
// use codemap::SymbolCollection;

// // type id, !0 for invalid
// pub struct TypeID(usize);
// impl fmt::Debug for TypeID {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match self.0 { ::std::usize::MAX => write!(f, "type#x"), n => write!(f, "type#{}", n) }
//     }
// }
// impl Default for TypeID { 
//     fn default() -> TypeID { TypeID(!0) } 
// }
// impl TypeID {
//     pub fn new(v: usize) -> TypeID { TypeID(v) }
//     pub fn is_valid(&self) -> bool { self.0 != !0 }
// }

// pub struct TypeField {
//     pub name: SymbolID,
//     pub name_span: Span,
//     pub typeid: TypeID,
//     pub typeuse_span: Span,
//     pub offset: usize,
// }
// impl TypeField {
//     fn new(name: SymbolID, name_span: Span, typeid: TypeID, typeuse_span: Span, offset: usize) -> TypeField {
//         TypeField{ name, name_span, typeid, typeuse_span, offset }
//     } 
// }

// pub struct TypeDef {
//     pub name: SymbolID,
//     pub params: Vec<TypeID>,    // absolute ID, for reflection and format
//     pub fields: Vec<TypeField>,
//     // pub methods: Vec<FnID>,
//     pub all_size: usize,
//     pub name_span: Span, 
// }
// impl TypeDef {
//     fn new(name: SymbolID, params: Vec<TypeID>, fields: Vec<TypeField>, all_size: usize, name_span: Span) -> TypeDef {
//         TypeDef{ name, params, fields, all_size, name_span }
//     }
// }

// pub struct TypeCollection {
//     items: Vec<TypeDef>,
// }
// impl TypeCollection {

//     pub fn new(symbols: &mut SymbolCollection) -> TypeCollection {
//         macro_rules! primitive_simple_typedef { 
//             ($name: expr, $size: expr) => 
//                 (TypeDef{ name: symbols.intern_str($name), params: Vec::new(), fields: Vec::new(), all_size: $size, name_span: Span::default() }) 
//         }

//         TypeCollection{ items: vec![
//             primitive_simple_typedef!("unit", 0),
//             primitive_simple_typedef!("bool", 1),
//             primitive_simple_typedef!("i8", 1),
//             primitive_simple_typedef!("u8", 1),
//             primitive_simple_typedef!("i16", 2),
//             primitive_simple_typedef!("u16", 2),
//             primitive_simple_typedef!("i32", 4),
//             primitive_simple_typedef!("u32", 4),
//             primitive_simple_typedef!("i64", 8),
//             primitive_simple_typedef!("u64", 8),
//             primitive_simple_typedef!("f32", 4),
//             primitive_simple_typedef!("f64", 8),
//             primitive_simple_typedef!("char", 4),
//             primitive_simple_typedef!("string", 24),
//         ]}
//     }
// }