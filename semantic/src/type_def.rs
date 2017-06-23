///! fff-lang
///!
///! semantic/type_def
// all type defs are owned by type collection which is singleton owned by package
// but every scope have their type id collection

use std::fmt;

use codemap::Span;
use codemap::SymbolID;
use codemap::SymbolCollection;

// type id, !0 for invalid
pub struct TypeID(usize);
impl fmt::Debug for TypeID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 { 0xFFFF_FFFF_FFFF_FFFF => write!(f, "#x"), n => write!(f, "{}", n) }
    }
}
impl Default for TypeID { 
    fn default() -> TypeID { TypeID(!0) } 
}
impl TypeID {
    pub fn new(v: usize) -> TypeID { TypeID(v) }
    pub fn is_valid(&self) -> bool { self.0 != !0 }
}

pub struct TypeField {
    pub name: SymbolID,
    pub name_span: Span,
    pub typeid: TypeID,        // absolute ID
    pub typeuse_span: Span,
    pub offset: usize,
}
impl TypeField {
    fn new(name: SymbolID, name_span: Span, typeid: TypeID, typeuse_span: Span, offset: usize) -> TypeField {
        TypeField{ name, name_span, typeid, typeuse_span, offset }
    } 
}

pub struct TypeDef {
    pub name: SymbolID,
    pub params: Vec<TypeID>,    // absolute ID, for reflection and format
    pub fields: Vec<TypeField>,
    // pub methods: Vec<FnID>,
    pub all_size: usize,
    pub name_span: Span, 
}
impl TypeDef {
    fn new(name: SymbolID, params: Vec<TypeID>, fields: Vec<TypeField>, all_size: usize, name_span: Span) -> TypeDef {
        TypeDef{ name, params, fields, all_size, name_span }
    }
}

pub struct TypeCollection {
    items: Vec<TypeDef>,
}
impl TypeCollection {
    pub fn new(symbols: &mut SymbolCollection) -> TypeCollection {
        TypeCollection{ items: vec![
            TypeDef{
                name: symbols.intern_str("unit"),
                params: Vec::new(),
                fields: Vec::new(), 
                all_size: 0, 
                name_span: Span::default(),
            },
            TypeDef{
                name: symbols.intern_str("bool"),
                params: Vec::new(),
                fields: Vec::new(),
                all_size: 4,
                name_span: Span::default(),
            },
            TypeDef{
                name: symbols.intern_str("u8"),
                params: Vec::new(),
                fields: Vec::new(),
                all_size: 1,
                name_span: Span::default(),
            },
            TypeDef{
                name: symbols.intern_str("i8"),
                params: Vec::new(),
                fields: Vec::new(),
                all_size: 1,
                name_span: Span::default(),
            },
        ]}
    }
}