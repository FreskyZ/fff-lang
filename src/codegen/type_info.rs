
// Type info

pub struct TypeField {
    pub name: String,
    pub ty: Type,
    pub offset: usize,
}

pub struct TypeInfo {
    pub name: String, // currently is only string, to be `Name` in the future
    pub fields: Vec<TypeField>,
}

pub enum Type {
    Base(Box<TypeInfo>),
    Array(Box<TypeInfo>),
    Tuple(Vec<TypeInfo>),
}