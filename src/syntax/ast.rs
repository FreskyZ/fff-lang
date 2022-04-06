
use crate::source::{Span, IsId, IdSpan, FileId};
use crate::lexical::{Separator, Keyword, Numeric};

// naming convension:
// - first field is `span` and represents span for complete node
// - Separator field is called `op`, its span is called `op_span`
// - bracket pair span field is called `quote_span`
// - get span methods for abc types are called `span`, e.g. `expr.span()`
// - LabelDef field should be called `label`
// - IsId from Token::Ident is normally called `name` type IdSpan
// - use `parameters` not `params`
// - concrete expression types should be called `XXXExpr`

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ArrayExpr {
    pub span: Span, // bracket span
    pub items: ExprList,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ArrayType {
    pub span: Span,
    pub base: Box<TypeRef>,
    pub size: Expr,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct BinaryExpr {
    pub span: Span,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub op: Separator,
    pub op_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct BlockStatement {
    pub span: Span,
    pub label: Option<IdSpan>,
    pub body: Block,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Block {
    pub span: Span,
    pub items: Vec<Statement>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct EnumDefVariant {
    pub span: Span,
    pub name: IdSpan,
    pub value: Option<Expr>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct EnumDef {
    pub span: Span,
    pub name: IdSpan,
    pub base_type: Option<PrimitiveType>,
    pub quote_span: Span,
    pub variants: Vec<EnumDefVariant>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ExprList {
    pub items: Vec<Expr>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct SimpleExprStatement {
    pub span: Span, // expr.span + semicolon_span
    pub expr: Expr, 
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct AssignExprStatement {
    pub span: Span, // left.span + right.span
    pub left: Expr,
    pub right: Expr,
    pub op: Separator,
    pub op_span: Span,
}

// TODO move to parser
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum ExprListParseResult {
    Empty(Span),
    SingleComma(Span),              // and quote span
    Normal(Span, ExprList),         // and quote span
    EndWithComma(Span, ExprList),   // and quote span
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct CallExpr {
    pub span: Span,
    pub base: Box<Expr>,
    pub parameters: ExprList,
    pub quote_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FnDefParameter {
    pub span: Span, // name_span + r#type.span
    pub name: IdSpan,
    pub r#type: TypeRef,
}
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FnDef {
    pub span: Span, // fn_span + body_span
    pub name: IdSpan,
    pub quote_span: Span, // parameter list quote
    pub parameters: Vec<FnDefParameter>,
    pub ret_type: Option<TypeRef>,
    pub body: Block,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FnTypeParameter {
    pub span: Span,
    pub name: Option<IdSpan>,
    pub r#type: TypeRef,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FnType {
    pub span: Span,
    pub quote_span: Span, // parameter list quote span
    pub parameters: Vec<FnTypeParameter>,
    pub ret_type: Option<Box<TypeRef>>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ForStatement {
    pub span: Span,
    pub label: Option<IdSpan>,
    pub iter_name: IdSpan,
    pub iter_expr: Expr,
    pub body: Block,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct IfClause {
    pub span: Span,
    pub condition: Expr,
    pub body: Block,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ElseClause {
    pub span: Span,
    pub body: Block,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct IfStatement {
    pub span: Span,
    pub if_clause: IfClause,
    pub elseif_clauses: Vec<IfClause>,
    pub else_clause: Option<ElseClause>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct IndexExpr {
    pub span: Span,
    pub base: Box<Expr>,
    pub parameters: ExprList,
    pub quote_span: Span, // bracket span
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ContinueStatement{
    pub span: Span,
    pub label: Option<IdSpan>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct BreakStatement{
    pub span: Span,
    pub label: Option<IdSpan>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum LitValue {
    Unit,
    Bool(bool),
    Char(char),
    Str(IsId),
    Num(Numeric),
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct LitExpr {
    pub span: Span,
    pub value: LitValue,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct LoopStatement {
    pub span: Span,
    pub label: Option<IdSpan>,
    pub body: Block,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum MemberNameBase {
    Ident(IsId),
    Numeric(Numeric),
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct MemberName {
    pub span: Span,
    pub base: MemberNameBase,
    pub base_span: Span,
    pub parameters: Option<TypeList>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct MemberExpr {
    pub span: Span,
    pub base: Box<Expr>,
    pub op_span: Span, // dot span
    pub name: MemberName,
}

// Attention: Clone for driver to store copy of import requests
#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ModuleStatement {
    pub span: Span,
    pub name: IdSpan,
    pub path: Option<IdSpan>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Module {
    pub file: FileId,
    pub items: Vec<Item>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum NameSegment {
    Normal(IdSpan),
    Generic(TypeList),
}

impl NameSegment {
    pub fn span(&self) -> Span {
        match self {
            Self::Normal(id) => id.span,
            Self::Generic(types) => types.span,
        }
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Name {
    pub span: Span,
    pub type_as_segment: Option<TypeAsSegment>,
    pub global: bool,
    pub segments: Vec<NameSegment>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ObjectExprField {
    pub span: Span,
    pub name: IdSpan,
    pub value: Expr,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ObjectExpr {
    pub span: Span,
    pub base: Box<Expr>,
    pub quote_span: Span,
    pub fields: Vec<ObjectExprField>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct RangeFullExpr {
    pub span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct RangeRightExpr {
    pub span: Span,
    pub base: Box<Expr>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct RangeLeftExpr {
    pub span: Span,
    pub base: Box<Expr>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct RangeBothExpr {
    pub span: Span,
    pub left: Box<Expr>,
    pub op_span: Span,
    pub right: Box<Expr>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypeAsSegment {
    pub span: Span,
    pub from: Box<TypeRef>,
    pub to: Box<TypeRef>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypeSegment {
    pub span: Span,
    pub base: IdSpan,
    pub parameters: Option<TypeList>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct PlainType {
    pub span: Span,
    pub type_as_segment: Option<TypeAsSegment>,
    pub global: bool,
    pub segments: Vec<TypeSegment>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct PrimitiveType {
    pub span: Span,
    pub base: Keyword,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct RefType {
    pub span: Span,
    pub base: Box<TypeRef>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ReturnStatement {
    pub span: Span,
    pub value: Option<Expr>,
}

// Paren expr is a side effect of TupleDef
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ParenExpr {
    pub span: Span,
    pub base: Box<Expr>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TupleExpr {
    pub span: Span,
    pub items: ExprList,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TupleType {
    pub span: Span,
    // not TypeList, TypeList is angle bracket quoted, tuple type is paren quoted and empty allowed
    pub parameters: Vec<TypeRef>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypeDefField {
    pub span: Span,
    pub name: IdSpan,
    pub colon_span: Span,
    pub r#type: TypeRef,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypeDef {
    pub span: Span,
    pub name: IdSpan,
    pub fields: Vec<TypeDefField>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypeList {
    pub span: Span,
    pub items: Vec<TypeRef>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct UnaryExpr {
    pub span: Span,
    pub base: Box<Expr>, 
    pub op: Separator, 
    pub op_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct VarDeclStatement {
    pub span: Span,
    pub r#const: bool,
    pub name: IdSpan,
    pub r#type: Option<TypeRef>,
    pub init_value: Option<Expr>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct UseStatement {
    pub span: Span,
    pub name: Name,
    pub alias: Option<IdSpan>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct WhileStatement {
    pub span: Span,
    pub label: Option<IdSpan>,
    pub condition: Expr,
    pub body: Block,
}

macro_rules! define_abc {
    ($(#[$attr:meta])* $vis:vis enum $name:ident { $($variant:ident($ty:ident),)+ }) => (
$(#[$attr])*
$vis enum $name {
    $($variant($ty),)+
}

$(impl From<$ty> for $name {
    fn from(v: $ty) -> $name {
        $name::$variant(v)
    }
})+

impl $name {
    pub fn span(&self) -> Span {
        match self {
            $( Self::$variant(v) => v.span, )+
        }
    }
}
    );
}

define_abc! {
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Expr {
    Lit(LitExpr),
    Name(Name),
    Paren(ParenExpr),
    Tuple(TupleExpr),
    Array(ArrayExpr),
    Call(CallExpr),
    Index(IndexExpr),
    Member(MemberExpr),
    Object(ObjectExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    RangeBoth(RangeBothExpr),
    RangeFull(RangeFullExpr),
    RangeLeft(RangeLeftExpr),
    RangeRight(RangeRightExpr),
}}

impl Expr {
    pub fn dummy() -> Expr { 
        Expr::Lit(LitExpr{ value: LitValue::Num(Numeric::I32(0)), span: Span::new(0, 0) }) 
    }
}

define_abc! {
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Statement {
    Type(TypeDef),
    Enum(EnumDef),
    Fn(FnDef),
    Block(BlockStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
    SimpleExpr(SimpleExprStatement),
    AssignExpr(AssignExprStatement),
    For(ForStatement),
    If(IfStatement),
    Loop(LoopStatement),
    Return(ReturnStatement),
    VarDecl(VarDeclStatement),
    While(WhileStatement),
    Use(UseStatement),
}}

define_abc! {
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Item {
    Type(TypeDef),
    Enum(EnumDef),
    Fn(FnDef),
    Block(BlockStatement),
    SimpleExpr(SimpleExprStatement),
    AssignExpr(AssignExprStatement),
    For(ForStatement),
    If(IfStatement),
    Loop(LoopStatement),
    VarDecl(VarDeclStatement),
    While(WhileStatement),
    Use(UseStatement),
    Import(ModuleStatement),
}}

define_abc! {
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum TypeRef {
    Primitive(PrimitiveType),
    Array(ArrayType),
    Fn(FnType),
    Ref(RefType),
    Tuple(TupleType),
    Plain(PlainType),
}}
