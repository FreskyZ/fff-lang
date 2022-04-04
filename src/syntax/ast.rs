
use crate::source::{Span, IsId, FileId};
use crate::lexical::{Separator, Keyword, Numeric};

// naming convension:
// first field is `span` and represents span for complete node
// Separator field is called `op`, its span is called `op_span`
// bracket span field is called `quote_span`
// get span methods for abc types are called `span`, e.g. `expr.span()`

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ArrayDef {
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
    pub left_expr: Box<Expr>,
    pub right_expr: Box<Expr>,
    pub op: Separator,
    pub op_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct BlockStatement {
    pub span: Span,
    pub name: Option<LabelDef>,
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
pub struct EnumVariant {
    pub span: Span,
    pub name: IsId,
    pub name_span: Span,
    pub value: Option<Expr>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct EnumDef {
    pub span: Span,
    pub name: IsId,
    pub name_span: Span,
    pub base_type: Option<PrimitiveType>,
    pub quote_span: Span,
    pub variants: Vec<EnumVariant>,
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
    pub span: Span, // left_expr.span + right_expr.span
    pub left_expr: Expr,
    pub right_expr: Expr,
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
pub struct FnCallExpr {
    pub span: Span,
    pub base: Box<Expr>,
    pub params: ExprList,
    pub quote_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FnParam {
    pub span: Span, // name_span + r#type.span
    pub name: IsId,
    pub name_span: Span,
    pub r#type: TypeRef,
}
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FnDef {
    pub span: Span, // fn_span + body_span
    pub name: IsId,
    pub name_span: Span,
    pub params: Vec<FnParam>,
    pub quote_span: Span, // parameter list quote
    pub ret_type: Option<TypeRef>,
    pub body: Block,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FnTypeParam {
    pub span: Span,
    pub name: Option<(IsId, Span)>,
    pub r#type: TypeRef,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FnType {
    pub span: Span,
    pub quote_span: Span, // parameter list quote span
    pub parameters: Vec<FnTypeParam>,
    pub ret_type: Option<Box<TypeRef>>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ForStatement {
    pub span: Span,
    pub loop_name: Option<LabelDef>,
    pub for_span: Span,
    pub iter_name: IsId,
    pub iter_span: Span,
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
pub struct IndexCallExpr {
    pub span: Span,
    pub base: Box<Expr>,
    pub params: ExprList,
    pub quote_span: Span, // bracket span
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ContinueStatement{
    pub span: Span,
    pub target: Option<(IsId, Span)>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct BreakStatement{
    pub span: Span,
    pub target: Option<(IsId, Span)>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct LabelDef {
    pub span: Span,
    pub name: IsId,
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
    pub name: Option<LabelDef>,
    pub body: Block,
    pub loop_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct MemberAccessExpr {
    pub span: Span,
    pub base: Box<Expr>,
    pub op_span: Span,
    pub name: Name,
}

// Attention: Clone for driver to store copy of import requests
#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ModuleStatement {
    pub span: Span,
    pub name: IsId,
    pub name_span: Span,
    pub path: Option<(IsId, Span)>,
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
    Normal(IsId, Span),
    Generic(Vec<TypeRef>, Span),
}

impl NameSegment {
    pub fn span(&self) -> Span {
        match self {
            Self::Normal(_, span) => *span,
            Self::Generic(_, span) => *span,
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
pub struct ObjectLiteralField {
    pub span: Span,
    pub name: IsId,
    pub name_span: Span,
    pub colon_span: Span,
    pub value: Expr,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ObjectLiteral {
    pub span: Span,
    pub base: Box<Expr>,
    pub quote_span: Span,
    pub fields: Vec<ObjectLiteralField>,
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
    pub expr: Box<Expr>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct RangeLeftExpr {
    pub span: Span,
    pub expr: Box<Expr>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct RangeBothExpr {
    pub span: Span,
    pub left_expr: Box<Expr>,
    pub op_span: Span,
    pub right_expr: Box<Expr>,
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
    pub ident: IsId,
    pub ident_span: Span,
    pub quote_span: Span,
    pub parameters: Vec<TypeRef>,
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
    pub name: Keyword,
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
    pub expr: Box<Expr>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TupleDef {
    pub span: Span,
    pub items: ExprList,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TupleType {
    pub span: Span,
    pub items: Vec<TypeRef>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypeFieldDef {
    pub span: Span,
    pub name: IsId,
    pub name_span: Span,
    pub colon_span: Span,
    pub r#type: TypeRef,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypeDef {
    pub span: Span,
    pub name: IsId,
    pub name_span: Span,
    pub fields: Vec<TypeFieldDef>,
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
    pub name: IsId,
    pub name_span: Span,
    pub r#type: Option<TypeRef>,
    pub init_expr: Option<Expr>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct UseStatement {
    pub span: Span,
    pub name: Name,
    pub alias: Option<(IsId, Span)>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct WhileStatement {
    pub span: Span,
    pub name: Option<LabelDef>,
    pub loop_expr: Expr,
    pub body: Block,
    pub while_span: Span,
}

// TODO REMOVE these constructors
impl Block {
    pub fn new(span: Span, statements: Vec<Statement>) -> Block { 
        Block{ span, items: statements } 
    }
}
impl SimpleExprStatement {
    pub fn new<T: Into<Expr>>(span: Span, expr: T) -> SimpleExprStatement { 
        SimpleExprStatement{ span, expr: expr.into() } 
    }
}
impl AssignExprStatement {
    pub fn new<T1: Into<Expr>, T2: Into<Expr>>(span: Span, 
        op: Separator, op_span: Span, left_expr: T1, right_expr: T2) -> AssignExprStatement {
        AssignExprStatement{
            left_expr: left_expr.into(),
            right_expr: right_expr.into(),
            span,
            op, op_span,
        }
    }
}
impl FnParam {
    pub fn new(name: impl Into<IsId>, name_span: Span, r#type: TypeRef) -> Self { 
        Self{ span: name_span + r#type.span(), r#type, name: name.into(), name_span } 
    }
}
impl FnDef {
    pub fn new(span: Span, 
        name: impl Into<IsId>, name_span: Span,
        quote_span: Span, params: Vec<FnParam>, 
        ret_type: Option<TypeRef>, body: Block) -> FnDef {
        FnDef{ name: name.into(), name_span, params, quote_span, ret_type, body, span }
    }
}

macro_rules! define_abc {
    ($(#[$attr:meta])* $vis:vis enum $name:ident { $($variant:ident($ty:ident),)+ }) => (
$(#[$attr])*
$vis enum $name {
    $($variant($ty),)+
}

impl $name {
    pub fn span(&self) -> Span {
        match self {
            $( Self::$variant(v) => v.span, )+
        }
    }
}
    )
}

define_abc! {
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Expr {
    Lit(LitExpr),
    Name(Name),
    Paren(ParenExpr),
    Tuple(TupleDef),
    Array(ArrayDef),
    FnCall(FnCallExpr),
    Index(IndexCallExpr),
    Member(MemberAccessExpr),
    Object(ObjectLiteral),
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
