///! ---------------------------------------------------------------------------------
///! This code is auto generated by a tool $repo/scripts/ast.py
///! Changes may cause incorrect behavior and will be lost if the code is regenerated.
///! ---------------------------------------------------------------------------------

use crate::source::{Span, IsId, IdSpan, FileId};
use crate::lexical::{Separator, Keyword, Numeric};
use crate::common::arena::{Arena, Index, Slice};

#[cfg(test)]
mod cmp;
mod new;
mod span;
mod ugly;
mod visit;
mod noauto;
mod pretty;
mod profile;

// put related methods in one module, because they are short
// and may conflict with other short names if included by ast::*, use like 
// ```
// use ...ast::*;
// ...
// asti::Eq::eq(actual, expect);
// (asti::debug(actual), asti::debug(expect));
// ```
pub mod asti {
    #[cfg(test)]
    pub use super::cmp::Eq;
    pub use super::ugly::debug;
    pub use super::pretty::{display, display1};
    pub use super::profile::MemoryProfiler;
}
pub use span::WithSpan;
// lit value can be regarded as a node type
pub use noauto::LitValue;
// natually extend to Arena when use ast::*
pub use new::EmplaceConcrete;

// AUTOGEN
pub struct ArrayExpr<'a> {
    pub span: Span,
    pub items: Slice<'a, Expr<'a>>,
}

pub struct ArrayIndexExpr<'a> {
    pub span: Span,
    pub base: Expr<'a>,
    pub parameters: Slice<'a, Expr<'a>>,
    pub quote_span: Span,
}

pub struct ArrayType<'a> {
    pub span: Span,
    pub base: TypeRef<'a>,
    pub size: Expr<'a>,
}

pub struct AssignExprStatement<'a> {
    pub span: Span,
    pub left: Expr<'a>,
    pub right: Expr<'a>,
    pub op: Separator,
    pub op_span: Span,
}

pub struct BinaryExpr<'a> {
    pub span: Span,
    pub left: Expr<'a>,
    pub right: Expr<'a>,
    pub op: Separator,
    pub op_span: Span,
}

pub struct Block<'a> {
    pub span: Span,
    pub items: Slice<'a, Statement<'a>>,
}

pub struct BlockStatement<'a> {
    pub span: Span,
    pub label: Option<IdSpan>,
    pub body: Index<'a, Block<'a>>,
}

pub struct BreakStatement {
    pub span: Span,
    pub label: Option<IdSpan>,
}

pub struct CallExpr<'a> {
    pub span: Span,
    pub base: Expr<'a>,
    pub quote_span: Span,
    pub parameters: Slice<'a, Expr<'a>>,
}

pub struct CastSegment<'a> {
    pub span: Span,
    pub left: TypeRef<'a>,
    pub right: TypeRef<'a>,
}

pub struct ClassDef<'a> {
    pub span: Span,
    pub name: Index<'a, GenericName<'a>>,
    pub quote_span: Span,
    pub items: Slice<'a, Item<'a>>,
}

pub struct ContinueStatement {
    pub span: Span,
    pub label: Option<IdSpan>,
}

pub struct ElseClause<'a> {
    pub span: Span,
    pub body: Index<'a, Block<'a>>,
}

pub struct EnumDef<'a> {
    pub span: Span,
    pub name: IdSpan,
    pub base_type: Option<Index<'a, PrimitiveType>>,
    pub quote_span: Span,
    pub variants: Slice<'a, Index<'a, EnumDefVariant<'a>>>,
}

pub struct EnumDefVariant<'a> {
    pub span: Span,
    pub name: IdSpan,
    pub value: Option<Expr<'a>>,
}

pub enum Expr<'a> {
    Lit(Index<'a, LitExpr>),
    Path(Index<'a, Path<'a>>),
    Paren(Index<'a, ParenExpr<'a>>),
    Tuple(Index<'a, TupleExpr<'a>>),
    Array(Index<'a, ArrayExpr<'a>>),
    Call(Index<'a, CallExpr<'a>>),
    ArrayIndex(Index<'a, ArrayIndexExpr<'a>>),
    TupleIndex(Index<'a, TupleIndexExpr<'a>>),
    Member(Index<'a, MemberExpr<'a>>),
    Object(Index<'a, ObjectExpr<'a>>),
    Unary(Index<'a, UnaryExpr<'a>>),
    Binary(Index<'a, BinaryExpr<'a>>),
    RangeBoth(Index<'a, RangeBothExpr<'a>>),
    RangeFull(Index<'a, RangeFullExpr>),
    RangeLeft(Index<'a, RangeLeftExpr<'a>>),
    RangeRight(Index<'a, RangeRightExpr<'a>>),
}

pub struct FieldDef<'a> {
    pub span: Span,
    pub name: IdSpan,
    pub colon_span: Span,
    pub r#type: TypeRef<'a>,
}

pub struct FnDef<'a> {
    pub span: Span,
    pub name: Index<'a, GenericName<'a>>,
    pub quote_span: Span,
    pub parameters: Slice<'a, Index<'a, FnDefParameter<'a>>>,
    pub ret_type: Option<TypeRef<'a>>,
    pub wheres: Slice<'a, Index<'a, WhereClause<'a>>>,
    pub body: Option<Index<'a, Block<'a>>>,
}

pub struct FnDefParameter<'a> {
    pub span: Span,
    pub name: IdSpan,
    pub r#type: TypeRef<'a>,
}

pub struct FnType<'a> {
    pub span: Span,
    pub quote_span: Span,
    pub parameters: Slice<'a, Index<'a, FnTypeParameter<'a>>>,
    pub ret_type: Option<TypeRef<'a>>,
}

pub struct FnTypeParameter<'a> {
    pub span: Span,
    pub name: Option<IdSpan>,
    pub r#type: TypeRef<'a>,
}

pub struct ForStatement<'a> {
    pub span: Span,
    pub label: Option<IdSpan>,
    pub iter_name: IdSpan,
    pub iter_expr: Expr<'a>,
    pub body: Index<'a, Block<'a>>,
}

pub struct GenericName<'a> {
    pub span: Span,
    pub base: IdSpan,
    pub quote_span: Span,
    pub parameters: Slice<'a, Index<'a, GenericParameter>>,
}

pub struct GenericParameter {
    pub span: Span,
    pub name: IdSpan,
}

pub struct GenericSegment<'a> {
    pub span: Span,
    pub base: IdSpan,
    pub parameters: Index<'a, TypeList<'a>>,
}

pub struct IfClause<'a> {
    pub span: Span,
    pub condition: Expr<'a>,
    pub body: Index<'a, Block<'a>>,
}

pub struct IfStatement<'a> {
    pub span: Span,
    pub if_clause: Index<'a, IfClause<'a>>,
    pub elseif_clauses: Slice<'a, Index<'a, IfClause<'a>>>,
    pub else_clause: Option<Index<'a, ElseClause<'a>>>,
}

pub struct Implementation<'a> {
    pub span: Span,
    pub parameters: Slice<'a, Index<'a, GenericParameter>>,
    pub class: Option<TypeRef<'a>>,
    pub r#type: TypeRef<'a>,
    pub wheres: Slice<'a, Index<'a, WhereClause<'a>>>,
    pub quote_span: Span,
    pub items: Slice<'a, Item<'a>>,
}

pub enum Item<'a> {
    Struct(Index<'a, StructDef<'a>>),
    Enum(Index<'a, EnumDef<'a>>),
    Fn(Index<'a, FnDef<'a>>),
    Impl(Index<'a, Implementation<'a>>),
    Type(Index<'a, TypeDef<'a>>),
    Class(Index<'a, ClassDef<'a>>),
    Block(Index<'a, BlockStatement<'a>>),
    SimpleExpr(Index<'a, SimpleExprStatement<'a>>),
    AssignExpr(Index<'a, AssignExprStatement<'a>>),
    For(Index<'a, ForStatement<'a>>),
    If(Index<'a, IfStatement<'a>>),
    Loop(Index<'a, LoopStatement<'a>>),
    VarDecl(Index<'a, VarDeclStatement<'a>>),
    While(Index<'a, WhileStatement<'a>>),
    Use(Index<'a, UseStatement<'a>>),
    Import(Index<'a, ModuleStatement>),
}

pub struct LitExpr {
    pub span: Span,
    pub value: LitValue,
}

pub struct LoopStatement<'a> {
    pub span: Span,
    pub label: Option<IdSpan>,
    pub body: Index<'a, Block<'a>>,
}

pub struct MemberExpr<'a> {
    pub span: Span,
    pub base: Expr<'a>,
    pub op_span: Span,
    pub name: IdSpan,
    pub parameters: Option<Index<'a, TypeList<'a>>>,
}

pub struct Module<'a> {
    pub file: FileId,
    pub items: Slice<'a, Item<'a>>,
}

pub struct ModuleStatement {
    pub span: Span,
    pub name: IdSpan,
    pub path: Option<IdSpan>,
}

pub struct ObjectExpr<'a> {
    pub span: Span,
    pub base: Expr<'a>,
    pub quote_span: Span,
    pub fields: Slice<'a, Index<'a, ObjectExprField<'a>>>,
}

pub struct ObjectExprField<'a> {
    pub span: Span,
    pub name: IdSpan,
    pub value: Expr<'a>,
}

pub struct ParenExpr<'a> {
    pub span: Span,
    pub base: Expr<'a>,
}

pub struct Path<'a> {
    pub span: Span,
    pub segments: Slice<'a, PathSegment<'a>>,
}

pub enum PathSegment<'a> {
    Global,
    Simple(Index<'a, SimpleSegment>),
    Cast(Index<'a, CastSegment<'a>>),
    Generic(Index<'a, GenericSegment<'a>>),
}

pub struct PrimitiveType {
    pub span: Span,
    pub base: Keyword,
}

pub struct RangeBothExpr<'a> {
    pub span: Span,
    pub left: Expr<'a>,
    pub op_span: Span,
    pub right: Expr<'a>,
}

pub struct RangeFullExpr {
    pub span: Span,
}

pub struct RangeLeftExpr<'a> {
    pub span: Span,
    pub base: Expr<'a>,
}

pub struct RangeRightExpr<'a> {
    pub span: Span,
    pub base: Expr<'a>,
}

pub struct RefType<'a> {
    pub span: Span,
    pub base: TypeRef<'a>,
}

pub struct ReturnStatement<'a> {
    pub span: Span,
    pub value: Option<Expr<'a>>,
}

pub struct SimpleExprStatement<'a> {
    pub span: Span,
    pub expr: Expr<'a>,
}

pub struct SimpleSegment {
    pub span: Span,
    pub name: IsId,
}

pub enum Statement<'a> {
    Struct(Index<'a, StructDef<'a>>),
    Enum(Index<'a, EnumDef<'a>>),
    Fn(Index<'a, FnDef<'a>>),
    Impl(Index<'a, Implementation<'a>>),
    Type(Index<'a, TypeDef<'a>>),
    Class(Index<'a, ClassDef<'a>>),
    Block(Index<'a, BlockStatement<'a>>),
    Break(Index<'a, BreakStatement>),
    Continue(Index<'a, ContinueStatement>),
    SimpleExpr(Index<'a, SimpleExprStatement<'a>>),
    AssignExpr(Index<'a, AssignExprStatement<'a>>),
    For(Index<'a, ForStatement<'a>>),
    If(Index<'a, IfStatement<'a>>),
    Loop(Index<'a, LoopStatement<'a>>),
    Return(Index<'a, ReturnStatement<'a>>),
    VarDecl(Index<'a, VarDeclStatement<'a>>),
    While(Index<'a, WhileStatement<'a>>),
    Use(Index<'a, UseStatement<'a>>),
}

pub struct StructDef<'a> {
    pub span: Span,
    pub name: Index<'a, GenericName<'a>>,
    pub fields: Slice<'a, Index<'a, FieldDef<'a>>>,
}

pub struct TupleExpr<'a> {
    pub span: Span,
    pub items: Slice<'a, Expr<'a>>,
}

pub struct TupleIndexExpr<'a> {
    pub span: Span,
    pub base: Expr<'a>,
    pub op_span: Span,
    pub value: i32,
    pub value_span: Span,
}

pub struct TupleType<'a> {
    pub span: Span,
    pub parameters: Slice<'a, TypeRef<'a>>,
}

pub struct TypeDef<'a> {
    pub span: Span,
    pub name: Index<'a, GenericName<'a>>,
    pub from: Option<TypeRef<'a>>,
}

pub struct TypeList<'a> {
    pub span: Span,
    pub items: Slice<'a, TypeRef<'a>>,
}

pub enum TypeRef<'a> {
    Array(Index<'a, ArrayType<'a>>),
    Fn(Index<'a, FnType<'a>>),
    Path(Index<'a, Path<'a>>),
    Primitive(Index<'a, PrimitiveType>),
    Ref(Index<'a, RefType<'a>>),
    Tuple(Index<'a, TupleType<'a>>),
}

pub struct UnaryExpr<'a> {
    pub span: Span,
    pub base: Expr<'a>,
    pub op: Separator,
    pub op_span: Span,
}

pub struct UseStatement<'a> {
    pub span: Span,
    pub path: Index<'a, Path<'a>>,
    pub alias: Option<IdSpan>,
}

pub struct VarDeclStatement<'a> {
    pub span: Span,
    pub r#const: bool,
    pub name: IdSpan,
    pub r#type: Option<TypeRef<'a>>,
    pub init_value: Option<Expr<'a>>,
}

pub struct WhereClause<'a> {
    pub span: Span,
    pub name: IdSpan,
    pub constraints: Slice<'a, TypeRef<'a>>,
}

pub struct WhileStatement<'a> {
    pub span: Span,
    pub label: Option<IdSpan>,
    pub condition: Expr<'a>,
    pub body: Index<'a, Block<'a>>,
}
