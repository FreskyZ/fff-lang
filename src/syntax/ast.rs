
use crate::source::{Span, IsId, FileId};
use crate::lexical::{Separator, Keyword, Numeric};

// TODO reverse derive PartialEq and Debug by find and replace
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ArrayDef {
    pub items: ExprList,
    pub bracket_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ArrayType {
    pub base: Box<TypeRef>,
    pub size: Expr,
    pub span: Span, // all span
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct BinaryExpr {
    pub left_expr: Box<Expr>,
    pub right_expr: Box<Expr>,
    pub operator: Separator,            // this means every binary operator matches a binary expr
    pub operator_span: Span,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct BlockStatement {
    pub name: Option<LabelDef>,
    pub body: Block,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Block {
    pub items: Vec<Statement>,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct EnumVariant {
    pub name: IsId,
    pub name_span: Span,
    pub value: Option<Expr>,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct EnumDef {
    pub name: IsId,
    pub name_span: Span,
    pub base_type: Option<PrimitiveType>,
    pub quote_span: Span,
    pub variants: Vec<EnumVariant>,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ExprList {
    pub items: Vec<Expr>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct SimpleExprStatement {
    pub expr: Expr, 
    pub all_span: Span,  // this span = expr.all_span + semicolon_span
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct AssignExprStatement {
    pub left_expr: Expr,
    pub right_expr: Expr,
    pub assign_op: Separator,
    pub assign_op_span: Span,
    pub all_span: Span,
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
    pub base: Box<Expr>,
    pub params: ExprList,
    pub paren_span: Span,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FnParam {
    pub name: IsId,
    pub name_span: Span,
    pub r#type: TypeRef,
}
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FnDef {
    pub name: IsId,
    pub name_span: Span,
    pub params: Vec<FnParam>,
    pub params_paren_span: Span,
    pub ret_type: Option<TypeRef>,
    pub body: Block,
    pub all_span: Span,   // fn_span = all_span.slice(0..2)
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FnTypeParam {
    pub name: Option<(IsId, Span)>,
    pub r#type: TypeRef,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FnType {
    pub paren_span: Span,
    pub parameters: Vec<FnTypeParam>,
    pub ret_type: Option<Box<TypeRef>>,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ForStatement {
    pub loop_name: Option<LabelDef>,
    pub for_span: Span,
    pub iter_name: IsId,
    pub iter_span: Span,
    pub iter_expr: Expr,
    pub body: Block,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct IfClause {
    pub condition: Expr,
    pub body: Block,
    pub all_span: Span, // start from `if` or `else`
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ElseClause {
    pub body: Block,
    pub all_span: Span, // else_span = all_span.slice(0..3)
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct IfStatement {
    pub if_clause: IfClause,
    pub elseif_clauses: Vec<IfClause>,
    pub else_clause: Option<ElseClause>,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct IndexCallExpr {
    pub base: Box<Expr>,
    pub params: ExprList,
    pub bracket_span: Span,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct JumpStatement {
    pub target: Option<(IsId, Span)>,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ContinueStatement(pub JumpStatement);

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct BreakStatement(pub JumpStatement);

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct LabelDef {
    pub name: IsId,
    pub all_span: Span,
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
    pub value: LitValue,
    pub span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct LoopStatement {
    pub name: Option<LabelDef>,
    pub body: Block,
    pub loop_span: Span,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct MemberAccessExpr {
    pub base: Box<Expr>,
    pub dot_span: Span,
    pub name: Name,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ModuleStatement {
    pub name: IsId,
    pub name_span: Span,
    pub path: Option<(IsId, Span)>,
    pub all_span: Span,
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
    pub fn get_span(&self) -> Span {
        match self {
            Self::Normal(_, span) => *span,
            Self::Generic(_, span) => *span,
        }
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Name {
    pub type_as_segment: Option<TypeAsSegment>,
    pub global: bool,
    pub segments: Vec<NameSegment>,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ObjectLiteralField {
    pub name: IsId,
    pub name_span: Span,
    pub colon_span: Span,
    pub value: Expr,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ObjectLiteral {
    pub base: Box<Expr>,
    pub quote_span: Span,
    pub fields: Vec<ObjectLiteralField>,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct RangeFullExpr {
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct RangeRightExpr {
    pub all_span: Span,  // all_span.slice(2) is range_op_span
    pub expr: Box<Expr>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct RangeLeftExpr {
    pub expr: Box<Expr>,
    pub all_span: Span, // all_span.slice(-2, 0) should get range_op_span
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct RangeBothExpr {
    pub left_expr: Box<Expr>,
    pub op_span: Span,
    pub right_expr: Box<Expr>,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypeAsSegment {
    pub from: Box<TypeRef>,
    pub to: Box<TypeRef>,
    pub span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypeSegment {
    pub ident: IsId,
    pub ident_span: Span,
    pub quote_span: Span,
    pub parameters: Vec<TypeRef>,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct PlainType {
    pub type_as_segment: Option<TypeAsSegment>,
    pub global: bool,
    pub segments: Vec<TypeSegment>,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct PrimitiveType {
    pub name: Keyword,
    pub span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct RefType {
    pub base: Box<TypeRef>,
    pub span: Span, // all span
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ReturnStatement {
    pub expr: Option<Expr>,
    pub all_span: Span,
}

// Paren expr is a side effect of TupleDef
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ParenExpr {
    pub expr: Box<Expr>,
    pub span: Span,  // paren_span also all_span
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TupleDef {
    pub items: ExprList,
    pub paren_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TupleType {
    pub items: Vec<TypeRef>,
    pub span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypeFieldDef {
    pub name: IsId,
    pub name_span: Span,
    pub colon_span: Span,
    pub r#type: TypeRef,
    pub all_span: Span,   // ident to TypeRef or ident to comma
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypeDef {
    pub all_span: Span,
    pub name: IsId,
    pub name_span: Span,
    pub fields: Vec<TypeFieldDef>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct UnaryExpr {
    pub base: Box<Expr>, 
    pub operator: Separator, 
    pub operator_span: Span,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct VarDeclStatement {
    pub is_const: bool,
    pub name: IsId,
    pub name_span: Span,
    pub r#type: Option<TypeRef>,
    pub init_expr: Option<Expr>,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct UseStatement {
    pub name: Name,
    pub alias: Option<(IsId, Span)>,
    pub all_span: Span,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct WhileStatement {
    pub name: Option<LabelDef>,
    pub loop_expr: Expr,
    pub body: Block,
    pub while_span: Span,
    pub all_span: Span,
}

// TODO REMOVE these constructors
impl Block {
    pub fn new(all_span: Span, statements: Vec<Statement>) -> Block { 
        Block{ all_span, items: statements } 
    }
}
impl SimpleExprStatement {
    pub fn new<T: Into<Expr>>(all_span: Span, expr: T) -> SimpleExprStatement { 
        SimpleExprStatement{ all_span, expr: expr.into() } 
    }
}
impl AssignExprStatement {
    pub fn new<T1: Into<Expr>, T2: Into<Expr>>(all_span: Span, 
        assign_op: Separator, assign_op_span: Span, left_expr: T1, right_expr: T2) -> AssignExprStatement {
        AssignExprStatement{
            left_expr: left_expr.into(),
            right_expr: right_expr.into(),
            all_span,
            assign_op, assign_op_span,
        }
    }
}
impl FnParam {
    pub fn new(name: impl Into<IsId>, name_span: Span, r#type: TypeRef) -> Self { 
        Self{ r#type, name: name.into(), name_span } 
    }
}
impl FnDef {
    pub fn new(all_span: Span, 
        name: impl Into<IsId>, name_span: Span,
        params_paren_span: Span, params: Vec<FnParam>, 
        ret_type: Option<TypeRef>, body: Block) -> FnDef {
        FnDef{ name: name.into(), name_span, params, params_paren_span, ret_type, body, all_span }
    }
}

impl LabelDef {
    pub fn new(name: impl Into<IsId>, all_span: Span) -> LabelDef { 
        LabelDef{ name: name.into(), all_span } 
    }
}

impl ReturnStatement {

    pub fn new_unit(all_span: Span) -> ReturnStatement {
        ReturnStatement{ all_span, expr: None }
    }
    pub fn new_expr(all_span: Span, expr: Expr) -> ReturnStatement {
        ReturnStatement{ all_span, expr: Some(expr) }
    }
}

macro_rules! define_expr {
    ($($ty:ty => $variant:ident, $visit:ident, $span:ident,)+) => (
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Expr {
$(
    $variant($ty),
)+
}

$( impl From<$ty> for Expr {
    fn from(s: $ty) -> Expr { Expr::$variant(s) }
} )+

impl Expr {
    pub fn get_all_span(&self) -> Span {
        match self {
            $(
                Expr::$variant(e) => e.$span,
            )+
        }
    }
}
    )
}

define_expr! {
    LitExpr => Lit, visit_lit_expr, span,
    Name => Name, visit_name, all_span,
    ParenExpr => Paren, visit_paren_expr, span,
    TupleDef => Tuple, visit_tuple_def, paren_span,
    ArrayDef => Array, visit_array_def, bracket_span,
    FnCallExpr => FnCall, visit_fn_call_expr, all_span,
    IndexCallExpr => IndexCall, visit_index_call_expr, all_span,
    MemberAccessExpr => MemberAccess, visit_member_access, all_span,
    ObjectLiteral => Object, visit_object_literal, all_span,
    UnaryExpr => Unary, visit_unary_expr, all_span,
    BinaryExpr => Binary, visit_binary_expr, all_span,
    RangeBothExpr => RangeBoth, visit_range_both_expr, all_span,
    RangeFullExpr => RangeFull, visit_range_full_expr, all_span,
    RangeLeftExpr => RangeLeft, visit_range_left_expr, all_span,
    RangeRightExpr => RangeRight, visit_range_right_expr, all_span,
}

// all variants has same priority
// nearly all variants are N == <N as Node>::ParseOutput
// nearly all variants have different first terminal symbol
// exception is simple-expr-stmt and assign-expr-stmt, simple-expr-stmt forwards all implementation to assign-expr-stmt
//
// expr is not here because expressions has complex priority, and a lot of N != <N as Node>::ParseOutput for that

macro_rules! define_abc {
    ($name:ident, $desc:literal, $visit_this:ident, $($subty:ty => $variant:ident, $visit:ident,)+) => (
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum $name {
    $($variant($subty),)+
}

$( impl From<$subty> for $name {
    fn from(s: $subty) -> $name { $name::$variant(s) }
} )+
    );
}

define_abc!{ Statement, "statement", visit_stmt,
    TypeDef => Type, visit_type_def,
    EnumDef => Enum, visit_enum_def,
    FnDef => Fn, visit_fn_def,
    BlockStatement => Block, visit_block_stmt,
    BreakStatement => Break, visit_break_stmt,
    ContinueStatement => Continue, visit_continue_stmt,
    SimpleExprStatement => SimpleExpr, visit_simple_expr_stmt,
    AssignExprStatement => AssignExpr, visit_assign_expr_stmt,
    ForStatement => For, visit_for_stmt,
    IfStatement => If, visit_if_stmt,
    LoopStatement => Loop, visit_loop_stmt,
    ReturnStatement => Return, visit_ret_stmt,
    VarDeclStatement => VarDecl, visit_var_decl,
    WhileStatement => While, visit_while_stmt,
    UseStatement => Use, visit_use_stmt,
}

// global item
define_abc!{ Item, "item", visit_item, 
    TypeDef => Type, visit_type_def,
    EnumDef => Enum, visit_enum_def,
    FnDef => Fn, visit_fn_def,
    BlockStatement => Block, visit_block_stmt,
    SimpleExprStatement => SimpleExpr, visit_simple_expr_stmt,
    AssignExprStatement => AssignExpr, visit_assign_expr_stmt,
    ForStatement => For, visit_for_stmt,
    IfStatement => If, visit_if_stmt,
    LoopStatement => Loop, visit_loop_stmt,
    VarDeclStatement => VarDecl, visit_var_decl,
    WhileStatement => While, visit_while_stmt,
    UseStatement => Use, visit_use_stmt,
    ModuleStatement => Import, visit_module_stmt,
}

define_abc!{ TypeRef, "type ref", visit_type_ref,
    PrimitiveType => Primitive, visit_primitive_type,
    ArrayType => Array, visit_array_type,
    FnType => Fn, visit_fn_type,
    RefType => Ref, visit_ref_type,
    TupleType => Tuple, visit_tuple_type,
    PlainType => Plain, visit_plain_type,
}

impl TypeRef {
    pub fn get_all_span(&self) -> Span {
        match self {
            Self::Primitive(t) => t.span,
            Self::Array(t) => t.span,
            Self::Fn(t) => t.all_span,
            Self::Ref(t) => t.span,
            Self::Tuple(t) => t.span,
            Self::Plain(t) => t.all_span,
        }
    }
}