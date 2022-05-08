///! ---------------------------------------------------------------------------------
///! This code is auto generated by a tool $repo/scripts/ast.py
///! Changes may cause incorrect behavior and will be lost if the code is regenerated.
///! ---------------------------------------------------------------------------------

///! span(arena) method with help of WithSpan trait

use super::*;

pub trait WithSpan {
    fn span(self, arena: &Arena) -> Span;
}

// AUTOGEN
impl WithSpan for Expr {
    fn span(self, arena: &Arena) -> Span {
        match self {
            Expr::Lit(n) => arena.get(n).span,
            Expr::Path(n) => arena.get(n).span,
            Expr::Paren(n) => arena.get(n).span,
            Expr::Tuple(n) => arena.get(n).span,
            Expr::Array(n) => arena.get(n).span,
            Expr::Call(n) => arena.get(n).span,
            Expr::ArrayIndex(n) => arena.get(n).span,
            Expr::TupleIndex(n) => arena.get(n).span,
            Expr::Member(n) => arena.get(n).span,
            Expr::Object(n) => arena.get(n).span,
            Expr::Unary(n) => arena.get(n).span,
            Expr::Binary(n) => arena.get(n).span,
            Expr::RangeBoth(n) => arena.get(n).span,
            Expr::RangeFull(n) => arena.get(n).span,
            Expr::RangeLeft(n) => arena.get(n).span,
            Expr::RangeRight(n) => arena.get(n).span,
        }
    }
}

impl WithSpan for Item {
    fn span(self, arena: &Arena) -> Span {
        match self {
            Item::Struct(n) => arena.get(n).span,
            Item::Enum(n) => arena.get(n).span,
            Item::Fn(n) => arena.get(n).span,
            Item::Impl(n) => arena.get(n).span,
            Item::Type(n) => arena.get(n).span,
            Item::Class(n) => arena.get(n).span,
            Item::Block(n) => arena.get(n).span,
            Item::SimpleExpr(n) => arena.get(n).span,
            Item::AssignExpr(n) => arena.get(n).span,
            Item::For(n) => arena.get(n).span,
            Item::If(n) => arena.get(n).span,
            Item::Loop(n) => arena.get(n).span,
            Item::VarDecl(n) => arena.get(n).span,
            Item::While(n) => arena.get(n).span,
            Item::Use(n) => arena.get(n).span,
            Item::Import(n) => arena.get(n).span,
        }
    }
}

impl WithSpan for PathSegment {
    fn span(self, arena: &Arena) -> Span {
        match self {
            PathSegment::Global => Span::new(0, 0),
            PathSegment::Simple(n) => arena.get(n).span,
            PathSegment::Cast(n) => arena.get(n).span,
            PathSegment::Generic(n) => arena.get(n).span,
        }
    }
}

impl WithSpan for Statement {
    fn span(self, arena: &Arena) -> Span {
        match self {
            Statement::Struct(n) => arena.get(n).span,
            Statement::Enum(n) => arena.get(n).span,
            Statement::Fn(n) => arena.get(n).span,
            Statement::Impl(n) => arena.get(n).span,
            Statement::Type(n) => arena.get(n).span,
            Statement::Class(n) => arena.get(n).span,
            Statement::Block(n) => arena.get(n).span,
            Statement::Break(n) => arena.get(n).span,
            Statement::Continue(n) => arena.get(n).span,
            Statement::SimpleExpr(n) => arena.get(n).span,
            Statement::AssignExpr(n) => arena.get(n).span,
            Statement::For(n) => arena.get(n).span,
            Statement::If(n) => arena.get(n).span,
            Statement::Loop(n) => arena.get(n).span,
            Statement::Return(n) => arena.get(n).span,
            Statement::VarDecl(n) => arena.get(n).span,
            Statement::While(n) => arena.get(n).span,
            Statement::Use(n) => arena.get(n).span,
        }
    }
}

impl WithSpan for TypeRef {
    fn span(self, arena: &Arena) -> Span {
        match self {
            TypeRef::Array(n) => arena.get(n).span,
            TypeRef::Fn(n) => arena.get(n).span,
            TypeRef::Path(n) => arena.get(n).span,
            TypeRef::Primitive(n) => arena.get(n).span,
            TypeRef::Ref(n) => arena.get(n).span,
            TypeRef::Tuple(n) => arena.get(n).span,
        }
    }
}
