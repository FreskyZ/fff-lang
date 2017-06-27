///! fff-lang
///!
///! semantic/expr

use codemap::SymbolID;
use lexical::LitValue;
use lexical::Seperator;

use syntax;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ArrayDef {
    pub items: ExprList,
}
impl From<syntax::ArrayDef> for ArrayDef {
    fn from(node: syntax::ArrayDef) -> ArrayDef {
        ArrayDef{
            items: node.items.into(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct BinaryExpr {
    pub left_expr: Box<Expr>,
    pub right_expr: Box<Expr>,
    pub operator: Seperator,
}
impl From<syntax::BinaryExpr> for BinaryExpr {
    fn from(node: syntax::BinaryExpr) -> BinaryExpr {
        BinaryExpr{
            left_expr: Box::new(syntax::Expr::unbox(node.left_expr).into()),
            right_expr: Box::new(syntax::Expr::unbox(node.right_expr).into()),
            operator: node.operator,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ExprList {
    pub items: Vec<Expr>,
}
impl From<syntax::ExprList> for ExprList {
    fn from(node: syntax::ExprList) -> ExprList {
        ExprList{
            items: node.items.into_iter().map(Into::into).collect(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct FnCall {
    pub base: Box<Expr>,
    pub params: ExprList,
}
impl From<syntax::FnCallExpr> for FnCall {
    fn from(node: syntax::FnCallExpr) -> FnCall {
        FnCall{
            base: Box::new(syntax::Expr::unbox(node.base).into()),
            params: node.params.into(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct IdentExpr {
    pub value: SymbolID,
}
impl From<syntax::IdentExpr> for IdentExpr {
    fn from(node: syntax::IdentExpr) -> IdentExpr {
        IdentExpr{
            value: node.value,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct IndexCall {
    pub base: Box<Expr>,
    pub params: ExprList,
}
impl From<syntax::IndexCallExpr> for IndexCall {
    fn from(node: syntax::IndexCallExpr) -> IndexCall {
        IndexCall{
            base: Box::new(syntax::Expr::unbox(node.base).into()),
            params: node.params.into(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct LitExpr {
    pub value: LitValue,
}
impl From<syntax::LitExpr> for LitExpr {
    fn from(node: syntax::LitExpr) -> LitExpr {
        LitExpr{ 
            value: node.value,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct MemberAccess {
    pub base: Box<Expr>,
    pub name: SymbolID,
}
impl From<syntax::MemberAccessExpr> for MemberAccess {
    fn from(node: syntax::MemberAccessExpr) -> MemberAccess {
        MemberAccess{
            base: Box::new(syntax::Expr::unbox(node.base).into()),
            name: node.name.value,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ParenExpr {
    pub expr: Box<Expr>,
}
impl From<syntax::ParenExpr> for ParenExpr {
    fn from(node: syntax::ParenExpr) -> ParenExpr {
        ParenExpr{
            expr: Box::new(syntax::Expr::unbox(node.expr).into()),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct TupleDef {
    pub items: ExprList,
}
impl From<syntax::TupleDef> for TupleDef {
    fn from(node: syntax::TupleDef) -> TupleDef {
        TupleDef{
            items: node.items.into(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct UnaryExpr {
    pub base: Box<Expr>,
    pub operator: Seperator,
}
impl From<syntax::UnaryExpr> for UnaryExpr {
    fn from(node: syntax::UnaryExpr) -> UnaryExpr {
        UnaryExpr{
            base: Box::new(syntax::Expr::unbox(node.base).into()),
            operator: node.operator,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub enum Expr {
    Array(ArrayDef),
    Binary(BinaryExpr),
    FnCall(FnCall),
    Ident(IdentExpr),
    IndexCall(IndexCall),
    Lit(LitExpr),
    MemberAccess(MemberAccess),
    Paren(ParenExpr),
    Tuple(TupleDef),
    Unary(UnaryExpr),
}
impl From<syntax::Expr> for Expr {
    fn from(node: syntax::Expr) -> Expr {
        match node {
            syntax::Expr::Array(array_def) => Expr::Array(array_def.into()),
            syntax::Expr::Binary(binary_expr) => Expr::Binary(binary_expr.into()),
            syntax::Expr::Ident(ident_expr) => Expr::Ident(ident_expr.into()),
            syntax::Expr::Lit(lit_expr) => Expr::Lit(lit_expr.into()),
            syntax::Expr::FnCall(fn_call) => Expr::FnCall(fn_call.into()),
            syntax::Expr::IndexCall(index_call) => Expr::IndexCall(index_call.into()),
            syntax::Expr::MemberAccess(member_access) => Expr::MemberAccess(member_access.into()),
            syntax::Expr::Paren(paren_expr) => Expr::Paren(paren_expr.into()),
            syntax::Expr::Tuple(tuple_def) => Expr::Tuple(tuple_def.into()),
            syntax::Expr::Unary(unary_expr) => Expr::Unary(unary_expr.into()),
        }
    }
}