///! fff-lang
///!
///! semantic/expr

use codemap::SymbolID;
use lexical::LitValue;
use lexical::Seperator;

use syntax;

use super::FromSyntax;
use super::SharedDefScope;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ArrayDef {
    pub items: ExprList,
}
impl FromSyntax<syntax::ArrayDef> for ArrayDef {
    fn from_syntax(node: syntax::ArrayDef, parent_scope: SharedDefScope) -> ArrayDef {
        ArrayDef{
            items: FromSyntax::from_syntax(node.items, parent_scope.clone()),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct BinaryExpr {
    pub left_expr: Box<Expr>,
    pub right_expr: Box<Expr>,
    pub operator: Seperator,
}
impl FromSyntax<syntax::BinaryExpr> for BinaryExpr {
    fn from_syntax(node: syntax::BinaryExpr, parent_scope: SharedDefScope) -> BinaryExpr {
        BinaryExpr{
            left_expr: Box::new(FromSyntax::from_syntax(syntax::Expr::unbox(node.left_expr), parent_scope.clone())),
            right_expr: Box::new(FromSyntax::from_syntax(syntax::Expr::unbox(node.right_expr), parent_scope.clone())),
            operator: node.operator,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ExprList {
    pub items: Vec<Expr>,
}
impl FromSyntax<syntax::ExprList> for ExprList {
    fn from_syntax(node: syntax::ExprList, parent_scope: SharedDefScope) -> ExprList {
        ExprList{
            items: node.items.into_iter().map(|item| FromSyntax::from_syntax(item, parent_scope.clone())).collect(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct FnCall {
    pub base: Box<Expr>,
    pub params: ExprList,
}
impl FromSyntax<syntax::FnCallExpr> for FnCall {
    fn from_syntax(node: syntax::FnCallExpr, parent_scope: SharedDefScope) -> FnCall {
        FnCall{
            base: Box::new(FromSyntax::from_syntax(syntax::Expr::unbox(node.base), parent_scope.clone())),
            params: FromSyntax::from_syntax(node.params, parent_scope.clone()),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct IdentExpr {
    pub value: SymbolID,
}
impl FromSyntax<syntax::IdentExpr> for IdentExpr {
    fn from_syntax(node: syntax::IdentExpr, parent_scope: SharedDefScope) -> IdentExpr {
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
impl FromSyntax<syntax::IndexCallExpr> for IndexCall {
    fn from_syntax(node: syntax::IndexCallExpr, parent_scope: SharedDefScope) -> IndexCall {
        IndexCall{
            base: Box::new(FromSyntax::from_syntax(syntax::Expr::unbox(node.base), parent_scope.clone())),
            params: FromSyntax::from_syntax(node.params, parent_scope.clone()),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct LitExpr {
    pub value: LitValue,
}
impl FromSyntax<syntax::LitExpr> for LitExpr {
    fn from_syntax(node: syntax::LitExpr, parent_scope: SharedDefScope) -> LitExpr {
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
impl FromSyntax<syntax::MemberAccessExpr> for MemberAccess {
    fn from_syntax(node: syntax::MemberAccessExpr, parent_scope: SharedDefScope) -> MemberAccess {
        MemberAccess{
            base: Box::new(FromSyntax::from_syntax(syntax::Expr::unbox(node.base), parent_scope.clone())),
            name: node.name.value,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ParenExpr {
    pub expr: Box<Expr>,
}
impl FromSyntax<syntax::ParenExpr> for ParenExpr {
    fn from_syntax(node: syntax::ParenExpr, parent_scope: SharedDefScope) -> ParenExpr {
        ParenExpr{
            expr: Box::new(FromSyntax::from_syntax(syntax::Expr::unbox(node.expr), parent_scope.clone())),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct TupleDef {
    pub items: ExprList,
}
impl FromSyntax<syntax::TupleDef> for TupleDef {
    fn from_syntax(node: syntax::TupleDef, parent_scope: SharedDefScope) -> TupleDef {
        TupleDef{
            items: FromSyntax::from_syntax(node.items, parent_scope.clone()),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct UnaryExpr {
    pub base: Box<Expr>,
    pub operator: Seperator,
}
impl FromSyntax<syntax::UnaryExpr> for UnaryExpr {
    fn from_syntax(node: syntax::UnaryExpr, parent_scope: SharedDefScope) -> UnaryExpr {
        UnaryExpr{
            base: Box::new(FromSyntax::from_syntax(syntax::Expr::unbox(node.base), parent_scope.clone())),
            operator: node.operator,
        }
    }
}

use std::marker::PhantomData;
#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct RangeFullExpr {
    pub phantom: PhantomData<()>,
}
impl FromSyntax<syntax::RangeFullExpr> for RangeFullExpr {
    fn from_syntax(_node: syntax::RangeFullExpr, parent_scope: SharedDefScope) -> RangeFullExpr {
        RangeFullExpr{
            phantom: PhantomData,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct RangeLeftExpr {
    pub expr: Box<Expr>,
}
impl FromSyntax<syntax::RangeLeftExpr> for RangeLeftExpr {
    fn from_syntax(node: syntax::RangeLeftExpr, parent_scope: SharedDefScope) -> RangeLeftExpr {
        RangeLeftExpr{
            expr: Box::new(FromSyntax::from_syntax(syntax::Expr::unbox(node.expr), parent_scope.clone())),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct RangeRightExpr {
    pub expr: Box<Expr>,
}
impl FromSyntax<syntax::RangeRightExpr> for RangeRightExpr {
    fn from_syntax(node: syntax::RangeRightExpr, parent_scope: SharedDefScope) -> RangeRightExpr {
        RangeRightExpr{
            expr: Box::new(FromSyntax::from_syntax(syntax::Expr::unbox(node.expr), parent_scope.clone())),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct RangeBothExpr {
    pub left_expr: Box<Expr>,
    pub right_expr: Box<Expr>,
}
impl FromSyntax<syntax::RangeBothExpr> for RangeBothExpr {
    fn from_syntax(node: syntax::RangeBothExpr, parent_scope: SharedDefScope) -> RangeBothExpr {
        RangeBothExpr{
            left_expr: Box::new(FromSyntax::from_syntax(syntax::Expr::unbox(node.left_expr), parent_scope.clone())),
            right_expr: Box::new(FromSyntax::from_syntax(syntax::Expr::unbox(node.right_expr), parent_scope.clone())),
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
    RangeFull(RangeFullExpr),
    RangeLeft(RangeLeftExpr),
    RangeRight(RangeRightExpr),
    RangeBoth(RangeBothExpr),
}
impl FromSyntax<syntax::Expr> for Expr {
    fn from_syntax(node: syntax::Expr, parent_scope: SharedDefScope) -> Expr {
        match node {
            syntax::Expr::Array(array_def) => Expr::Array(FromSyntax::from_syntax(array_def, parent_scope)),
            syntax::Expr::Binary(binary_expr) => Expr::Binary(FromSyntax::from_syntax(binary_expr, parent_scope)),
            syntax::Expr::Ident(ident_expr) => Expr::Ident(FromSyntax::from_syntax(ident_expr, parent_scope)),
            syntax::Expr::Lit(lit_expr) => Expr::Lit(FromSyntax::from_syntax(lit_expr, parent_scope)),
            syntax::Expr::FnCall(fn_call) => Expr::FnCall(FromSyntax::from_syntax(fn_call, parent_scope)),
            syntax::Expr::IndexCall(index_call) => Expr::IndexCall(FromSyntax::from_syntax(index_call, parent_scope)),
            syntax::Expr::MemberAccess(member_access) => Expr::MemberAccess(FromSyntax::from_syntax(member_access, parent_scope)),
            syntax::Expr::Paren(paren_expr) => Expr::Paren(FromSyntax::from_syntax(paren_expr, parent_scope)),
            syntax::Expr::Tuple(tuple_def) => Expr::Tuple(FromSyntax::from_syntax(tuple_def, parent_scope)),
            syntax::Expr::Unary(unary_expr) => Expr::Unary(FromSyntax::from_syntax(unary_expr, parent_scope)),
            syntax::Expr::RangeBoth(range_both) => Expr::RangeBoth(FromSyntax::from_syntax(range_both, parent_scope)),
            syntax::Expr::RangeLeft(range_left) => Expr::RangeLeft(FromSyntax::from_syntax(range_left, parent_scope)),
            syntax::Expr::RangeRight(range_right) => Expr::RangeRight(FromSyntax::from_syntax(range_right, parent_scope)),
            syntax::Expr::RangeFull(range_full) => Expr::RangeFull(FromSyntax::from_syntax(range_full, parent_scope)),
        }
    }
}