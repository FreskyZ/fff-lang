///! fff-lang
///!
///! semantic/expr

use codemap::SymbolID;
use lexical::LitValue;
use lexical::Seperator;

use syntax;

mod range_expr;

use super::SharedDefScope;
use super::ISemanticAnalyze;

pub use self::range_expr::RangeBothExpr;
pub use self::range_expr::RangeFullExpr;
pub use self::range_expr::RangeLeftExpr;
pub use self::range_expr::RangeRightExpr;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ArrayDef {
    pub items: ExprList,
}
impl ISemanticAnalyze for ArrayDef {

    type SyntaxItem = syntax::ArrayDef;

    fn from_syntax(node: syntax::ArrayDef, parent_scope: SharedDefScope) -> ArrayDef {
        ArrayDef{
            items: ExprList::from_syntax(node.items, parent_scope.clone()),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct BinaryExpr {
    pub left_expr: Box<Expr>,
    pub right_expr: Box<Expr>,
    pub operator: Seperator,
}
impl ISemanticAnalyze for BinaryExpr {

    type SyntaxItem = syntax::BinaryExpr;

    fn from_syntax(node: syntax::BinaryExpr, parent_scope: SharedDefScope) -> BinaryExpr {
        BinaryExpr{
            left_expr: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.left_expr), parent_scope.clone())),
            right_expr: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.right_expr), parent_scope.clone())),
            operator: node.operator,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ExprList {
    pub items: Vec<Expr>,
}
impl ISemanticAnalyze for ExprList {

    type SyntaxItem = syntax::ExprList;

    fn from_syntax(node: syntax::ExprList, parent_scope: SharedDefScope) -> ExprList {
        ExprList{
            items: node.items.into_iter().map(|item| Expr::from_syntax(item, parent_scope.clone())).collect(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct FnCall {
    pub base: Box<Expr>,
    pub params: ExprList,
}
impl ISemanticAnalyze for FnCall {

    type SyntaxItem = syntax::FnCallExpr;

    fn from_syntax(node: syntax::FnCallExpr, parent_scope: SharedDefScope) -> FnCall {
        FnCall{
            base: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.base), parent_scope.clone())),
            params: ExprList::from_syntax(node.params, parent_scope.clone()),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct SimpleName {
    pub value: SymbolID,
}
impl ISemanticAnalyze for SimpleName {

    type SyntaxItem = syntax::SimpleName;

    fn from_syntax(node: syntax::SimpleName, _parent_scope: SharedDefScope) -> SimpleName {
        SimpleName{
            value: node.value,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct Name {
    pub segments: Vec<SimpleName>,
}
impl ISemanticAnalyze for Name {

    type SyntaxItem = syntax::Name;

    fn from_syntax(node: syntax::Name, parent_scope: SharedDefScope) -> Name {
        Name{
            segments: node.segments.into_iter().map(|segment| SimpleName::from_syntax(segment, parent_scope.clone())).collect(),
        }
    } 
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct IndexCall {
    pub base: Box<Expr>,
    pub params: ExprList,
}
impl ISemanticAnalyze for IndexCall {

    type SyntaxItem = syntax::IndexCallExpr;

    fn from_syntax(node: syntax::IndexCallExpr, parent_scope: SharedDefScope) -> IndexCall {
        IndexCall{
            base: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.base), parent_scope.clone())),
            params: ExprList::from_syntax(node.params, parent_scope.clone()),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct LitExpr {
    pub value: LitValue,
}
impl ISemanticAnalyze for LitExpr {

    type SyntaxItem = syntax::LitExpr;

    fn from_syntax(node: syntax::LitExpr, _parent_scope: SharedDefScope) -> LitExpr {
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
impl ISemanticAnalyze for MemberAccess {

    type SyntaxItem = syntax::MemberAccessExpr;

    fn from_syntax(node: syntax::MemberAccessExpr, parent_scope: SharedDefScope) -> MemberAccess {
        MemberAccess{
            base: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.base), parent_scope.clone())),
            name: node.name.value,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ParenExpr {
    pub expr: Box<Expr>,
}
impl ISemanticAnalyze for ParenExpr {

    type SyntaxItem = syntax::ParenExpr;

    fn from_syntax(node: syntax::ParenExpr, parent_scope: SharedDefScope) -> ParenExpr {
        ParenExpr{
            expr: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.expr), parent_scope.clone())),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct TupleDef {
    pub items: ExprList,
}
impl ISemanticAnalyze for TupleDef {

    type SyntaxItem = syntax::TupleDef;

    fn from_syntax(node: syntax::TupleDef, parent_scope: SharedDefScope) -> TupleDef {
        TupleDef{
            items: ExprList::from_syntax(node.items, parent_scope.clone()),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct UnaryExpr {
    pub base: Box<Expr>,
    pub operator: Seperator,
}
impl ISemanticAnalyze for UnaryExpr {

    type SyntaxItem = syntax::UnaryExpr;

    fn from_syntax(node: syntax::UnaryExpr, parent_scope: SharedDefScope) -> UnaryExpr {
        UnaryExpr{
            base: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.base), parent_scope.clone())),
            operator: node.operator,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub enum Expr {
    Array(ArrayDef),
    Binary(BinaryExpr),
    FnCall(FnCall),
    Name(Name),
    SimpleName(SimpleName),
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
impl ISemanticAnalyze for Expr {

    type SyntaxItem = syntax::Expr;

    fn from_syntax(node: syntax::Expr, parent_scope: SharedDefScope) -> Expr {
        match node {
            syntax::Expr::Array(array_def) => Expr::Array(ArrayDef::from_syntax(array_def, parent_scope)),
            syntax::Expr::Binary(binary_expr) => Expr::Binary(BinaryExpr::from_syntax(binary_expr, parent_scope)),
            syntax::Expr::Name(name) => Expr::Name(Name::from_syntax(name, parent_scope)),
            syntax::Expr::Lit(lit_expr) => Expr::Lit(LitExpr::from_syntax(lit_expr, parent_scope)),
            syntax::Expr::FnCall(fn_call) => Expr::FnCall(FnCall::from_syntax(fn_call, parent_scope)),
            syntax::Expr::IndexCall(index_call) => Expr::IndexCall(IndexCall::from_syntax(index_call, parent_scope)),
            syntax::Expr::MemberAccess(member_access) => Expr::MemberAccess(MemberAccess::from_syntax(member_access, parent_scope)),
            syntax::Expr::Paren(paren_expr) => Expr::Paren(ParenExpr::from_syntax(paren_expr, parent_scope)),
            syntax::Expr::Tuple(tuple_def) => Expr::Tuple(TupleDef::from_syntax(tuple_def, parent_scope)),
            syntax::Expr::Unary(unary_expr) => Expr::Unary(UnaryExpr::from_syntax(unary_expr, parent_scope)),
            syntax::Expr::RangeBoth(range_both) => Expr::RangeBoth(RangeBothExpr::from_syntax(range_both, parent_scope)),
            syntax::Expr::RangeLeft(range_left) => Expr::RangeLeft(RangeLeftExpr::from_syntax(range_left, parent_scope)),
            syntax::Expr::RangeRight(range_right) => Expr::RangeRight(RangeRightExpr::from_syntax(range_right, parent_scope)),
            syntax::Expr::RangeFull(range_full) => Expr::RangeFull(RangeFullExpr::from_syntax(range_full, parent_scope)),
            syntax::Expr::SimpleName(simple_name) => Expr::SimpleName(SimpleName::from_syntax(simple_name, parent_scope)),
        }
    }
}