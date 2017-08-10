///! fff-lang
///!
///! semantic/expr

use codemap::SymbolID;
use codemap::SymbolCollection;
use lexical::LitValue;
use lexical::Seperator;

use syntax;

mod range_expr;

use super::SharedDefScope;
use super::ISemanticAnalyze;
use super::Formatter;

pub use self::range_expr::RangeBothExpr;
pub use self::range_expr::RangeFullExpr;
pub use self::range_expr::RangeLeftExpr;
pub use self::range_expr::RangeRightExpr;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ArrayDef {
    pub items: Vec<Expr>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for ArrayDef {

    type SyntaxItem = syntax::ArrayDef;

    fn from_syntax(node: syntax::ArrayDef, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> ArrayDef {
        ArrayDef{
            items: node.items.items.into_iter().map(|item| Expr::from_syntax(item, parent_scope.clone(), symbols)).collect(),
            parent_scope: parent_scope,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct BinaryExpr {
    pub left_expr: Box<Expr>,
    pub right_expr: Box<Expr>,
    pub operator: Seperator,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for BinaryExpr {

    type SyntaxItem = syntax::BinaryExpr;

    fn from_syntax(node: syntax::BinaryExpr, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> BinaryExpr {
        BinaryExpr{
            left_expr: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.left_expr), parent_scope.clone(), symbols)),
            right_expr: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.right_expr), parent_scope.clone(), symbols)),
            operator: node.operator,
            parent_scope: parent_scope,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct FnCall {
    pub base: Box<Expr>,
    pub params: Vec<Expr>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for FnCall {

    type SyntaxItem = syntax::FnCallExpr;

    fn from_syntax(node: syntax::FnCallExpr, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> FnCall {
        FnCall{
            base: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.base), parent_scope.clone(), symbols)),
            params: node.params.items.into_iter().map(|item| Expr::from_syntax(item, parent_scope.clone(), symbols)).collect(),
            parent_scope: parent_scope,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct SimpleName {
    pub value: SymbolID,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for SimpleName {

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("simple-name").space().sym(self.value).space().debug(&self.parent_scope).finish()
    }

    type SyntaxItem = syntax::SimpleName;

    fn from_syntax(node: syntax::SimpleName, parent_scope: SharedDefScope, _symbols: &mut SymbolCollection) -> SimpleName {
        SimpleName{
            value: node.value,
            parent_scope: parent_scope,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct Name {
    pub segments: Vec<SimpleName>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for Name {

    type SyntaxItem = syntax::Name;

    fn from_syntax(node: syntax::Name, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> Name {
        Name{
            segments: node.segments.into_iter().map(|segment| SimpleName::from_syntax(segment, parent_scope.clone(), symbols)).collect(),
            parent_scope: parent_scope,
        }
    } 
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct IndexCall {
    pub base: Box<Expr>,
    pub params: Vec<Expr>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for IndexCall {

    type SyntaxItem = syntax::IndexCallExpr;

    fn from_syntax(node: syntax::IndexCallExpr, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> IndexCall {
        IndexCall{
            base: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.base), parent_scope.clone(), symbols)),
            params: node.params.items.into_iter().map(|item| Expr::from_syntax(item, parent_scope.clone(), symbols)).collect(),
            parent_scope: parent_scope,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct LitExpr {
    pub value: LitValue,
}
impl ISemanticAnalyze for LitExpr {

    type SyntaxItem = syntax::LitExpr;

    fn from_syntax(node: syntax::LitExpr, _parent_scope: SharedDefScope, _symbols: &mut SymbolCollection) -> LitExpr {
        LitExpr{ 
            value: node.value,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct MemberAccess {
    pub base: Box<Expr>,
    pub name: SimpleName,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for MemberAccess {

    type SyntaxItem = syntax::MemberAccessExpr;

    fn from_syntax(node: syntax::MemberAccessExpr, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> MemberAccess {
        MemberAccess{
            base: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.base), parent_scope.clone(), symbols)),
            name: SimpleName::from_syntax(node.name, parent_scope.clone(), symbols),
            parent_scope: parent_scope,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ParenExpr {
    pub expr: Box<Expr>,
    pub parent_scope: SharedDefScope
}
impl ISemanticAnalyze for ParenExpr {

    type SyntaxItem = syntax::ParenExpr;

    fn from_syntax(node: syntax::ParenExpr, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> ParenExpr {
        ParenExpr{
            expr: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.expr), parent_scope.clone(), symbols)),
            parent_scope: parent_scope,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct TupleDef {
    pub items: Vec<Expr>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for TupleDef {

    type SyntaxItem = syntax::TupleDef;

    fn from_syntax(node: syntax::TupleDef, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> TupleDef {
        TupleDef{
            items: node.items.items.into_iter().map(|item| Expr::from_syntax(item, parent_scope.clone(), symbols)).collect(),
            parent_scope: parent_scope,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct UnaryExpr {
    pub base: Box<Expr>,
    pub operator: Seperator,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for UnaryExpr {

    type SyntaxItem = syntax::UnaryExpr;

    fn from_syntax(node: syntax::UnaryExpr, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> UnaryExpr {
        UnaryExpr{
            base: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.base), parent_scope.clone(), symbols)),
            operator: node.operator,
            parent_scope: parent_scope,
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

    fn from_syntax(node: syntax::Expr, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> Expr {
        match node {
            syntax::Expr::Array(array_def) => Expr::Array(ArrayDef::from_syntax(array_def, parent_scope, symbols)),
            syntax::Expr::Binary(binary_expr) => Expr::Binary(BinaryExpr::from_syntax(binary_expr, parent_scope, symbols)),
            syntax::Expr::Name(name) => Expr::Name(Name::from_syntax(name, parent_scope, symbols)),
            syntax::Expr::Lit(lit_expr) => Expr::Lit(LitExpr::from_syntax(lit_expr, parent_scope, symbols)),
            syntax::Expr::FnCall(fn_call) => Expr::FnCall(FnCall::from_syntax(fn_call, parent_scope, symbols)),
            syntax::Expr::IndexCall(index_call) => Expr::IndexCall(IndexCall::from_syntax(index_call, parent_scope, symbols)),
            syntax::Expr::MemberAccess(member_access) => Expr::MemberAccess(MemberAccess::from_syntax(member_access, parent_scope, symbols)),
            syntax::Expr::Paren(paren_expr) => Expr::Paren(ParenExpr::from_syntax(paren_expr, parent_scope, symbols)),
            syntax::Expr::Tuple(tuple_def) => Expr::Tuple(TupleDef::from_syntax(tuple_def, parent_scope, symbols)),
            syntax::Expr::Unary(unary_expr) => Expr::Unary(UnaryExpr::from_syntax(unary_expr, parent_scope, symbols)),
            syntax::Expr::RangeBoth(range_both) => Expr::RangeBoth(RangeBothExpr::from_syntax(range_both, parent_scope, symbols)),
            syntax::Expr::RangeLeft(range_left) => Expr::RangeLeft(RangeLeftExpr::from_syntax(range_left, parent_scope, symbols)),
            syntax::Expr::RangeRight(range_right) => Expr::RangeRight(RangeRightExpr::from_syntax(range_right, parent_scope, symbols)),
            syntax::Expr::RangeFull(range_full) => Expr::RangeFull(RangeFullExpr::from_syntax(range_full, parent_scope, symbols)),
            syntax::Expr::SimpleName(simple_name) => Expr::SimpleName(SimpleName::from_syntax(simple_name, parent_scope, symbols)),
        }
    }
}