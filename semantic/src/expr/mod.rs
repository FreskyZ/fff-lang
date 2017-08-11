///! fff-lang
///!
///! semantic/expr

use codemap::SymbolID;
use lexical::LitValue;
use lexical::Seperator;

use syntax;

mod range_expr;

use super::Formatter;
use super::FromSession;
use super::SharedDefScope;
use super::ISemanticAnalyze;

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

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("array-def").space().debug(&self.parent_scope)
            .foreach(&self.items, |f, expr| f.endl().apply1(expr)).finish()
    }

    type SyntaxItem = syntax::ArrayDef;

    fn from_syntax(node: syntax::ArrayDef, sess: FromSession) -> ArrayDef {
        ArrayDef{
            items: node.items.items.into_iter().map(|item| Expr::from_syntax(item, sess.clone_scope())).collect(),
            parent_scope: sess.into_scope(),
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

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("binary-expr").space().debug(&self.parent_scope).endl()
            .apply1_with_prefix_text("left-is", self.left_expr.as_ref()).endl()
            .indent1().lit("\"").debug(&self.operator).lit("\"").endl()
            .apply1_with_prefix_text("right-is", self.right_expr.as_ref())
            .finish()
    }

    type SyntaxItem = syntax::BinaryExpr;

    fn from_syntax(node: syntax::BinaryExpr, sess: FromSession) -> BinaryExpr {
        BinaryExpr{
            left_expr: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.left_expr), sess.clone_scope())),
            right_expr: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.right_expr), sess.clone_scope())),
            operator: node.operator,
            parent_scope: sess.into_scope(),
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

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("fn-call").space().debug(&self.parent_scope).endl()
            .apply1_with_prefix_text("base-is", self.base.as_ref())
            .foreach_or_else(&self.params, |f, expr| f.endl().apply1(expr), |f| f.endl().indent1().lit("no-argument"))
            .finish()
    }

    type SyntaxItem = syntax::FnCallExpr;

    fn from_syntax(node: syntax::FnCallExpr, sess: FromSession) -> FnCall {
        FnCall{
            base: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.base), sess.clone_scope())),
            params: node.params.items.into_iter().map(|item| Expr::from_syntax(item, sess.clone_scope())).collect(),
            parent_scope: sess.into_scope(),
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

    fn from_syntax(node: syntax::SimpleName, sess: FromSession) -> SimpleName {
        SimpleName{
            value: node.value,
            parent_scope: sess.into_scope(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct Name {
    pub segments: Vec<SimpleName>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for Name {

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("name").space().debug(&self.parent_scope)
            .foreach(&self.segments, |f, segment| f.endl().apply1_with_header_text("segment", segment))
            .finish()
    }

    type SyntaxItem = syntax::Name;

    fn from_syntax(node: syntax::Name, sess: FromSession) -> Name {
        Name{
            segments: node.segments.into_iter().map(|segment| SimpleName::from_syntax(segment, sess.clone_scope())).collect(),
            parent_scope: sess.into_scope(),
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

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("fn-call").space().debug(&self.parent_scope).endl()
            .apply1_with_prefix_text("base-is", self.base.as_ref())
            .foreach_or_else(&self.params, |f, expr| f.endl().apply1(expr), |f| f.endl().indent1().lit("no-argument"))
            .finish()
    }

    type SyntaxItem = syntax::IndexCallExpr;

    fn from_syntax(node: syntax::IndexCallExpr, sess: FromSession) -> IndexCall {
        IndexCall{
            base: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.base), sess.clone_scope())),
            params: node.params.items.into_iter().map(|item| Expr::from_syntax(item, sess.clone_scope())).collect(),
            parent_scope: sess.into_scope(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct LitExpr {
    pub value: LitValue,
}
impl ISemanticAnalyze for LitExpr {

    fn format(&self, f: Formatter) -> String {
        let f = f.indent().header_text_or("literal").space();
        let f = match self.value { LitValue::Str(Some(ref id)) => f.sym(*id), ref other => f.debug(other) };
        f.finish()
    }

    type SyntaxItem = syntax::LitExpr;

    fn from_syntax(node: syntax::LitExpr, _sess: FromSession) -> LitExpr {
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

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("member-access").space().debug(&self.parent_scope).endl()
            .apply1_with_prefix_text("base-is", self.base.as_ref()).endl()
            .apply1_with_header_text("member-name-is", &self.name)
            .finish()
    }

    type SyntaxItem = syntax::MemberAccessExpr;

    fn from_syntax(node: syntax::MemberAccessExpr, sess: FromSession) -> MemberAccess {
        MemberAccess{
            base: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.base), sess.clone_scope())),
            name: SimpleName::from_syntax(node.name, sess.clone_scope()),
            parent_scope: sess.into_scope(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ParenExpr {
    pub expr: Box<Expr>,
    pub parent_scope: SharedDefScope
}
impl ISemanticAnalyze for ParenExpr {

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("paren-expr").space().debug(&self.parent_scope).endl()
            .apply1(self.expr.as_ref())
            .finish()
    }

    type SyntaxItem = syntax::ParenExpr;

    fn from_syntax(node: syntax::ParenExpr, sess: FromSession) -> ParenExpr {
        ParenExpr{
            expr: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.expr), sess.clone_scope())),
            parent_scope: sess.into_scope(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct TupleDef {
    pub items: Vec<Expr>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for TupleDef {

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("tuple-def").space().debug(&self.parent_scope)
            .foreach(&self.items, |f, expr| f.endl().apply1(expr)).finish()
    }

    type SyntaxItem = syntax::TupleDef;

    fn from_syntax(node: syntax::TupleDef, sess: FromSession) -> TupleDef {
        TupleDef{
            items: node.items.items.into_iter().map(|item| Expr::from_syntax(item, sess.clone_scope())).collect(),
            parent_scope: sess.into_scope(),
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

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("unary-expr").space().debug(&self.parent_scope).endl()
            .indent1().lit("\"").debug(&self.operator).lit("\"").endl()
            .apply1_with_prefix_text("prefix-is", self.base.as_ref())
            .finish()
    }

    type SyntaxItem = syntax::UnaryExpr;

    fn from_syntax(node: syntax::UnaryExpr, sess: FromSession) -> UnaryExpr {
        UnaryExpr{
            base: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.base), sess.clone_scope())),
            operator: node.operator,
            parent_scope: sess.into_scope(),
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

    fn format(&self, f: Formatter) -> String {
        match self {
            &Expr::Array(ref array_def) => array_def.format(f),
            &Expr::Lit(ref lit_expr) => lit_expr.format(f),
            &Expr::SimpleName(ref name) => name.format(f),
            &Expr::Name(ref name) => name.format(f),
            &Expr::FnCall(ref fn_call) => fn_call.format(f),
            &Expr::MemberAccess(ref access) => access.format(f),
            &Expr::RangeBoth(ref range) => range.format(f),
            &Expr::Binary(ref binary) => binary.format(f),
            &Expr::IndexCall(ref index) => index.format(f),
            &Expr::Paren(ref paren) => paren.format(f),
            &Expr::RangeFull(ref full) => full.format(f),
            &Expr::RangeLeft(ref left) => left.format(f),
            &Expr::RangeRight(ref right) => right.format(f),
            &Expr::Tuple(ref tuple) => tuple.format(f),
            &Expr::Unary(ref unary) => unary.format(f),
        }
    }

    type SyntaxItem = syntax::Expr;

    fn from_syntax(node: syntax::Expr, sess: FromSession) -> Expr {
        match node {
            syntax::Expr::Array(array_def) => Expr::Array(ArrayDef::from_syntax(array_def, sess)),
            syntax::Expr::Binary(binary_expr) => Expr::Binary(BinaryExpr::from_syntax(binary_expr, sess)),
            syntax::Expr::Name(name) => Expr::Name(Name::from_syntax(name, sess)),
            syntax::Expr::Lit(lit_expr) => Expr::Lit(LitExpr::from_syntax(lit_expr, sess)),
            syntax::Expr::FnCall(fn_call) => Expr::FnCall(FnCall::from_syntax(fn_call, sess)),
            syntax::Expr::IndexCall(index_call) => Expr::IndexCall(IndexCall::from_syntax(index_call, sess)),
            syntax::Expr::MemberAccess(member_access) => Expr::MemberAccess(MemberAccess::from_syntax(member_access, sess)),
            syntax::Expr::Paren(paren_expr) => Expr::Paren(ParenExpr::from_syntax(paren_expr, sess)),
            syntax::Expr::Tuple(tuple_def) => Expr::Tuple(TupleDef::from_syntax(tuple_def, sess)),
            syntax::Expr::Unary(unary_expr) => Expr::Unary(UnaryExpr::from_syntax(unary_expr, sess)),
            syntax::Expr::RangeBoth(range_both) => Expr::RangeBoth(RangeBothExpr::from_syntax(range_both, sess)),
            syntax::Expr::RangeLeft(range_left) => Expr::RangeLeft(RangeLeftExpr::from_syntax(range_left, sess)),
            syntax::Expr::RangeRight(range_right) => Expr::RangeRight(RangeRightExpr::from_syntax(range_right, sess)),
            syntax::Expr::RangeFull(range_full) => Expr::RangeFull(RangeFullExpr::from_syntax(range_full, sess)),
            syntax::Expr::SimpleName(simple_name) => Expr::SimpleName(SimpleName::from_syntax(simple_name, sess)),
        }
    }
}