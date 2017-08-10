///! fff-lang
///!
///! semantic/range_expr, syntax/range_expr direct maps

use codemap::SymbolCollection;
use syntax;

use super::Expr;
use super::super::SharedDefScope;
use super::super::ISemanticAnalyze;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct RangeFullExpr;

impl ISemanticAnalyze for RangeFullExpr {

    type SyntaxItem = syntax::RangeFullExpr;

    fn from_syntax(_node: syntax::RangeFullExpr, _parent_scope: SharedDefScope, _symbols: &mut SymbolCollection) -> RangeFullExpr {
        RangeFullExpr
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct RangeLeftExpr {
    pub expr: Box<Expr>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for RangeLeftExpr {

    type SyntaxItem = syntax::RangeLeftExpr;

    fn from_syntax(node: syntax::RangeLeftExpr, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> RangeLeftExpr {
        RangeLeftExpr{
            expr: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.expr), parent_scope.clone(), symbols)),
            parent_scope: parent_scope,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct RangeRightExpr {
    pub expr: Box<Expr>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for RangeRightExpr {

    type SyntaxItem = syntax::RangeRightExpr;

    fn from_syntax(node: syntax::RangeRightExpr, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> RangeRightExpr {
        RangeRightExpr{
            expr: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.expr), parent_scope.clone(), symbols)),
            parent_scope: parent_scope,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct RangeBothExpr {
    pub left_expr: Box<Expr>,
    pub right_expr: Box<Expr>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for RangeBothExpr {

    type SyntaxItem = syntax::RangeBothExpr;

    fn from_syntax(node: syntax::RangeBothExpr, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> RangeBothExpr {
        RangeBothExpr{
            left_expr: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.left_expr), parent_scope.clone(), symbols)),
            right_expr: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.right_expr), parent_scope.clone(), symbols)),
            parent_scope: parent_scope,
        }
    }
}