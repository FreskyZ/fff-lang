///! fff-lang
///!
///! semantic/statement

use codemap::Span;
use codemap::SymbolID;
use lexical::Seperator;

use syntax;

use super::Expr;
use super::Block;
use super::LabelDef;
use super::TypeUse;
use super::FnDef;
use super::TypeDef;
use super::FromSyntax;
use super::DefScope;
use super::SharedDefScope;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct BlockStatement {
    pub name: Option<LabelDef>,
    pub body: Block,
    pub all_span: Span,
    pub this_scope: SharedDefScope,
}
impl FromSyntax<syntax::BlockStatement> for BlockStatement {
    fn from_syntax(node: syntax::BlockStatement, parent_scope: SharedDefScope) -> BlockStatement {
        BlockStatement{
            name: node.name.map(|name| FromSyntax::from_syntax(name, parent_scope.clone())),
            body: FromSyntax::from_syntax(node.body, parent_scope.clone()),
            all_span: node.all_span,
            this_scope: DefScope::with_parent(format!("block-stmt"), parent_scope),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct SimpleExprStatement {
    pub expr: Expr,
    pub parent_scope: SharedDefScope,
}
impl FromSyntax<syntax::SimpleExprStatement> for SimpleExprStatement {
    fn from_syntax(node: syntax::SimpleExprStatement, parent_scope: SharedDefScope) -> SimpleExprStatement {
        SimpleExprStatement{
            expr: FromSyntax::from_syntax(node.expr, parent_scope.clone()),
            parent_scope,
        }       
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct AssignExprStatement {
    pub left_expr: Expr,
    pub right_expr: Expr,
    pub assign_op: Seperator,
    parent_scope: SharedDefScope,
}
impl FromSyntax<syntax::AssignExprStatement> for AssignExprStatement {
    fn from_syntax(node: syntax::AssignExprStatement, parent_scope: SharedDefScope) -> AssignExprStatement {
        AssignExprStatement{
            left_expr: FromSyntax::from_syntax(node.left_expr, parent_scope.clone()),
            right_expr: FromSyntax::from_syntax(node.right_expr, parent_scope.clone()),
            assign_op: node.assign_op,
            parent_scope,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ForStatement {
    pub loop_name: Option<LabelDef>,
    pub iter_name: SymbolID,
    pub iter_expr: Expr,
    pub body: Block,
    parent_scope: SharedDefScope,
}
impl FromSyntax<syntax::ForStatement> for ForStatement {
    fn from_syntax(node: syntax::ForStatement, parent_scope: SharedDefScope) -> ForStatement {
        ForStatement{
            loop_name: node.loop_name.map(|name| FromSyntax::from_syntax(name, parent_scope.clone())),
            iter_name: node.iter_name,
            iter_expr: FromSyntax::from_syntax(node.iter_expr, parent_scope.clone()),
            body: FromSyntax::from_syntax(node.body, parent_scope.clone()),
            parent_scope,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct IfClause {
    pub cond_expr: Expr,
    pub body: Block,
}
#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ElseIfClause {
    pub cond_expr: Expr,
    pub body: Block,
}
#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ElseClause {
    pub body: Block,
}
#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct IfStatement {
    pub if_clause: IfClause,
    pub elseif_clauses: Vec<ElseIfClause>,
    pub else_clause: Option<ElseClause>,
    parent_scope: SharedDefScope
}
impl FromSyntax<syntax::IfStatement> for IfStatement {
    fn from_syntax(node: syntax::IfStatement, parent_scope: SharedDefScope) -> IfStatement {
        IfStatement{
            if_clause: IfClause{ 
                cond_expr: FromSyntax::from_syntax(node.if_clause.cond_expr, parent_scope.clone()),
                body: FromSyntax::from_syntax(node.if_clause.body, parent_scope.clone()),
            },
            elseif_clauses: node.elseif_clauses.into_iter().map(|elseif| ElseIfClause{
                cond_expr: FromSyntax::from_syntax(elseif.cond_expr, parent_scope.clone()),
                body: FromSyntax::from_syntax(elseif.body, parent_scope.clone()),
            }).collect(),
            else_clause: node.else_clause.map(|else_clause| ElseClause{
                body: FromSyntax::from_syntax(else_clause.body, parent_scope.clone()),
            }),
            parent_scope,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct BreakStatement {
    pub target: Option<SymbolID>,
    parent_scope: SharedDefScope,
}
impl FromSyntax<syntax::BreakStatement> for BreakStatement {
    fn from_syntax(node: syntax::BreakStatement, parent_scope: SharedDefScope) -> BreakStatement {
        BreakStatement{
            target: node.0.target,
            parent_scope,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ContinueStatement {
    pub target: Option<SymbolID>,
}
impl FromSyntax<syntax::ContinueStatement> for ContinueStatement {
    fn from_syntax(node: syntax::ContinueStatement, parent_scope: SharedDefScope) -> ContinueStatement {
        ContinueStatement{
            target: node.0.target,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct LoopStatement {
    pub name: Option<LabelDef>,
    pub body: Block,
}
impl FromSyntax<syntax::LoopStatement> for LoopStatement {
    fn from_syntax(node: syntax::LoopStatement, parent_scope: SharedDefScope) -> LoopStatement {
        LoopStatement{
            name: node.name.map(|name| FromSyntax::from_syntax(name, parent_scope.clone())),
            body: FromSyntax::from_syntax(node.body, parent_scope.clone()),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ReturnStatement {
    pub expr: Option<Expr>,
}
impl FromSyntax<syntax::ReturnStatement> for ReturnStatement {
    fn from_syntax(node: syntax::ReturnStatement, parent_scope: SharedDefScope) -> ReturnStatement {
        ReturnStatement{
            expr: node.expr.map(|expr| FromSyntax::from_syntax(expr, parent_scope.clone())),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct VarDecl {
    pub is_const: bool,
    pub name: SymbolID,
    pub typeuse: Option<TypeUse>,
    pub init_expr: Option<Expr>,
}
impl FromSyntax<syntax::VarDeclStatement> for VarDecl {
    fn from_syntax(node: syntax::VarDeclStatement, parent_scope: SharedDefScope) -> VarDecl {
        VarDecl{
            is_const: node.is_const,
            name: node.name,
            typeuse: node.typeuse.map(|ty| FromSyntax::from_syntax(ty, parent_scope.clone())),
            init_expr: node.init_expr.map(|expr| FromSyntax::from_syntax(expr, parent_scope.clone())),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct WhileStatement {
    pub name: Option<LabelDef>,
    pub loop_expr: Expr,
    pub body: Block,
}
impl FromSyntax<syntax::WhileStatement> for WhileStatement {
    fn from_syntax(node: syntax::WhileStatement, parent_scope: SharedDefScope) -> WhileStatement {
        WhileStatement{
            name: node.name.map(|name| FromSyntax::from_syntax(name, parent_scope.clone())),
            loop_expr: FromSyntax::from_syntax(node.loop_expr, parent_scope.clone()),
            body: FromSyntax::from_syntax(node.body, parent_scope.clone()),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub enum Statement {
    AssignExpr(AssignExprStatement),
    Block(BlockStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
    For(ForStatement),
    If(IfStatement),
    SimpleExpr(SimpleExprStatement),
    Loop(LoopStatement),
    Return(ReturnStatement),
    VarDecl(VarDecl),
    While(WhileStatement),
    Type(TypeDef),
    Fn(FnDef),
}
impl FromSyntax<syntax::Statement> for Statement {
    fn from_syntax(node: syntax::Statement, parent_scope: SharedDefScope) -> Statement {
        match node {
            syntax::Statement::Block(block_stmt) => Statement::Block(FromSyntax::from_syntax(block_stmt, parent_scope)),
            syntax::Statement::SimpleExpr(simple_expr) => Statement::SimpleExpr(FromSyntax::from_syntax(simple_expr, parent_scope)),
            syntax::Statement::AssignExpr(assign_expr) => Statement::AssignExpr(FromSyntax::from_syntax(assign_expr, parent_scope)),
            syntax::Statement::For(for_stmt) => Statement::For(FromSyntax::from_syntax(for_stmt, parent_scope)),
            syntax::Statement::If(if_stmt) => Statement::If(FromSyntax::from_syntax(if_stmt, parent_scope)),
            syntax::Statement::Break(break_stmt) => Statement::Break(FromSyntax::from_syntax(break_stmt, parent_scope)),
            syntax::Statement::Continue(continue_stmt) => Statement::Continue(FromSyntax::from_syntax(continue_stmt, parent_scope)),
            syntax::Statement::Loop(loop_stmt) => Statement::Loop(FromSyntax::from_syntax(loop_stmt, parent_scope)),
            syntax::Statement::Return(ret_stmt) => Statement::Return(FromSyntax::from_syntax(ret_stmt, parent_scope)),
            syntax::Statement::VarDecl(var_decl) => Statement::VarDecl(FromSyntax::from_syntax(var_decl, parent_scope)),
            syntax::Statement::While(while_stmt) => Statement::While(FromSyntax::from_syntax(while_stmt, parent_scope)),
            syntax::Statement::Fn(fn_def) => Statement::Fn(FromSyntax::from_syntax(fn_def, parent_scope)),
            syntax::Statement::Type(type_def) => Statement::Type(FromSyntax::from_syntax(type_def, parent_scope)),
        }
    }
}