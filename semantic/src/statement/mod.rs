///! fff-lang
///!
///! semantic/statement

use codemap::SymbolID;
use lexical::Seperator;

use syntax;

use super::Expr;
use super::Block;
use super::LabelDef;
use super::TypeUse;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct BlockStatement {
    pub name: Option<LabelDef>,
    pub body: Block,
}
impl From<syntax::BlockStatement> for BlockStatement {
    fn from(node: syntax::BlockStatement) -> BlockStatement {
        BlockStatement{
            name: node.name.map(Into::into),
            body: node.body.into(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct SimpleExprStatement {
    pub expr: Expr,
}
impl From<syntax::SimpleExprStatement> for SimpleExprStatement {
    fn from(node: syntax::SimpleExprStatement) -> SimpleExprStatement {
        SimpleExprStatement{
            expr: node.expr.into(),
        }       
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct AssignExprStatement {
    pub left_expr: Expr,
    pub right_expr: Expr,
    pub assign_op: Seperator,
}
impl From<syntax::AssignExprStatement> for AssignExprStatement {
    fn from(node: syntax::AssignExprStatement) -> AssignExprStatement {
        AssignExprStatement{
            left_expr: node.left_expr.into(),
            right_expr: node.right_expr.into(),
            assign_op: node.assign_op,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ForStatement {
    pub loop_name: Option<LabelDef>,
    pub iter_name: SymbolID,
    pub iter_expr: Expr,
    pub body: Block,
}
impl From<syntax::ForStatement> for ForStatement {
    fn from(node: syntax::ForStatement) -> ForStatement {
        ForStatement{
            loop_name: node.loop_name.map(Into::into),
            iter_name: node.iter_name,
            iter_expr: node.iter_expr.into(),
            body: node.body.into(),
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
}
impl From<syntax::IfStatement> for IfStatement {
    fn from(node: syntax::IfStatement) -> IfStatement {
        IfStatement{
            if_clause: IfClause{ 
                cond_expr: node.if_clause.cond_expr.into(),
                body: node.if_clause.body.into(),
            },
            elseif_clauses: node.elseif_clauses.into_iter().map(|elseif| ElseIfClause{
                cond_expr: elseif.cond_expr.into(),
                body: elseif.body.into(),
            }).collect(),
            else_clause: node.else_clause.map(|else_clause| ElseClause{
                body: else_clause.body.into(),
            })
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct BreakStatement {
    pub target: Option<SymbolID>,
}
impl From<syntax::BreakStatement> for BreakStatement {
    fn from(node: syntax::BreakStatement) -> BreakStatement {
        BreakStatement{
            target: node.0.target,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ContinueStatement {
    pub target: Option<SymbolID>,
}
impl From<syntax::ContinueStatement> for ContinueStatement {
    fn from(node: syntax::ContinueStatement) -> ContinueStatement {
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
impl From<syntax::LoopStatement> for LoopStatement {
    fn from(node: syntax::LoopStatement) -> LoopStatement {
        LoopStatement{
            name: node.name.map(Into::into),
            body: node.body.into(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ReturnStatement {
    pub expr: Option<Expr>,
}
impl From<syntax::ReturnStatement> for ReturnStatement {
    fn from(node: syntax::ReturnStatement) -> ReturnStatement {
        ReturnStatement{
            expr: node.expr.map(Into::into),
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
impl From<syntax::VarDeclStatement> for VarDecl {
    fn from(node: syntax::VarDeclStatement) -> VarDecl {
        VarDecl{
            is_const: node.is_const,
            name: node.name,
            typeuse: node.typeuse.map(Into::into),
            init_expr: node.init_expr.map(Into::into),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct WhileStatement {
    pub name: Option<LabelDef>,
    pub loop_expr: Expr,
    pub body: Block,
}
impl From<syntax::WhileStatement> for WhileStatement {
    fn from(node: syntax::WhileStatement) -> WhileStatement {
        WhileStatement{
            name: node.name.map(Into::into),
            loop_expr: node.loop_expr.into(),
            body: node.body.into(),
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
}
impl From<syntax::Statement> for Statement {
    fn from(node: syntax::Statement) -> Statement {
        match node {
            syntax::Statement::Block(block_stmt) => Statement::Block(block_stmt.into()),
            syntax::Statement::SimpleExpr(simple_expr) => Statement::SimpleExpr(simple_expr.into()),
            syntax::Statement::AssignExpr(assign_expr) => Statement::AssignExpr(assign_expr.into()),
            syntax::Statement::For(for_stmt) => Statement::For(for_stmt.into()),
            syntax::Statement::If(if_stmt) => Statement::If(if_stmt.into()),
            syntax::Statement::Break(break_stmt) => Statement::Break(break_stmt.into()),
            syntax::Statement::Continue(continue_stmt) => Statement::Continue(continue_stmt.into()),
            syntax::Statement::Loop(loop_stmt) => Statement::Loop(loop_stmt.into()),
            syntax::Statement::Return(ret_stmt) => Statement::Return(ret_stmt.into()),
            syntax::Statement::VarDecl(var_decl) => Statement::VarDecl(var_decl.into()),
            syntax::Statement::While(while_stmt) => Statement::While(while_stmt.into()),
        }
    }
}