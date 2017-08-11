///! fff-lang
///!
///! semantic/statement

use codemap::Span;
use codemap::SymbolID;
use lexical::Seperator;

use syntax;

use super::Expr;
use super::Name;
use super::Block;
use super::FnDef;
use super::Module;
use super::TypeUse;
use super::TypeDef;
use super::LabelDef;
use super::Formatter;
use super::SimpleName;
use super::FromSession;
use super::SharedDefScope;
use super::ISemanticAnalyze;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct BlockStatement {
    pub name: Option<LabelDef>,
    pub body: Block,
    pub all_span: Span,
    pub this_scope: SharedDefScope,
}
impl ISemanticAnalyze for BlockStatement {

    type SyntaxItem = syntax::BlockStatement;

    fn from_syntax(node: syntax::BlockStatement, sess: FromSession) -> BlockStatement {
        let this_sess = sess.sub_with_span("block", node.all_span);
        BlockStatement{
            name: node.name.map(|name| LabelDef::from_syntax(name, this_sess.clone_scope())),
            body: Block::from_syntax(node.body, sess.clone_scope()),
            all_span: node.all_span,
            this_scope: this_sess.into_scope(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct SimpleExprStatement {
    pub expr: Expr,
    pub this_scope: SharedDefScope,
}
impl ISemanticAnalyze for SimpleExprStatement {

    type SyntaxItem = syntax::SimpleExprStatement;

    fn from_syntax(node: syntax::SimpleExprStatement, sess: FromSession) -> SimpleExprStatement {
        let this_sess = sess.sub_with_span("expr", node.all_span);
        SimpleExprStatement{
            expr: Expr::from_syntax(node.expr, this_sess.clone_scope()),
            this_scope: this_sess.into_scope(),
        }       
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct AssignExprStatement {
    pub left_expr: Expr,
    pub right_expr: Expr,
    pub assign_op: Seperator,
    pub this_scope: SharedDefScope,
}
impl ISemanticAnalyze for AssignExprStatement {

    type SyntaxItem = syntax::AssignExprStatement;

    fn from_syntax(node: syntax::AssignExprStatement, sess: FromSession) -> AssignExprStatement {
        let this_sess = sess.sub_with_span("expr", node.all_span);
        AssignExprStatement{
            left_expr: Expr::from_syntax(node.left_expr, this_sess.clone_scope()),
            right_expr: Expr::from_syntax(node.right_expr, this_sess.clone_scope()),
            assign_op: node.assign_op,
            this_scope: this_sess.into_scope(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ForStatement {
    pub loop_name: Option<LabelDef>,
    pub iter_name: SymbolID,
    pub iter_expr: Expr,
    pub body: Block,
    pub this_scope: SharedDefScope,
}
impl ISemanticAnalyze for ForStatement {

    type SyntaxItem = syntax::ForStatement;

    fn from_syntax(node: syntax::ForStatement, sess: FromSession) -> ForStatement {
        let this_sess = sess.sub_with_span("for", node.all_span);
        ForStatement{
            loop_name: node.loop_name.map(|name| LabelDef::from_syntax(name, this_sess.clone_scope())),
            iter_name: node.iter_name,
            iter_expr: Expr::from_syntax(node.iter_expr, this_sess.clone_scope()),
            body: Block::from_syntax(node.body, this_sess.clone_scope()),
            this_scope: this_sess.into_scope(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct IfClause {
    pub cond_expr: Expr,
    pub body: Block,
    pub this_scope: SharedDefScope,
}
#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ElseIfClause {
    pub cond_expr: Expr,
    pub body: Block,
    pub this_scope: SharedDefScope,
}
#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ElseClause {
    pub body: Block,
    pub this_scope: SharedDefScope,
}
#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct IfStatement {
    pub if_clause: IfClause,
    pub elseif_clauses: Vec<ElseIfClause>,
    pub else_clause: Option<ElseClause>,
}
impl ISemanticAnalyze for IfStatement {

    type SyntaxItem = syntax::IfStatement;

    fn from_syntax(node: syntax::IfStatement, sess: FromSession) -> IfStatement {

        let if_clause_sess = sess.sub_with_span("if", node.if_clause.all_span);
        let if_clause = IfClause{ 
            cond_expr: Expr::from_syntax(node.if_clause.cond_expr, if_clause_sess.clone_scope()),
            body: Block::from_syntax(node.if_clause.body, if_clause_sess.clone_scope()),
            this_scope: if_clause_sess.into_scope(),
        };

        let elseif_clauses = node.elseif_clauses.into_iter().map(|elseif| {
            let elseif_clause_sess = sess.sub_with_span("else-if", elseif.all_span);
            ElseIfClause{
                cond_expr: Expr::from_syntax(elseif.cond_expr, elseif_clause_sess.clone_scope()),
                body: Block::from_syntax(elseif.body, elseif_clause_sess.clone_scope()),
                this_scope: elseif_clause_sess.into_scope(),
            }
        }).collect();

        let else_clause = node.else_clause.map(|else_clause| {
            let else_clause_sess = sess.sub_with_span("else", else_clause.all_span);
            ElseClause{
                body: Block::from_syntax(else_clause.body, else_clause_sess.clone_scope()),
                this_scope: else_clause_sess.into_scope(),
            }
        });

        IfStatement{ if_clause, elseif_clauses, else_clause }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct BreakStatement {
    pub target: Option<SymbolID>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for BreakStatement {

    type SyntaxItem = syntax::BreakStatement;

    fn from_syntax(node: syntax::BreakStatement, sess: FromSession) -> BreakStatement {
        BreakStatement{
            target: node.0.target,
            parent_scope: sess.into_scope(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ContinueStatement {
    pub target: Option<SymbolID>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for ContinueStatement {

    type SyntaxItem = syntax::ContinueStatement;

    fn from_syntax(node: syntax::ContinueStatement, sess: FromSession) -> ContinueStatement {
        ContinueStatement{
            target: node.0.target,
            parent_scope: sess.into_scope(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct LoopStatement {
    pub name: Option<LabelDef>,
    pub body: Block,
    pub this_scope: SharedDefScope,
}
impl ISemanticAnalyze for LoopStatement {

    type SyntaxItem = syntax::LoopStatement;

    fn from_syntax(node: syntax::LoopStatement, sess: FromSession) -> LoopStatement {
        let this_sess = sess.sub_with_span("loop", node.all_span);
        LoopStatement{
            name: node.name.map(|name| LabelDef::from_syntax(name, this_sess.clone_scope())),
            body: Block::from_syntax(node.body, this_sess.clone_scope()),
            this_scope: this_sess.into_scope(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ReturnStatement {
    pub expr: Option<Expr>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for ReturnStatement {

    type SyntaxItem = syntax::ReturnStatement;

    fn from_syntax(node: syntax::ReturnStatement, sess: FromSession) -> ReturnStatement {
        ReturnStatement{
            expr: node.expr.map(|expr| Expr::from_syntax(expr, sess.clone_scope())),
            parent_scope: sess.into_scope(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct VarDecl {
    pub is_const: bool,
    pub name: SymbolID,
    pub typeuse: Option<TypeUse>,
    pub init_expr: Option<Expr>,
    pub parent_scope: SharedDefScope,  // if var decl has scope, then it will define variable in its own scope, that's, yes, you understand it
}
impl ISemanticAnalyze for VarDecl {

    type SyntaxItem = syntax::VarDeclStatement;

    fn from_syntax(node: syntax::VarDeclStatement, sess: FromSession) -> VarDecl {
        VarDecl{
            is_const: node.is_const,
            name: node.name,
            typeuse: node.typeuse.map(|ty| TypeUse::from_syntax(ty, sess.clone_scope())),
            init_expr: node.init_expr.map(|expr| Expr::from_syntax(expr, sess.clone_scope())),
            parent_scope: sess.into_scope(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct WhileStatement {
    pub name: Option<LabelDef>,
    pub loop_expr: Expr,
    pub body: Block,
    pub this_scope: SharedDefScope,
}
impl ISemanticAnalyze for WhileStatement {

    type SyntaxItem = syntax::WhileStatement;

    fn from_syntax(node: syntax::WhileStatement, sess: FromSession) -> WhileStatement {
        let this_sess = sess.sub_with_span("while", node.all_span);
        WhileStatement{
            name: node.name.map(|name| LabelDef::from_syntax(name, this_sess.clone_scope())),
            loop_expr: Expr::from_syntax(node.loop_expr, this_sess.clone_scope()),
            body: Block::from_syntax(node.body, this_sess.clone_scope()),
            this_scope: this_sess.into_scope(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct UseStatement {
    pub name: Name, 
    pub alias: Option<SimpleName>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for UseStatement {

    type SyntaxItem = syntax::UseStatement;

    fn from_syntax(node: syntax::UseStatement, sess: FromSession) -> UseStatement {
        UseStatement{
            name: Name::from_syntax(node.name, sess.clone_scope()),
            alias: node.target.map(|target| SimpleName::from_syntax(target, sess.clone_scope())),
            parent_scope: sess.into_scope(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ImportStatement {
    pub name: SimpleName,
    pub module: Option<Module>,
    pub alias: Option<SimpleName>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for ImportStatement {

    fn format(&self, f: Formatter) -> String {
        let mut f = f.indent().header_text_or("import-stmt").space().debug(&self.parent_scope).endl()
            .apply1(&self.name);
        if let Some(ref alias) = self.alias {
            f = f.endl().set_header_text("alias-as").apply1(alias).unset_header_text();
        }
        if let Some(ref module) = self.module {
            f.endl().apply1(module).finish()
        } else {
            f.endl().indent1().lit("<no-module>").finish()
        }
    }

    type SyntaxItem = syntax::ImportStatement;

    fn from_syntax(node: syntax::ImportStatement, sess: FromSession) -> ImportStatement {
        ImportStatement{
            name: SimpleName::from_syntax(node.name, sess.clone_scope()),
            alias: node.target.map(|target| SimpleName::from_syntax(target, sess.clone_scope())),
            module: None,
            parent_scope: sess.into_scope(),
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
    Use(UseStatement),
}
impl ISemanticAnalyze for Statement {

    type SyntaxItem = syntax::Statement;

    fn from_syntax(node: syntax::Statement, sess: FromSession) -> Statement {
        match node {
            syntax::Statement::Block(block_stmt) => Statement::Block(BlockStatement::from_syntax(block_stmt, sess)),
            syntax::Statement::SimpleExpr(simple_expr) => Statement::SimpleExpr(SimpleExprStatement::from_syntax(simple_expr, sess)),
            syntax::Statement::AssignExpr(assign_expr) => Statement::AssignExpr(AssignExprStatement::from_syntax(assign_expr, sess)),
            syntax::Statement::For(for_stmt) => Statement::For(ForStatement::from_syntax(for_stmt, sess)),
            syntax::Statement::If(if_stmt) => Statement::If(IfStatement::from_syntax(if_stmt, sess)),
            syntax::Statement::Break(break_stmt) => Statement::Break(BreakStatement::from_syntax(break_stmt, sess)),
            syntax::Statement::Continue(continue_stmt) => Statement::Continue(ContinueStatement::from_syntax(continue_stmt, sess)),
            syntax::Statement::Loop(loop_stmt) => Statement::Loop(LoopStatement::from_syntax(loop_stmt, sess)),
            syntax::Statement::Return(ret_stmt) => Statement::Return(ReturnStatement::from_syntax(ret_stmt, sess)),
            syntax::Statement::VarDecl(var_decl) => Statement::VarDecl(VarDecl::from_syntax(var_decl, sess)),
            syntax::Statement::While(while_stmt) => Statement::While(WhileStatement::from_syntax(while_stmt, sess)),
            syntax::Statement::Fn(fn_def) => Statement::Fn(FnDef::from_syntax(fn_def, sess)),
            syntax::Statement::Type(type_def) => Statement::Type(TypeDef::from_syntax(type_def, sess)),
            syntax::Statement::Use(use_def) => Statement::Use(UseStatement::from_syntax(use_def, sess)),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub enum Item {
    Type(TypeDef),
    Fn(FnDef),
    Block(BlockStatement),
    SimpleExpr(SimpleExprStatement),
    AssignExpr(AssignExprStatement),
    For(ForStatement),
    If(IfStatement),
    Loop(LoopStatement),
    VarDecl(VarDecl),
    While(WhileStatement),
    Use(UseStatement),
    Import(ImportStatement),
}
impl ISemanticAnalyze for Item {

    fn format(&self, f: Formatter) -> String {
        match self {
            &Item::Type(ref type_def) => type_def.format(f),
            &Item::Fn(ref fn_def) => fn_def.format(f),
            &Item::Block(ref block_stmt) => block_stmt.format(f),
            &Item::Import(ref import_stmt) => import_stmt.format(f),
            _ => "<unknown-item>".to_owned(),
        }
    }

    type SyntaxItem = syntax::Item;

    fn from_syntax(node: syntax::Item, sess: FromSession) -> Item {
        match node {
            syntax::Item::Type(type_def) => Item::Type(TypeDef::from_syntax(type_def, sess)),
            syntax::Item::Fn(fn_def) => Item::Fn(FnDef::from_syntax(fn_def, sess)),
            syntax::Item::Block(block) => Item::Block(BlockStatement::from_syntax(block, sess)),
            syntax::Item::SimpleExpr(simple_expr) => Item::SimpleExpr(SimpleExprStatement::from_syntax(simple_expr, sess)),
            syntax::Item::AssignExpr(assign_expr) => Item::AssignExpr(AssignExprStatement::from_syntax(assign_expr, sess)),
            syntax::Item::For(for_stmt) => Item::For(ForStatement::from_syntax(for_stmt, sess)),
            syntax::Item::If(if_stmt) => Item::If(IfStatement::from_syntax(if_stmt, sess)),
            syntax::Item::Loop(loop_stmt) => Item::Loop(LoopStatement::from_syntax(loop_stmt, sess)),
            syntax::Item::VarDecl(var_decl) => Item::VarDecl(VarDecl::from_syntax(var_decl, sess)),
            syntax::Item::While(while_stmt) => Item::While(WhileStatement::from_syntax(while_stmt, sess)),
            syntax::Item::Use(use_def) => Item::Use(UseStatement::from_syntax(use_def, sess)),
            syntax::Item::Import(import_def) => Item::Import(ImportStatement::from_syntax(import_def, sess)),
        }
    }
}