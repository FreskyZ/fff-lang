///! fff-lang
///!
///! semantic/statement

use codemap::Span;
use codemap::SymbolID;
use codemap::SymbolCollection;
use lexical::Seperator;

use syntax;

use super::Expr;
use super::Block;
use super::LabelDef;
use super::TypeUse;
use super::FnDef;
use super::TypeDef;
use super::Name;
use super::SimpleName;
use super::SharedDefScope;
use super::ISemanticAnalyze;
use super::Module;
use super::Formatter;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct BlockStatement {
    pub name: Option<LabelDef>,
    pub body: Block,
    pub all_span: Span,
    pub this_scope: SharedDefScope,
}
impl ISemanticAnalyze for BlockStatement {

    type SyntaxItem = syntax::BlockStatement;

    fn from_syntax(node: syntax::BlockStatement, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> BlockStatement {
        let this_scope = parent_scope.sub_with_span("block", node.all_span);
        BlockStatement{
            name: node.name.map(|name| LabelDef::from_syntax(name, this_scope.clone(), symbols)),
            body: Block::from_syntax(node.body, parent_scope.clone(), symbols),
            all_span: node.all_span,
            this_scope: this_scope,
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

    fn from_syntax(node: syntax::SimpleExprStatement, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> SimpleExprStatement {
        let this_scope = parent_scope.sub_with_span("expr", node.all_span);
        SimpleExprStatement{
            expr: Expr::from_syntax(node.expr, this_scope.clone(), symbols),
            this_scope: this_scope,
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

    fn from_syntax(node: syntax::AssignExprStatement, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> AssignExprStatement {
        let this_scope = parent_scope.sub_with_span("expr", node.all_span);
        AssignExprStatement{
            left_expr: Expr::from_syntax(node.left_expr, this_scope.clone(), symbols),
            right_expr: Expr::from_syntax(node.right_expr, this_scope.clone(), symbols),
            assign_op: node.assign_op,
            this_scope: this_scope,
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

    fn from_syntax(node: syntax::ForStatement, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> ForStatement {
        let this_scope = parent_scope.sub_with_span("for", node.all_span);
        ForStatement{
            loop_name: node.loop_name.map(|name| LabelDef::from_syntax(name, this_scope.clone(), symbols)),
            iter_name: node.iter_name,
            iter_expr: Expr::from_syntax(node.iter_expr, this_scope.clone(), symbols),
            body: Block::from_syntax(node.body, this_scope.clone(), symbols),
            this_scope: this_scope,
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

    fn from_syntax(node: syntax::IfStatement, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> IfStatement {

        let if_clause_scope = parent_scope.sub_with_span("if", node.if_clause.all_span);
        let if_clause = IfClause{ 
            cond_expr: Expr::from_syntax(node.if_clause.cond_expr, if_clause_scope.clone(), symbols),
            body: Block::from_syntax(node.if_clause.body, if_clause_scope.clone(), symbols),
            this_scope: if_clause_scope,
        };

        let elseif_clauses = node.elseif_clauses.into_iter().map(|elseif| {
            let elseif_clause_scope = parent_scope.sub_with_span("else-if", elseif.all_span);
            ElseIfClause{
                cond_expr: Expr::from_syntax(elseif.cond_expr, elseif_clause_scope.clone(), symbols),
                body: Block::from_syntax(elseif.body, elseif_clause_scope.clone(), symbols),
                this_scope: elseif_clause_scope,
            }
        }).collect();

        let else_clause = node.else_clause.map(|else_clause| {
            let else_clause_scope = parent_scope.sub_with_span("else", else_clause.all_span);
            ElseClause{
                body: Block::from_syntax(else_clause.body, else_clause_scope.clone(), symbols),
                this_scope: else_clause_scope,
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

    fn from_syntax(node: syntax::BreakStatement, parent_scope: SharedDefScope, _symbols: &mut SymbolCollection) -> BreakStatement {
        BreakStatement{
            target: node.0.target,
            parent_scope: parent_scope,
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

    fn from_syntax(node: syntax::ContinueStatement, parent_scope: SharedDefScope, _symbols: &mut SymbolCollection) -> ContinueStatement {
        ContinueStatement{
            target: node.0.target,
            parent_scope: parent_scope,
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

    fn from_syntax(node: syntax::LoopStatement, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> LoopStatement {
        let this_scope = parent_scope.sub_with_span("loop", node.all_span);
        LoopStatement{
            name: node.name.map(|name| LabelDef::from_syntax(name, this_scope.clone(), symbols)),
            body: Block::from_syntax(node.body, this_scope.clone(), symbols),
            this_scope: this_scope,
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

    fn from_syntax(node: syntax::ReturnStatement, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> ReturnStatement {
        ReturnStatement{
            expr: node.expr.map(|expr| Expr::from_syntax(expr, parent_scope.clone(), symbols)),
            parent_scope: parent_scope,
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

    fn from_syntax(node: syntax::VarDeclStatement, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> VarDecl {
        VarDecl{
            is_const: node.is_const,
            name: node.name,
            typeuse: node.typeuse.map(|ty| TypeUse::from_syntax(ty, parent_scope.clone(), symbols)),
            init_expr: node.init_expr.map(|expr| Expr::from_syntax(expr, parent_scope.clone(), symbols)),
            parent_scope: parent_scope,
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

    fn from_syntax(node: syntax::WhileStatement, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> WhileStatement {
        let this_scope = parent_scope.sub_with_span("while", node.all_span);
        WhileStatement{
            name: node.name.map(|name| LabelDef::from_syntax(name, this_scope.clone(), symbols)),
            loop_expr: Expr::from_syntax(node.loop_expr, this_scope.clone(), symbols),
            body: Block::from_syntax(node.body, this_scope.clone(), symbols),
            this_scope: this_scope,
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

    fn from_syntax(node: syntax::UseStatement, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> UseStatement {
        UseStatement{
            name: Name::from_syntax(node.name, parent_scope.clone(), symbols),
            alias: node.target.map(|target| SimpleName::from_syntax(target, parent_scope.clone(), symbols)),
            parent_scope: parent_scope,
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

    fn from_syntax(node: syntax::ImportStatement, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> ImportStatement {
        ImportStatement{
            name: SimpleName::from_syntax(node.name, parent_scope.clone(), symbols),
            alias: node.target.map(|target| SimpleName::from_syntax(target, parent_scope.clone(), symbols)),
            module: None,
            parent_scope: parent_scope,
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

    fn from_syntax(node: syntax::Statement, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> Statement {
        match node {
            syntax::Statement::Block(block_stmt) => Statement::Block(BlockStatement::from_syntax(block_stmt, parent_scope, symbols)),
            syntax::Statement::SimpleExpr(simple_expr) => Statement::SimpleExpr(SimpleExprStatement::from_syntax(simple_expr, parent_scope, symbols)),
            syntax::Statement::AssignExpr(assign_expr) => Statement::AssignExpr(AssignExprStatement::from_syntax(assign_expr, parent_scope, symbols)),
            syntax::Statement::For(for_stmt) => Statement::For(ForStatement::from_syntax(for_stmt, parent_scope, symbols)),
            syntax::Statement::If(if_stmt) => Statement::If(IfStatement::from_syntax(if_stmt, parent_scope, symbols)),
            syntax::Statement::Break(break_stmt) => Statement::Break(BreakStatement::from_syntax(break_stmt, parent_scope, symbols)),
            syntax::Statement::Continue(continue_stmt) => Statement::Continue(ContinueStatement::from_syntax(continue_stmt, parent_scope, symbols)),
            syntax::Statement::Loop(loop_stmt) => Statement::Loop(LoopStatement::from_syntax(loop_stmt, parent_scope, symbols)),
            syntax::Statement::Return(ret_stmt) => Statement::Return(ReturnStatement::from_syntax(ret_stmt, parent_scope, symbols)),
            syntax::Statement::VarDecl(var_decl) => Statement::VarDecl(VarDecl::from_syntax(var_decl, parent_scope, symbols)),
            syntax::Statement::While(while_stmt) => Statement::While(WhileStatement::from_syntax(while_stmt, parent_scope, symbols)),
            syntax::Statement::Fn(fn_def) => Statement::Fn(FnDef::from_syntax(fn_def, parent_scope, symbols)),
            syntax::Statement::Type(type_def) => Statement::Type(TypeDef::from_syntax(type_def, parent_scope, symbols)),
            syntax::Statement::Use(use_def) => Statement::Use(UseStatement::from_syntax(use_def, parent_scope, symbols)),
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

    fn from_syntax(node: syntax::Item, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> Item {
        match node {
            syntax::Item::Type(type_def) => Item::Type(TypeDef::from_syntax(type_def, parent_scope, symbols)),
            syntax::Item::Fn(fn_def) => Item::Fn(FnDef::from_syntax(fn_def, parent_scope, symbols)),
            syntax::Item::Block(block) => Item::Block(BlockStatement::from_syntax(block, parent_scope, symbols)),
            syntax::Item::SimpleExpr(simple_expr) => Item::SimpleExpr(SimpleExprStatement::from_syntax(simple_expr, parent_scope, symbols)),
            syntax::Item::AssignExpr(assign_expr) => Item::AssignExpr(AssignExprStatement::from_syntax(assign_expr, parent_scope, symbols)),
            syntax::Item::For(for_stmt) => Item::For(ForStatement::from_syntax(for_stmt, parent_scope, symbols)),
            syntax::Item::If(if_stmt) => Item::If(IfStatement::from_syntax(if_stmt, parent_scope, symbols)),
            syntax::Item::Loop(loop_stmt) => Item::Loop(LoopStatement::from_syntax(loop_stmt, parent_scope, symbols)),
            syntax::Item::VarDecl(var_decl) => Item::VarDecl(VarDecl::from_syntax(var_decl, parent_scope, symbols)),
            syntax::Item::While(while_stmt) => Item::While(WhileStatement::from_syntax(while_stmt, parent_scope, symbols)),
            syntax::Item::Use(use_def) => Item::Use(UseStatement::from_syntax(use_def, parent_scope, symbols)),
            syntax::Item::Import(import_def) => Item::Import(ImportStatement::from_syntax(import_def, parent_scope, symbols)),
        }
    }
}