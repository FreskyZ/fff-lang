///! fff-lang
///!
///! semantic/statement

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
    pub this_scope: SharedDefScope,
}
impl ISemanticAnalyze for BlockStatement {

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("block-stmt").space().debug(&self.this_scope).endl()
            .map_or_else(&self.name, |f, name| f.apply1_with_header_text("loop-name", name), |f| f.indent1().lit("no-loop-name"))
            .apply1_with_header_text("body", &self.body)
            .finish()
    }

    type SyntaxItem = syntax::BlockStatement;

    fn from_syntax(node: syntax::BlockStatement, sess: FromSession) -> BlockStatement {
        let this_sess = sess.sub_with_span("block", node.all_span);
        BlockStatement{
            name: node.name.map(|name| LabelDef::from_syntax(name, this_sess.clone_scope())),
            body: Block::from_syntax(node.body, sess.clone_scope()),
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

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("expr-stmt simple").space().debug(&self.this_scope).endl().apply1(&self.expr).finish()
    }

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

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("expr-stmt assign").space().debug(&self.this_scope).endl()
            .apply1_with_prefix_text("left-is", &self.left_expr).endl()
            .indent1().lit("\"").debug(&self.assign_op).lit("\"").endl()
            .apply1_with_prefix_text("right-is", &self.right_expr)
            .finish()
    }

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

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("for-stmt").space().debug(&self.this_scope).endl()
            .map_or_else(&self.loop_name, |f, name| f.apply1_with_header_text("loop-name", name), |f| f.indent1().lit("no-loop-name")).endl()
            .indent1().lit("iter-var").space().sym(self.iter_name).endl()
            .apply1_with_prefix_text("iter-expr-is", &self.iter_expr).endl()
            .apply1_with_header_text("body", &self.body)
            .finish()
    }

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

    fn format(&self, f: Formatter) -> String {

        f.indent().header_text_or("if-stmt").endl()
            .indent1().lit("if-clause").space().debug(&self.if_clause.this_scope).endl()
            .apply2_with_prefix_text("cond-expr-is", &self.if_clause.cond_expr).endl()
            .apply2_with_header_text("body", &self.if_clause.body)
            .foreach(&self.elseif_clauses, |f, &ElseIfClause{ ref cond_expr, ref body, ref this_scope }| f.endl()
                .indent1().lit("else-if-clause").space().debug(this_scope).endl()
                .apply2_with_prefix_text("cond-expr-is", cond_expr)
                .apply2_with_header_text("body", body))
            .map_or_else(&self.else_clause, |f, &ElseClause{ ref body, ref this_scope }| f.endl()
                .indent1().lit("else-clause").space().debug(this_scope).endl()
                .apply2_with_header_text("body", body), |f| f)
            .finish()
    }

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

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("break-stmt").space().debug(&self.parent_scope).endl()
            .map_or_else(&self.target, |f, target| f.indent1().lit("to").space().lit("@").sym(*target), |f| f.indent1().lit("default-target"))
            .finish()
    }

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

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("continue-stmt").space().debug(&self.parent_scope).endl()
            .map_or_else(&self.target, |f, target| f.indent1().lit("to").space().lit("@").sym(*target), |f| f.indent1().lit("default-target"))
            .finish()
    }

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

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("loop-stmt").space().debug(&self.this_scope).endl()
            .map_or_else(&self.name, |f, name| f.apply1_with_header_text("loop-name", name), |f| f.indent1().lit("no-loop-name")).endl()
            .apply1_with_header_text("body", &self.body)
            .finish()
    }

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

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("return-stmt").space().debug(&self.parent_scope)
            .map_or_else(&self.expr, |f, expr| f.endl().apply1_with_prefix_text("return-value-is", expr), |f| f.indent1().lit("return-unit"))
            .finish()
    }

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

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("var-decl").space().debug(&self.parent_scope).endl()
            .indent1().lit(if self.is_const { "const" } else { "var" }).space().sym(self.name).endl()
            .map_or_else(&self.typeuse, |f, typeuse| f.apply1_with_header_text("declared-as", typeuse), |f| f.indent1().lit("declared-as-auto-type")).endl()
            .map_or_else(&self.init_expr, |f, expr| f.apply1_with_prefix_text("init-as", expr), |f| f.indent1().lit("no-init-expr"))
            .finish()
    }

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
    
    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("while-stmt").space().debug(&self.this_scope).endl()
            .map_or_else(&self.name, |f, name| f.apply1_with_header_text("loop-name", name), |f| f.indent1().lit("no-loop-name")).endl()
            .apply1_with_prefix_text("loop-expr-is", &self.loop_expr).endl()
            .apply1_with_header_text("body", &self.body)
            .finish()
    }

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
        f.indent().header_text_or("import-stmt").space().debug(&self.parent_scope).endl()
            .apply1(&self.name)
            .map_or_else(&self.alias, |f, alias| f.endl().apply1_with_header_text("alias-as", alias), |f| f)
            .map_or_else(&self.module, |f, module| f.endl().apply1(module), |f| f.endl().indent1().lit("<no-module>"))
            .finish()
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

    fn format(&self, f: Formatter) -> String {
        match self {
            &Statement::VarDecl(ref var_decl) => var_decl.format(f),
            &Statement::SimpleExpr(ref simple_expr) => simple_expr.format(f),
            &Statement::AssignExpr(ref assign_expr) => assign_expr.format(f),
            &Statement::For(ref for_stmt) => for_stmt.format(f),
            &Statement::Loop(ref loop_stmt) => loop_stmt.format(f),
            &Statement::If(ref if_stmt) => if_stmt.format(f),
            &Statement::Break(ref break_stmt) => break_stmt.format(f),
            &Statement::Continue(ref continue_stmt) => continue_stmt.format(f),
            &Statement::Return(ref ret_stmt) => ret_stmt.format(f),
            &Statement::While(ref while_stmt) => while_stmt.format(f),
            &Statement::Block(ref block_stmt) => block_stmt.format(f),
            _ => "<unknown_stmt>".to_owned(),
        }
    }

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
            &Item::VarDecl(ref var_decl) => var_decl.format(f),
            &Item::SimpleExpr(ref simple_expr) => simple_expr.format(f),
            &Item::AssignExpr(ref assign_expr) => assign_expr.format(f),
            &Item::For(ref for_stmt) => for_stmt.format(f),
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