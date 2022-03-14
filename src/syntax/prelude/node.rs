///! syntax::node: syntax tree node trait

use crate::source::{SourceContext, FileSystem};
use crate::lexical::Token;
use super::context::{ParseSession, ParseResult};
use super::display::NodeDisplay;
use super::super::*;

// visitor visits node
// - each visit method visits specified node type and do different things to collect information (not alter, at least this trait)
//   after that, Node::walk is called to let node itself guide visitor to child nodes, this decouples visitor implementation with tree structure
// - on the other hand, Node::accept implementation should call this node's matching visit_* method without dothing other things
//   only then can you visit an arbitray 'impl Node' type, or 'dyn Node' if it is object safe (but not this Node)
// - according to this design
//   - default visit implementation walks self
//   - default walk implementation does nothing
//   - no default accept implementation
// - the return type is Result, because Node::walk normally need to call visit methods multiple times,
//   so there should be some way to "aggregate" them, and Result (Try) is one confortable way to do that
//   if result is some other type and use other aggregate method, like adding numeric value, use only T for that
//   if result is not care, just use default (unit) for type parameter T and E
// TODO: use Try when stablized // will I still work on this project at that time?
pub trait Visitor<T: Default = (), E = ()>: Sized {
    fn visit_array_def(&mut self, node: &ArrayDef) -> Result<T, E> { node.walk(self) }
    fn visit_binary_expr(&mut self, node: &BinaryExpr) -> Result<T, E> { node.walk(self) }
    fn visit_block_stmt(&mut self, node: &BlockStatement) -> Result<T, E> { node.walk(self) }
    fn visit_block(&mut self, node: &Block) -> Result<T, E> { node.walk(self) }
    fn visit_expr_list(&mut self, node: &ExprList) -> Result<T, E> { node.walk(self) }
    fn visit_expr(&mut self, node: &Expr) -> Result<T, E> { node.walk(self) }
    fn visit_assign_expr_stmt(&mut self, node: &AssignExprStatement) -> Result<T, E> { node.walk(self) }
    fn visit_simple_expr_stmt(&mut self, node: &SimpleExprStatement) -> Result<T, E> { node.walk(self) }
    fn visit_fn_call_expr(&mut self, node: &FnCallExpr) -> Result<T, E> { node.walk(self) }
    fn visit_fn_def(&mut self, node: &FnDef) -> Result<T, E> { node.walk(self) }
    fn visit_fn_param(&mut self, node: &FnParam) -> Result<T, E> { node.walk(self) }
    fn visit_for_stmt(&mut self, node: &ForStatement) -> Result<T, E> { node.walk(self) }
    fn visit_if_stmt(&mut self, node: &IfStatement) -> Result<T, E> { node.walk(self) }
    fn visit_if_clause(&mut self, node: &IfClause) -> Result<T, E> { node.walk(self) }
    fn visit_else_if_clause(&mut self, node: &ElseIfClause) -> Result<T, E> { node.walk(self) }
    fn visit_else_clause(&mut self, node: &ElseClause) -> Result<T, E> { node.walk(self) }
    fn visit_import_stmt(&mut self, node: &ImportStatement) -> Result<T, E> { node.walk(self) }
    fn visit_index_call_expr(&mut self, node: &IndexCallExpr) -> Result<T, E> { node.walk(self) }
    fn visit_break_stmt(&mut self, node: &BreakStatement) -> Result<T, E> { node.walk(self) }
    fn visit_continue_stmt(&mut self, node: &ContinueStatement) -> Result<T, E> { node.walk(self) }
    fn visit_label_def(&mut self, node: &LabelDef) -> Result<T, E> { node.walk(self) }
    fn visit_lit_expr(&mut self, node: &LitExpr) -> Result<T, E> { node.walk(self) }
    fn visit_loop_stmt(&mut self, node: &LoopStatement) -> Result<T, E> { node.walk(self) }
    fn visit_member_access(&mut self, node: &MemberAccessExpr) -> Result<T, E> { node.walk(self) }
    fn visit_module(&mut self, node: &Module) -> Result<T, E> { node.walk(self) }
    fn visit_name(&mut self, node: &Name) -> Result<T, E> { node.walk(self) }
    fn visit_simple_name(&mut self, node: &SimpleName) -> Result<T, E> { node.walk(self) }
    fn visit_range_both_expr(&mut self, node: &RangeBothExpr) -> Result<T, E> { node.walk(self) }
    fn visit_range_full_expr(&mut self, node: &RangeFullExpr) -> Result<T, E> { node.walk(self) }
    fn visit_range_left_expr(&mut self, node: &RangeLeftExpr) -> Result<T, E> { node.walk(self) }
    fn visit_range_right_expr(&mut self, node: &RangeRightExpr) -> Result<T, E> { node.walk(self) }
    fn visit_ret_stmt(&mut self, node: &ReturnStatement) -> Result<T, E> { node.walk(self) }
    fn visit_stmt(&mut self, node: &Statement) -> Result<T, E> { node.walk(self) }
    fn visit_item(&mut self, node: &Item) -> Result<T, E> { node.walk(self) } 
    fn visit_paren_expr(&mut self, node: &ParenExpr) -> Result<T, E> { node.walk(self) }
    fn visit_tuple_def(&mut self, node: &TupleDef) -> Result<T, E> { node.walk(self) }
    fn visit_type_def(&mut self, node: &TypeDef) -> Result<T, E> { node.walk(self) }
    fn visit_type_field_def(&mut self, node: &TypeFieldDef) -> Result<T, E> { node.walk(self) }
    fn visit_type_use(&mut self, node: &TypeUse) -> Result<T, E> { node.walk(self) }
    fn visit_unary_expr(&mut self, node: &UnaryExpr) -> Result<T, E> { node.walk(self) }
    fn visit_use_stmt(&mut self, node: &UseStatement) -> Result<T, E> { node.walk(self) }
    fn visit_var_decl(&mut self, node: &VarDeclStatement) -> Result<T, E> { node.walk(self) }
    fn visit_while_stmt(&mut self, node: &WhileStatement) -> Result<T, E> { node.walk(self) }
}

pub trait Node: Sized {
    type ParseOutput;
    
    fn matches(_current: &Token) -> bool {
        return false;
    }
    fn matches3(_current: &Token, _peek1: &Token, _peek2: &Token) -> bool {
        return false;
    }

    // some nodes are parsed by other nodes not self
    fn parse<F: FileSystem>(_sess: &mut ParseSession<F>) -> ParseResult<Self::ParseOutput> {
        Err(())
    }

    // check matches_first, if pass, parse, return Ok(Some(T)) or Err(()), else return None
    fn try_parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<Option<Self::ParseOutput>> {
        Ok(if Self::matches(&sess.current) || Self::matches3(&sess.current, &sess.peek, &sess.peek2) { Some(Self::parse(sess)?) } else { None })
    }

    // some nodes are only parsing proxy and not actually on result tree
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, _visitor: &mut V) -> Result<T, E> {
        Ok(Default::default())
    }

    fn walk<T: Default, E, V: Visitor<T, E>>(&self, _visitor: &mut V) -> Result<T, E> { 
        Ok(Default::default())
    }

    fn display<'n, 'scx, F>(&'n self, scx: &'scx SourceContext<F>) -> NodeDisplay<'n, 'scx, Self, F> {
        NodeDisplay(self, scx)
    }
}

#[cfg(test)]
macro_rules! make_node {
    ($code:literal) => {{
        let mut ecx = crate::diagnostics::make_errors!();
        let mut scx = crate::source::make_source!($code);
        let chars = scx.entry("1");
        let lcx = crate::lexical::Parser::new(chars, &mut ecx);
        let mut pcx = ParseSession::new(lcx);
        let node = crate::syntax::Module::parse(&mut pcx).expect("failed to parse test input");
        assert_eq!(ecx, crate::diagnostics::make_errors!());
        node
    }};
    ($code:literal as $ty:ty) => {{
        let mut ecx = crate::diagnostics::make_errors!();
        let mut scx = crate::source::make_source!($code);
        let chars = scx.entry("1");
        let lcx = crate::lexical::Parser::new(chars, &mut ecx);
        let mut pcx = ParseSession::new(lcx);
        let node = <$ty>::parse(&mut pcx).expect("failed to parse test input");
        assert_eq!(ecx, crate::diagnostics::make_errors!());
        node
    }};
    // TODO change span string and value string to be expected value not prepare value
    ($code:literal as $ty:ty, and messages) => {{
        let mut ecx = crate::diagnostics::make_errors!();
        let mut scx = crate::source::make_source!($code);
        let chars = scx.entry("1");
        let lcx = crate::lexical::Parser::new(chars, &mut ecx);
        let mut pcx = crate::syntax::prelude::ParseSession::new(lcx);
        let node = <$ty>::parse(&mut pcx).expect("failed to parse test input");
        (node, ecx)
    }};
    // TODO change span string and value string to be expected value not prepare value
    ($code:literal as $ty:ty, [$($span_string:expr),*$(,)?]) => {{
        let mut ecx = crate::diagnostics::make_errors!();
        let mut scx = crate::source::make_source!($code);
        let mut chars = scx.entry("1");
        $( chars.intern_span($span_string); )*
        let lcx = crate::lexical::Parser::new(chars, &mut ecx);
        let mut pcx = crate::syntax::prelude::ParseSession::new(lcx);
        let node = <$ty>::parse(&mut pcx).expect("failed to parse test input");
        assert_eq!(ecx, crate::diagnostics::make_errors!());
        node
    }};
    // TODO change span string and value string to be expected value not prepare value
    ($code:literal as $ty:ty, [$($span_string:expr),*$(,)?], [$($value_string:expr),*$(,)?]) => {{
        let mut ecx = crate::diagnostics::make_errors!();
        let mut scx = crate::source::make_source!($code);
        let mut chars = scx.entry("1");
        $( chars.intern_span($span_string); )*
        $( chars.intern($value_string); )*
        let lcx = crate::lexical::Parser::new(chars, &mut ecx);
        let mut pcx = crate::syntax::prelude::ParseSession::new(lcx);
        let node = <$ty>::parse(&mut pcx).expect("failed to parse test input");
        assert_eq!(ecx, crate::diagnostics::make_errors!());
        node
    }};
    // TODO change span string and value string to be expected value not prepare value
    ($code:literal as $ty:ty, [$($span_string:expr),*$(,)?], [$($value_string:expr),*$(,)?], and source) => {{
        let mut ecx = crate::diagnostics::make_errors!();
        let mut scx = crate::source::make_source!($code);
        let mut chars = scx.entry("1");
        $( chars.intern_span($span_string); )*
        $( chars.intern($value_string); )*
        let lcx = crate::lexical::Parser::new(chars, &mut ecx);
        let mut pcx = crate::syntax::prelude::ParseSession::new(lcx);
        let node = <$ty>::parse(&mut pcx).expect("failed to parse test input");
        pcx.base.finish();
        assert_eq!(ecx, crate::diagnostics::make_errors!());
        (node, scx)
    }};
    // TODO change span string and value string to be expected value not prepare value
    ($code:literal as $ty:ty, [$($span_string:expr),*$(,)?], [$($value_string:expr),*$(,)?], and messages) => {{
        let mut ecx = crate::diagnostics::MessageCollection::new();
        let mut scx = crate::source::make_source!($code);
        let mut chars = scx.entry("1");
        $( chars.intern_span($span_string); )*
        $( chars.intern($value_string); )*
        let lcx = crate::lexical::Parser::new(chars, &mut ecx);
        let mut pcx = crate::syntax::prelude::ParseSession::new(lcx);
        let node = <$ty>::parse(&mut pcx).expect("failed to parse test input");
        (node, ecx)
    }};
}
#[cfg(test)]
pub(crate) use make_node;
