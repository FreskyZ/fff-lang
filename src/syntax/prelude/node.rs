///! syntax::node: syntax tree node trait

use crate::source::FileSystem;
use crate::lexical::Token;
use super::context::{ParseSession, ParseResult};
use super::super::*;

// default visit implementation accepts self
// the difference between visit and accept is that only accept is allowed to call visit methods
// visit methods itself it logically theoritically only about node information not about tree structure
pub trait Visitor: Sized {
    fn visit_array_def(&mut self, node: &ArrayDef) { node.walk(self) }
    fn visit_binary_expr(&mut self, node: &BinaryExpr) { node.walk(self) }
    fn visit_block_stmt(&mut self, node: &BlockStatement) { node.walk(self) }
    fn visit_expr_list(&mut self, node: &ExprList) { node.walk(self) }
    fn visit_expr(&mut self, node: &Expr) { node.walk(self) }
    fn visit_assign_expr_stmt(&mut self, node: &AssignExprStatement) { node.walk(self) }
    fn visit_simple_expr_stmt(&mut self, node: &SimpleExprStatement) { node.walk(self) }
    fn visit_fn_call_expr(&mut self, node: &FnCallExpr) { node.walk(self) }
    fn visit_fn_def(&mut self, node: &FnDef) { node.walk(self); }
    fn visit_for_stmt(&mut self, node: &ForStatement) { node.walk(self) }
    fn visit_if_stmt(&mut self, node: &IfStatement) { node.walk(self) }
    fn visit_import_stmt(&mut self, node: &ImportStatement) { node.walk(self) }
    fn visit_index_call(&mut self, node: &IndexCallExpr) { node.walk(self) }
    fn visit_break_stmt(&mut self, node: &BreakStatement) { node.walk(self) }
    fn visit_continue_stmt(&mut self, node: &ContinueStatement) { node.walk(self) }
    fn visit_label_def(&mut self, node: &LabelDef) { node.walk(self) }
    fn visit_lit_expr(&mut self, node: &LitExpr) { node.walk(self) }
    fn visit_loop_stmt(&mut self, node: &LoopStatement) { node.walk(self) }
    fn visit_member_access(&mut self, node: &MemberAccessExpr) { node.walk(self) }
    fn visit_module(&mut self, node: &Module) { node.walk(self) }
    fn visit_name(&mut self, node: &Name) { node.walk(self) }
    fn visit_simple_name(&mut self, node: &SimpleName) { node.walk(self) }
    fn visit_range_both_expr(&mut self, node: &RangeBothExpr) { node.walk(self) }
    fn visit_range_full_expr(&mut self, node: &RangeFullExpr) { node.walk(self) }
    fn visit_range_left_expr(&mut self, node: &RangeLeftExpr) { node.walk(self) }
    fn visit_range_right_expr(&mut self, node: &RangeRightExpr) { node.walk(self) }
    fn visit_ret_stmt(&mut self, node: &ReturnStatement) { node.walk(self) }
    fn visit_stmt(&mut self, node: &Statement) { node.walk(self) }
    fn visit_paren_expr(&mut self, node: &ParenExpr) { node.walk(self) }
    fn visit_tuple_def(&mut self, node: &TupleDef) { node.walk(self) }
    fn visit_type_def(&mut self, node: &TypeDef) { node.walk(self) }
    fn visit_type_use(&mut self, node: &TypeUse) { node.walk(self) }
    fn visit_unary_expr(&mut self, node: &UnaryExpr) { node.walk(self) }
    fn visit_use_stmt(&mut self, node: &UnaryExpr) { node.walk(self) }
    fn visit_var_decl(&mut self, node: &VarDeclStatement) { node.walk(self) }
    fn visit_while_stmt(&mut self, node: &WhileStatement) { node.walk(self) }
}

pub trait Node {
    type ParseOutput;
    
    fn matches(_current: &Token) -> bool {
        return false;
    }
    fn matches3(_current: &Token, _peek1: &Token, _peek2: &Token) -> bool {
        return false;
    }

    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<Self::ParseOutput>;

    // check matches_first, if pass, parse, return Ok(Some(T)) or Err(()), else return None
    fn try_parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<Option<Self::ParseOutput>> {
        Ok(if Self::matches(&sess.current) || Self::matches3(&sess.current, &sess.peek, &sess.peek2) { Some(Self::parse(sess)?) } else { None })
    }

    // default accept implementation does nothing
    fn walk<V: Visitor>(&self, _visitor: &mut V) { }
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
        let mut pcx = ParseSession::new(lcx);
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
        let mut pcx = ParseSession::new(lcx);
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
        let mut pcx = ParseSession::new(lcx);
        let node = <$ty>::parse(&mut pcx).expect("failed to parse test input");
        assert_eq!(ecx, crate::diagnostics::make_errors!());
        node
    }};
    // TODO change span string and value string to be expected value not prepare value
    ($code:literal as $ty:ty, [$($span_string:expr),*$(,)?], [$($value_string:expr),*$(,)?], and messages) => {{
        let mut ecx = crate::diagnostics::MessageCollection::new();
        let mut scx = crate::source::make_source!($code);
        let mut chars = scx.entry("1");
        $( chars.intern_span($span_string); )*
        $( chars.intern($value_string); )*
        let lcx = crate::lexical::Parser::new(chars, &mut ecx);
        let mut pcx = ParseSession::new(lcx);
        let node = <$ty>::parse(&mut pcx).expect("failed to parse test input");
        (node, ecx)
    }};
}
#[cfg(test)]
pub(crate) use make_node;
