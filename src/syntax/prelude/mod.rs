///! syntax::prelude: node trait and related common functions

// 1. put syntax tree nodes in different 
//    groups expr/stmt/item makes a lot of a lot of 'super::super's
// 2. but put them in src/syntax/nodes will make this directory too large while kind of strange 
//    that src/syntax/nodes contains more than 30 files while src/syntax only contains several files
// 3. so put nodes directly in src/syntax while put common things in this module
// 4. functions in this module is used by nearly all node modules (not node_modules),
//    so this module is directly called prelude and included other common imports from source/diagnostics/lexical

pub use std::fmt;
use crate::source::SourceContext;
pub use crate::source::{Span, IsId};
pub use crate::diagnostics::strings;
pub use crate::lexical::{Token, Numeric, Separator, SeparatorKind, Keyword, KeywordKind};
use super::*;

mod context;
mod node_display;

pub use context::{ParseContext};
pub use node_display::{FormatVisitor, NodeDisplay};

pub trait Node: Sized {

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, _visitor: &mut V) -> Result<T, E>;

    fn walk<T: Default, E, V: Visitor<T, E>>(&self, _visitor: &mut V) -> Result<T, E> { 
        Ok(Default::default())
    }

    fn display<'n, 'scx, F>(&'n self, scx: &'scx SourceContext<F>) -> NodeDisplay<'n, 'scx, Self, F> {
        NodeDisplay(self, scx)
    }
}

/// unrecoverable unexpected for this parser, detail in diagnostics
// this should be more readable than previous Result<Self::Output, ()>
#[derive(Debug)]
pub struct Unexpected;

pub trait Parser: Sized {
    type Output: Node;

    fn matches(_current: &Token) -> bool {
        return false;
    }
    fn matches3(_current: &Token, _peek1: &Token, _peek2: &Token) -> bool {
        return false;
    }

    fn parse(_cx: &mut ParseContext) -> Result<Self::Output, Unexpected>;

    // check matches_first, if pass, parse, return Ok(Some(T)) or Err(Unexpected), else return None
    fn try_parse(cx: &mut ParseContext) -> Result<Option<Self::Output>, Unexpected> {
        Ok(if Self::matches(&cx.current) || Self::matches3(&cx.current, &cx.peek, &cx.peek2) { Some(cx.expect::<Self>()?) } else { None })
    }
}

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
    fn visit_array_type(&mut self, node: &ArrayType) -> Result<T, E> { node.walk(self) }
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
    fn visit_fn_type(&mut self, node: &FnType) -> Result<T, E> { node.walk(self) }
    fn visit_fn_type_param(&mut self, node: &FnTypeParam) -> Result<T, E> { node.walk(self) }
    fn visit_for_stmt(&mut self, node: &ForStatement) -> Result<T, E> { node.walk(self) }
    fn visit_if_stmt(&mut self, node: &IfStatement) -> Result<T, E> { node.walk(self) }
    fn visit_if_clause(&mut self, node: &IfClause) -> Result<T, E> { node.walk(self) }
    fn visit_else_if_clause(&mut self, node: &ElseIfClause) -> Result<T, E> { node.walk(self) }
    fn visit_else_clause(&mut self, node: &ElseClause) -> Result<T, E> { node.walk(self) }
    fn visit_index_call_expr(&mut self, node: &IndexCallExpr) -> Result<T, E> { node.walk(self) }
    fn visit_break_stmt(&mut self, node: &BreakStatement) -> Result<T, E> { node.walk(self) }
    fn visit_continue_stmt(&mut self, node: &ContinueStatement) -> Result<T, E> { node.walk(self) }
    fn visit_label_def(&mut self, node: &LabelDef) -> Result<T, E> { node.walk(self) }
    fn visit_lit_expr(&mut self, node: &LitExpr) -> Result<T, E> { node.walk(self) }
    fn visit_loop_stmt(&mut self, node: &LoopStatement) -> Result<T, E> { node.walk(self) }
    fn visit_member_access(&mut self, node: &MemberAccessExpr) -> Result<T, E> { node.walk(self) }
    fn visit_module(&mut self, node: &Module) -> Result<T, E> { node.walk(self) }
    fn visit_module_stmt(&mut self, node: &ModuleStatement) -> Result<T, E> { node.walk(self) }
    fn visit_name(&mut self, node: &Name) -> Result<T, E> { node.walk(self) }
    fn visit_name_segment(&mut self, node: &NameSegment) -> Result<T, E> { node.walk(self) }
    fn visit_plain_type(&mut self, node: &PlainType) -> Result<T, E> { node.walk(self) }
    fn visit_type_segment(&mut self, node: &TypeSegment) -> Result<T, E> { node.walk(self) }
    fn visit_type_as_segment(&mut self, node: &TypeAsSegment) -> Result<T, E> { node.walk(self) }
    fn visit_range_both_expr(&mut self, node: &RangeBothExpr) -> Result<T, E> { node.walk(self) }
    fn visit_range_full_expr(&mut self, node: &RangeFullExpr) -> Result<T, E> { node.walk(self) }
    fn visit_range_left_expr(&mut self, node: &RangeLeftExpr) -> Result<T, E> { node.walk(self) }
    fn visit_range_right_expr(&mut self, node: &RangeRightExpr) -> Result<T, E> { node.walk(self) }
    fn visit_ref_type(&mut self, node: &RefType) -> Result<T, E> { node.walk(self) }
    fn visit_ret_stmt(&mut self, node: &ReturnStatement) -> Result<T, E> { node.walk(self) }
    fn visit_stmt(&mut self, node: &Statement) -> Result<T, E> { node.walk(self) }
    fn visit_item(&mut self, node: &Item) -> Result<T, E> { node.walk(self) } 
    fn visit_paren_expr(&mut self, node: &ParenExpr) -> Result<T, E> { node.walk(self) }
    fn visit_primitive_type(&mut self, node: &PrimitiveType) -> Result<T, E> { node.walk(self) }
    fn visit_tuple_def(&mut self, node: &TupleDef) -> Result<T, E> { node.walk(self) }
    fn visit_tuple_type(&mut self, node: &TupleType) -> Result<T, E> { node.walk(self) }
    fn visit_type_def(&mut self, node: &TypeDef) -> Result<T, E> { node.walk(self) }
    fn visit_type_field_def(&mut self, node: &TypeFieldDef) -> Result<T, E> { node.walk(self) }
    fn visit_type_ref(&mut self, node: &TypeRef) -> Result<T, E> { node.walk(self) }
    fn visit_unary_expr(&mut self, node: &UnaryExpr) -> Result<T, E> { node.walk(self) }
    fn visit_use_stmt(&mut self, node: &UseStatement) -> Result<T, E> { node.walk(self) }
    fn visit_var_decl(&mut self, node: &VarDeclStatement) -> Result<T, E> { node.walk(self) }
    fn visit_while_stmt(&mut self, node: &WhileStatement) -> Result<T, E> { node.walk(self) }
}

#[cfg(test)]
pub fn ast_test_case<
    O: PartialEq + Node + fmt::Debug,
    N: Parser<Output = O>,
>(
    input: &'static str,
    expect_node: O, 
    expect_diagnostics: crate::diagnostics::Diagnostics, 
    expect_strings: &[&'static str], 
    backtrace: u32,
) {
    use std::fmt::Write;
    let mut actual_diagnostics = crate::diagnostics::Diagnostics::new();
    let mut source = SourceContext::new_file_system(crate::source::VirtualFileSystem {
        cwd: "/".into(),
        files: [("1".into(), input.into())].into_iter().collect(),
    });
    let mut context = ParseContext::new(crate::lexical::Parser::new(source.entry("1"), &mut actual_diagnostics));
    if let Ok(actual_node) = N::parse(&mut context) {
        context.finish();
        // for now does not check expect strings not provided, but ideally should always check interned strings
        if expect_strings.len() > 0 {
            // put interned string expect before node expect and diagnostics expect, or else it is too hard to fix expected values
            for (i, expect_string) in expect_strings.into_iter().enumerate() {
                assert_eq!(source.resolve_string(IsId::new(i as u32 + 2)), *expect_string, "line {} string #{} not same", backtrace, i + 2);
            }
        }
        if actual_node == expect_node {
            if actual_diagnostics == expect_diagnostics {
                // finally success
            } else {
                let mut buf = format!("line {} diagnostics not same\n", backtrace);
                write!(buf, "{}", actual_diagnostics.display(&source)).unwrap();
                write!(buf, "{}", expect_diagnostics.display(&source)).unwrap();
                panic!("{}", buf)
            }
        } else {
            let (actual_display, expect_display) = (actual_node.display(&source).to_string(), expect_node.display(&source).to_string());
            if actual_display == expect_display {
                panic!("line {} node not same while display is same\n{}\n{:?}\n{:?}\n", backtrace, actual_display, actual_node, expect_node);
            }

            let mut buf = format!("line {} node not same\n", backtrace);
            let (actual_lines, expect_lines) = (actual_display.lines().collect::<Vec<_>>(), expect_display.lines().collect::<Vec<_>>());
            let common_line_count = std::cmp::min(actual_lines.len(), expect_lines.len());
            for line in 0..common_line_count {
                if actual_lines[line] != expect_lines[line] {
                    write!(buf, "{: >3} |- {}\n", line + 1, actual_lines[line]).unwrap();
                    write!(buf, "    |+ {}\n", expect_lines[line]).unwrap();
                } else {
                    write!(buf, "{: >3} |  {}\n", line + 1, actual_lines[line]).unwrap();
                }
            }
            if actual_lines.len() > common_line_count {
                for line in common_line_count..actual_lines.len() {
                    write!(buf, "{: >3} |- {}\n", line + 1, actual_lines[line]).unwrap();
                }
            }
            if expect_lines.len() > common_line_count {
                for line in common_line_count..expect_lines.len() {
                    write!(buf, "{: >3} |+ {}\n", line + 1, expect_lines[line]).unwrap();
                }
            }
            panic!("{}", buf)
        }
    } else {
        context.finish();
        panic!("line {} parse failed\n{}", backtrace, actual_diagnostics.display(&source))
    }
}

#[cfg(test)]
macro_rules! case {
    ($code:literal as $ty:ty, $expect:expr $(,)?) => (
        ast_test_case::<_, $ty>($code, $expect, crate::diagnostics::make_errors!(), &[], line!());
    );
    ($code:literal as $ty:ty, $expect:expr, errors $expect_diagnostics:expr $(,)?) => (
        ast_test_case::<_, $ty>($code, $expect, $expect_diagnostics, &[], line!());
    );
    ($code:literal as $ty:ty, $expect:expr, strings $expect_strings:expr $(,)?) => (
        ast_test_case::<_, $ty>($code, $expect, crate::diagnostics::make_errors![], &$expect_strings, line!());
    );
    ($code:literal as $ty:ty, $expect:expr, errors $expect_diagnostics:expr, strings $expect_strings:expr $(,)?) => (
        ast_test_case::<_, $ty>($code, $expect, $expect_diagnostics, &$expect_strings, line!());
    );
}

#[cfg(test)]
pub(crate) use case;
#[cfg(test)]
pub(crate) use crate::diagnostics::make_errors;
#[cfg(test)]
pub(crate) use super::expr::{make_lit, make_expr};
#[cfg(test)]
pub(crate) use super::expr_list::make_exprs;
#[cfg(test)]
pub(crate) use super::name::make_name;
#[cfg(test)]
pub(crate) use super::plain_type::make_type;
