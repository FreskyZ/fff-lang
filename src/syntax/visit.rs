///! syntax::visit: visitor and visitee traits

use crate::source::SourceContext;
use super::pretty::NodeDisplay;
use super::ast::*;

pub trait Node: Sized {

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, _visitor: &mut V) -> Result<T, E>;

    fn walk<T: Default, E, V: Visitor<T, E>>(&self, _visitor: &mut V) -> Result<T, E> { 
        Ok(Default::default())
    }

    // this should be some trait NodeDisplay: Node in theory but directly put this here is convenient
    fn display<'n, 'scx, F>(&'n self, context: &'scx SourceContext<F>) -> NodeDisplay<'n, 'scx, Self, F> {
        NodeDisplay{ node: self, context }
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
// TODO: use Try when stablized, see branch try-v2 // will I still work on this project at that time?
pub trait Visitor<T: Default = (), E = ()>: Sized {
    fn visit_array_def(&mut self, node: &ArrayDef) -> Result<T, E> { node.walk(self) }
    fn visit_array_type(&mut self, node: &ArrayType) -> Result<T, E> { node.walk(self) }
    fn visit_binary_expr(&mut self, node: &BinaryExpr) -> Result<T, E> { node.walk(self) }
    fn visit_block_stmt(&mut self, node: &BlockStatement) -> Result<T, E> { node.walk(self) }
    fn visit_block(&mut self, node: &Block) -> Result<T, E> { node.walk(self) }
    fn visit_enum_def(&mut self, node: &EnumDef) -> Result<T, E> { node.walk(self) }
    fn visit_enum_variant(&mut self, node: &EnumVariant) -> Result<T, E> { node.walk(self) }
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
    fn visit_object_literal(&mut self, node: &ObjectLiteral) -> Result<T, E> { node.walk(self) }
    fn visit_object_literal_field(&mut self, node: &ObjectLiteralField) -> Result<T, E> { node.walk(self) }
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
