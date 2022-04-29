///! ---------------------------------------------------------------------------------
///! This code is auto generated by a tool $repo/scripts/ast.py
///! Changes may cause incorrect behavior and will be lost if the code is regenerated.
///! ---------------------------------------------------------------------------------

///! impl Visit for each node

use super::super::visit::{Visit, Visitor};
use super::*;

impl<'a> Visit for ArrayExpr<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_array_expr(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        for item in arena.get_iter(&self.items) {
            v.visit_expr(item.as_repr().as_ref(), arena)?;
        }
        Default::default()
    }
}

impl<'a> Visit for ArrayIndexExpr<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_array_index_expr(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_expr(self.base.as_repr().as_ref(), arena)?;
        for parameter in arena.get_iter(&self.parameters) {
            v.visit_expr(parameter.as_repr().as_ref(), arena)?;
        }
        Default::default()
    }
}

impl<'a> Visit for ArrayType<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_array_type(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_type_ref(self.base.as_repr().as_ref(), arena)?;
        v.visit_expr(self.size.as_repr().as_ref(), arena)
    }
}

impl<'a> Visit for AssignExprStatement<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_assign_expr_stmt(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_expr(self.left.as_repr().as_ref(), arena)?;
        v.visit_expr(self.right.as_repr().as_ref(), arena)
    }
}

impl<'a> Visit for BinaryExpr<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_binary_expr(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_expr(self.left.as_repr().as_ref(), arena)?;
        v.visit_expr(self.right.as_repr().as_ref(), arena)
    }
}

impl<'a> Visit for Block<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_block(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        for item in arena.get_iter(&self.items) {
            v.visit_stmt(item.as_repr().as_ref(), arena)?;
        }
        Default::default()
    }
}

impl<'a> Visit for BlockStatement<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_block_stmt(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_block(arena.get(&self.body), arena)
    }
}

impl Visit for BreakStatement {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_break_stmt(self, arena)
    }
}

impl<'a> Visit for CallExpr<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_call_expr(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_expr(self.base.as_repr().as_ref(), arena)?;
        for parameter in arena.get_iter(&self.parameters) {
            v.visit_expr(parameter.as_repr().as_ref(), arena)?;
        }
        Default::default()
    }
}

impl<'a> Visit for CastSegment<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_cast_segment(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_type_ref(self.left.as_repr().as_ref(), arena)?;
        v.visit_type_ref(self.right.as_repr().as_ref(), arena)
    }
}

impl<'a> Visit for ClassDef<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_class_def(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_generic_name(arena.get(&self.name), arena)?;
        for item in arena.get_iter(&self.items) {
            v.visit_item(item.as_repr().as_ref(), arena)?;
        }
        Default::default()
    }
}

impl Visit for ContinueStatement {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_continue_stmt(self, arena)
    }
}

impl<'a> Visit for ElseClause<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_else_clause(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_block(arena.get(&self.body), arena)
    }
}

impl<'a> Visit for EnumDef<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_enum_def(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        if let Some(base_type) = &self.base_type {
            v.visit_primitive_type(arena.get(base_type), arena)?;
        }
        for variant in arena.get_iter(&self.variants) {
            v.visit_enum_def_variant(variant, arena)?;
        }
        Default::default()
    }
}

impl<'a> Visit for EnumDefVariant<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_enum_def_variant(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        if let Some(value) = &self.value {
            v.visit_expr(value.as_repr().as_ref(), arena)?;
        }
        Default::default()
    }
}

impl<'a> Visit for Expr<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_expr(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        match self {
            Expr::Lit(n) => v.visit_lit_expr(arena.get(n), arena),
            Expr::Path(n) => v.visit_path(arena.get(n), arena),
            Expr::Paren(n) => v.visit_paren_expr(arena.get(n), arena),
            Expr::Tuple(n) => v.visit_tuple_expr(arena.get(n), arena),
            Expr::Array(n) => v.visit_array_expr(arena.get(n), arena),
            Expr::Call(n) => v.visit_call_expr(arena.get(n), arena),
            Expr::ArrayIndex(n) => v.visit_array_index_expr(arena.get(n), arena),
            Expr::TupleIndex(n) => v.visit_tuple_index_expr(arena.get(n), arena),
            Expr::Member(n) => v.visit_member_expr(arena.get(n), arena),
            Expr::Object(n) => v.visit_object_expr(arena.get(n), arena),
            Expr::Unary(n) => v.visit_unary_expr(arena.get(n), arena),
            Expr::Binary(n) => v.visit_binary_expr(arena.get(n), arena),
            Expr::RangeBoth(n) => v.visit_range_both_expr(arena.get(n), arena),
            Expr::RangeFull(n) => v.visit_range_full_expr(arena.get(n), arena),
            Expr::RangeLeft(n) => v.visit_range_left_expr(arena.get(n), arena),
            Expr::RangeRight(n) => v.visit_range_right_expr(arena.get(n), arena),
        }
    }
}

impl<'a> Visit for FieldDef<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_field_def(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_type_ref(self.r#type.as_repr().as_ref(), arena)
    }
}

impl<'a> Visit for FnDef<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_fn_def(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_generic_name(arena.get(&self.name), arena)?;
        for parameter in arena.get_iter(&self.parameters) {
            v.visit_fn_def_parameter(parameter, arena)?;
        }
        if let Some(ret_type) = &self.ret_type {
            v.visit_type_ref(ret_type.as_repr().as_ref(), arena)?;
        }
        for r#where in arena.get_iter(&self.wheres) {
            v.visit_where_clause(r#where, arena)?;
        }
        if let Some(body) = &self.body {
            v.visit_block(arena.get(body), arena)?;
        }
        Default::default()
    }
}

impl<'a> Visit for FnDefParameter<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_fn_def_parameter(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_type_ref(self.r#type.as_repr().as_ref(), arena)
    }
}

impl<'a> Visit for FnType<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_fn_type(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        for parameter in arena.get_iter(&self.parameters) {
            v.visit_fn_type_parameter(parameter, arena)?;
        }
        if let Some(ret_type) = &self.ret_type {
            v.visit_type_ref(ret_type.as_repr().as_ref(), arena)?;
        }
        Default::default()
    }
}

impl<'a> Visit for FnTypeParameter<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_fn_type_parameter(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_type_ref(self.r#type.as_repr().as_ref(), arena)
    }
}

impl<'a> Visit for ForStatement<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_for_stmt(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_expr(self.iter_expr.as_repr().as_ref(), arena)?;
        v.visit_block(arena.get(&self.body), arena)
    }
}

impl<'a> Visit for GenericName<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_generic_name(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        for parameter in arena.get_iter(&self.parameters) {
            v.visit_generic_parameter(parameter, arena)?;
        }
        Default::default()
    }
}

impl Visit for GenericParameter {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_generic_parameter(self, arena)
    }
}

impl<'a> Visit for GenericSegment<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_generic_segment(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_type_list(arena.get(&self.parameters), arena)
    }
}

impl<'a> Visit for IfClause<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_if_clause(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_expr(self.condition.as_repr().as_ref(), arena)?;
        v.visit_block(arena.get(&self.body), arena)
    }
}

impl<'a> Visit for IfStatement<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_if_stmt(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_if_clause(arena.get(&self.if_clause), arena)?;
        for elseif_clause in arena.get_iter(&self.elseif_clauses) {
            v.visit_if_clause(elseif_clause, arena)?;
        }
        if let Some(else_clause) = &self.else_clause {
            v.visit_else_clause(arena.get(else_clause), arena)?;
        }
        Default::default()
    }
}

impl<'a> Visit for Implementation<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_impl_block(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        for parameter in arena.get_iter(&self.parameters) {
            v.visit_generic_parameter(parameter, arena)?;
        }
        if let Some(class) = &self.class {
            v.visit_type_ref(class.as_repr().as_ref(), arena)?;
        }
        v.visit_type_ref(self.r#type.as_repr().as_ref(), arena)?;
        for r#where in arena.get_iter(&self.wheres) {
            v.visit_where_clause(r#where, arena)?;
        }
        for item in arena.get_iter(&self.items) {
            v.visit_item(item.as_repr().as_ref(), arena)?;
        }
        Default::default()
    }
}

impl<'a> Visit for Item<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_item(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        match self {
            Item::Struct(n) => v.visit_struct_def(arena.get(n), arena),
            Item::Enum(n) => v.visit_enum_def(arena.get(n), arena),
            Item::Fn(n) => v.visit_fn_def(arena.get(n), arena),
            Item::Impl(n) => v.visit_impl_block(arena.get(n), arena),
            Item::Type(n) => v.visit_type_def(arena.get(n), arena),
            Item::Class(n) => v.visit_class_def(arena.get(n), arena),
            Item::Block(n) => v.visit_block_stmt(arena.get(n), arena),
            Item::SimpleExpr(n) => v.visit_simple_expr_stmt(arena.get(n), arena),
            Item::AssignExpr(n) => v.visit_assign_expr_stmt(arena.get(n), arena),
            Item::For(n) => v.visit_for_stmt(arena.get(n), arena),
            Item::If(n) => v.visit_if_stmt(arena.get(n), arena),
            Item::Loop(n) => v.visit_loop_stmt(arena.get(n), arena),
            Item::VarDecl(n) => v.visit_var_decl_stmt(arena.get(n), arena),
            Item::While(n) => v.visit_while_stmt(arena.get(n), arena),
            Item::Use(n) => v.visit_use_stmt(arena.get(n), arena),
            Item::Import(n) => v.visit_module_stmt(arena.get(n), arena),
        }
    }
}

impl Visit for LitExpr {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_lit_expr(self, arena)
    }
}

impl<'a> Visit for LoopStatement<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_loop_stmt(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_block(arena.get(&self.body), arena)
    }
}

impl<'a> Visit for MemberExpr<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_member_expr(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_expr(self.base.as_repr().as_ref(), arena)?;
        if let Some(parameters) = &self.parameters {
            v.visit_type_list(arena.get(parameters), arena)?;
        }
        Default::default()
    }
}

impl<'a> Visit for Module<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_module(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        for item in arena.get_iter(&self.items) {
            v.visit_item(item.as_repr().as_ref(), arena)?;
        }
        Default::default()
    }
}

impl Visit for ModuleStatement {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_module_stmt(self, arena)
    }
}

impl<'a> Visit for ObjectExpr<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_object_expr(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_expr(self.base.as_repr().as_ref(), arena)?;
        for field in arena.get_iter(&self.fields) {
            v.visit_object_expr_field(field, arena)?;
        }
        Default::default()
    }
}

impl<'a> Visit for ObjectExprField<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_object_expr_field(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_expr(self.value.as_repr().as_ref(), arena)
    }
}

impl<'a> Visit for ParenExpr<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_paren_expr(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_expr(self.base.as_repr().as_ref(), arena)
    }
}

impl<'a> Visit for Path<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_path(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        for segment in arena.get_iter(&self.segments) {
            v.visit_path_segment(segment.as_repr().as_ref(), arena)?;
        }
        Default::default()
    }
}

impl<'a> Visit for PathSegment<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_path_segment(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        match self {
            PathSegment::Global => Default::default(),
            PathSegment::Simple(n) => v.visit_simple_segment(arena.get(n), arena),
            PathSegment::Cast(n) => v.visit_cast_segment(arena.get(n), arena),
            PathSegment::Generic(n) => v.visit_generic_segment(arena.get(n), arena),
        }
    }
}

impl Visit for PrimitiveType {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_primitive_type(self, arena)
    }
}

impl<'a> Visit for RangeBothExpr<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_range_both_expr(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_expr(self.left.as_repr().as_ref(), arena)?;
        v.visit_expr(self.right.as_repr().as_ref(), arena)
    }
}

impl Visit for RangeFullExpr {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_range_full_expr(self, arena)
    }
}

impl<'a> Visit for RangeLeftExpr<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_range_left_expr(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_expr(self.base.as_repr().as_ref(), arena)
    }
}

impl<'a> Visit for RangeRightExpr<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_range_right_expr(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_expr(self.base.as_repr().as_ref(), arena)
    }
}

impl<'a> Visit for RefType<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_ref_type(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_type_ref(self.base.as_repr().as_ref(), arena)
    }
}

impl<'a> Visit for ReturnStatement<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_ret_stmt(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        if let Some(value) = &self.value {
            v.visit_expr(value.as_repr().as_ref(), arena)?;
        }
        Default::default()
    }
}

impl<'a> Visit for SimpleExprStatement<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_simple_expr_stmt(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_expr(self.expr.as_repr().as_ref(), arena)
    }
}

impl Visit for SimpleSegment {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_simple_segment(self, arena)
    }
}

impl<'a> Visit for Statement<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_stmt(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        match self {
            Statement::Struct(n) => v.visit_struct_def(arena.get(n), arena),
            Statement::Enum(n) => v.visit_enum_def(arena.get(n), arena),
            Statement::Fn(n) => v.visit_fn_def(arena.get(n), arena),
            Statement::Impl(n) => v.visit_impl_block(arena.get(n), arena),
            Statement::Type(n) => v.visit_type_def(arena.get(n), arena),
            Statement::Class(n) => v.visit_class_def(arena.get(n), arena),
            Statement::Block(n) => v.visit_block_stmt(arena.get(n), arena),
            Statement::Break(n) => v.visit_break_stmt(arena.get(n), arena),
            Statement::Continue(n) => v.visit_continue_stmt(arena.get(n), arena),
            Statement::SimpleExpr(n) => v.visit_simple_expr_stmt(arena.get(n), arena),
            Statement::AssignExpr(n) => v.visit_assign_expr_stmt(arena.get(n), arena),
            Statement::For(n) => v.visit_for_stmt(arena.get(n), arena),
            Statement::If(n) => v.visit_if_stmt(arena.get(n), arena),
            Statement::Loop(n) => v.visit_loop_stmt(arena.get(n), arena),
            Statement::Return(n) => v.visit_ret_stmt(arena.get(n), arena),
            Statement::VarDecl(n) => v.visit_var_decl_stmt(arena.get(n), arena),
            Statement::While(n) => v.visit_while_stmt(arena.get(n), arena),
            Statement::Use(n) => v.visit_use_stmt(arena.get(n), arena),
        }
    }
}

impl<'a> Visit for StructDef<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_struct_def(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_generic_name(arena.get(&self.name), arena)?;
        for field in arena.get_iter(&self.fields) {
            v.visit_field_def(field, arena)?;
        }
        Default::default()
    }
}

impl<'a> Visit for TupleExpr<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_tuple_expr(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        for item in arena.get_iter(&self.items) {
            v.visit_expr(item.as_repr().as_ref(), arena)?;
        }
        Default::default()
    }
}

impl<'a> Visit for TupleIndexExpr<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_tuple_index_expr(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_expr(self.base.as_repr().as_ref(), arena)
    }
}

impl<'a> Visit for TupleType<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_tuple_type(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        for parameter in arena.get_iter(&self.parameters) {
            v.visit_type_ref(parameter.as_repr().as_ref(), arena)?;
        }
        Default::default()
    }
}

impl<'a> Visit for TypeDef<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_type_def(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_generic_name(arena.get(&self.name), arena)?;
        if let Some(from) = &self.from {
            v.visit_type_ref(from.as_repr().as_ref(), arena)?;
        }
        Default::default()
    }
}

impl<'a> Visit for TypeList<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_type_list(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        for item in arena.get_iter(&self.items) {
            v.visit_type_ref(item.as_repr().as_ref(), arena)?;
        }
        Default::default()
    }
}

impl<'a> Visit for TypeRef<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_type_ref(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        match self {
            TypeRef::Array(n) => v.visit_array_type(arena.get(n), arena),
            TypeRef::Fn(n) => v.visit_fn_type(arena.get(n), arena),
            TypeRef::Path(n) => v.visit_path(arena.get(n), arena),
            TypeRef::Primitive(n) => v.visit_primitive_type(arena.get(n), arena),
            TypeRef::Ref(n) => v.visit_ref_type(arena.get(n), arena),
            TypeRef::Tuple(n) => v.visit_tuple_type(arena.get(n), arena),
        }
    }
}

impl<'a> Visit for UnaryExpr<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_unary_expr(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_expr(self.base.as_repr().as_ref(), arena)
    }
}

impl<'a> Visit for UseStatement<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_use_stmt(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_path(arena.get(&self.path), arena)
    }
}

impl<'a> Visit for VarDeclStatement<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_var_decl_stmt(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        if let Some(r#type) = &self.r#type {
            v.visit_type_ref(r#type.as_repr().as_ref(), arena)?;
        }
        if let Some(init_value) = &self.init_value {
            v.visit_expr(init_value.as_repr().as_ref(), arena)?;
        }
        Default::default()
    }
}

impl<'a> Visit for WhereClause<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_where_clause(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        for constraint in arena.get_iter(&self.constraints) {
            v.visit_type_ref(constraint.as_repr().as_ref(), arena)?;
        }
        Default::default()
    }
}

impl<'a> Visit for WhileStatement<'a> {
    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_while_stmt(self, arena)
    }
    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {
        v.visit_expr(self.condition.as_repr().as_ref(), arena)?;
        v.visit_block(arena.get(&self.body), arena)
    }
}