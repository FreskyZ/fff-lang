///! syntax::visit_impl

use super::visit::{Node, Visitor};
use super::ast::*;

// TODO use macro to avoid the redundent very long function signature
macro_rules! impl_node {
    ($ty:ty, $visit_this:ident$(, |$self:ident, $v:ident| $walk:expr)?$(,)?) => (
impl Node for $ty {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.$visit_this(self)
    }
    $(fn walk<T: Default, E, V: Visitor<T, E>>(&$self, $v: &mut V) -> Result<T, E> {
        $walk
    })?
}
    )
}

macro_rules! impl_abc_node {
    ($ty:path, $visit_this:ident, $($variant:ident => $visit:ident,)+) => (
impl_node!{ $ty, $visit_this, |self, v| {
    use $ty::*; // amazingly $($ty::$variant(n))+ is not supported but this is supported
    match self {
    $(
        $variant(n) => v.$visit(n),
    )+
    }
}}
    )
}

impl_node!{ ArrayExpr, visit_array_expr, |self, v| {
    v.visit_expr_list(&self.items)
}}

impl_node!{ ArrayType, visit_array_type, |self, v| {
    v.visit_type_ref(&self.base)?;
    v.visit_expr(&self.size)
}}

impl_node!{ AssignExprStatement, visit_assign_expr_stmt, |self, v| {
    v.visit_expr(&self.left)?;
    v.visit_expr(&self.right)
}}

impl_node!{ BinaryExpr, visit_binary_expr, |self, v| {
    v.visit_expr(&self.left)?;
    v.visit_expr(&self.right)
}}

impl_node!{ BlockStatement, visit_block_stmt, |self, v| {
    v.visit_block(&self.body)
}}

impl_node!{ Block, visit_block, |self, v| {
    for item in &self.items {
        v.visit_stmt(item)?;
    }
    Ok(Default::default())
}}

impl_node!{ BreakStatement, visit_break_stmt }

impl_node!{ CallExpr, visit_call_expr, |self, v| {
    v.visit_expr(&self.base)?;
    v.visit_expr_list(&self.parameters)
}}

impl_node!{ ContinueStatement, visit_continue_stmt }

impl_node!{ ElseClause, visit_else_clause, |self, v| {
    v.visit_block(&self.body)
}}

impl_node!{ EnumDef, visit_enum_def, |self, v| {
    if let Some(base_type) = &self.base_type {
        v.visit_primitive_type(base_type)?;
    }
    for variant in &self.variants {
        v.visit_enum_def_variant(variant)?;
    }
    Ok(Default::default())
}}

impl_node! { EnumDefVariant, visit_enum_def_variant, |self, v| {
    if let Some(value) = &self.value {
        v.visit_expr(value)?;
    }
    Ok(Default::default())
}}

impl_abc_node! { Expr, visit_expr,
    Lit => visit_lit_expr,
    Name => visit_name,
    Paren => visit_paren_expr,
    Tuple => visit_tuple_expr,
    Array => visit_array_expr,
    Call => visit_call_expr,
    Index => visit_index_expr,
    Member => visit_member_expr,
    Object => visit_object_expr,
    Unary => visit_unary_expr,
    Binary => visit_binary_expr,
    RangeBoth => visit_range_both_expr,
    RangeFull => visit_range_full_expr,
    RangeLeft => visit_range_left_expr,
    RangeRight => visit_range_right_expr,
}

impl_node!{ ExprList, visit_expr_list, |self, v| {
    for item in &self.items {
        v.visit_expr(item)?;
    }
    Ok(Default::default())
}}

impl_node!{ FnDef, visit_fn_def, |self, v| {
    for parameter in &self.parameters {
        v.visit_fn_def_parameter(parameter)?;
    }
    if let Some(ret_type) = &self.ret_type {
        v.visit_type_ref(ret_type)?;
    }
    v.visit_block(&self.body)
}}

impl_node! { FnDefParameter, visit_fn_def_parameter, |self, v| {
    v.visit_type_ref(&self.r#type)
}}

impl_node!{ FnType, visit_fn_type, |self, v| {
    for parameter in &self.parameters {
        v.visit_fn_type_parameter(parameter)?;
    }
    if let Some(ret_type) = &self.ret_type {
        v.visit_type_ref(ret_type.as_ref())?;
    }
    Ok(Default::default())
}}

impl_node!{ FnTypeParameter, visit_fn_type_parameter, |self, v| {
    v.visit_type_ref(&self.r#type)
}}

impl_node!{ ForStatement, visit_for_stmt, |self, v| {
    v.visit_expr(&self.iter_expr)?;
    v.visit_block(&self.body)
}}

impl_node!{ IfStatement, visit_if_stmt, |self, v| {
    v.visit_if_clause(&self.if_clause)?;
    for elseif in &self.elseif_clauses {
        v.visit_if_clause(elseif)?;
    }
    if let Some(r#else) = &self.else_clause {
        v.visit_else_clause(r#else)?;
    }
    Ok(Default::default())
}}

impl_node!{ IfClause, visit_if_clause, |self, v| {
    v.visit_expr(&self.condition)?;
    v.visit_block(&self.body)
}}


impl_node!{ IndexExpr, visit_index_expr, |self, v| {
    v.visit_expr(&self.base)?;
    v.visit_expr_list(&self.parameters)
}}

impl_abc_node!{ Item, visit_item, 
    Type => visit_type_def,
    Enum => visit_enum_def,
    Fn => visit_fn_def,
    Block => visit_block_stmt,
    SimpleExpr => visit_simple_expr_stmt,
    AssignExpr => visit_assign_expr_stmt,
    For => visit_for_stmt,
    If => visit_if_stmt,
    Loop => visit_loop_stmt,
    VarDecl => visit_var_decl,
    While => visit_while_stmt,
    Use => visit_use_stmt,
    Import => visit_module_stmt,
}

impl_node!{ LitExpr, visit_lit_expr }

impl_node!{ LoopStatement, visit_loop_stmt, |self, v| {
    v.visit_block(&self.body)
}}

impl_node!{ MemberExpr, visit_member_expr, |self, v| {
    v.visit_expr(self.base.as_ref())?;
    v.visit_member_name(&self.name)
}}

impl_node!{ MemberName, visit_member_name, |self, v| {
    if let Some(parameters) = &self.parameters {
        v.visit_type_list(parameters)?;
    }
    Ok(Default::default())
}}

impl_node!{ Module, visit_module, |self, v| {
    for item in &self.items {
        v.visit_item(item)?;
    }
    Ok(Default::default())
}}

impl_node!{ ModuleStatement, visit_module_stmt }

impl_node!{ Name, visit_name, |self, v| {
    if let Some(type_as_segment) = &self.type_as_segment {
        v.visit_type_as_segment(type_as_segment)?;
    }
    for segment in &self.segments {
        v.visit_name_segment(segment)?;
    }
    Ok(Default::default())
}}

impl_node!{ NameSegment, visit_name_segment, |self, v| {
    if let Self::Generic(parameters) = self {
        v.visit_type_list(parameters)?;
    }
    Ok(Default::default())
}}

impl_node!{ ObjectExpr, visit_object_expr, |self, v| {
    v.visit_expr(self.base.as_ref())?;
    for field in &self.fields {
        v.visit_object_expr_field(field)?;
    }
    Ok(Default::default())
}}

impl_node! { ObjectExprField, visit_object_expr_field, |self, v| {
    v.visit_expr(&self.value)
}}

impl_node!{ ParenExpr, visit_paren_expr, |self, v| {
    v.visit_expr(self.base.as_ref())
}}

impl_node!{ PlainType, visit_plain_type, |self, v| {
    if let Some(type_as_segment) = &self.type_as_segment {
        v.visit_type_as_segment(type_as_segment)?;
    }
    for segment in &self.segments {
        v.visit_type_segment(segment)?;
    }
    Ok(Default::default())
}}

impl_node!{ PrimitiveType, visit_primitive_type }

impl_node!{ RangeBothExpr, visit_range_both_expr, |self, v| {
    v.visit_expr(self.left.as_ref())?;
    v.visit_expr(self.right.as_ref())
}}

impl_node!{ RangeFullExpr, visit_range_full_expr }

impl_node!{ RangeLeftExpr, visit_range_left_expr, |self, v| {
    v.visit_expr(self.base.as_ref())
}}

impl_node!{ RangeRightExpr, visit_range_right_expr, |self, v| {
    v.visit_expr(self.base.as_ref())
}}

impl_node!{ RefType, visit_ref_type, |self, v| {
    v.visit_type_ref(&self.base)
}}

impl_node!{ ReturnStatement, visit_ret_stmt, |self, v| {
    if let Some(expr) = &self.value {
        v.visit_expr(expr)?;
    }
    Ok(Default::default())
}}

impl_node!{ SimpleExprStatement, visit_simple_expr_stmt, |self, v| {
    v.visit_expr(&self.expr)
}}

impl_abc_node!{ Statement, visit_stmt,
    Type => visit_type_def,
    Enum => visit_enum_def,
    Fn => visit_fn_def,
    Block => visit_block_stmt,
    Break => visit_break_stmt,
    Continue => visit_continue_stmt,
    SimpleExpr => visit_simple_expr_stmt,
    AssignExpr => visit_assign_expr_stmt,
    For => visit_for_stmt,
    If => visit_if_stmt,
    Loop => visit_loop_stmt,
    Return => visit_ret_stmt,
    VarDecl => visit_var_decl,
    While => visit_while_stmt,
    Use => visit_use_stmt,
}

impl_node!{ TypeAsSegment, visit_type_as_segment, |self, v| {
    v.visit_type_ref(self.from.as_ref())?;
    v.visit_type_ref(self.to.as_ref())
}}

impl_node!{ TypeSegment, visit_type_segment, |self, v| {
    if let Some(parameters) = &self.parameters {
        v.visit_type_list(parameters)?;
    }
    Ok(Default::default())
}}

impl_node!{ TypeDef, visit_type_def, |self, v| {
    for field in &self.fields {
        v.visit_type_def_field(field)?;
    }
    Ok(Default::default())
}}

impl_node!{ TypeDefField, visit_type_def_field, |self, v| {
    v.visit_type_ref(&self.r#type)
}}

impl_node!{ TypeList, visit_type_list, |self, v| {
    for item in &self.items {
        v.visit_type_ref(item)?;
    }
    Ok(Default::default())
}}

impl_abc_node!{ TypeRef, visit_type_ref,
    Primitive => visit_primitive_type,
    Array => visit_array_type,
    Fn => visit_fn_type,
    Ref => visit_ref_type,
    Tuple => visit_tuple_type,
    Plain => visit_plain_type,
}

impl_node!{ TupleExpr, visit_tuple_expr, |self, v| {
    v.visit_expr_list(&self.items)
}}

impl_node!{ TupleType, visit_tuple_type, |self, v| {
    for parameter in &self.parameters {
        v.visit_type_ref(parameter)?;
    }
    Ok(Default::default())
}}

impl_node!{ UnaryExpr, visit_unary_expr, |self, v| {
    v.visit_expr(self.base.as_ref())
}}

impl_node!{ UseStatement, visit_use_stmt, |self, v| {
    v.visit_name(&self.name)?;
    Ok(Default::default())
}}

impl_node!{ VarDeclStatement, visit_var_decl, |self, v| {
    if let Some(r#type) = &self.r#type {
        v.visit_type_ref(r#type)?;
    }
    if let Some(init_expr) = &self.init_value {
        v.visit_expr(init_expr)?;
    }
    Ok(Default::default())
}}

impl_node!{ WhileStatement, visit_while_stmt, |self, v| {
    v.visit_expr(&self.condition)?;
    v.visit_block(&self.body)
}}
