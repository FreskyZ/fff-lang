///! --------------------------------------------------------------------------------
///! This code is auto generated by a tool $repo/scripts/ast.py
///! Changes may cause incorrect behavior and will be lost if the code is regenerated
///! --------------------------------------------------------------------------------

///! debug format with arena, make index/slice transparent

use std::fmt::{self, Write};
use super::super::visit::{Visitor, Visit};
use super::na::EmptyResult;
use super::*;

pub fn debug<'a, 'b, N: Visit>(node: &'a N, arena: &'b Arena) -> Debug<'a, 'b, N> {
    Debug(node, arena)
}

pub struct Debug<'a, 'b, N>(&'a N, &'b Arena);

impl<'a, 'b, N> fmt::Debug for Debug<'a, 'b, N> where N: Visit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut formatter = Formatter(f);
        self.0.accept(self.1, &mut formatter).into_result().map_err(|_| fmt::Error)
    }
}

struct Formatter<'f1, 'f2>(&'f1 mut fmt::Formatter<'f2>);

impl<'f1, 'f2> Formatter<'f1, 'f2> {

    fn start_struct(&mut self, name: &'static str) -> Result<&mut Self, fmt::Error> {
        self.0.write_str(name)?;
        self.0.write_char('{')?;
        Ok(self)
    }

    fn end_struct(&mut self) -> EmptyResult {
        self.0.write_str(" }")?;
        EmptyResult(true)
    }

    fn field(&mut self, name: &'static str, value: impl fmt::Debug + Copy) -> Result<&mut Self, fmt::Error> {
        self.0.write_str(name)?;
        self.0.write_str(": ")?;
        write!(self.0, "{:?}", value)?;
        self.0.write_char(',')?;
        Ok(self)
    }

    // LitValue is not Copy
    fn lit_value(&mut self, name: &'static str, value: &LitValue) -> Result<&mut Self, fmt::Error> {
        self.0.write_str(name)?;
        self.0.write_str(": ")?;
        write!(self.0, "{:?}", value)?;
        self.0.write_char(',')?;
        Ok(self)
    }

    // NOTE: this visitor is also not walkable, because you need insert ", field_name: " between 2 visit_* calls, // this may require walk return yield

    fn index<'a, 'b: 'a, N: Visit>(&mut self, name: &'static str, index: &'b Index<'a, N>, arena: &'a Arena) -> Result<&mut Self, fmt::Error> {
        self.0.write_str(name)?;
        self.0.write_str(": ")?;
        arena.get(index).accept(arena, self).into_result().map_err(|_| fmt::Error)?;
        self.0.write_char(',')?;
        Ok(self)
    }

    fn optional_index<'a, 'b: 'a, N: Visit>(&mut self, name: &'static str, index: &'b Option<Index<'a, N>>, arena: &'a Arena) -> Result<&mut Self, fmt::Error> {
        self.0.write_str(name)?;
        self.0.write_str(": ")?;
        if let Some(index) = index {
            arena.get(index).accept(arena, self).into_result().map_err(|_| fmt::Error)?;
        } else {
            self.0.write_str("None")?;
        }
        self.0.write_char(',')?;
        Ok(self)
    }

    fn slice<'a, 'b: 'a, N: Visit>(&mut self, name: &'static str, slice: &'b Slice<'a, N>, arena: &'a Arena) -> Result<&mut Self, fmt::Error> {
        self.0.write_str(name)?;
        self.0.write_str(": [")?;
        for item in arena.get_iter(slice) {
            item.accept(arena, self).into_result().map_err(|_| fmt::Error)?;
        }
        Ok(self)
    }

    fn variant<'a, 'b: 'a, N: Visit>(&mut self, name: &'static str, index: &'b Index<'a, N>, arena: &'a Arena) -> EmptyResult {
        self.0.write_str(name)?;
        self.0.write_char('(')?;
        arena.get(index).accept(arena, self).into_result().map_err(|_| fmt::Error)?;
        self.0.write_char(')')?;
        EmptyResult(true)
    }
}

impl<'f1, 'f2> Visitor for Formatter<'f1, 'f2> {
    type Result = EmptyResult;

    // AUTOGEN
    fn visit_array_expr<'a, 'b: 'a>(&mut self, node: &'b ArrayExpr<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("ArrayExpr")?
            .field("span", node.span)?
            .slice("items", &node.items, arena)?
            .end_struct()
    }

    fn visit_array_index_expr<'a, 'b: 'a>(&mut self, node: &'b ArrayIndexExpr<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("ArrayIndexExpr")?
            .field("span", node.span)?
            .index("base", &node.base, arena)?
            .slice("parameters", &node.parameters, arena)?
            .field("quote_span", node.quote_span)?
            .end_struct()
    }

    fn visit_array_type<'a, 'b: 'a>(&mut self, node: &'b ArrayType<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("ArrayType")?
            .field("span", node.span)?
            .index("base", &node.base, arena)?
            .index("size", &node.size, arena)?
            .end_struct()
    }

    fn visit_assign_expr_stmt<'a, 'b: 'a>(&mut self, node: &'b AssignExprStatement<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("AssignExprStatement")?
            .field("span", node.span)?
            .index("left", &node.left, arena)?
            .index("right", &node.right, arena)?
            .field("op", node.op)?
            .field("op_span", node.op_span)?
            .end_struct()
    }

    fn visit_binary_expr<'a, 'b: 'a>(&mut self, node: &'b BinaryExpr<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("BinaryExpr")?
            .field("span", node.span)?
            .index("left", &node.left, arena)?
            .index("right", &node.right, arena)?
            .field("op", node.op)?
            .field("op_span", node.op_span)?
            .end_struct()
    }

    fn visit_block<'a, 'b: 'a>(&mut self, node: &'b Block<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("Block")?
            .field("span", node.span)?
            .slice("items", &node.items, arena)?
            .end_struct()
    }

    fn visit_block_stmt<'a, 'b: 'a>(&mut self, node: &'b BlockStatement<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("BlockStatement")?
            .field("span", node.span)?
            .field("label", node.label)?
            .index("body", &node.body, arena)?
            .end_struct()
    }

    fn visit_break_stmt<'a, 'b: 'a>(&mut self, node: &'b BreakStatement, _: &'a Arena) -> Self::Result {
        self.start_struct("BreakStatement")?
            .field("span", node.span)?
            .field("label", node.label)?
            .end_struct()
    }

    fn visit_call_expr<'a, 'b: 'a>(&mut self, node: &'b CallExpr<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("CallExpr")?
            .field("span", node.span)?
            .index("base", &node.base, arena)?
            .slice("parameters", &node.parameters, arena)?
            .field("quote_span", node.quote_span)?
            .end_struct()
    }

    fn visit_cast_segment<'a, 'b: 'a>(&mut self, node: &'b CastSegment<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("CastSegment")?
            .field("span", node.span)?
            .index("left", &node.left, arena)?
            .index("right", &node.right, arena)?
            .end_struct()
    }

    fn visit_class_def<'a, 'b: 'a>(&mut self, node: &'b ClassDef<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("ClassDef")?
            .field("span", node.span)?
            .index("name", &node.name, arena)?
            .field("quote_span", node.quote_span)?
            .slice("types", &node.types, arena)?
            .slice("functions", &node.functions, arena)?
            .end_struct()
    }

    fn visit_continue_stmt<'a, 'b: 'a>(&mut self, node: &'b ContinueStatement, _: &'a Arena) -> Self::Result {
        self.start_struct("ContinueStatement")?
            .field("span", node.span)?
            .field("label", node.label)?
            .end_struct()
    }

    fn visit_else_clause<'a, 'b: 'a>(&mut self, node: &'b ElseClause<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("ElseClause")?
            .field("span", node.span)?
            .index("body", &node.body, arena)?
            .end_struct()
    }

    fn visit_enum_def<'a, 'b: 'a>(&mut self, node: &'b EnumDef<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("EnumDef")?
            .field("span", node.span)?
            .field("name", node.name)?
            .optional_index("base_type", &node.base_type, arena)?
            .field("quote_span", node.quote_span)?
            .slice("variants", &node.variants, arena)?
            .end_struct()
    }

    fn visit_enum_def_variant<'a, 'b: 'a>(&mut self, node: &'b EnumDefVariant<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("EnumDefVariant")?
            .field("span", node.span)?
            .field("name", node.name)?
            .optional_index("value", &node.value, arena)?
            .end_struct()
    }

    fn visit_expr<'a, 'b: 'a>(&mut self, node: &'b Expr<'a>, arena: &'a Arena) -> Self::Result {
        match node {
            Expr::Lit(n) => self.variant("Lit", n, arena),
            Expr::Path(n) => self.variant("Path", n, arena),
            Expr::Paren(n) => self.variant("Paren", n, arena),
            Expr::Tuple(n) => self.variant("Tuple", n, arena),
            Expr::Array(n) => self.variant("Array", n, arena),
            Expr::Call(n) => self.variant("Call", n, arena),
            Expr::ArrayIndex(n) => self.variant("ArrayIndex", n, arena),
            Expr::TupleIndex(n) => self.variant("TupleIndex", n, arena),
            Expr::Member(n) => self.variant("Member", n, arena),
            Expr::Object(n) => self.variant("Object", n, arena),
            Expr::Unary(n) => self.variant("Unary", n, arena),
            Expr::Binary(n) => self.variant("Binary", n, arena),
            Expr::RangeBoth(n) => self.variant("RangeBoth", n, arena),
            Expr::RangeFull(n) => self.variant("RangeFull", n, arena),
            Expr::RangeLeft(n) => self.variant("RangeLeft", n, arena),
            Expr::RangeRight(n) => self.variant("RangeRight", n, arena),
        }
    }

    fn visit_field_def<'a, 'b: 'a>(&mut self, node: &'b FieldDef<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("FieldDef")?
            .field("span", node.span)?
            .field("name", node.name)?
            .field("colon_span", node.colon_span)?
            .index("r#type", &node.r#type, arena)?
            .end_struct()
    }

    fn visit_fn_def<'a, 'b: 'a>(&mut self, node: &'b FnDef<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("FnDef")?
            .field("span", node.span)?
            .index("name", &node.name, arena)?
            .field("quote_span", node.quote_span)?
            .slice("parameters", &node.parameters, arena)?
            .optional_index("ret_type", &node.ret_type, arena)?
            .slice("wheres", &node.wheres, arena)?
            .optional_index("body", &node.body, arena)?
            .end_struct()
    }

    fn visit_fn_def_parameter<'a, 'b: 'a>(&mut self, node: &'b FnDefParameter<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("FnDefParameter")?
            .field("span", node.span)?
            .field("name", node.name)?
            .index("r#type", &node.r#type, arena)?
            .end_struct()
    }

    fn visit_fn_type<'a, 'b: 'a>(&mut self, node: &'b FnType<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("FnType")?
            .field("span", node.span)?
            .field("quote_span", node.quote_span)?
            .slice("parameters", &node.parameters, arena)?
            .optional_index("ret_type", &node.ret_type, arena)?
            .end_struct()
    }

    fn visit_fn_type_parameter<'a, 'b: 'a>(&mut self, node: &'b FnTypeParameter<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("FnTypeParameter")?
            .field("span", node.span)?
            .field("name", node.name)?
            .index("r#type", &node.r#type, arena)?
            .end_struct()
    }

    fn visit_for_stmt<'a, 'b: 'a>(&mut self, node: &'b ForStatement<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("ForStatement")?
            .field("span", node.span)?
            .field("label", node.label)?
            .field("iter_name", node.iter_name)?
            .index("iter_expr", &node.iter_expr, arena)?
            .index("body", &node.body, arena)?
            .end_struct()
    }

    fn visit_generic_name<'a, 'b: 'a>(&mut self, node: &'b GenericName<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("GenericName")?
            .field("span", node.span)?
            .field("base", node.base)?
            .field("quote_span", node.quote_span)?
            .slice("parameters", &node.parameters, arena)?
            .end_struct()
    }

    fn visit_generic_parameter<'a, 'b: 'a>(&mut self, node: &'b GenericParameter, _: &'a Arena) -> Self::Result {
        self.start_struct("GenericParameter")?
            .field("span", node.span)?
            .field("name", node.name)?
            .end_struct()
    }

    fn visit_generic_segment<'a, 'b: 'a>(&mut self, node: &'b GenericSegment<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("GenericSegment")?
            .field("span", node.span)?
            .field("base", node.base)?
            .index("parameters", &node.parameters, arena)?
            .end_struct()
    }

    fn visit_if_clause<'a, 'b: 'a>(&mut self, node: &'b IfClause<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("IfClause")?
            .field("span", node.span)?
            .index("condition", &node.condition, arena)?
            .index("body", &node.body, arena)?
            .end_struct()
    }

    fn visit_if_stmt<'a, 'b: 'a>(&mut self, node: &'b IfStatement<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("IfStatement")?
            .field("span", node.span)?
            .index("if_clause", &node.if_clause, arena)?
            .slice("elseif_clauses", &node.elseif_clauses, arena)?
            .optional_index("else_clause", &node.else_clause, arena)?
            .end_struct()
    }

    fn visit_impl_block<'a, 'b: 'a>(&mut self, node: &'b Implementation<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("Implementation")?
            .field("span", node.span)?
            .slice("parameters", &node.parameters, arena)?
            .optional_index("class", &node.class, arena)?
            .index("r#type", &node.r#type, arena)?
            .slice("wheres", &node.wheres, arena)?
            .field("quote_span", node.quote_span)?
            .slice("types", &node.types, arena)?
            .slice("functions", &node.functions, arena)?
            .end_struct()
    }

    fn visit_item<'a, 'b: 'a>(&mut self, node: &'b Item<'a>, arena: &'a Arena) -> Self::Result {
        match node {
            Item::Struct(n) => self.variant("Struct", n, arena),
            Item::Enum(n) => self.variant("Enum", n, arena),
            Item::Fn(n) => self.variant("Fn", n, arena),
            Item::Impl(n) => self.variant("Impl", n, arena),
            Item::Type(n) => self.variant("Type", n, arena),
            Item::Class(n) => self.variant("Class", n, arena),
            Item::Block(n) => self.variant("Block", n, arena),
            Item::SimpleExpr(n) => self.variant("SimpleExpr", n, arena),
            Item::AssignExpr(n) => self.variant("AssignExpr", n, arena),
            Item::For(n) => self.variant("For", n, arena),
            Item::If(n) => self.variant("If", n, arena),
            Item::Loop(n) => self.variant("Loop", n, arena),
            Item::VarDecl(n) => self.variant("VarDecl", n, arena),
            Item::While(n) => self.variant("While", n, arena),
            Item::Use(n) => self.variant("Use", n, arena),
            Item::Import(n) => self.variant("Import", n, arena),
        }
    }

    fn visit_lit_expr<'a, 'b: 'a>(&mut self, node: &'b LitExpr, _: &'a Arena) -> Self::Result {
        self.start_struct("LitExpr")?
            .field("span", node.span)?
            .lit_value("value", &node.value)?
            .end_struct()
    }

    fn visit_loop_stmt<'a, 'b: 'a>(&mut self, node: &'b LoopStatement<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("LoopStatement")?
            .field("span", node.span)?
            .field("label", node.label)?
            .index("body", &node.body, arena)?
            .end_struct()
    }

    fn visit_member_expr<'a, 'b: 'a>(&mut self, node: &'b MemberExpr<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("MemberExpr")?
            .field("span", node.span)?
            .index("base", &node.base, arena)?
            .field("op_span", node.op_span)?
            .field("name", node.name)?
            .optional_index("parameters", &node.parameters, arena)?
            .end_struct()
    }

    fn visit_module<'a, 'b: 'a>(&mut self, node: &'b Module<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("Module")?
            .field("file", node.file)?
            .slice("items", &node.items, arena)?
            .end_struct()
    }

    fn visit_module_stmt<'a, 'b: 'a>(&mut self, node: &'b ModuleStatement, _: &'a Arena) -> Self::Result {
        self.start_struct("ModuleStatement")?
            .field("span", node.span)?
            .field("name", node.name)?
            .field("path", node.path)?
            .end_struct()
    }

    fn visit_object_expr<'a, 'b: 'a>(&mut self, node: &'b ObjectExpr<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("ObjectExpr")?
            .field("span", node.span)?
            .index("base", &node.base, arena)?
            .field("quote_span", node.quote_span)?
            .slice("fields", &node.fields, arena)?
            .end_struct()
    }

    fn visit_object_expr_field<'a, 'b: 'a>(&mut self, node: &'b ObjectExprField<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("ObjectExprField")?
            .field("span", node.span)?
            .field("name", node.name)?
            .index("value", &node.value, arena)?
            .end_struct()
    }

    fn visit_paren_expr<'a, 'b: 'a>(&mut self, node: &'b ParenExpr<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("ParenExpr")?
            .field("span", node.span)?
            .index("base", &node.base, arena)?
            .end_struct()
    }

    fn visit_path<'a, 'b: 'a>(&mut self, node: &'b Path<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("Path")?
            .field("span", node.span)?
            .slice("segments", &node.segments, arena)?
            .end_struct()
    }

    fn visit_path_segment<'a, 'b: 'a>(&mut self, node: &'b PathSegment<'a>, arena: &'a Arena) -> Self::Result {
        match node {
            PathSegment::Global => EmptyResult(self.0.write_str("Global").is_ok()),
            PathSegment::Simple(n) => self.variant("Simple", n, arena),
            PathSegment::Cast(n) => self.variant("Cast", n, arena),
            PathSegment::Generic(n) => self.variant("Generic", n, arena),
        }
    }

    fn visit_primitive_type<'a, 'b: 'a>(&mut self, node: &'b PrimitiveType, _: &'a Arena) -> Self::Result {
        self.start_struct("PrimitiveType")?
            .field("span", node.span)?
            .field("base", node.base)?
            .end_struct()
    }

    fn visit_range_both_expr<'a, 'b: 'a>(&mut self, node: &'b RangeBothExpr<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("RangeBothExpr")?
            .field("span", node.span)?
            .index("left", &node.left, arena)?
            .field("op_span", node.op_span)?
            .index("right", &node.right, arena)?
            .end_struct()
    }

    fn visit_range_full_expr<'a, 'b: 'a>(&mut self, node: &'b RangeFullExpr, _: &'a Arena) -> Self::Result {
        self.start_struct("RangeFullExpr")?
            .field("span", node.span)?
            .end_struct()
    }

    fn visit_range_left_expr<'a, 'b: 'a>(&mut self, node: &'b RangeLeftExpr<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("RangeLeftExpr")?
            .field("span", node.span)?
            .index("base", &node.base, arena)?
            .end_struct()
    }

    fn visit_range_right_expr<'a, 'b: 'a>(&mut self, node: &'b RangeRightExpr<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("RangeRightExpr")?
            .field("span", node.span)?
            .index("base", &node.base, arena)?
            .end_struct()
    }

    fn visit_ref_type<'a, 'b: 'a>(&mut self, node: &'b RefType<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("RefType")?
            .field("span", node.span)?
            .index("base", &node.base, arena)?
            .end_struct()
    }

    fn visit_ret_stmt<'a, 'b: 'a>(&mut self, node: &'b ReturnStatement<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("ReturnStatement")?
            .field("span", node.span)?
            .optional_index("value", &node.value, arena)?
            .end_struct()
    }

    fn visit_simple_expr_stmt<'a, 'b: 'a>(&mut self, node: &'b SimpleExprStatement<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("SimpleExprStatement")?
            .field("span", node.span)?
            .index("expr", &node.expr, arena)?
            .end_struct()
    }

    fn visit_simple_segment<'a, 'b: 'a>(&mut self, node: &'b SimpleSegment, _: &'a Arena) -> Self::Result {
        self.start_struct("SimpleSegment")?
            .field("span", node.span)?
            .field("name", node.name)?
            .end_struct()
    }

    fn visit_stmt<'a, 'b: 'a>(&mut self, node: &'b Statement<'a>, arena: &'a Arena) -> Self::Result {
        match node {
            Statement::Struct(n) => self.variant("Struct", n, arena),
            Statement::Enum(n) => self.variant("Enum", n, arena),
            Statement::Fn(n) => self.variant("Fn", n, arena),
            Statement::Impl(n) => self.variant("Impl", n, arena),
            Statement::Type(n) => self.variant("Type", n, arena),
            Statement::Class(n) => self.variant("Class", n, arena),
            Statement::Block(n) => self.variant("Block", n, arena),
            Statement::Break(n) => self.variant("Break", n, arena),
            Statement::Continue(n) => self.variant("Continue", n, arena),
            Statement::SimpleExpr(n) => self.variant("SimpleExpr", n, arena),
            Statement::AssignExpr(n) => self.variant("AssignExpr", n, arena),
            Statement::For(n) => self.variant("For", n, arena),
            Statement::If(n) => self.variant("If", n, arena),
            Statement::Loop(n) => self.variant("Loop", n, arena),
            Statement::Return(n) => self.variant("Return", n, arena),
            Statement::VarDecl(n) => self.variant("VarDecl", n, arena),
            Statement::While(n) => self.variant("While", n, arena),
            Statement::Use(n) => self.variant("Use", n, arena),
        }
    }

    fn visit_struct_def<'a, 'b: 'a>(&mut self, node: &'b StructDef<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("StructDef")?
            .field("span", node.span)?
            .index("name", &node.name, arena)?
            .slice("fields", &node.fields, arena)?
            .end_struct()
    }

    fn visit_tuple_expr<'a, 'b: 'a>(&mut self, node: &'b TupleExpr<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("TupleExpr")?
            .field("span", node.span)?
            .slice("items", &node.items, arena)?
            .end_struct()
    }

    fn visit_tuple_index_expr<'a, 'b: 'a>(&mut self, node: &'b TupleIndexExpr<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("TupleIndexExpr")?
            .field("span", node.span)?
            .index("base", &node.base, arena)?
            .field("op_span", node.op_span)?
            .field("value", node.value)?
            .field("value_span", node.value_span)?
            .end_struct()
    }

    fn visit_tuple_type<'a, 'b: 'a>(&mut self, node: &'b TupleType<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("TupleType")?
            .field("span", node.span)?
            .slice("parameters", &node.parameters, arena)?
            .end_struct()
    }

    fn visit_type_def<'a, 'b: 'a>(&mut self, node: &'b TypeDef<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("TypeDef")?
            .field("span", node.span)?
            .index("name", &node.name, arena)?
            .optional_index("from", &node.from, arena)?
            .end_struct()
    }

    fn visit_type_list<'a, 'b: 'a>(&mut self, node: &'b TypeList<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("TypeList")?
            .field("span", node.span)?
            .slice("items", &node.items, arena)?
            .end_struct()
    }

    fn visit_type_ref<'a, 'b: 'a>(&mut self, node: &'b TypeRef<'a>, arena: &'a Arena) -> Self::Result {
        match node {
            TypeRef::Array(n) => self.variant("Array", n, arena),
            TypeRef::Fn(n) => self.variant("Fn", n, arena),
            TypeRef::Path(n) => self.variant("Path", n, arena),
            TypeRef::Primitive(n) => self.variant("Primitive", n, arena),
            TypeRef::Ref(n) => self.variant("Ref", n, arena),
            TypeRef::Tuple(n) => self.variant("Tuple", n, arena),
        }
    }

    fn visit_unary_expr<'a, 'b: 'a>(&mut self, node: &'b UnaryExpr<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("UnaryExpr")?
            .field("span", node.span)?
            .index("base", &node.base, arena)?
            .field("op", node.op)?
            .field("op_span", node.op_span)?
            .end_struct()
    }

    fn visit_use_stmt<'a, 'b: 'a>(&mut self, node: &'b UseStatement<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("UseStatement")?
            .field("span", node.span)?
            .index("path", &node.path, arena)?
            .field("alias", node.alias)?
            .end_struct()
    }

    fn visit_var_decl_stmt<'a, 'b: 'a>(&mut self, node: &'b VarDeclStatement<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("VarDeclStatement")?
            .field("span", node.span)?
            .field("r#const", node.r#const)?
            .field("name", node.name)?
            .optional_index("r#type", &node.r#type, arena)?
            .optional_index("init_value", &node.init_value, arena)?
            .end_struct()
    }

    fn visit_where_clause<'a, 'b: 'a>(&mut self, node: &'b WhereClause<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("WhereClause")?
            .field("span", node.span)?
            .field("name", node.name)?
            .slice("constraints", &node.constraints, arena)?
            .end_struct()
    }

    fn visit_while_stmt<'a, 'b: 'a>(&mut self, node: &'b WhileStatement<'a>, arena: &'a Arena) -> Self::Result {
        self.start_struct("WhileStatement")?
            .field("span", node.span)?
            .field("label", node.label)?
            .index("condition", &node.condition, arena)?
            .index("body", &node.body, arena)?
            .end_struct()
    }
}
