///! pretty format with span/isid explained
// // this one is too customized to auto generate

use std::fmt::{self, Write};
use crate::source::{SourceContext, FileSystem, Span, IsId, IdSpan};
use super::super::visit::{Visitor, Visit};
use super::noauto::EmptyResult;
use super::*;

pub struct Display<'n, 's, 'a, N, F, const I: usize>{
    node: &'n N,
    source: &'s SourceContext<F>,
    arena: &'a Arena,
}

// this name should be simpler than NodeDisplay::new()
// // this is not to be add to parse_fn_def and parse_fn_type, because after removing lifetime this is actually simple
pub fn display<'n, 's, 'a, N: Visit, F: FileSystem>(node: &'n N, source: &'s SourceContext<F>, arena: &'a Arena) -> Display<'n, 's, 'a, N, F, 0> {
    Display{ node, source, arena }
}

// default const generic is not allowed in functions currently
pub fn display1<'n, 's, 'a, N: Visit, F: FileSystem>(node: &'n N, source: &'s SourceContext<F>, arena: &'a Arena) -> Display<'n, 's, 'a, N, F, 1> {
    Display{ node, source, arena }
}

impl<'n, 's, 'a, N: Visit, F: FileSystem, const I: usize> fmt::Display for Display<'n, 's, 'a, N, F, I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut formatter = Formatter::<F, I>{ base: f, source: self.source, level: 0, additional: "" };
        self.node.accept(self.arena, &mut formatter).into_result().map_err(|_| fmt::Error)
    }
}

// I: indention strategy index
// // this is very context-ful compared to ugly formatter
pub struct Formatter<'s, 'f1, 'f2, F, const I: usize> {
    base: &'f1 mut fmt::Formatter<'f2>,
    source: &'s SourceContext<F>,
    level: usize,
    additional: &'static str, // additional to node name explain role in parent node
}

static INDENTIONS: [&str; 2] = [
    "                                                                          ",
    "| : | : | : | : | : | : | : | : | : | : | : | : | : | : | : | : | : | : | ",
];

impl<'s, 'f1, 'f2, F, const I: usize> Formatter<'s, 'f1, 'f2, F, I> where F: FileSystem {

    fn role(&mut self, _role: &'static str) -> &mut Self {
        // self.additional = role;
        self
    }
    fn title(&mut self, title: &'static str) -> Result<&mut Self, fmt::Error> {
        self.base.write_str(&INDENTIONS[I][0..self.level * 2])?;
        self.base.write_str(title)?;
        self.base.write_str(self.additional)?;
        self.additional = "";
        Ok(self)
    }

    fn span(&mut self, span: Span) -> Result<&mut Self, fmt::Error> {
        let (_, start_line, end_line, start_column, end_column) = self.source.map_span_to_line_column(span);
        write!(self.base, "<{}:{}-{}:{}>", start_line, end_line, start_column, end_column)?;
        Ok(self)
    }
    fn isid(&mut self, id: IsId) -> Result<&mut Self, fmt::Error> {
        if id.unwrap() == 1 {
            self.base.write_str("<empty>")?;
        } else {
            let content = self.source.resolve_string(id);
            if content.len() > 24 {
                self.base.write_str(&content[..24])?;
                self.base.write_str("...")?;
            } else {
                self.base.write_str(content)?;
            }
        }
        Ok(self)
    }
    fn file_id(&mut self, file_id: FileId) -> Result<&mut Self, fmt::Error> {
        write!(self.base, "{}", self.source.get_relative_path(file_id).display())?;
        Ok(self)
    }
    fn idspan(&mut self, idspan: IdSpan) -> Result<&mut Self, fmt::Error> {
        self.isid(idspan.id)?.space()?.span(idspan.span)
    }
    fn label(&mut self, label: Option<IdSpan>) -> Result<&mut Self, fmt::Error> {
        if let Some(label) = label { self.str(" @")?.idspan(label) } else { Ok(self) }
    }
    fn optional_idspan(&mut self, idspan: Option<IdSpan>) -> Result<&mut Self, fmt::Error> {
        if let Some(idspan) = idspan { self.space()?.idspan(idspan) } else { Ok(self) }
    }

    fn space(&mut self) -> Result<&mut Self, fmt::Error> {
        self.base.write_char(' ')?;
        Ok(self)
    }
    fn str(&mut self, v: &'static str) -> Result<&mut Self, fmt::Error> {
        self.base.write_str(v)?;
        Ok(self)
    }
    fn any(&mut self, args: impl fmt::Debug) -> Result<&mut Self, fmt::Error> {
        write!(self.base, "{:?}", args)?;
        Ok(self)
    }
    fn finish(&self) -> EmptyResult {
        EmptyResult(true)
    }
}

macro_rules! next_level {
    ($self:ident, $e:expr) => {{
        $self.base.write_char('\n')?;
        $self.level += 1;
        $e.into_result().map_err(|_| fmt::Error)?;
        $self.level -= 1;
        Ok($self)
    }};
}

impl<'s, 'f1, 'f2, F, const I: usize> Formatter<'s, 'f1, 'f2, F, I> where F: FileSystem {

    fn index<'a, N: Visit>(&mut self, index: &Index<'a, N>, arena: &'a Arena) -> Result<&mut Self, fmt::Error> {
        next_level!(self, arena.get(index).accept(arena, self))
    }
    fn tagged_index<'a, N: Visit>(&mut self, index: &TagIndex<'a, N>, arena: &'a Arena) -> Result<&mut Self, fmt::Error> {
        next_level!(self, index.as_repr().as_ref().accept(arena, self))
    }

    fn optional<'a, N: Visit>(&mut self, index: &Option<Index<'a, N>>, arena: &'a Arena) -> Result<&mut Self, fmt::Error> {
        next_level!(self, if let Some(index) = index { arena.get(index).accept(arena, self) } else { self.additional = ""; EmptyResult(true) })
    }
    fn optional_tagged<'a, N: Visit>(&mut self, index: &Option<TagIndex<'a, N>>, arena: &'a Arena) -> Result<&mut Self, fmt::Error> {
        next_level!(self, if let Some(index) = index { index.as_repr().as_ref().accept(arena, self) } else { self.additional = ""; EmptyResult(true) })
    }

    fn slice<'a, N: Visit>(&mut self, slice: &Slice<'a, N>, arena: &'a Arena) -> Result<&mut Self, fmt::Error> {
        next_level!(self, { for item in arena.get_iter(slice) { item.accept(arena, self); } EmptyResult(true) })
    }
    fn tagged_slice<'a, N: Visit>(&mut self, slice: &TagSlice<'a, N>, arena: &'a Arena) -> Result<&mut Self, fmt::Error> {
        next_level!(self, { for index in arena.get_iter(slice) { index.as_repr().as_ref().accept(arena, self); } EmptyResult(true) })
    }
}

impl<'s, 'f1, 'f2, F, const I: usize> Visitor for Formatter<'s, 'f1, 'f2, F, I> where F: FileSystem {
    type Result = EmptyResult;

    // default implementation correctly transparents this type of node
    // fn visit_expr<'a, 'b: 'a>(&mut self, node: &'b Expr, arena: &'a Arena) -> Self::Result;
    // fn visit_stmt<'a, 'b: 'a>(&mut self, node: &'b Statement, arena: &'a Arena) -> Self::Result;
    // fn visit_item<'a, 'b: 'a>(&mut self, node: &'b Item, arena: &'a Arena) -> Self::Result;
    // fn visit_type_list<'a, 'b: 'a>(&mut self, node: &'b TypeList, arena: &'a Arena) -> Self::Result;
    // fn visit_type_ref<'a, 'b: 'a>(&mut self, node: &'b TypeRef, arena: &'a Arena) -> Self::Result;

    // implementation principle / pretty format style
    // 1. each node instance is one line, each line is one node instance
    // 2. each line starts with `{indent}{title} {primary span}`, primary span may missing for some type of nodes
    //    remaining part of this line is normally one or several `{desc/stringid} {span}`

    fn visit_array_expr<'a, 'b: 'a>(&mut self, node: &'b ArrayExpr, arena: &'a Arena) -> Self::Result {
        self.title("array-expr")?
            .span(node.span)?
            .tagged_slice(&node.items, arena)?.finish()
    }

    fn visit_array_index_expr<'a, 'b: 'a>(&mut self, node: &'b ArrayIndexExpr, arena: &'a Arena) -> Self::Result {
        self.title("array-index-expr")?
            .span(node.span)?
            .str(" [] ")?.span(node.quote_span)?
            .role("base").tagged_index(&node.base, arena)?
            .tagged_slice(&node.parameters, arena)?.finish()
    }

    fn visit_array_type<'a, 'b: 'a>(&mut self, node: &'b ArrayType, arena: &'a Arena) -> Self::Result {
        self.title("array-type")?
            .span(node.span)?
            .role("base").tagged_index(&node.base, arena)?
            .role("size").tagged_index(&node.size, arena)?.finish()
    }

    fn visit_assign_expr_stmt<'a, 'b: 'a>(&mut self, node: &'b AssignExprStatement, arena: &'a Arena) -> Self::Result {
        self.title("assign-expr-stmt")?
            .span(node.span)?
            .space()?.str(node.op.display())?
            .space()?.span(node.op_span)?
            .role("left").tagged_index(&node.left, arena)?
            .role("right").tagged_index(&node.right, arena)?.finish()
    }

    fn visit_binary_expr<'a, 'b: 'a>(&mut self, node: &'b BinaryExpr, arena: &'a Arena) -> Self::Result {
        self.title("binary-expr")?
            .span(node.span)?
            .space()?.str(node.op.display())?
            .space()?.span(node.op_span)?
            .role("left").tagged_index(&node.left, arena)?
            .role("right").tagged_index(&node.right, arena)?.finish()
    }

    fn visit_block<'a, 'b: 'a>(&mut self, node: &'b Block, arena: &'a Arena) -> Self::Result {
        self.title("block")?
            .span(node.span)?
            .tagged_slice(&node.items, arena)?.finish()
    }

    fn visit_block_stmt<'a, 'b: 'a>(&mut self, node: &'b BlockStatement, arena: &'a Arena) -> Self::Result {
        self.title("block-stmt")?
            .span(node.span)?
            .label(node.label)?
            .role("body").index(&node.body, arena)?.finish()
    }

    fn visit_break_stmt<'a, 'b: 'a>(&mut self, node: &'b BreakStatement, _: &'a Arena) -> Self::Result {
        self.title("break-stmt")?
            .span(node.span)?
            .label(node.label)?.finish()
    }

    fn visit_call_expr<'a, 'b: 'a>(&mut self, node: &'b CallExpr, arena: &'a Arena) -> Self::Result {
        self.title("call-expr")?
            .span(node.span)?
            .str(" () ")?.span(node.quote_span)?
            .role("base").tagged_index(&node.base, arena)?
            .tagged_slice(&node.parameters, arena)?.finish()
    }

    fn visit_cast_segment<'a, 'b: 'a>(&mut self, node: &'b CastSegment, arena: &'a Arena) -> Self::Result {
        self.title("cast-segment")?
            .span(node.span)?
            .role("left").tagged_index(&node.left, arena)?
            .role("right").tagged_index(&node.right, arena)?.finish()
    }

    fn visit_class_def<'a, 'b: 'a>(&mut self, node: &'b ClassDef, arena: &'a Arena) -> Self::Result {
        self.title("class-def")?
            .span(node.span)?
            .str(" {} ")?.span(node.quote_span)?
            .role("name").index(&node.name, arena)?
            .tagged_slice(&node.items, arena)?.finish()
    }

    fn visit_continue_stmt<'a, 'b: 'a>(&mut self, node: &'b ContinueStatement, _: &'a Arena) -> Self::Result {
        self.title("continue-stmt")?
            .span(node.span)?
            .label(node.label)?.finish()
    }
    
    fn visit_else_clause<'a, 'b: 'a>(&mut self, node: &'b ElseClause, arena: &'a Arena) -> Self::Result {
        self.title("else-clause")?
            .span(node.span)?
            .role("body").index(&node.body, &arena)?.finish()
    }

    fn visit_enum_def<'a, 'b: 'a>(&mut self, node: &'b EnumDef, arena: &'a Arena) -> Self::Result {
        self.title("enum-def")?
            .span(node.span)?
            .space()?.idspan(node.name)?
            .str(" {} ")?.span(node.quote_span)?
            .role("base-type").optional(&node.base_type, arena)?
            .slice(&node.variants, arena)?.finish()
    }

    fn visit_enum_def_variant<'a, 'b: 'a>(&mut self, node: &'b EnumDefVariant, arena: &'a Arena) -> Self::Result {
        self.title("enum-def-variant")?
            .span(node.span)?
            .space()?.idspan(node.name)?
            .role("init").optional_tagged(&node.value, arena)?.finish()
    }

    fn visit_field_def<'a, 'b: 'a>(&mut self, node: &'b FieldDef, arena: &'a Arena) -> Self::Result {
        self.title("field-def")?
            .span(node.span)?
            .space()?.idspan(node.name)?
            .str(" : ")?.span(node.colon_span)?
            .tagged_index(&node.r#type, arena)?.finish()
    }

    fn visit_fn_def<'a, 'b: 'a>(&mut self, node: &'b FnDef, arena: &'a Arena) -> Self::Result {
        self.title("fn-def")?
            .span(node.span)?
            .str(" () ")?.span(node.quote_span)?
            .index(&node.name, arena)?
            .slice(&node.parameters, arena)?
            .role("ret-type").optional_tagged(&node.ret_type, arena)?
            .slice(&node.wheres, arena)?.finish()
    }

    fn visit_fn_def_parameter<'a, 'b: 'a>(&mut self, node: &'b FnDefParameter, arena: &'a Arena) -> Self::Result {
        self.title("fn-def-parameter")?
            .span(node.span)?
            .space()?.idspan(node.name)?
            .tagged_index(&node.r#type, arena)?.finish()
    }

    fn visit_fn_type<'a, 'b: 'a>(&mut self, node: &'b FnType, arena: &'a Arena) -> Self::Result {
        self.title("fn-type")?
            .span(node.span)?
            .str(" () ")?.span(node.quote_span)?
            .slice(&node.parameters, arena)?
            .role("ret-type").optional_tagged(&node.ret_type, arena)?.finish()
    }

    fn visit_fn_type_parameter<'a, 'b: 'a>(&mut self, node: &'b FnTypeParameter, arena: &'a Arena) -> Self::Result {
        self.title("fn-type-parameter")?
            .span(node.span)?
            .optional_idspan(node.name)?
            .tagged_index(&node.r#type, arena)?.finish()
    }

    fn visit_for_stmt<'a, 'b: 'a>(&mut self, node: &'b ForStatement, arena: &'a Arena) -> Self::Result {
        self.title("for-stmt")?
            .span(node.span)?
            .str(" iter-var ")?.idspan(node.iter_name)?
            // TODO: should be before iter-var?
            .label(node.label)?
            .role("iter-expr").tagged_index(&node.iter_expr, arena)?
            .role("body").index(&node.body, arena)?.finish()
    }

    fn visit_generic_name<'a, 'b: 'a>(&mut self, node: &'b GenericName, arena: &'a Arena) -> Self::Result {
        self.title("generic-name")?
            .span(node.span)?
            .space()?.idspan(node.base)?;
        if node.quote_span != Span::new(0, 0) {
            self.str(" <> ")?.span(node.quote_span)?;
        }
        self.slice(&node.parameters, arena)?.finish()
    }

    fn visit_generic_parameter<'a, 'b: 'a>(&mut self, node: &'b GenericParameter, _: &'a Arena) -> Self::Result {
        self.title("generic-parameter")?
            .span(node.span)?
            .space()?.isid(node.name.id)?.finish()
    }

    fn visit_generic_segment<'a, 'b: 'a>(&mut self, node: &'b GenericSegment, arena: &'a Arena) -> Self::Result {
        self.title("generic-segment")?
            .span(node.span)?
            .space()?.idspan(node.base)?
            .index(&node.parameters, arena)?.finish()
    }

    fn visit_if_clause<'a, 'b: 'a>(&mut self, node: &'b IfClause, arena: &'a Arena) -> Self::Result {
        self.title("if-clause")?
            .span(node.span)?
            .role("condition").tagged_index(&node.condition, arena)?
            .role("body").index(&node.body, arena)?.finish()
    }

    fn visit_if_stmt<'a, 'b: 'a>(&mut self, node: &'b IfStatement, arena: &'a Arena) -> Self::Result {
        self.title("if-stmt")?
            .span(node.span)?
            .index(&node.if_clause, arena)?
            .slice(&node.elseif_clauses, arena)?
            .optional(&node.else_clause, arena)?.finish()
    }
    
    fn visit_impl_block<'a, 'b: 'a>(&mut self, node: &'b Implementation, arena: &'a Arena) -> Self::Result {
        self.title("impl-block")?
            .span(node.span)?
            .str(" {} ")?.span(node.quote_span)?
            .slice(&node.parameters, arena)?
            .role("class").optional_tagged(&node.class, arena)?
            .tagged_index(&node.r#type, arena)?
            .slice(&node.wheres, arena)?
            .tagged_slice(&node.items, arena)?.finish()
    }

    fn visit_lit_expr<'a, 'b: 'a>(&mut self, node: &'b LitExpr, _: &'a Arena) -> Self::Result {
        self.title("lit-expr")?
            .space()?;
        match node.value {
            LitValue::Unit => self.str("unit")?,
            LitValue::Bool(v) => self.any(format_args!("bool {v}"))?,
            LitValue::Char(v) => self.any(format_args!("char {v:?}"))?,
            LitValue::Str(id) => self.str("str \"")?.isid(id)?.str("\"")?,
            LitValue::Num(v) => self.any(format_args!("{v}"))?,
        };
        self.space()?.span(node.span)?.finish()
    }

    fn visit_loop_stmt<'a, 'b: 'a>(&mut self, node: &'b LoopStatement, arena: &'a Arena) -> Self::Result {
        self.title("loop-stmt")?
            .span(node.span)?
            .label(node.label)?
            .role("body").index(&node.body, arena)?.finish()
    }

    fn visit_member_expr<'a, 'b: 'a>(&mut self, node: &'b MemberExpr, arena: &'a Arena) -> Self::Result {
        self.title("member-expr")?
            .span(node.span)?
            .str(" . ")?.span(node.op_span)?
            .space()?.idspan(node.name)?
            .role("base").tagged_index(&node.base, arena)?
            .optional(&node.parameters, arena)?.finish()
    }

    fn visit_module<'a, 'b: 'a>(&mut self, node: &'b Module, arena: &'a Arena) -> Self::Result {
        self.title("module")?
            .space()?.file_id(node.file)?
            .tagged_slice(&node.items, arena)?.finish()
    }

    fn visit_module_stmt<'a, 'b: 'a>(&mut self, node: &'b ModuleStatement, _: &'a Arena) -> Self::Result {
        self.title("module-stmt")?
            .span(node.span)?
            .space()?.idspan(node.name)?;
        if let Some(path) = node.path {
            self.str(" path ")?.idspan(path)?;
        }
        self.finish()
    }

    fn visit_object_expr<'a, 'b: 'a>(&mut self, node: &'b ObjectExpr, arena: &'a Arena) -> Self::Result {
        self.title("object-expr")?
            .span(node.span)?
            .str(" {} ")?.span(node.quote_span)?
            .role("base").tagged_index(&node.base, arena)?
            .slice(&node.fields, arena)?.finish()
    }

    fn visit_object_expr_field<'a, 'b: 'a>(&mut self, node: &'b ObjectExprField, arena: &'a Arena) -> Self::Result {
        self.title("object-expr-field")?
            .span(node.span)?
            .space()?.idspan(node.name)?
            .tagged_index(&node.value, arena)?.finish()
    }

    fn visit_paren_expr<'a, 'b: 'a>(&mut self, node: &'b ParenExpr, arena: &'a Arena) -> Self::Result {
        self.title("paren-expr")?
            .span(node.span)?
            .tagged_index(&node.base, arena)?.finish()
    }

    fn visit_path<'a, 'b: 'a>(&mut self, node: &'b Path, arena: &'a Arena) -> Self::Result {
        self.title("path")?
            .span(node.span)?
            .tagged_slice(&node.segments, arena)?.finish()
    }

    fn visit_path_segment<'a, 'b: 'a>(&mut self, node: &'b PathSegment, arena: &'a Arena) -> Self::Result {
        match node {
            PathSegment::Global => self.title("global-segment")?.finish(),
            PathSegment::Simple(v) => self.visit_simple_segment(arena.get(v), arena),
            PathSegment::Cast(v) => self.visit_cast_segment(arena.get(v), arena),
            PathSegment::Generic(v) => self.visit_generic_segment(arena.get(v), arena),
        }
    }

    fn visit_primitive_type<'a, 'b: 'a>(&mut self, node: &'b PrimitiveType, _: &'a Arena) -> Self::Result {
        self.title("primitive-type")?
            .space()?.str(node.base.display())?
            .space()?.span(node.span)?.finish()
    }
    
    fn visit_range_both_expr<'a, 'b: 'a>(&mut self, node: &'b RangeBothExpr, arena: &'a Arena) -> Self::Result {
        self.title("range-both-expr")?
            .span(node.span)?
            .str(" dotdot ")?.span(node.op_span)?
            .role("left").tagged_index(&node.left, arena)?
            .role("right").tagged_index(&node.right, arena)?.finish()
    }
    
    fn visit_range_full_expr<'a, 'b: 'a>(&mut self, node: &'b RangeFullExpr, _: &'a Arena) -> Self::Result {
        self.title("range-full-expr")?
            .span(node.span)?.finish()
    }

    fn visit_range_left_expr<'a, 'b: 'a>(&mut self, node: &'b RangeLeftExpr, arena: &'a Arena) -> Self::Result {
        self.title("range-left-expr")?
            .span(node.span)?
            .tagged_index(&node.base, arena)?.finish()
    }

    fn visit_range_right_expr<'a, 'b: 'a>(&mut self, node: &'b RangeRightExpr, arena: &'a Arena) -> Self::Result {
        self.title("range-right-expr")?
            .span(node.span)?
            .tagged_index(&node.base, arena)?.finish()
    }

    fn visit_ref_type<'a, 'b: 'a>(&mut self, node: &'b RefType, arena: &'a Arena) -> Self::Result {
        self.title("ref-type")?
            .span(node.span)?
            .tagged_index(&node.base, arena)?.finish()
    }

    fn visit_ret_stmt<'a, 'b: 'a>(&mut self, node: &'b ReturnStatement, arena: &'a Arena) -> Self::Result {
        self.title("ret-stmt")?
            .span(node.span)?
            .optional_tagged(&node.value, arena)?.finish()
    }

    fn visit_simple_expr_stmt<'a, 'b: 'a>(&mut self, node: &'b SimpleExprStatement, arena: &'a Arena) -> Self::Result {
        self.title("simple-expr-stmt")?
            .span(node.span)?
            .tagged_index(&node.expr, arena)?.finish()
    }

    fn visit_simple_segment<'a, 'b: 'a>(&mut self, node: &'b SimpleSegment, _: &'a Arena) -> Self::Result {
        self.title("segment")?
            .span(node.span)?
            .space()?.isid(node.name)?.finish()
    }

    fn visit_struct_def<'a, 'b: 'a>(&mut self, node: &'b StructDef, arena: &'a Arena) -> Self::Result {
        self.title("struct-def")?
            .span(node.span)?
            .index(&node.name, arena)?
            .slice(&node.fields, arena)?.finish()
    }

    fn visit_tuple_expr<'a, 'b: 'a>(&mut self, node: &'b TupleExpr, arena: &'a Arena) -> Self::Result {
        self.title("tuple-expr")?
            .span(node.span)?
            .tagged_slice(&node.items, arena)?.finish()
    }

    fn visit_tuple_index_expr<'a, 'b: 'a>(&mut self, node: &'b TupleIndexExpr, arena: &'a Arena) -> Self::Result {
        self.title("tuple-index-expr")?
            .span(node.span)?
            .str(" . ")?.span(node.op_span)?
            .space()?.any(node.value)?
            .space()?.span(node.value_span)?
            .role("base").tagged_index(&node.base, arena)?.finish()
    }

    fn visit_tuple_type<'a, 'b: 'a>(&mut self, node: &'b TupleType, arena: &'a Arena) -> Self::Result {
        self.title("tuple-type")?
            .span(node.span)?
            .tagged_slice(&node.parameters, arena)?.finish()
    }

    fn visit_type_def<'a, 'b: 'a>(&mut self, node: &'b TypeDef, arena: &'a Arena) -> Self::Result {
        self.title("type-def")?
            .span(node.span)?
            .index(&node.name, arena)?
            .optional_tagged(&node.from, arena)?.finish()
    }

    fn visit_unary_expr<'a, 'b: 'a>(&mut self, node: &'b UnaryExpr, arena: &'a Arena) -> Self::Result {
        self.title("unary-expr")?
            .span(node.span)?
            .space()?.str(node.op.display())?
            .space()?.span(node.op_span)?
            .tagged_index(&node.base, arena)?.finish()
    }

    fn visit_use_stmt<'a, 'b: 'a>(&mut self, node: &'b UseStatement, arena: &'a Arena) -> Self::Result {
        self.title("use-stmt")?
            .span(node.span)?;
        if let Some(alias) = node.alias {
            self.str(" alias ")?.idspan(alias)?;
        }
        self.index(&node.path, arena)?.finish()
    }

    fn visit_var_decl_stmt<'a, 'b: 'a>(&mut self, node: &'b VarDeclStatement, arena: &'a Arena) -> Self::Result {
        self.title("var-decl-stmt")?
            .span(node.span)?
            .str(if node.r#const { " const " } else { " mutable " })?.idspan(node.name)?
            .optional_tagged(&node.r#type, arena)?
            .optional_tagged(&node.init_value, arena)?.finish()
    }

    fn visit_where_clause<'a, 'b: 'a>(&mut self, node: &'b WhereClause, arena: &'a Arena) -> Self::Result {
        self.title("where-clause")?
            .span(node.span)?
            .space()?.idspan(node.name)?
            .tagged_slice(&node.constraints, arena)?.finish()
    }

    fn visit_while_stmt<'a, 'b: 'a>(&mut self, node: &'b WhileStatement, arena: &'a Arena) -> Self::Result {
        self.title("while-stmt")?
            .span(node.span)?
            .label(node.label)?
            .role("condition").tagged_index(&node.condition, arena)?
            .role("body").index(&node.body, arena)?.finish()
    }
}
