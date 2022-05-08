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

// copy from one ast integration test, require all types of node covered
#[cfg(test)]
#[test]
fn pretty() {
    use crate::source::VirtualFileSystem;
    use crate::diagnostics::make_errors;
    use crate::lexical::Parser as Scanner;
    use crate::syntax::{Parser, ast::asti};
    
    let arena = Arena::new();
    let mut sources = SourceContext::new_file_system(VirtualFileSystem{
        cwd: "/".into(), 
        files: [("1".into(), include_str!("../../../tests/ast/generic.f3").into())].into_iter().collect() 
    });
    let mut diagnostics = make_errors!();
    let mut parser = Parser::new(Scanner::new(sources.entry("1", &mut diagnostics).unwrap(), &mut diagnostics));
    let actual = parser.parse_module(&arena);
    parser.finish();
    if let Ok(actual) = actual {
        let mut profiler = asti::MemoryProfiler::new();
        actual.accept(&arena, &mut profiler);
        let (covered, all, not_covered) = profiler.coverage();
        assert_eq!(covered, all, "{not_covered}");
    } else {
        assert!(false, "{}", diagnostics.display(&sources));
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
        if self.additional.len() > 0 {
            self.base.write_char('(')?;
            self.base.write_str(self.additional)?;
            self.base.write_char(')')?;
        }
        self.base.write_char(' ')?;
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

    fn endl(&mut self) -> Result<&mut Self, fmt::Error> {
        self.base.write_char('\n')?;
        Ok(self)
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
        $self.level += 1;
        $e.into_result().map_err(|_| fmt::Error)?;
        $self.level -= 1;
        Ok($self)
    }};
}

impl<'s, 'f1, 'f2, F, const I: usize> Formatter<'s, 'f1, 'f2, F, I> where F: FileSystem {

    // first call in visit implement, map index to object, also record to check circular reference
    fn enter<'a, N>(&self, node: Index<N>, arena: &'a Arena) -> &'a N {
        arena.get(node)
    }

    // forward index or enum
    fn forward<N: Visit>(&mut self, index: N, arena: &Arena) -> Result<&mut Self, fmt::Error> {
        next_level!(self, index.accept(arena, self))
    }

    // optional index or enum
    fn optional<N: Visit>(&mut self, index: Option<N>, arena: &Arena) -> Result<&mut Self, fmt::Error> {
        next_level!(self, if let Some(index) = index { index.accept(arena, self) } else { self.additional = ""; EmptyResult(true) })
    }

    fn slice<N: Visit>(&mut self, slice: Slice<N>, arena: &Arena) -> Result<&mut Self, fmt::Error> {
        next_level!(self, { for item in arena.get_iter(slice) { item.accept(arena, self); } EmptyResult(true) })
    }
}

impl<'s, 'f1, 'f2, F, const I: usize> Visitor for Formatter<'s, 'f1, 'f2, F, I> where F: FileSystem {
    type Result = EmptyResult;

    // default implementation correctly transparents this type of node
    // fn visit_expr(&mut self, node: &'b Expr, arena: &Arena) -> Self::Result;
    // fn visit_stmt(&mut self, node: &'b Statement, arena: &Arena) -> Self::Result;
    // fn visit_item(&mut self, node: &'b Item, arena: &Arena) -> Self::Result;
    // fn visit_type_list(&mut self, node: &'b TypeList, arena: &Arena) -> Self::Result;
    // fn visit_type_ref(&mut self, node: &'b TypeRef, arena: &Arena) -> Self::Result;

    // implementation principle / pretty format style
    // 1. each node instance is one line, each line is one node instance
    // 2. each line starts with `{indent}{title} {primary span}`, primary span may missing for some type of nodes
    //    remaining part of this line is normally one or several `{desc/stringid} {span}`

    fn visit_array_expr(&mut self, node: Index<ArrayExpr>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("array-expr")?
            .span(node.span)?.endl()?
            .slice(node.items, arena)?.finish()
    }

    fn visit_array_index_expr(&mut self, node: Index<ArrayIndexExpr>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("array-index-expr")?
            .span(node.span)?
            .str(" [] ")?.span(node.quote_span)?.endl()?
            .role("base").forward(node.base, arena)?
            .slice(node.parameters, arena)?.finish()
    }

    fn visit_array_type(&mut self, node: Index<ArrayType>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("array-type")?
            .span(node.span)?.endl()?
            .role("base").forward(node.base, arena)?
            .role("size").forward(node.size, arena)?.finish()
    }

    fn visit_assign_expr_stmt(&mut self, node: Index<AssignExprStatement>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("assign-expr-stmt")?
            .span(node.span)?
            .space()?.str(node.op.display())?
            .space()?.span(node.op_span)?.endl()?
            .role("left").forward(node.left, arena)?
            .role("right").forward(node.right, arena)?.finish()
    }

    fn visit_binary_expr(&mut self, node: Index<BinaryExpr>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("binary-expr")?
            .span(node.span)?
            .space()?.str(node.op.display())?
            .space()?.span(node.op_span)?.endl()?
            .role("left").forward(node.left, arena)?
            .role("right").forward(node.right, arena)?.finish()
    }

    fn visit_block(&mut self, node: Index<Block>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("block")?
            .span(node.span)?.endl()?
            .slice(node.items, arena)?.finish()
    }

    fn visit_block_stmt(&mut self, node: Index<BlockStatement>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("block-stmt")?
            .span(node.span)?
            .label(node.label)?.endl()?
            .role("body").forward(node.body, arena)?.finish()
    }

    fn visit_break_stmt(&mut self, node: Index<BreakStatement>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("break-stmt")?
            .span(node.span)?
            .label(node.label)?.endl()?.finish()
    }

    fn visit_call_expr(&mut self, node: Index<CallExpr>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("call-expr")?
            .span(node.span)?
            .str(" () ")?.span(node.quote_span)?.endl()?
            .role("base").forward(node.base, arena)?
            .slice(node.parameters, arena)?.finish()
    }

    fn visit_cast_segment(&mut self, node: Index<CastSegment>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("cast-segment")?
            .span(node.span)?.endl()?
            .role("left").forward(node.left, arena)?
            .role("right").forward(node.right, arena)?.finish()
    }

    fn visit_class_def(&mut self, node: Index<ClassDef>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("class-def")?
            .span(node.span)?
            .str(" {} ")?.span(node.quote_span)?.endl()?
            .role("name").forward(node.name, arena)?
            .slice(node.items, arena)?.finish()
    }

    fn visit_continue_stmt(&mut self, node: Index<ContinueStatement>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("continue-stmt")?
            .span(node.span)?
            .label(node.label)?.endl()?.finish()
    }
    
    fn visit_else_clause(&mut self, node: Index<ElseClause>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("else-clause")?
            .span(node.span)?.endl()?
            .role("body").forward(node.body, &arena)?.finish()
    }

    fn visit_enum_def(&mut self, node: Index<EnumDef>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("enum-def")?
            .span(node.span)?
            .space()?.idspan(node.name)?
            .str(" {} ")?.span(node.quote_span)?.endl()?
            .role("base-type").optional(node.base_type, arena)?
            .slice(node.variants, arena)?.finish()
    }

    fn visit_enum_def_variant(&mut self, node: Index<EnumDefVariant>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("enum-def-variant")?
            .span(node.span)?
            .space()?.idspan(node.name)?.endl()?
            .role("init").optional(node.value, arena)?.finish()
    }

    fn visit_field_def(&mut self, node: Index<FieldDef>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("field-def")?
            .span(node.span)?
            .space()?.idspan(node.name)?
            .str(" : ")?.span(node.colon_span)?.endl()?
            .forward(node.r#type, arena)?.finish()
    }

    fn visit_fn_def(&mut self, node: Index<FnDef>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("fn-def")?
            .span(node.span)?
            .str(" () ")?.span(node.quote_span)?.endl()?
            .forward(node.name, arena)?
            .slice(node.parameters, arena)?
            .role("ret-type").optional(node.ret_type, arena)?
            .slice(node.wheres, arena)?
            .role("body").optional(node.body, arena)?.finish()
    }

    fn visit_fn_def_parameter(&mut self, node: Index<FnDefParameter>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("fn-def-parameter")?
            .span(node.span)?
            .space()?.idspan(node.name)?.endl()?
            .forward(node.r#type, arena)?.finish()
    }

    fn visit_fn_type(&mut self, node: Index<FnType>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("fn-type")?
            .span(node.span)?
            .str(" () ")?.span(node.quote_span)?.endl()?
            .slice(node.parameters, arena)?
            .role("ret-type").optional(node.ret_type, arena)?.finish()
    }

    fn visit_fn_type_parameter(&mut self, node: Index<FnTypeParameter>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("fn-type-parameter")?
            .span(node.span)?
            .optional_idspan(node.name)?.endl()?
            .forward(node.r#type, arena)?.finish()
    }

    fn visit_for_stmt(&mut self, node: Index<ForStatement>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("for-stmt")?
            .span(node.span)?
            .str(" iter-var ")?.idspan(node.iter_name)?
            // TODO: should be before iter-var?
            .label(node.label)?.endl()?
            .role("iter-expr").forward(node.iter_expr, arena)?
            .role("body").forward(node.body, arena)?.finish()
    }

    fn visit_generic_name(&mut self, node: Index<GenericName>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("generic-name")?
            .span(node.span)?
            .space()?.idspan(node.base)?;
        if node.quote_span != Span::new(0, 0) {
            self.str(" <> ")?.span(node.quote_span)?;
        }
        self.endl()?.slice(node.parameters, arena)?.finish()
    }

    fn visit_generic_parameter(&mut self, node: Index<GenericParameter>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("generic-parameter")?
            .span(node.span)?
            .space()?.isid(node.name.id)?.endl()?.finish()
    }

    fn visit_generic_segment(&mut self, node: Index<GenericSegment>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("generic-segment")?
            .span(node.span)?
            .space()?.idspan(node.base)?.endl()?
            .forward(node.parameters, arena)?.finish()
    }

    fn visit_if_clause(&mut self, node: Index<IfClause>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("if-clause")?
            .span(node.span)?.endl()?
            .role("condition").forward(node.condition, arena)?
            .role("body").forward(node.body, arena)?.finish()
    }

    fn visit_if_stmt(&mut self, node: Index<IfStatement>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("if-stmt")?
            .span(node.span)?.endl()?
            .forward(node.if_clause, arena)?
            .slice(node.elseif_clauses, arena)?
            .optional(node.else_clause, arena)?.finish()
    }
    
    fn visit_impl_block(&mut self, node: Index<Implementation>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("impl-block")?
            .span(node.span)?
            .str(" {} ")?.span(node.quote_span)?.endl()?
            .slice(node.parameters, arena)?
            .role("class").optional(node.class, arena)?
            .forward(node.r#type, arena)?
            .slice(node.wheres, arena)?
            .slice(node.items, arena)?.finish()
    }

    fn visit_lit_expr(&mut self, node: Index<LitExpr>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("lit-expr")?;
        match node.value {
            LitValue::Unit => self.str("unit")?,
            LitValue::Bool(v) => self.any(format_args!("bool {v}"))?,
            LitValue::Char(v) => self.any(format_args!("char {v:?}"))?,
            LitValue::Str(id) => self.str("str \"")?.isid(id)?.str("\"")?,
            LitValue::Num(v) => self.any(format_args!("{v}"))?,
        };
        self.space()?.span(node.span)?.endl()?.finish()
    }

    fn visit_loop_stmt(&mut self, node: Index<LoopStatement>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("loop-stmt")?
            .span(node.span)?
            .label(node.label)?.endl()?
            .role("body").forward(node.body, arena)?.finish()
    }

    fn visit_member_expr(&mut self, node: Index<MemberExpr>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("member-expr")?
            .span(node.span)?
            .str(" . ")?.span(node.op_span)?
            .space()?.idspan(node.name)?.endl()?
            .role("base").forward(node.base, arena)?
            .optional(node.parameters, arena)?.finish()
    }

    fn visit_module(&mut self, node: Index<Module>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("module")?
            .file_id(node.file)?.endl()?
            .slice(node.items, arena)?.finish()
    }

    fn visit_module_stmt(&mut self, node: Index<ModuleStatement>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("module-stmt")?
            .span(node.span)?
            .space()?.idspan(node.name)?;
        if let Some(path) = node.path {
            self.str(" path ")?.idspan(path)?;
        }
        self.endl()?.finish()
    }

    fn visit_object_expr(&mut self, node: Index<ObjectExpr>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("object-expr")?
            .span(node.span)?
            .str(" {} ")?.span(node.quote_span)?.endl()?
            .role("base").forward(node.base, arena)?
            .slice(node.fields, arena)?.finish()
    }

    fn visit_object_expr_field(&mut self, node: Index<ObjectExprField>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("object-expr-field")?
            .span(node.span)?
            .space()?.idspan(node.name)?.endl()?
            .forward(node.value, arena)?.finish()
    }

    fn visit_paren_expr(&mut self, node: Index<ParenExpr>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("paren-expr")?
            .span(node.span)?.endl()?
            .forward(node.base, arena)?.finish()
    }

    fn visit_path(&mut self, node: Index<Path>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("path")?
            .span(node.span)?.endl()?
            .slice(node.segments, arena)?.finish()
    }

    fn visit_path_segment(&mut self, node: PathSegment, arena: &Arena) -> Self::Result {
        match node {
            PathSegment::Global => self.title("global-segment")?.endl()?.finish(),
            PathSegment::Simple(v) => self.visit_simple_segment(v, arena),
            PathSegment::Cast(v) => self.visit_cast_segment(v, arena),
            PathSegment::Generic(v) => self.visit_generic_segment(v, arena),
        }
    }

    fn visit_primitive_type(&mut self, node: Index<PrimitiveType>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("primitive-type")?
            .str(node.base.display())?
            .space()?.span(node.span)?.endl()?.finish()
    }
    
    fn visit_range_both_expr(&mut self, node: Index<RangeBothExpr>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("range-both-expr")?
            .span(node.span)?
            .str(" dotdot ")?.span(node.op_span)?.endl()?
            .role("left").forward(node.left, arena)?
            .role("right").forward(node.right, arena)?.finish()
    }
    
    fn visit_range_full_expr(&mut self, node: Index<RangeFullExpr>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("range-full-expr")?
            .span(node.span)?.endl()?.finish()
    }

    fn visit_range_left_expr(&mut self, node: Index<RangeLeftExpr>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("range-left-expr")?
            .span(node.span)?.endl()?
            .forward(node.base, arena)?.finish()
    }

    fn visit_range_right_expr(&mut self, node: Index<RangeRightExpr>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("range-right-expr")?
            .span(node.span)?.endl()?
            .forward(node.base, arena)?.finish()
    }

    fn visit_ref_type(&mut self, node: Index<RefType>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("ref-type")?
            .span(node.span)?.endl()?
            .forward(node.base, arena)?.finish()
    }

    fn visit_ret_stmt(&mut self, node: Index<ReturnStatement>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("ret-stmt")?
            .span(node.span)?.endl()?
            .optional(node.value, arena)?.finish()
    }

    fn visit_simple_expr_stmt(&mut self, node: Index<SimpleExprStatement>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("simple-expr-stmt")?
            .span(node.span)?.endl()?
            .forward(node.expr, arena)?.finish()
    }

    fn visit_simple_segment(&mut self, node: Index<SimpleSegment>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("segment")?
            .span(node.span)?
            .space()?.isid(node.name)?.endl()?.finish()
    }

    fn visit_struct_def(&mut self, node: Index<StructDef>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("struct-def")?
            .span(node.span)?.endl()?
            .forward(node.name, arena)?
            .slice(node.fields, arena)?.finish()
    }

    fn visit_tuple_expr(&mut self, node: Index<TupleExpr>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("tuple-expr")?
            .span(node.span)?.endl()?
            .slice(node.items, arena)?.finish()
    }

    fn visit_tuple_index_expr(&mut self, node: Index<TupleIndexExpr>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("tuple-index-expr")?
            .span(node.span)?
            .str(" . ")?.span(node.op_span)?
            .space()?.any(node.value)?
            .space()?.span(node.value_span)?.endl()?
            .role("base").forward(node.base, arena)?.finish()
    }

    fn visit_tuple_type(&mut self, node: Index<TupleType>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("tuple-type")?
            .span(node.span)?.endl()?
            .slice(node.parameters, arena)?.finish()
    }

    fn visit_type_def(&mut self, node: Index<TypeDef>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("type-def")?
            .span(node.span)?.endl()?
            .forward(node.name, arena)?
            .optional(node.from, arena)?.finish()
    }

    fn visit_unary_expr(&mut self, node: Index<UnaryExpr>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("unary-expr")?
            .span(node.span)?
            .space()?.str(node.op.display())?
            .space()?.span(node.op_span)?.endl()?
            .forward(node.base, arena)?.finish()
    }

    fn visit_use_stmt(&mut self, node: Index<UseStatement>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("use-stmt")?
            .span(node.span)?;
        if let Some(alias) = node.alias {
            self.str(" alias ")?.idspan(alias)?;
        }
        self.endl()?.forward(node.path, arena)?.finish()
    }

    fn visit_var_decl_stmt(&mut self, node: Index<VarDeclStatement>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("var-decl-stmt")?
            .span(node.span)?
            .str(if node.r#const { " const " } else { " mutable " })?.idspan(node.name)?.endl()?
            .optional(node.r#type, arena)?
            .optional(node.init_value, arena)?.finish()
    }

    fn visit_where_clause(&mut self, node: Index<WhereClause>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("where-clause")?
            .span(node.span)?
            .space()?.idspan(node.name)?.endl()?
            .slice(node.constraints, arena)?.finish()
    }

    fn visit_while_stmt(&mut self, node: Index<WhileStatement>, arena: &Arena) -> Self::Result {
        let node = self.enter(node, arena);
        self.title("while-stmt")?
            .span(node.span)?
            .label(node.label)?.endl()?
            .role("condition").forward(node.condition, arena)?
            .role("body").forward(node.body, arena)?.finish()
    }
}
