///! syntax::pretty: ast type pretty formatter

use std::fmt::{self, Write};
use crate::source::{SourceContext, FileSystem, Span, IsId, IdSpan};
use super::visit::{Node, Visitor};
use super::ast::*;

static INDENTIONS: [[&str; 16]; 3] = [
    [
        "", "1 ", "2 | ", "3 | | ", "4 | | | ", "5 | | | | ", "6 | | | | | ", "7 | | | | | | ", "8 | | | | | | | ", "9 | | | | | | | | ", "10| | | | | | | | | ",
        "11| | | | | | | | | | ", "12| | | | | | | | | | | ", "13| | | | | | | | | | | | ", "14| | | | | | | | | | | | | ", "15| | | | | | | | | | | | "
    ], [
        "", "| ", "| | ", "| | | ", "| | | | ", "| | | | | ", "| | | | | | ", "| | | | | | | ", "| | | | | | | | ", "| | | | | | | | | ", "| | | | | | | | | | ",
        "| | | | | | | | | | | ", "| | | | | | | | | | | | ", "| | | | | | | | | | | | | ", "| | | | | | | | | | | | | | ", "| | | | | | | | | | | | | "
    ], [
        "", "  ", "    ", "      ", "        ", "          ", "            ", "              ", "                ", "                  ", "                    ",
        "                      ", "                        ", "                          ", "                            ", "                              "
    ]
];

pub struct FormatVisitor<'scx, 'f1, 'f2, F> {
    level: usize,
    scx: &'scx SourceContext<F>,
    f: &'f1 mut fmt::Formatter<'f2>,
}

impl<'scx, 'f1, 'f2, F> FormatVisitor<'scx, 'f1, 'f2, F> where F: FileSystem {

    pub(super) fn new(scx: &'scx SourceContext<F>, f: &'f1 mut fmt::Formatter<'f2>) -> Self {
        Self{ level: 0, scx, f }
    }

    fn write_str(&mut self, v: &'static str) -> Result<&mut Self, fmt::Error> {
        self.f.write_str(v)?;
        Ok(self)
    }
    fn write_space(&mut self) -> Result<&mut Self, fmt::Error> {
        self.f.write_char(' ')?;
        Ok(self)
    }

    fn write_indent(&mut self) -> Result<&mut Self, fmt::Error> {
        let mut level = self.level;
        while level > 15 {
            self.f.write_str(INDENTIONS[2][15])?;
            level -= 15;
        }
        self.f.write_str(INDENTIONS[2][level])?;
        Ok(self)
    }

    fn write_span(&mut self, span: Span) -> Result<&mut Self, fmt::Error> {
        let (_, start_line, end_line, start_column, end_column) = self.scx.map_span_to_line_column(span);
        write!(self.f, "<{}:{}-{}:{}>", start_line, end_line, start_column, end_column)?;
        Ok(self)
    }
    fn write_isid(&mut self, id: IsId) -> Result<&mut Self, fmt::Error> {
        if id.unwrap() == 1 {
            self.f.write_str("<empty>")?;
        } else {
            let content = self.scx.resolve_string(id);
            self.f.write_str(if content.len() > 24 { &content[..24] } else { content })?;
        }
        Ok(self)
    }
    fn write_idspan(&mut self, id: IdSpan) -> Result<&mut Self, fmt::Error> {
        self.write_isid(id.id)?;
        self.write_space()?;
        self.write_span(id.span)
    }

    fn invoke_walk<N: Node>(&mut self, node: &N) -> fmt::Result {
        self.f.write_char('\n')?;
        self.level += 1;
        let result = node.walk(self);
        self.level -= 1;
        result
    }

    fn impl_visit_simple<N: Node>(&mut self, node: &N, title: &'static str, span: Span) -> fmt::Result {
        self.write_indent()?.write_str(title)?.write_space()?.write_span(span)?;
        self.invoke_walk(node)
    }
    
    fn impl_visit<N: Node>(&mut self, node: &N, title: &'static str, span: Span, and: impl FnOnce(&mut Self) -> Result<&mut Self, fmt::Error>) -> fmt::Result {
        self.write_indent()?.write_str(title)?.write_space()?.write_span(span)?;
        and(self)?;
        self.invoke_walk(node)
    }

    fn impl_visit_no_primary_span<N: Node>(&mut self, node: &N, title: &'static str, and: impl FnOnce(&mut Self) -> Result<&mut Self, fmt::Error>) -> fmt::Result {
        self.write_indent()?.write_str(title)?;
        and(self)?;
        self.invoke_walk(node)
    }
}

impl<'scx, 'f1, 'f2, F> Visitor<(), fmt::Error> for FormatVisitor<'scx, 'f1, 'f2, F> where F: FileSystem {

    // default implementation correctly transparents this type of node
    // fn visit_expr_list(&mut self, node: &ExprList) -> fmt::Result;
    // fn visit_expr(&mut self, node: &Expr) -> fmt::Result;
    // fn visit_stmt(&mut self, node: &Statement) -> fmt::Result;
    // fn visit_item(&mut self, node: &Item) -> fmt::Result;
    // fn visit_type_list(&mut self, node: &TypeList) -> fmt::Result;
    // fn visit_type_ref(&mut self, node: &TypeRef) -> fmt::Result;

    // implementation principle / syntax tree format style
    // 1. each node instance is one line, each line is one node instance
    // 2. each line starts with `{indent}{title} {primary span}`, primary span may missing for some type of nodes
    //    remaining part of this line is normally one or several `{desc/stringid} {span}`
    // 3. this concrete visitor should never call other visit methods in visit implementations, include this method, only call node.walk

    fn visit_array_expr(&mut self, node: &ArrayExpr) -> fmt::Result {
        self.impl_visit_simple(node, "array-expr", node.span)
    }

    fn visit_array_type(&mut self, node: &ArrayType) -> fmt::Result {
        self.impl_visit_simple(node, "array-type", node.span)
    }

    fn visit_assign_expr_stmt(&mut self, node: &AssignExprStatement) -> fmt::Result {
        self.impl_visit(node, "assign-expr-stmt", node.span, |f|
            f.write_space()?.write_str(node.op.display())?.write_space()?.write_span(node.op_span))
    }

    fn visit_binary_expr(&mut self, node: &BinaryExpr) -> fmt::Result {
        self.impl_visit(node, "binary-expr", node.span, |f|
            f.write_space()?.write_str(node.op.display())?.write_space()?.write_span(node.op_span))
    }

    fn visit_block_stmt(&mut self, node: &BlockStatement) -> fmt::Result {
        self.impl_visit(node, "block-stmt", node.span, |f| {
            if let Some(label) = node.label {
                f.write_str(" @")?.write_idspan(label)?;
            }
            Ok(f)
        })
    }

    fn visit_block(&mut self, node: &Block) -> fmt::Result {
        self.impl_visit_simple(node, "block", node.span)
    }

    fn visit_break_stmt(&mut self, node: &BreakStatement) -> fmt::Result {
        self.impl_visit(node, "break-stmt", node.span, |f| {
            if let Some(label) = node.label {
                f.write_str(" @")?.write_idspan(label)?;
            }
            Ok(f)
        })
    }

    fn visit_continue_stmt(&mut self, node: &ContinueStatement) -> fmt::Result {
        self.impl_visit(node, "continue-stmt", node.span, |f| {
            if let Some(label) = node.label {
                f.write_str(" @")?.write_idspan(label)?;
            }
            Ok(f)
        })
    }

    fn visit_enum_def(&mut self, node: &EnumDef) -> fmt::Result {
        self.impl_visit(node, "enum-def", node.span, |f| f
            .write_space()?.write_idspan(node.name)?
            .write_str(" {} ")?.write_span(node.quote_span))
    }

    fn visit_enum_def_variant(&mut self, node: &EnumDefVariant) -> fmt::Result {
        self.impl_visit(node, "enum-def-variant", node.span, |f|
            f.write_space()?.write_idspan(node.name))
    }

    fn visit_call_expr(&mut self, node: &CallExpr) -> fmt::Result {
        self.impl_visit(node, "call-expr", node.span, |f|
            f.write_str(" () ")?.write_span(node.quote_span))
    }

    fn visit_fn_def(&mut self, node: &FnDef) -> fmt::Result {
        self.impl_visit(node, "fn-def", node.span, |f| f
            .write_space()?.write_idspan(node.name)?
            .write_str(" () ")?.write_span(node.quote_span))
    }

    fn visit_fn_def_parameter(&mut self, node: &FnDefParameter) -> fmt::Result {
        self.impl_visit(node, "fn-def-parameter", node.span, |f| 
            f.write_space()?.write_idspan(node.name))
    }

    fn visit_fn_type(&mut self, node: &FnType) -> fmt::Result {
        self.impl_visit(node, "fn-type", node.span, |f|
            f.write_str(" () ")?.write_span(node.quote_span))
    }

    fn visit_fn_type_parameter(&mut self, node: &FnTypeParameter) -> fmt::Result {
        self.impl_visit(node, "fn-type-parameter", node.span, |f| {
            if let Some(name) = node.name {
                f.write_space()?.write_idspan(name)?;
            }
            Ok(f)
        })
    }

    fn visit_for_stmt(&mut self, node: &ForStatement) -> fmt::Result {
        self.impl_visit(node, "for-stmt", node.span, |f| {
            f.write_str(" iter-var ")?.write_idspan(node.iter_name)?;
            if let Some(label) = node.label {
                f.write_str(" @")?.write_idspan(label)?;
            }
            Ok(f)
        })
    }

    fn visit_if_stmt(&mut self, node: &IfStatement) -> fmt::Result {
        self.impl_visit_simple(node, "if-stmt", node.span)
    }

    fn visit_if_clause(&mut self, node: &IfClause) -> fmt::Result {
        self.impl_visit_simple(node, "if-clause", node.span)
    }
    
    fn visit_else_clause(&mut self, node: &ElseClause) -> fmt::Result {
        self.impl_visit_simple(node, "else-clause", node.span)
    }

    fn visit_index_expr(&mut self, node: &IndexExpr) -> fmt::Result {
        self.impl_visit(node, "index-expr", node.span, |f|
            f.write_str(" [] ")?.write_span(node.quote_span))
    }
    
    fn visit_lit_expr(&mut self, node: &LitExpr) -> fmt::Result {
        self.impl_visit_no_primary_span(node, "literal", |f| {
            f.write_space()?;
            match node.value {
                LitValue::Unit => { f.write_str("unit")?; },
                LitValue::Bool(v) => { write!(f.f, "bool {v}")?; },
                LitValue::Char(v) => { write!(f.f, "char {v:?}")?; },
                LitValue::Str(id) => { f.write_str("str \"")?.write_isid(id)?.write_str("\"")?; },
                LitValue::Num(v) => { write!(f.f, "{v}")?; },
            }
            f.write_space()?.write_span(node.span)
        })
    }

    fn visit_loop_stmt(&mut self, node: &LoopStatement) -> fmt::Result {
        self.impl_visit(node, "loop-stmt", node.span, |f| {
            if let Some(label) = node.label {
                f.write_str(" @")?.write_idspan(label)?;
            }
            Ok(f)
        })
    }

    fn visit_member_expr(&mut self, node: &MemberExpr) -> fmt::Result {
        self.impl_visit(node, "member-expr", node.span, |f| f
            .write_str(" . ")?.write_span(node.op_span))
    }

    fn visit_member_name(&mut self, node: &MemberName) -> fmt::Result {
        self.impl_visit(node, "member-name", node.span, |f| {
            f.write_space()?;
            match &node.base {
                MemberNameBase::Ident(id) => { f.write_isid(*id)?; },
                MemberNameBase::Numeric(v) => write!(f.f, "{v}")?,
            }
            f.write_space()?.write_span(node.base_span)?;
            Ok(f)
        })
    }

    fn visit_module(&mut self, node: &Module) -> fmt::Result {
        self.impl_visit_no_primary_span(node, "module", |f| {
            f.write_space()?;
            write!(f.f, "{}", self.scx.get_relative_path(node.file).display())?;
            Ok(f)
        })
    }

    fn visit_module_stmt(&mut self, node: &ModuleStatement) -> fmt::Result {
        self.impl_visit(node, "module-stmt", node.span, |f| {
            f.write_space()?.write_idspan(node.name)?;
            if let Some(path) = node.path {
                f.write_str(" path ")?.write_idspan(path)?;
            }
            Ok(f)
        })
    }

    fn visit_name(&mut self, node: &Name) -> fmt::Result {
        self.impl_visit(node, "name", node.span, |f| {
            if node.global {
                f.write_str(" global")?;
            }
            Ok(f)
        })
    }

    fn visit_object_expr(&mut self, node: &ObjectExpr) -> fmt::Result {
        self.impl_visit(node, "object-expr", node.span, |f|
            f.write_str(" {} ")?.write_span(node.quote_span))
    }

    fn visit_object_expr_field(&mut self, node: &ObjectExprField) -> fmt::Result {
        self.impl_visit(node, "object-expr-field", node.span, |f| 
            f.write_space()?.write_idspan(node.name))
    }

    fn visit_plain_type(&mut self, node: &PlainType) -> fmt::Result {
        self.impl_visit(node, "plain-type", node.span, |f| {
            if node.global {
                f.write_str(" global")?;
            }
            Ok(f)
        })
    }

    fn visit_type_segment(&mut self, node: &TypeSegment) -> fmt::Result {
        self.impl_visit(node, "type-segment", node.span, |f| {
            f.write_space()?.write_idspan(node.base)?;
            Ok(f)
        })
    }

    fn visit_type_as_segment(&mut self, node: &TypeAsSegment) -> fmt::Result {
        self.impl_visit_simple(node, "type-as-segment", node.span)
    }

    fn visit_paren_expr(&mut self, node: &ParenExpr) -> fmt::Result {
        self.impl_visit_simple(node, "paren-expr", node.span)
    }

    fn visit_path(&mut self, node: &Path) -> fmt::Result {
        self.impl_visit_simple(node, "path", node.span)
    }

    fn visit_path_segment(&mut self, node: &PathSegment) -> fmt::Result {
        self.impl_visit(node, "segment", node.span(), |f| {
            match node {
                PathSegment::Global => f.write_str(" global"),
                PathSegment::Simple(name) => f.write_space()?.write_idspan(*name),
                PathSegment::TypeCast{ .. } => Ok(f),
                PathSegment::Generic{ base, .. } => f.write_space()?.write_idspan(*base),
            }
        })
    }

    fn visit_primitive_type(&mut self, node: &PrimitiveType) -> fmt::Result {
        self.impl_visit_no_primary_span(node, "primitive-type", |f|
            f.write_space()?.write_str(node.base.display())?.write_space()?.write_span(node.span))
    }
    
    fn visit_range_both_expr(&mut self, node: &RangeBothExpr) -> fmt::Result {
        self.impl_visit(node, "range-both-expr", node.span, |f|
            f.write_str(" dotdot ")?.write_span(node.op_span))
    }
    
    fn visit_range_full_expr(&mut self, node: &RangeFullExpr) -> fmt::Result {
        self.impl_visit_simple(node, "range-full-expr", node.span)
    }

    fn visit_range_left_expr(&mut self, node: &RangeLeftExpr) -> fmt::Result {
        self.impl_visit_simple(node, "range-left-expr", node.span)
    }

    fn visit_range_right_expr(&mut self, node: &RangeRightExpr) -> fmt::Result {
        self.impl_visit_simple(node, "range-right-expr", node.span)
    }

    fn visit_ref_type(&mut self, node: &RefType) -> fmt::Result {
        self.impl_visit_simple(node, "ref-type", node.span)
    }

    fn visit_ret_stmt(&mut self, node: &ReturnStatement) -> fmt::Result {
        self.impl_visit_simple(node, "ret-stmt", node.span)
    }

    fn visit_simple_expr_stmt(&mut self, node: &SimpleExprStatement) -> fmt::Result {
        self.impl_visit_simple(node, "simple-expr-stmt", node.span)
    }

    fn visit_name_segment(&mut self, node: &NameSegment) -> fmt::Result {
        self.impl_visit(node, "name-segment", node.span(), |f| {
            if let NameSegment::Normal(name) = node {
                f.write_space()?.write_isid(name.id)?;
            }
            Ok(f)
        })
    }

    fn visit_tuple_expr(&mut self, node: &TupleExpr) -> fmt::Result {
        self.impl_visit_simple(node, "tuple-expr", node.span)
    }

    fn visit_type_def(&mut self, node: &TypeDef) -> fmt::Result {
        self.impl_visit(node, "type-ref", node.span, |f|
            f.write_space()?.write_idspan(node.name))
    }

    fn visit_tuple_type(&mut self, node: &TupleType) -> fmt::Result {
        self.impl_visit_simple(node, "tuple-type", node.span)
    }

    fn visit_type_def_field(&mut self, node: &TypeDefField) -> fmt::Result {
        self.impl_visit(node, "type-def-field", node.span, |f| f
            .write_space()?.write_idspan(node.name)?
            .write_str(" : ")?.write_span(node.colon_span))
    }

    fn visit_unary_expr(&mut self, node: &UnaryExpr) -> fmt::Result {
        self.impl_visit(node, "unary-expr", node.span, |f|
            f.write_space()?.write_str(node.op.display())?.write_space()?.write_span(node.op_span))
    }

    fn visit_use_stmt(&mut self, node: &UseStatement) -> fmt::Result {
        self.impl_visit(node, "use-stmt", node.span, |f| {
            if let Some(alias) = node.alias {
                f.write_str(" alias ")?.write_idspan(alias)?;
            }
            Ok(f)
        })
    }

    fn visit_var_decl(&mut self, node: &VarDeclStatement) -> fmt::Result {
        self.impl_visit(node, "var-def", node.span, |f| f
            .write_str(if node.r#const { " const " } else { " mutable " })?.write_idspan(node.name))
    }

    fn visit_while_stmt(&mut self, node: &WhileStatement) -> fmt::Result {
        self.impl_visit(node, "while-stmt", node.span, |f| {
            if let Some(label) = node.label {
                f.write_str(" @")?.write_idspan(label)?;
            }
            Ok(f)
        })
    }
}

pub struct NodeDisplay<'n, 'scx, N, F>{
    pub(in super) node: &'n N,
    pub(in super) context: &'scx SourceContext<F>,
}

impl<'n, 'scx, N: Node, F: FileSystem> fmt::Display for NodeDisplay<'n, 'scx, N, F> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut visitor = FormatVisitor::new(self.context, f);
        self.node.accept(&mut visitor)
    }
}
