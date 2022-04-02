///! syntax::pretty: ast type pretty formatter

use std::fmt::{self, Write};
use crate::source::{SourceContext, FileSystem, Span, IsId};
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
        "                      ", "                        ", "                          ", "                            ", "                          "
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

    fn write_str(&mut self, v: &str) -> Result<&mut Self, fmt::Error> {
        self.f.write_str(v)?;
        Ok(self)
    }
    fn write_space(&mut self) -> Result<&mut Self, fmt::Error> {
        self.f.write_char(' ')?;
        Ok(self)
    }

    fn write_indent(&mut self) -> Result<&mut Self, fmt::Error> { 
        self.f.write_str(INDENTIONS[2][self.level])?;
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
    // fn visit_type_ref(&mut self, node: &TypeRef) -> fmt::Result;

    // implementation principle / syntax tree format style
    // 1. each node instance is one line, each line is one node instance
    // 2. each line starts with `{indent}{title} {primary span}`, primary span may missing for some type of nodes
    //    remaining part of this line is normally one or several `{desc/stringid} {span}`
    // 3. this concrete visitor should never call other visit methods in visit implementations, include this method, only call node.walk

    fn visit_array_def(&mut self, node: &ArrayDef) -> fmt::Result {
        self.impl_visit_simple(node, "array-def", node.bracket_span)
    }

    fn visit_array_type(&mut self, node: &ArrayType) -> fmt::Result {
        self.impl_visit_simple(node, "array-type", node.span)
    }

    fn visit_assign_expr_stmt(&mut self, node: &AssignExprStatement) -> fmt::Result {
        self.impl_visit(node, "assign-expr-stmt", node.all_span, |f|
            f.write_str(node.assign_op.display())?.write_space()?.write_span(node.assign_op_span))
    }

    fn visit_binary_expr(&mut self, node: &BinaryExpr) -> fmt::Result {
        self.impl_visit(node, "binary-expr", node.all_span, |f|
            f.write_space()?.write_str(node.operator.display())?.write_space()?.write_span(node.operator_span))
    }

    fn visit_block_stmt(&mut self, node: &BlockStatement) -> fmt::Result {
        self.impl_visit_simple(node, "block-stmt", node.all_span)
    }

    fn visit_block(&mut self, node: &Block) -> fmt::Result {
        self.impl_visit_simple(node, "block", node.all_span)
    }

    fn visit_break_stmt(&mut self, node: &BreakStatement) -> fmt::Result {
        self.impl_visit(node, "break-stmt", node.0.all_span, |f| {
            if let Some((label, label_span)) = node.0.target {
                f.write_str(" @")?.write_isid(label)?.write_space()?.write_span(label_span)?;
            }
            Ok(f)
        })
    }

    fn visit_continue_stmt(&mut self, node: &ContinueStatement) -> fmt::Result {
        self.impl_visit(node, "continue-stmt", node.0.all_span, |f| {
            if let Some((label, label_span)) = node.0.target {
                f.write_str(" @")?.write_isid(label)?.write_space()?.write_span(label_span)?;
            }
            Ok(f)
        })
    }

    fn visit_enum_def(&mut self, node: &EnumDef) -> fmt::Result {
        self.impl_visit(node, "enum-def", node.all_span, |f| f.write_space()?
            .write_isid(node.name)?.write_space()?.write_span(node.name_span)?
            .write_str(" {} ")?.write_span(node.quote_span))
    }

    fn visit_enum_variant(&mut self, node: &EnumVariant) -> fmt::Result {
        self.impl_visit(node, "enum-variant", node.all_span, |f|
            f.write_space()?.write_isid(node.name)?.write_space()?.write_span(node.name_span))
    }

    fn visit_fn_call_expr(&mut self, node: &FnCallExpr) -> fmt::Result {
        self.impl_visit(node, "fn-call", node.all_span, |f|
            f.write_str(" () ")?.write_span(node.paren_span))
    }

    fn visit_fn_def(&mut self, node: &FnDef) -> fmt::Result {
        self.impl_visit(node, "fn", node.all_span, |f| f.write_space()?
            .write_isid(node.name)?.write_space()?.write_span(node.name_span)?
            .write_str(" () ")?.write_span(node.params_paren_span))
    }

    fn visit_fn_param(&mut self, node: &FnParam) -> fmt::Result {
        self.impl_visit(node, "fn-param", node.name_span + node.r#type.get_all_span(), |f| 
            f.write_space()?.write_isid(node.name)?.write_space()?.write_span(node.name_span))
    }

    fn visit_fn_type(&mut self, node: &FnType) -> fmt::Result {
        self.impl_visit(node, "fn-type", node.all_span, |f|
            f.write_str(" () ")?.write_span(node.paren_span))
    }

    fn visit_fn_type_param(&mut self, node: &FnTypeParam) -> fmt::Result {
        self.impl_visit(node, "fn-type-param", node.all_span, |f| {
            if let Some((parameter_id, parameter_span)) = node.name {
                f.write_space()?.write_isid(parameter_id)?.write_space()?.write_span(parameter_span)?;
            }
            Ok(f)
        })
    }

    fn visit_for_stmt(&mut self, node: &ForStatement) -> fmt::Result {
        self.impl_visit(node, "for-stmt", node.all_span, |f| f.write_space()?
            .write_str("for ")?.write_span(node.for_span)?
            .write_str(" iter-var ")?.write_isid(node.iter_name)?.write_space()?.write_span(node.iter_span))
    }

    fn visit_if_stmt(&mut self, node: &IfStatement) -> fmt::Result {
        self.impl_visit_simple(node, "if-stmt", node.all_span)
    }

    fn visit_if_clause(&mut self, node: &IfClause) -> fmt::Result {
        self.impl_visit_simple(node, "if-clause", node.all_span)
    }
    
    fn visit_else_clause(&mut self, node: &ElseClause) -> fmt::Result {
        self.impl_visit_simple(node, "else-clause", node.all_span)
    }

    fn visit_index_call_expr(&mut self, node: &IndexCallExpr) -> fmt::Result {
        self.impl_visit(node, "index-call", node.all_span, |f|
            f.write_str(" [] ")?.write_span(node.bracket_span))
    }

    fn visit_label_def(&mut self, node: &LabelDef) -> fmt::Result {
        self.impl_visit_no_primary_span(node, "label @", |f|
            f.write_isid(node.name)?.write_space()?.write_span(node.all_span))
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
        self.impl_visit(node, "loop-stmt", node.all_span, |f|
            f.write_str(" loop ")?.write_span(node.loop_span))
    }

    fn visit_member_access(&mut self, node: &MemberAccessExpr) -> fmt::Result {
        self.impl_visit(node, "member-access", node.all_span, |f| f
            .write_str(" . ")?.write_span(node.dot_span))
    }

    fn visit_module(&mut self, node: &Module) -> fmt::Result {
        self.impl_visit_no_primary_span(node, "module", |f| {
            f.write_space()?;
            write!(f.f, "{}", self.scx.get_relative_path(node.file).display())?;
            Ok(f)
        })
    }

    fn visit_module_stmt(&mut self, node: &ModuleStatement) -> fmt::Result {
        self.impl_visit(node, "module-stmt", node.all_span, |f| {
            f.write_space()?.write_isid(node.name)?.write_space()?.write_span(node.name_span)?;
            if let Some((path, path_span)) = node.path {
                f.write_space()?.write_isid(path)?.write_space()?.write_span(path_span)?;
            }
            Ok(f)
        })
    }

    fn visit_name(&mut self, node: &Name) -> fmt::Result {
        self.impl_visit(node, "name", node.all_span, |f| {
            if node.global {
                f.write_str(" global")?;
            }
            Ok(f)
        })
    }

    fn visit_object_literal(&mut self, node: &ObjectLiteral) -> fmt::Result {
        self.impl_visit(node, "object", node.all_span, |f|
            f.write_str(" {} ")?.write_span(node.quote_span))
    }

    fn visit_object_literal_field(&mut self, node: &ObjectLiteralField) -> fmt::Result {
        self.impl_visit(node, "object-field", node.all_span, |f| f.write_space()?
            .write_isid(node.name)?.write_space()?.write_span(node.name_span)?
            .write_str(" : ")?.write_span(node.colon_span))
    }

    fn visit_plain_type(&mut self, node: &PlainType) -> fmt::Result {
        self.impl_visit(node, "plain-type", node.all_span, |f| {
            if node.global {
                f.write_str(" global")?;
            }
            Ok(f)
        })
    }

    fn visit_type_segment(&mut self, node: &TypeSegment) -> fmt::Result {
        self.impl_visit(node, "type-segment", node.all_span, |f| {
            f.write_space()?.write_isid(node.ident)?.write_space()?.write_span(node.ident_span)?;
            if !node.parameters.is_empty() {
                f.write_str(" <> ")?.write_span(node.quote_span)?;
            }
            Ok(f)
        })
    }

    fn visit_type_as_segment(&mut self, node: &TypeAsSegment) -> fmt::Result {
        self.impl_visit_simple(node, "type-as-segment", node.span)
    }

    fn visit_paren_expr(&mut self, node: &ParenExpr) -> fmt::Result {
        self.impl_visit_simple(node, "paren-expr", node.span)
    }

    fn visit_primitive_type(&mut self, node: &PrimitiveType) -> fmt::Result {
        self.impl_visit_no_primary_span(node, "primitive-type", |f|
            f.write_space()?.write_str(node.name.display())?.write_space()?.write_span(node.span))
    }
    
    fn visit_range_both_expr(&mut self, node: &RangeBothExpr) -> fmt::Result {
        self.impl_visit(node, "range-both-expr", node.all_span, |f|
            f.write_str(" dotdot ")?.write_span(node.op_span))
    }
    
    fn visit_range_full_expr(&mut self, node: &RangeFullExpr) -> fmt::Result {
        self.impl_visit_simple(node, "range-full-expr", node.all_span)
    }

    fn visit_range_left_expr(&mut self, node: &RangeLeftExpr) -> fmt::Result {
        self.impl_visit_simple(node, "range-left-expr", node.all_span)
    }

    fn visit_range_right_expr(&mut self, node: &RangeRightExpr) -> fmt::Result {
        self.impl_visit_simple(node, "range-right-expr", node.all_span)
    }

    fn visit_ref_type(&mut self, node: &RefType) -> fmt::Result {
        self.impl_visit_simple(node, "ref-type", node.span)
    }

    fn visit_ret_stmt(&mut self, node: &ReturnStatement) -> fmt::Result {
        self.impl_visit_simple(node, "ret-stmt", node.all_span)
    }

    fn visit_simple_expr_stmt(&mut self, node: &SimpleExprStatement) -> fmt::Result {
        self.impl_visit_simple(node, "simple-expr-stmt", node.all_span)
    }

    fn visit_name_segment(&mut self, node: &NameSegment) -> fmt::Result {
        self.impl_visit(node, "name-segment", node.get_span(), |f| {
            if let NameSegment::Normal(name, _) = node {
                f.write_space()?.write_isid(*name)?;
            }
            Ok(f)
        })
    }

    fn visit_tuple_def(&mut self, node: &TupleDef) -> fmt::Result {
        self.impl_visit_simple(node, "tuple-def", node.paren_span)
    }

    fn visit_type_def(&mut self, node: &TypeDef) -> fmt::Result {
        self.impl_visit(node, "type", node.all_span, |f|
            f.write_space()?.write_isid(node.name)?.write_space()?.write_span(node.name_span))
    }

    fn visit_tuple_type(&mut self, node: &TupleType) -> fmt::Result {
        self.impl_visit_simple(node, "tuple-type", node.span)
    }

    fn visit_type_field_def(&mut self, node: &TypeFieldDef) -> fmt::Result {
        self.impl_visit(node, "type-field-def", node.all_span, |f| f.write_space()?
            .write_isid(node.name)?.write_space()?.write_span(node.name_span)?
            .write_str(" : ")?.write_span(node.colon_span))
    }

    fn visit_unary_expr(&mut self, node: &UnaryExpr) -> fmt::Result {
        self.impl_visit(node, "unary-expr", node.all_span, |f|
            f.write_space()?.write_str(node.operator.display())?.write_space()?.write_span(node.operator_span))
    }

    fn visit_use_stmt(&mut self, node: &UseStatement) -> fmt::Result {
        self.impl_visit(node, "use-stmt", node.all_span, |f| {
            if let Some((alias, alias_span)) = node.alias {
                f.write_str(" alias ")?.write_isid(alias)?.write_space()?.write_span(alias_span)?;
            }
            Ok(f)
        })
    }

    fn visit_var_decl(&mut self, node: &VarDeclStatement) -> fmt::Result {
        self.impl_visit(node, "var-def", node.all_span, |f| f
            .write_str(if node.is_const { " const " } else { " mutable " })?
            .write_isid(node.name)?.write_space()?.write_span(node.name_span))
    }

    fn visit_while_stmt(&mut self, node: &WhileStatement) -> fmt::Result {
        self.impl_visit(node, "while-stmt", node.all_span, |f| 
            f.write_str(" while ")?.write_span(node.while_span))
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
