///! syntax::display: default display implementation

use std::fmt::{self, Write};
use crate::source::{SourceContext, FileSystem, Span, IsId};
use super::node::Visitor;
use super::super::*;

static INDENTIONS: [[&str; 16]; 3] = [
    [
        "", "1 ", "2 | ", "3 | | ", "4 | | | ", "5 | | | | ", "6 | | | | | ", "7 | | | | | | ", "8 | | | | | | | ", "9 | | | | | | | | ", "10 | | | | | | | | | ",
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

    pub fn new(scx: &'scx SourceContext<F>, f: &'f1 mut fmt::Formatter<'f2>) -> Self {
        Self{ level: 0, scx, f }
    }

    fn write_str(&mut self, v: &str) -> fmt::Result {
        self.f.write_str(v)
    }
    fn write_char(&mut self, c: char) -> fmt::Result {
        self.f.write_char(c)
    }
    fn write_endl(&mut self) -> fmt::Result {
        self.f.write_char('\n')
    }
    fn write_space(&mut self) -> fmt::Result {
        self.f.write_char(' ')
    }

    fn write_indent(&mut self) -> fmt::Result { 
        self.f.write_str(INDENTIONS[2][self.level])
    }
    fn write_next_indent(&mut self) -> fmt::Result { 
        self.f.write_str(INDENTIONS[2][self.level + 1])
    }

    fn write_span(&mut self, span: Span) -> fmt::Result {
        let (_, start_line, end_line, start_column, end_column) = self.scx.map_span_to_line_column(span);
        write!(self.f, "<{}:{}-{}:{}>", start_line, end_line, start_column, end_column)
    }
    fn write_isid(&mut self, id: IsId) -> fmt::Result {
        if id.unwrap() == 1 {
            self.f.write_str("<empty>")
        } else {
            let content = self.scx.resolve_string(id);
            self.f.write_str(if content.len() > 24 { &content[..24] } else { content })
        }
    }

    fn invoke_walk<N: Node>(&mut self, node: &N) -> fmt::Result {
        self.level += 1;
        let result = node.walk(self);
        self.level -= 1;
        result
    }
}

impl<'scx, 'f1, 'f2, F> Visitor<(), fmt::Error> for FormatVisitor<'scx, 'f1, 'f2, F> where F: FileSystem {

    fn visit_array_def(&mut self, node: &ArrayDef) -> fmt::Result {
        self.write_indent()?;
        self.write_str("array-def ")?;
        self.write_span(node.bracket_span)?;
        self.write_endl()?;
        self.invoke_walk(node)
    }

    fn visit_binary_expr(&mut self, node: &BinaryExpr) -> fmt::Result {

        self.write_indent()?;
        self.write_str("binary-expr ")?;
        self.write_span(node.all_span)?;
        self.write_char('\n')?;
        self.write_next_indent()?;
        self.write_str("operator ")?;
        self.write_str(node.operator.display())?;
        self.write_space()?;
        self.write_span(node.operator_span)?;
        self.write_char('\n')?;
        self.invoke_walk(node)
    }
    
    fn visit_lit_expr(&mut self, node: &LitExpr) -> fmt::Result {
        self.write_indent()?;
        self.write_str("literal ")?;
        match node.value {
            LitValue::Unit => self.f.write_str("unit")?,
            LitValue::Bool(v) => write!(self.f, "bool {v}")?,
            LitValue::Char(v) => write!(self.f, "char {v:?}")?,
            LitValue::Str(id) => { self.write_str("str ")?; self.write_isid(id)? },
            LitValue::Num(v) => write!(self.f, "{v}")?,
        }
        self.write_space()?;
        self.write_span(node.span)?;
        self.write_endl()?;
        self.invoke_walk(node)
    }

    // use default implementation correctly transparents this type of node
    // fn visit_expr_list(&mut self, node: &ExprList) -> fmt::Result;

    fn visit_loop_stmt(&mut self, node: &LoopStatement) -> fmt::Result {
        self.write_indent()?;
        self.write_str("loop-stmt ")?;
        self.write_span(node.all_span)?;
        self.write_endl()?;
        self.write_next_indent()?;
        self.write_str("loop ")?;
        self.write_span(node.loop_span)?;
        self.write_endl()?;
        self.invoke_walk(node)
    }

    fn visit_label_def(&mut self, node: &LabelDef) -> fmt::Result {
        self.write_indent()?;
        self.write_str("label @")?;
        self.write_isid(node.name)?;
        self.write_space()?;
        self.write_span(node.all_span)?;
        self.write_endl()?;
        self.invoke_walk(node)
    }

    fn visit_block(&mut self, node: &Block) -> fmt::Result {
        self.write_indent()?;
        self.write_str("block ")?;
        self.write_span(node.all_span)?;
        self.write_endl()?;
        self.invoke_walk(node)
    }
    
    fn visit_simple_expr_stmt(&mut self, node: &SimpleExprStatement) -> fmt::Result {
        self.write_indent()?;
        self.write_str("simple-expr-stmt ")?;
        self.write_span(node.all_span)?;
        self.write_endl()?;
        self.invoke_walk(node)
    }

    fn visit_fn_call_expr(&mut self, node: &FnCallExpr) -> fmt::Result {
        self.write_indent()?;
        self.write_str("fn-call ")?;
        self.write_span(node.all_span)?;
        self.write_endl()?;
        self.write_next_indent()?;
        self.write_str("paren ")?;
        self.write_span(node.paren_span)?;
        self.write_endl()?;
        self.invoke_walk(node)
    }

    fn visit_simple_name(&mut self, node: &SimpleName) -> fmt::Result {
        self.write_indent()?;
        self.write_str("simple-name ")?;
        self.write_isid(node.value)?;
        self.write_space()?;
        self.write_span(node.span)?;
        self.write_endl()?;
        self.invoke_walk(node)
    }

    fn visit_tuple_def(&mut self, node: &TupleDef) -> fmt::Result {
        self.write_indent()?;
        self.write_str("tuple-def ")?;
        self.write_span(node.paren_span)?;
        self.write_endl()?;
        self.invoke_walk(node)
    }

    fn visit_index_call_expr(&mut self, node: &IndexCallExpr) -> fmt::Result {
        self.write_indent()?;
        self.write_str("index-call ")?;
        self.write_span(node.all_span)?;
        self.write_endl()?;
        self.write_next_indent()?;
        self.write_str("bracket ")?;
        self.write_span(node.bracket_span)?;
        self.write_endl()?;
        self.invoke_walk(node)
    }

    fn visit_member_access(&mut self, node: &MemberAccessExpr) -> fmt::Result {
        self.write_indent()?;
        self.write_str("member-access ")?;
        self.write_span(node.all_span)?;
        self.write_endl()?;
        self.write_next_indent()?;
        self.write_str("dot ")?;
        self.write_span(node.dot_span)?;
        self.write_endl()?;
        self.invoke_walk(node)
    }
}

pub struct NodeDisplay<'n, 'scx, N, F>(pub (in super)&'n N, pub (in super) &'scx SourceContext<F>);

impl<'n, 'scx, N: Node, F: FileSystem> fmt::Display for NodeDisplay<'n, 'scx, N, F> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut visitor = FormatVisitor::new(self.1, f);
        self.0.accept(&mut visitor)
    }
}

#[derive(Clone)]
pub struct Formatter<'a> {
    indent_index: usize,
    source: Option<&'a SourceContext>,
    header_text: Option<&'static str>, // lazy to add a 'c
    prefix_text: Option<&'static str>,
    buf: String,
}
impl<'a> Formatter<'a> {

    pub fn new(source: Option<&'a SourceContext>) -> Self {
        Formatter{ indent_index: 0, source, header_text: None, prefix_text: None, buf: String::new() }
    }
    pub fn with_test_indent(indent: usize) -> Self {
        Formatter{ indent_index: indent, source: None, header_text: None, prefix_text: None, buf: String::new() }
    }
    pub const fn empty() -> Self {
        Formatter{ indent_index: 0, source: None, header_text: None, prefix_text: None, buf: String::new() }
    }

    // set only once
    pub fn set_header_text(mut self, header_text: &'static str) -> Self {
        self.header_text = Some(header_text);
        self
    }
    pub fn unset_header_text(mut self) -> Self {
        self.header_text = None;
        self
    }
    pub fn set_prefix_text(mut self, prefix_text: &'static str) -> Self {
        self.prefix_text = Some(prefix_text);
        self
    }
    pub fn unset_prefix_text(mut self) -> Self {
        self.prefix_text = None;
        self
    }
}
impl<'a> Formatter<'a> {

    pub fn lit(mut self, v: &str) -> Self {
        self.buf.push_str(v);
        self
    }
    pub fn debug<T: fmt::Debug>(mut self, v: &T) -> Self {
        self.buf.push_str(&format!("{:?}", v));
        self
    }
    pub fn space(mut self) -> Self {
        self.buf.push(' ');
        self
    }
    pub fn endl(mut self) -> Self {
        self.buf.push('\n');
        self
    }

    pub fn span(mut self, span: Span) -> Self {
        if let Some(source) = &self.source {
            // TODO: use write!
            self.buf.push_str(&format!("{}", span.display(source)));
        } else {
            self.buf.push_str(&format!("{:?}", span));
        }
        self
    }
    pub fn isid(mut self, id: IsId) -> Self {
        if let Some(source) = &self.source {
            self.buf.push_str(&format!("{}", source.resolve_string(id)));
        } else {
            self.buf.push_str(&format!("{:?}", id));
        }
        self
    }
    pub fn header_text_or(mut self, v: &'static str) -> Self {
        let need_extra_empty = self.prefix_text.is_some();
        self.buf.push_str(self.prefix_text.unwrap_or(""));
        self.prefix_text = None;
        if need_extra_empty { self.buf.push(' '); }
        self.buf.push_str(self.header_text.unwrap_or(v));
        self.header_text = None;
        self
    }

    pub fn indent(mut self) -> Self { 
        self.buf.push_str(INDENTIONS[2][self.indent_index]);
        self
    }
    pub fn indent1(mut self) -> Self { 
        self.buf.push_str(INDENTIONS[2][self.indent_index + 1]);
        self
    }
    pub fn indent2(mut self) -> Self { 
        self.buf.push_str(INDENTIONS[2][self.indent_index + 2]);
        self
    }

    pub fn apply<T: ISyntaxFormat>(mut self, item: &T) -> Self {
        self.buf.push_str(&item.format(Formatter{        // a manual clone because lazy to add derive(Clone)
            indent_index: self.indent_index,
            source: self.source,
            header_text: self.header_text, prefix_text: self.prefix_text, buf: String::new(),
        }));
        self
    }
    pub fn apply1<T: ISyntaxFormat>(mut self, item: &T) -> Self {
        self.buf.push_str(&item.format(Formatter{        // a manual clone because lazy to add derive(Clone)
            indent_index: self.indent_index + 1,
            source: self.source,
            header_text: self.header_text, prefix_text: self.prefix_text, buf: String::new(),
        }));
        self
    }
    pub fn apply2<T: ISyntaxFormat>(mut self, item: &T) -> Self {
        self.buf.push_str(&item.format(Formatter{        // a manual clone because lazy to add derive(Clone)
            indent_index: self.indent_index + 2,
            source: self.source,
            header_text: self.header_text, prefix_text: self.prefix_text, buf: String::new(),
        }));
        self
    }

    pub fn finish(self) -> String {
        self.buf
    }
}
pub trait ISyntaxFormat {
    fn format(&self, f: Formatter) -> String;
}
