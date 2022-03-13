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

    fn indent(&mut self) -> fmt::Result { 
        self.f.write_str(INDENTIONS[2][self.level])
    }
    fn indent1(&mut self) -> fmt::Result { 
        self.f.write_str(INDENTIONS[2][self.level + 1])
    }
    fn indent2(&mut self) -> fmt::Result { 
        self.f.write_str(INDENTIONS[2][self.level + 2])
    }

    pub fn span(&mut self, span: Span) -> fmt::Result {
        let (_, start_line, end_line, start_column, end_column) = self.scx.map_span_to_line_column(span);
        write!(self.f, "{}:{}-{}:{}", start_line, end_line, start_column, end_column)
    }
    pub fn string(&mut self, id: IsId) -> fmt::Result {
        self.f.write_str(self.scx.resolve_string(id))
    }
}

impl<'scx, 'f1, 'f2, F> Visitor<(), fmt::Error> for FormatVisitor<'scx, 'f1, 'f2, F> where F: FileSystem {

    fn visit_array_def(&mut self, node: &ArrayDef) -> fmt::Result {
        self.indent()?;
        self.f.write_str("array-def ")?;
        self.span(node.bracket_span)?;
        self.f.write_char('\n')?;
        self.level += 1;
        let result = node.walk(self);
        self.level -= 1;
        result
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
