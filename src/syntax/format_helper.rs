#![allow(dead_code)] // temp remove
///! fff-lang
///!
///! syntax/format_helper

use std::fmt::Debug;
use crate::source::Span;
use crate::source::SymbolID;
use crate::source::SourceCode;
use crate::source::SymbolCollection;

const INDENTION_FILLERS: [[&str; 16]; 3] = [ [
    "", "1 ", "2 | ", "3 | | ", "4 | | | ", "5 | | | | ", "6 | | | | | ", "7 | | | | | | ", "8 | | | | | | | ", "9 | | | | | | | | ", "10 | | | | | | | | | ",
    "11| | | | | | | | | | ", "12| | | | | | | | | | | ", "13| | | | | | | | | | | | ", "14| | | | | | | | | | | | | ", "15| | | | | | | | | | | | "
], [
    "", "| ", "| | ", "| | | ", "| | | | ", "| | | | | ", "| | | | | | ", "| | | | | | | ", "| | | | | | | | ", "| | | | | | | | | ", "| | | | | | | | | | ",
    "| | | | | | | | | | | ", "| | | | | | | | | | | | ", "| | | | | | | | | | | | | ", "| | | | | | | | | | | | | | ", "| | | | | | | | | | | | | "
], [
    "", "  ", "    ", "      ", "        ", "          ", "            ", "              ", "                ", "                  ", "                    ",
    "                      ", "                        ", "                          ", "                            ", "                          "
]];

#[derive(Clone)]
pub struct Formatter<'a, 'b> {
    indent_index: usize,
    source: Option<&'a SourceCode>,
    symbols: Option<&'b SymbolCollection>,
    header_text: Option<&'static str>, // lazy to add a 'c
    prefix_text: Option<&'static str>,
    buf: String,
}
impl<'a, 'b> Formatter<'a, 'b> {

    pub fn new(source: Option<&'a SourceCode>, symbols: Option<&'b SymbolCollection>) -> Self {
        Formatter{ indent_index: 0, source, symbols, header_text: None, prefix_text: None, buf: String::new() }
    }
    pub fn with_test_indent(indent: usize) -> Self {
        Formatter{ indent_index: indent, source: None, symbols: None, header_text: None, prefix_text: None, buf: String::new() }
    }
    pub fn empty() -> Self {
        Formatter{ indent_index: 0, source: None, symbols: None, header_text: None, prefix_text: None, buf: String::new() }
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
impl<'a, 'b> Formatter<'a, 'b> {

    pub fn lit(mut self, v: &str) -> Self {
        self.buf.push_str(v);
        self
    }
    pub fn debug<T: Debug>(mut self, v: &T) -> Self {
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
        self.buf.push_str(&format!("{}", span.format(self.source)));
        self
    }
    pub fn sym(mut self, id: SymbolID) -> Self {
        self.buf.push_str(&format!("{}", id.format(self.symbols)));
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
        self.buf.push_str(INDENTION_FILLERS[2][self.indent_index]);
        self
    }
    pub fn indent1(mut self) -> Self { 
        self.buf.push_str(INDENTION_FILLERS[2][self.indent_index + 1]);
        self
    }
    pub fn indent2(mut self) -> Self { 
        self.buf.push_str(INDENTION_FILLERS[2][self.indent_index + 2]);
        self
    }

    pub fn apply<T: ISyntaxFormat>(mut self, item: &T) -> Self {
        self.buf.push_str(&item.format(Formatter{        // a manual clone because lazy to add derive(Clone)
            indent_index: self.indent_index,
            source: self.source, symbols: self.symbols,
            header_text: self.header_text, prefix_text: self.prefix_text, buf: String::new(),
        }));
        self
    }
    pub fn apply1<T: ISyntaxFormat>(mut self, item: &T) -> Self {
        self.buf.push_str(&item.format(Formatter{        // a manual clone because lazy to add derive(Clone)
            indent_index: self.indent_index + 1,
            source: self.source, symbols: self.symbols,
            header_text: self.header_text, prefix_text: self.prefix_text, buf: String::new(),
        }));
        self
    }
    pub fn apply2<T: ISyntaxFormat>(mut self, item: &T) -> Self {
        self.buf.push_str(&item.format(Formatter{        // a manual clone because lazy to add derive(Clone)
            indent_index: self.indent_index + 2,
            source: self.source, symbols: self.symbols,
            header_text: self.header_text, prefix_text: self.prefix_text, buf: String::new(),
        }));
        self
    }

    // TODO: try deref to String!
    pub fn finish(self) -> String {
        self.buf
    }
}
pub trait ISyntaxFormat {
    fn format(&self, f: Formatter) -> String;
}
