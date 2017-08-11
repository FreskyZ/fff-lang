///! fff-lang
///!
///! semantic/format_helper
///! almost formatter DSL, or, in functional concept, format combinator

use std::fmt;

use codemap::Span;
use codemap::SymbolID;
use codemap::SourceCode;
use codemap::SymbolCollection;

use super::ISemanticAnalyze;

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
const CURRENT_INDEX_INDEX: usize = 2;

#[derive(Clone)]
pub struct Formatter<'a, 'b> {
    indent_index: usize,
    source: Option<&'a SourceCode>,
    symbols: Option<&'b SymbolCollection>,
    header_text: Option<&'static str>, // lazy to add a 'c
    prefix_text: Option<&'static str>,
    buf: String,
}
#[allow(dead_code)]
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
    pub fn set_header_text(mut self, header_text: &'static str) -> Self { self.header_text = Some(header_text); self }
    pub fn unset_header_text(mut self) -> Self { self.header_text = None; self }
    pub fn set_prefix_text(mut self, prefix_text: &'static str) -> Self { self.prefix_text = Some(prefix_text); self }
    pub fn unset_prefix_text(mut self) -> Self { self.prefix_text = None; self }
}
#[allow(dead_code)]
impl<'a, 'b> Formatter<'a, 'b> {

    pub fn lit(mut self, v: &str) -> Self { self.buf.push_str(v); self }
    pub fn debug<T: fmt::Debug>(mut self, v: &T) -> Self { self.buf.push_str(&format!("{:?}", v)); self }
    pub fn display<T: fmt::Display>(mut self, v: &T) -> Self { self.buf.push_str(&format!("{}", v)); self }
    pub fn space(mut self) -> Self { self.buf.push(' '); self }
    pub fn endl(mut self) -> Self { self.buf.push('\n'); self }

    pub fn span(mut self, span: Span) -> Self { self.buf.push_str(&format!("{}", span.format(self.source))); self }
    pub fn sym(mut self, id: SymbolID) -> Self { self.buf.push_str(&format!("{}", id.format(self.symbols))); self  }
    pub fn header_text_or(mut self, v: &'static str) -> Self {
        let need_extra_empty = self.prefix_text.is_some();
        self.buf.push_str(self.prefix_text.unwrap_or(""));
        self.prefix_text = None;
        if need_extra_empty { self.buf.push(' '); }
        self.buf.push_str(self.header_text.unwrap_or(v));
        self.header_text = None;
        self
    }

    // NOTE: I'm suprised when I discoverd that I have to write indent exactly as first method in a format method, that is, it is not natural
    pub fn indent(mut self) -> Self { self.buf.push_str(INDENTION_FILLERS[CURRENT_INDEX_INDEX][self.indent_index]); self }
    pub fn indent1(mut self) -> Self { self.buf.push_str(INDENTION_FILLERS[CURRENT_INDEX_INDEX][self.indent_index + 1]); self }
    pub fn indent2(mut self) -> Self { self.buf.push_str(INDENTION_FILLERS[CURRENT_INDEX_INDEX][self.indent_index + 2]); self }

    pub fn apply<T: ISemanticAnalyze>(mut self, item: &T) -> Self {
        self.buf.push_str(&item.format(Formatter{        // a manual clone because lazy to add derive(Clone)
            indent_index: self.indent_index,
            source: self.source, symbols: self.symbols,
            header_text: self.header_text, prefix_text: self.prefix_text, buf: String::new(),
        }));
        self
    }
    pub fn apply1<T: ISemanticAnalyze>(mut self, item: &T) -> Self {
        self.buf.push_str(&item.format(Formatter{        // a manual clone because lazy to add derive(Clone)
            indent_index: self.indent_index + 1,
            source: self.source, symbols: self.symbols,
            header_text: self.header_text, prefix_text: self.prefix_text, buf: String::new(),
        }));
        self
    }
    pub fn apply2<T: ISemanticAnalyze>(mut self, item: &T) -> Self {
        self.buf.push_str(&item.format(Formatter{        // a manual clone because lazy to add derive(Clone)
            indent_index: self.indent_index + 2,
            source: self.source, symbols: self.symbols,
            header_text: self.header_text, prefix_text: self.prefix_text, buf: String::new(),
        }));
        self
    }

    pub fn apply1_with_header_text<T: ISemanticAnalyze>(self, text: &'static str, item: &T) -> Self {
        self.set_header_text(text).apply1(item).unset_header_text()
    }
    pub fn apply1_with_prefix_text<T: ISemanticAnalyze>(self, text: &'static str, item: &T) -> Self {
        self.set_prefix_text(text).apply1(item).unset_prefix_text()
    }
    pub fn apply2_with_header_text<T: ISemanticAnalyze>(self, text: &'static str, item: &T) -> Self {
        self.set_header_text(text).apply2(item).unset_header_text()
    }
    pub fn apply2_with_prefix_text<T: ISemanticAnalyze>(self, text: &'static str, item: &T) -> Self {
        self.set_prefix_text(text).apply2(item).unset_prefix_text()
    }

    pub fn map_or_else<T, F1: FnOnce(Self, &T) -> Self, F2: FnOnce(Self) -> Self>(self, maybe: &Option<T>, f1: F1, f2: F2) -> Self {
        match maybe.as_ref() { Some(t) => f1(self, t), None => f2(self) }
    }

    pub fn foreach<T, U: IntoIterator<Item = T>, F: FnMut(Self, T) -> Self>(mut self, items: U, mut f: F) -> Self {
        for item in items {
            self = f(self, item);
        }
        self
    }
    pub fn foreach_or_else<T, U: IntoIterator<Item = T>, F1: FnMut(Self, T) -> Self, F2: FnOnce(Self) -> Self>(mut self, items: U, mut f1: F1, f2: F2) -> Self {
        let mut iterated = false;  // require for-else statement
        for item in items {
            self = f1(self, item);
            iterated = true;
        }
        if !iterated {
            self = f2(self);
        }
        self
    }

    pub fn finish(self) -> String { self.buf }
}
