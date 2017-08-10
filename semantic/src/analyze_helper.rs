///! fff-lang
///!
///! semantic/traits

use std::fmt;

use codemap::Span;
use codemap::SymbolID;
use codemap::SourceCode;
use codemap::SymbolCollection;

use super::SharedDefScope;

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

    pub fn finish(self) -> String { self.buf }
}

pub struct Wrapper<'a, T: 'a>(&'a T);
impl<'a, T: ISemanticAnalyze> fmt::Display for Wrapper<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n{}", self.0.format(Formatter::empty()))
    }
}

pub trait ISemanticAnalyze {

    // Display, which is actually Debug but I don't like `fn debug() -> String`, should not be implemented
    fn display<'a>(&'a self) -> Wrapper<'a, Self> where Self: Sized { Wrapper(self) }
    // format, should be implemented
    fn format(&self, f: Formatter) -> String { "<unknown>".to_owned() }

    // phase 1: direct map from syntax node
    type SyntaxItem;
    // @param symbols: because when contructing scope tree, many nodes (type, fn) need to convert id to string as its path segment, mut ref for future use
    fn from_syntax(item: Self::SyntaxItem, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> Self;

    // TODO: an empty implement for compatibility temporarily, remove it in future
    fn collect_type_declarations(&mut self) { }
}
