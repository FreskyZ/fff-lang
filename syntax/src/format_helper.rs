///! fff-lang
///!
///! syntax/format_helper

use std::ops::Add;

use codemap::Span;
use codemap::SymbolID;
use codemap::SourceCode;
use codemap::SymbolCollection;

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
    m_indent: usize,
    source: Option<&'a SourceCode>,
    symbols: Option<&'b SymbolCollection>,
}
#[allow(dead_code)] // helper methods may not be used
impl<'a, 'b> Formatter<'a, 'b> {

    pub fn new(source: Option<&'a SourceCode>, symbols: Option<&'b SymbolCollection>) -> Self { Formatter{ m_indent: 0, source, symbols } }
    pub fn with_test_indent(indent: usize) -> Self { Formatter{ m_indent: indent, source: None, symbols: None } }
    pub fn default() -> Self { Formatter{ m_indent: 0, source: None, symbols: None } }

    pub fn indent(&self) -> &'static str { INDENTION_FILLERS[2][self.m_indent] }
    pub fn indent1(&self) -> &'static str { INDENTION_FILLERS[2][self.m_indent + 1] }
    pub fn indentn(&self, n: usize) -> &'static str { INDENTION_FILLERS[2][self.m_indent + n] }

    pub fn span(&self, span: Span) -> String { span.format(self.source) }
    pub fn sym(&self, sym: SymbolID) -> String { sym.format(self.symbols) }

    pub fn apply<T: ISyntaxItemFormat>(&self, item: &T) -> String { item.format(self.clone()) /* `+0` instead of `Clone::clone()` */ }
    pub fn apply1<T: ISyntaxItemFormat>(&self, item: &T) -> String { item.format(self.clone() + 1) }
    pub fn applyn<T: ISyntaxItemFormat>(&self, item: &T, n: usize) -> String { item.format(self.clone() + n) } // replace parameter order
}
impl<'a, 'b> Add<usize> for Formatter<'a, 'b> {
    type Output = Self;
    fn add(self, rhs: usize) -> Self::Output { Formatter{ m_indent: self.m_indent + rhs, source: self.source, symbols: self.symbols } }
}

pub trait ISyntaxItemFormat {
    fn format(&self, f: Formatter) -> String;
}

// TODO: may require header text setter