///! fff-lang
///!
///! lexical/test_helper

use codemap::SymbolCollection;
use message::MessageCollection;

pub struct TestInput<'a> {
    pub src: &'a str,
    pub src_id: usize,
    pub syms: Option<SymbolCollection>,
    pub msgs: Option<MessageCollection>,
}
impl<'a> TestInput<'a> {

    pub fn new(src: &'a str) -> Self {
        TestInput{ src, src_id: 0, syms: None, msgs: None }
    }
    pub fn with_syms(src: &'a str, syms: SymbolCollection) -> Self {
        TestInput{ src, src_id: 0, msgs: None, syms: Some(syms) }
    }
}
