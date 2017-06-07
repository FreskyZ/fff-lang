///! fff-lang
///!
///! lexical/parse_sess

// currently it only contains 2 fields and do nothing,
// but in future it may do more things, so leave it in seperated file

use codemap::SymbolCollection;
use message::MessageCollection;

pub struct ParseSession<'a, 'b> {
    pub messages: &'a mut MessageCollection,
    pub symbols: &'b mut SymbolCollection,
}
impl<'a, 'b> ParseSession<'a, 'b> {
    
    pub fn new(messages: &'a mut MessageCollection, symbols: &'b mut SymbolCollection) -> Self { ParseSession{ messages, symbols } }
}