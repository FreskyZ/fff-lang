///! fff-lang
///!
///! semantic/resolve_sess
///! resolve session, many things during resolve

use super::SymbolCollection;

pub struct ResolveSession {
    pub symbols: SymbolCollection, // try leave it pub here because it is internal struct
}
impl ResolveSession {
    pub fn new() -> ResolveSession {
        ResolveSession{ symbols: SymbolCollection::new() }
    }
}