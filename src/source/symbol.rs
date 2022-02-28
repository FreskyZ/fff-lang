// source::symbol

use std::collections::{HashMap, hash_map::DefaultHasher};
use std::hash::{Hash, Hasher};
use super::Span;

/// a handle to an interned string
///
/// it is u32 not usize because it is widely used
/// and not reasonable to have more than u32::MAX symbols in all source files
#[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]
pub struct SymId(u32);

impl SymId {
    pub fn new(v: u32) -> Self {
        Self(v)
    }
    pub fn unwrap(self) -> u32 {
        self.0
    }
}
impl From<u32> for SymId {
    fn from(v: u32) -> Self {
        Self(v)
    }
}

fn get_hash(content: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    Hash::hash(content, &mut hasher);
    hasher.finish()
}

// symbol itself cannot map span to content so result is this
pub enum ResolveResult<'a> {
    Span(Span),
    Str(&'a str),
}

pub const SYMID_MASK: u32 = 1 << 31;

#[derive(Debug)]
pub struct Symbols {
    // map string content to symbol id, but cannot reference self.files[...].content, so key is already hashed
    // symbol id starts from 1 for span and from 0x1000_0000 for value
    pub items: HashMap<u64, SymId>,
    // .0: reverse map symbol id to span to symbol content, item 0 is dummy
    // .1: reverse map symbol id to symbol content, clearing first bit of symbol id value
    // // this vec string is the only thing to make it look like string intern facility compare to previous u64 and span
    pub rev_items: (Vec<Span>, Vec<String>),
}

impl Symbols {

    pub fn new() -> Self {
        Self{ items: HashMap::new(), rev_items: (vec![Span::new(0, 0)], Vec::new())}
    }

    // intern symbol at location
    // caller to map span to content before calling
    pub fn intern_span(&mut self, location: Span, value: &str) -> SymId {
        let hash = get_hash(value);
        if let Some(symbol_id) = self.items.get(&hash) {
            *symbol_id
        } else {
            let symbol_id = SymId::new(self.rev_items.0.len() as u32);
            self.items.insert(hash, symbol_id);
            self.rev_items.0.push(location);
            symbol_id
        }
    }

    // the advantage compare to intern_string is str is only copied when creating new symbol
    // it is actually used in syntax parser
    pub fn intern_str(&mut self, value: &str) -> SymId {
        let hash = get_hash(&value);
        if let Some(symbol_id) = self.items.get(&hash) {
            *symbol_id
        } else {
            let symbol_id = SymId::new(self.rev_items.1.len() as u32 | SYMID_MASK);
            self.items.insert(hash, symbol_id);
            self.rev_items.1.push(value.to_owned());
            symbol_id
        }
    }

    // the advantage compare to intern_str is no need to copy when creating new symbol
    pub fn intern_string(&mut self, value: String) -> SymId {
        let hash = get_hash(&value);
        if let Some(symbol_id) = self.items.get(&hash) {
            *symbol_id
        } else {
            let symbol_id = SymId::new(self.rev_items.1.len() as u32 | SYMID_MASK);
            self.items.insert(hash, symbol_id);
            self.rev_items.1.push(value);
            symbol_id
        }
    }

    // caller to map result span to str
    // result does not option because symbol id is only created by methods above
    // and should not have invalid value if no unexpected error happens
    pub fn resolve(&self, symbol_id: SymId) -> ResolveResult {
        let symbol_id = symbol_id.0;
        debug_assert!(symbol_id > 0, "invalid symbol id");

        if (symbol_id & SYMID_MASK) == SYMID_MASK {
            let index = (symbol_id & !SYMID_MASK) as usize;
            debug_assert!(index < self.rev_items.1.len(), "invalid symbol id");
            ResolveResult::Str(&self.rev_items.1[index])
        } else {
            debug_assert!((symbol_id as usize) < self.rev_items.0.len(), "invalid symbol id");
            ResolveResult::Span(self.rev_items.0[symbol_id as usize])
        }
    }
}
