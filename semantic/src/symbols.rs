///! fff-lang
///!
///! semantc/symbols
///! a symbol is a string in syntax tree
///!     intern them to save memory and save copy time
// symbol collection is singleton owned by package, currently
// maybe accessed by resolve session

use std::collections::HashMap;
use super::SymbolID;

pub struct SymbolCollection {
    items: HashMap<String, SymbolID>,
}
impl SymbolCollection {

    pub fn new() -> SymbolCollection { SymbolCollection{ items: HashMap::new() } }

    pub fn intern(&mut self, str: String) -> SymbolID {
        if let Some(id) = self.items.get(&str) { 
            return *id;
        }
        let newid = SymbolID::new(self.items.len());
        self.items.insert(str, newid);
        return newid;
    }
    pub fn intern_str(&mut self, symbol: &str) -> SymbolID { self.intern(symbol.to_owned()) }

    pub fn get(&self, expect_id: SymbolID) -> Option<&str> {
        for (ref symbol, ref id) in self.items.iter() {
            if id == &&expect_id {
                return Some(symbol);
            }
        }
        return None;
    }
}

/// for test, make symbols with fixed id before analyze
#[macro_export]
macro_rules! make_symbols {
    ($($x:expr),*) => ({
        let mut retval = SymbolCollection::new();
        {
            let _retval = &mut retval; // `&mut` for statisfy 'unused mut', `_` for statisfy unused var
            $(
                _retval.intern_str($x);
            )*
        }
        retval
    });
    ($($x:expr,)*) => (make_messages![$($x),*])
}

#[cfg(test)] #[test]
fn intern_symbols() {

    let mut symbols = SymbolCollection::new();
    let id1 = symbols.intern("abc".to_owned());
    let id2 = symbols.intern("123".to_owned());
    let id3 = symbols.intern("abc".to_owned());
    assert!(id1 != id2);
    assert!(id2 != id3);
    assert!(id1 == id3);
    assert_eq!(symbols.get(id1), Some("abc"));
    assert_eq!(symbols.get(id2), Some("123"));
    assert_eq!(symbols.get(id3), Some("abc"));
    assert_eq!(symbols.get(SymbolID::new(123)), None);
}