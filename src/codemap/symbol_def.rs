#![macro_use]
///! fff-lang
///!
///! codemap/symbol_def
///! all kinds of strings, intern them to save memory and save copy time

use std::fmt;
use std::collections::HashMap;

#[derive(Eq, PartialEq, Clone, Copy, Hash)]
pub struct SymbolID(usize);
impl SymbolID { pub fn new(value: usize) -> SymbolID { SymbolID(value) } }
impl From<usize> for SymbolID { fn from(value: usize) -> SymbolID { SymbolID(value) } }
impl SymbolID { 
    pub fn format(&self, symbols: Option<&SymbolCollection>) -> String { 
        match symbols {
            None => format!("#{}", self.0),
            Some(symbols) => format!("{:?}", symbols.get(*self).unwrap_or("<no-sym>")),
        }
    }
}
impl fmt::Debug for SymbolID { 
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(None)) } 
}

pub struct SymbolCollection {
    items: HashMap<String, SymbolID>,
}
impl fmt::Debug for SymbolCollection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Symbols:")?;
        for (ref symbol, ref id) in self.items.iter() {
            write!(f, "\n  {:?} => {:?}", id, symbol)?;
        }
        write!(f, "\n")
    }
}
impl Default for SymbolCollection {
    fn default() -> SymbolCollection { SymbolCollection{ items: HashMap::new() } }
}
impl SymbolCollection {

    pub fn new() -> SymbolCollection { 
        SymbolCollection{ items: HashMap::new() } 
    }

    pub fn intern(&mut self, value: String) -> SymbolID {
        if let Some(id) = self.items.get(&value) {
            return *id;
        }
        let newid = SymbolID(self.items.len() + 1);     // reserve 0
        self.items.insert(value, newid);
        return newid;
    }
    pub fn intern_str(&mut self, symbol: &str) -> SymbolID { 
        if let Some(id) = self.items.get(symbol) { 
            return *id;
        }
        let newid = SymbolID(self.items.len() + 1);     // reserve 0
        self.items.insert(symbol.to_owned(), newid);
        return newid; 
    }

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
    ($($x:expr,)*) => (make_symbols![$($x),*])
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
    assert_eq!(symbols.get(SymbolID(123)), None);

    assert_eq!(id1.format(Some(&symbols)), "\"abc\"");
    assert_eq!(id2.format(Some(&symbols)), "\"123\"");
    assert_eq!(id3.format(Some(&symbols)), "\"abc\"");
    assert_eq!(SymbolID::from(42).format(Some(&symbols)), "\"<no-sym>\"");
    assert_eq!(id1.format(None), "#1");
    assert_eq!(id2.format(None), "#2");

    let mut symbols = make_symbols!["abc", "123", "abc"];
    assert_eq!(symbols.intern_str("abc"), symbols.intern_str("abc"));
}
