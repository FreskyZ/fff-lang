///! fff-lang
///!
///! semantic/iddef
///! strongly typed id def macro

use std::fmt;

macro_rules! define_id_type {
    ($typename: ident) => (
        #[derive(Eq, PartialEq, Clone, Copy, Hash)]
        pub struct $typename(usize);

        impl $typename {
            pub fn new(value: usize) -> $typename { $typename(value) }
            pub fn new_invalid() -> $typename { $typename(!1) }
            pub fn is_some(&self) -> bool { self.0 != !1 }
            pub fn is_invalid(&self) -> bool { self.0 == !1 }
        }
        impl fmt::Debug for $typename {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                if self.is_some() { write!(f, "#{}", self.0) }
                else { write!(f, "#invalid") }
            }
        }
    )
}

define_id_type!{ SymbolID }
#[cfg(do_not_declare_this_in_vscode_rust)] pub struct SymbolID(usize);
define_id_type!{ DefID }
#[cfg(do_not_declare_this_in_vscode_rust)] pub struct DefID(usize);