///! fff-lang
///!
///! semantic/type_use
///! semantic type use is identifier-symbol-id-replaced syntax type use
// currently (v0.1.2 at 17/5/20) syntax/typeuse has special enum members for array and tuple
// but you always have to support at least generic type or even type template
// so support type template here, also rename `()` to `unit`

use std::fmt;

use codepos::StringPosition;
use syntax;

use super::super::SymbolID;
use super::super::ResolveSession;
use super::super::ISemanticItemFormat;
use super::super::ISemanticItemFromSyntaxItem;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct TypeUse {
    all_strpos: StringPosition,
    base_name: SymbolID,
    params: Vec<TypeUse>,
}
impl ISemanticItemFormat for TypeUse {
    fn format(&self, indent: u32) -> String {
        format!("{}TypeUse <{:?}>\n{}{:?}{}", 
            TypeUse::indent_str(indent), self.all_strpos,
            TypeUse::indent_str(indent + 1), self.base_name,
            self.params.iter().fold(String::new(), |mut buf, typeuse| { buf.push_str("\n"); buf.push_str(&typeuse.format(0)); buf })
        )
    }
}
impl fmt::Debug for TypeUse {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(0)) }
}
impl TypeUse {
    fn new_simple(strpos: StringPosition, id: SymbolID) -> TypeUse {
        TypeUse{ all_strpos: strpos, base_name: id, params: Vec::new() }
    }
    /// New template instantiated
    fn new_templated(all_strpos: StringPosition, base_name: SymbolID, params: Vec<TypeUse>) -> TypeUse {
        TypeUse{ all_strpos, base_name, params }
    }
}
impl ISemanticItemFromSyntaxItem<syntax::TypeUse> for TypeUse {

    fn from_syntax_item(rtypeuse: syntax::TypeUse, sess: &mut ResolveSession) -> TypeUse {

        match rtypeuse.actual {
            syntax::ActualTypeUse::Unit => {
                TypeUse::new_simple(rtypeuse.all_strpos, sess.symbols.intern_str("unit"))
            }
            syntax::ActualTypeUse::Simple(base_name) => {
                TypeUse::new_simple(rtypeuse.all_strpos, sess.symbols.intern(base_name))
            }
            syntax::ActualTypeUse::Array(boxed_inner) => {
                let inner = unsafe { (*Box::into_raw(boxed_inner)).clone() };
                let inner = TypeUse::with_syntax_item(inner, sess);
                TypeUse::new_templated(rtypeuse.all_strpos, sess.symbols.intern_str("array"), vec![inner])
            }
            syntax::ActualTypeUse::Tuple(items) => {
                let items = items.into_iter().map(|item| TypeUse::with_syntax_item(item, sess)).collect();
                TypeUse::new_templated(rtypeuse.all_strpos, sess.symbols.intern_str("tuple"), items)
            }
        }
    }
}

#[cfg(test)] #[test]
fn type_use_with_syntax() {
    use super::super::ISemanticItemWithStr;
    use super::super::SymbolCollection;

    assert_eq!{ TypeUse::with_test_str("a", make_symbols!["a"]), TypeUse::new_simple(make_strpos!(1, 1, 1, 1), SymbolID::new(0)) }
    assert_eq!{ TypeUse::with_test_str("[i32]", make_symbols!["array", "i32"]), 
        TypeUse::new_templated(make_strpos!(1, 1, 1, 5), SymbolID::new(0), vec![
            TypeUse::new_simple(make_strpos!(1, 2, 1, 4), SymbolID::new(1))
        ])
    }
}