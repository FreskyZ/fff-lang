///! fff-lang
///!
///! syntax/items, common syntax item types

// TODO: support pythonic range relationship expr, support `expr rel_op expr rel_op expr`
// maybe not here but semantic, check python's grammar def

pub(super) mod block;
pub(super) mod fn_def;
pub(super) mod label_def;
pub(super) mod type_use;
pub(super) mod type_def;
