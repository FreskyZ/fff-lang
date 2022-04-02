///! fff-lang
///!
///! syntax/label
///! 
///! it's here because for, while, loop and block all needs it

#[cfg(test)] #[test]
fn label_def_parse() {use super::prelude::*;
    case!{ "@1:" as LabelDef, LabelDef::new(2, Span::new(0, 2)) }
}
