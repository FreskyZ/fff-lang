
#[cfg(test)]
use super::prelude::*;

#[cfg(test)]
#[test]
fn enum_def_parse() {

    //      012345678901234567890
    case!{ "enum E { M1, M2 = 1,}" as EnumDef, 
        EnumDef{ name: IsId::new(2), name_span: Span::new(5, 5), quote_span: Span::new(7, 20), all_span: Span::new(0, 20), base_type: None, variants: vec![
            EnumVariant{ name: IsId::new(3), name_span: Span::new(9, 10), value: None, all_span: Span::new(9, 10) },
            EnumVariant{ name: IsId::new(4), name_span: Span::new(13, 14), value: Some(make_expr!(i32 1 18:18)), all_span: Span::new(13, 18) }]}
    }

    //      0123456789012
    case!{ "enum E: u8 {}" as EnumDef, 
        EnumDef{ name: IsId::new(2), name_span: Span::new(5, 5), quote_span: Span::new(11, 12), all_span: Span::new(0, 12), variants: Vec::new(),
            base_type: Some(PrimitiveType{ name: Keyword::U8, span: Span::new(8, 9) }), }
    }
}
