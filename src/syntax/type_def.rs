///! syntax::type_def
///! type_def = 'type' (identifier | keyword_primitive_type)  '{' [ type_field_def { ',' type_field_def } [ ',' ] ] '}'
///! type_field_def = identifier ':' type_ref

use super::prelude::*;


impl Parser for TypeDef {
    type Output = Self;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Keyword(Keyword::Type)) 
    }

    fn parse(cx: &mut ParseContext) -> Result<TypeDef, Unexpected> {

        let starting_span = cx.expect_keyword(Keyword::Type)?;
        let (name, name_span) = cx.expect_ident_or_keyword_kind(KeywordKind::Primitive)?;
        let _left_brace_span = cx.expect_sep(Separator::LeftBrace)?;

        let mut fields = Vec::new();
        let right_brace_span = loop { 
            if let Some(right_brace_span) = cx.try_expect_sep(Separator::RightBrace) {
                break right_brace_span;     // rustc 1.19 stablize break-expr
            }

            let (field_name, field_name_span) = cx.expect_ident()?;
            let colon_span = cx.expect_sep(Separator::Colon)?;
            let field_type = cx.expect::<TypeRef>()?;
            fields.push(if let Some(comma_span) = cx.try_expect_sep(Separator::Comma) {
                TypeFieldDef{ all_span: field_name_span + comma_span, name: field_name, name_span: field_name_span, colon_span, r#type: field_type }
            } else {
                TypeFieldDef{ all_span: field_name_span + field_type.get_all_span(), name: field_name, name_span: field_name_span, colon_span, r#type: field_type }
            });
        };

        Ok(TypeDef{ all_span: starting_span + right_brace_span, name, name_span, fields })
    }
}


#[cfg(test)] #[test]
fn type_def_parse() {
    //                                  01234567890123456
    case!{ "type x { x: i32 }" as TypeDef,
        TypeDef{ all_span: Span::new(0, 16), name: IsId::new(2), name_span: Span::new(5, 5), fields: vec![
            TypeFieldDef{ all_span: Span::new(9, 14), name: IsId::new(2), name_span: Span::new(9, 9), colon_span: Span::new(10, 10),
                r#type: make_type!(prim 12:14 I32) }] }
    }
    case!{ "type x { x: i32,}" as TypeDef,
        TypeDef{ all_span: Span::new(0, 16), name: IsId::new(2), name_span: Span::new(5, 5), fields: vec![
            TypeFieldDef{ all_span:Span::new(9, 15), name: IsId::new(2), name_span: Span::new(9, 9), colon_span: Span::new(10, 10),
                r#type: make_type!(prim 12:14 I32) }] }
    }
    //                                    0         1         2         3         4
    //                                    0123456789012345678901234567890123456789012345
    case!{ "type array { data:  &u8, size: u64, cap: u64 }" as TypeDef,
        TypeDef{ all_span: Span::new(0, 45), name: IsId::new(2), name_span: Span::new(5, 9), fields: vec![
            TypeFieldDef{ all_span: Span::new(13, 23), name: IsId::new(3), name_span: Span::new(13, 16), colon_span: Span::new(17, 17),
                r#type: make_type!(ref 20:22 make_type!(prim 21:22 U8)) },
            TypeFieldDef{ all_span: Span::new(25, 34), name: IsId::new(4), name_span: Span::new(25, 28), colon_span: Span::new(29, 29),
                r#type: make_type!(prim 31:33 U64) },
            TypeFieldDef{ all_span: Span::new(36, 43), name: IsId::new(5), name_span: Span::new(36, 38), colon_span: Span::new(39, 39),
                r#type: make_type!(prim 41:43 U64) },
        ] }, strings ["array", "data", "size", "cap"]
    }
}
