///! fff-lang
///! 
///! syntax/type_def
///! required by removing hardcode macros in semantic typedef collection and fndef collections
///! type_def = 'type' (identifier | keyword_primitive_type)  '{' [ type_field_def { ',' type_field_def } [ ',' ] ] '}'
///! type_field_def = identifier ':' type_use

use super::prelude::*;
use super::{TypeRef, SimpleName};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct TypeFieldDef {
    pub name: SimpleName,
    pub colon_span: Span,
    pub r#type: TypeRef,
    pub all_span: Span,   // ident to TypeRef or ident to comma
}
impl TypeFieldDef {
    pub fn new(all_span: Span, name: SimpleName, colon_span: Span, r#type: TypeRef) -> TypeFieldDef {
        TypeFieldDef{ all_span, name, colon_span, r#type }
    }
}
impl Node for TypeFieldDef {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_type_field_def(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_simple_name(&self.name)?;
        v.visit_type_ref(&self.r#type)
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct TypeDef {
    pub all_span: Span,
    pub name: SimpleName,
    pub fields: Vec<TypeFieldDef>,
}

impl TypeDef {
    pub fn new(all_span: Span, name: SimpleName, fields: Vec<TypeFieldDef>) -> TypeDef {
        TypeDef{ all_span, name, fields }
    }
}

impl Parser for TypeDef {
    type Output = Self;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Keyword(Keyword::Type)) 
    }

    fn parse(cx: &mut ParseContext) -> Result<TypeDef, Unexpected> {

        let starting_span = cx.expect_keyword(Keyword::Type)?;
        let (symid, name_span) = cx.expect_ident_or_keyword_kind(KeywordKind::Primitive)?;
        let name = SimpleName::new(symid, name_span);
        let _left_brace_span = cx.expect_sep(Separator::LeftBrace)?;

        let mut fields = Vec::new();
        let right_brace_span = loop { 
            if let Some(right_brace_span) = cx.try_expect_sep(Separator::RightBrace) {
                break right_brace_span;     // rustc 1.19 stablize break-expr
            }

            let field_name = cx.expect::<SimpleName>()?;
            let colon_span = cx.expect_sep(Separator::Colon)?;
            let field_type = cx.expect::<TypeRef>()?;
            fields.push(if let Some(comma_span) = cx.try_expect_sep(Separator::Comma) {
                TypeFieldDef::new(field_name.span + comma_span, field_name, colon_span, field_type)
            } else {
                TypeFieldDef::new(field_name.span + field_type.all_span, field_name, colon_span, field_type)
            });
        };

        Ok(TypeDef::new(starting_span + right_brace_span, name, fields))
    }
}

impl Node for TypeDef {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_type_def(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_simple_name(&self.name)?;
        for field in &self.fields {
            v.visit_type_field_def(field)?;
        }
        Ok(Default::default())
    }
}

#[cfg(test)] #[test]
fn type_def_parse() {
    //                                  01234567890123456
    case!{ "type x { x: i32 }" as TypeDef,
        TypeDef::new(Span::new(0, 16), SimpleName::new(2, Span::new(5, 5)), vec![
            TypeFieldDef::new(Span::new(9, 14), 
                SimpleName::new(2, Span::new(9, 9)),
                Span::new(10, 10),
                TypeRef::new_simple(3, Span::new(12, 14))
            )
        ])
    }
    case!{ "type x { x: i32,}" as TypeDef,
        TypeDef::new(Span::new(0, 16), SimpleName::new(2, Span::new(5, 5)), vec![
            TypeFieldDef::new(Span::new(9, 15), 
                SimpleName::new(2, Span::new(9, 9)),
                Span::new(10, 10),
                TypeRef::new_simple(3, Span::new(12, 14))
            )
        ])
    }
    //                                    0         1         2         3         4
    //                                    0123456789012345678901234567890123456789012345
    case!{ "type array { data: [u8], size: u64, cap: u64 }" as TypeDef,
        TypeDef::new(Span::new(0, 45), SimpleName::new(2, Span::new(5, 9)), vec![
            TypeFieldDef::new(Span::new(13, 23),
                SimpleName::new(3, Span::new(13, 16)),
                Span::new(17, 17),
                TypeRef::new_template(2, Span::new(0, 0), Span::new(19, 22), vec![
                    TypeRef::new_simple(4, Span::new(20, 21))
                ])
            ),
            TypeFieldDef::new(Span::new(25, 34), 
                SimpleName::new(5, Span::new(25, 28)),
                Span::new(29, 29),
                TypeRef::new_simple(7, Span::new(31, 33))
            ),
            TypeFieldDef::new(Span::new(36, 43), 
                SimpleName::new(6, Span::new(36, 38)),
                Span::new(39, 39),
                TypeRef::new_simple(7, Span::new(41, 43))
            )
        ]), strings ["array", "data", "u8", "size", "cap", "u64"]
    }
}

#[cfg(feature = "test_stdlib_parse")]
#[cfg(test)] #[test]
fn type_def_stdlib() {
    use std::io::Read;
    use std::fs::File;
    use super::WithTestInput;
    use super::SyntaxTree;

    let mut file = File::open("..\\tests\\syntax\\std.ff").expect("open std.ff failed");
    let mut src = String::new();
    file.read_to_string(&mut src).expect("read std.ff failed");

    let (tree, symbols) = SyntaxTree::with_test_str_ret_symbols(&src);
    panic!("result: {:?}{:?}", symbols, tree);
}
