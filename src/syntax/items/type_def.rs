///! fff-lang
///! 
///! syntax/type_def
///! required by removing hardcode macros in semantic typedef collection and fndef collections
///! type_def = 'type' (identifier | keyword_primitive_type)  '{' [ type_field_def { ',' type_field_def } [ ',' ] ] '}'
///! type_field_def = identifier ':' type_use

// TODO: support Keyword::PrimType in type name, for primitive declarations

use crate::syntax::prelude::*;
use super::super::{TypeUse, SimpleName};

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct TypeFieldDef {
    pub name: SimpleName,
    pub colon_span: Span,
    pub typeuse: TypeUse,
    pub all_span: Span,   // ident to typeuse or ident to comma
}
impl TypeFieldDef {
    pub fn new(all_span: Span, name: SimpleName, colon_span: Span, typeuse: TypeUse) -> TypeFieldDef {
        TypeFieldDef{ all_span, name, colon_span, typeuse }
    }
}
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct TypeDef {
    pub all_span: Span,
    pub name: SimpleName,
    pub fields: Vec<TypeFieldDef>,
}
impl ISyntaxFormat for TypeDef {
    fn format(&self, f: Formatter) -> String {
        
        let mut f = f.indent().header_text_or("type-def").space().span(self.all_span).endl()
            .apply1(&self.name);
        for &TypeFieldDef{ ref name, ref colon_span, ref typeuse, ref all_span } in &self.fields {
            f = f.endl()
                .indent1().lit("field").space().span(*all_span).endl()
                .apply2(name).endl()
                .indent2().lit("\":\"").space().span(*colon_span).endl()
                .apply2(typeuse);
        }
        f.finish()
    }
}
impl fmt::Debug for TypeDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl TypeDef {
    pub fn new(all_span: Span, name: SimpleName, fields: Vec<TypeFieldDef>) -> TypeDef {
        TypeDef{ all_span, name, fields }
    }
}
impl ISyntaxGrammar for TypeDef {
    fn matches_first(tokens: [&Token; 3]) -> bool { matches!(tokens[0], &Token::Keyword(Keyword::Type)) }
}
impl<'ecx, 'scx, F> ISyntaxParse<'ecx, 'scx, F> for TypeDef where F: FileSystem {
    type Output = Self;

    fn parse(sess: &mut ParseSession<'ecx, 'scx, F>) -> ParseResult<TypeDef> {

        let starting_span = sess.expect_keyword(Keyword::Type)?;
        let (symid, name_span) = sess.expect_ident_or_keyword_kind(KeywordKind::Primitive)?;
        let name = SimpleName::new(symid, name_span);
        let _left_brace_span = sess.expect_sep(Separator::LeftBrace)?;

        let mut fields = Vec::new();
        let right_brace_span = loop { 
            if let Some(right_brace_span) = sess.try_expect_sep(Separator::RightBrace) {
                break right_brace_span;     // rustc 1.19 stablize break-expr
            }

            let field_name = SimpleName::parse(sess)?;
            let colon_span = sess.expect_sep(Separator::Colon)?;
            let field_type = TypeUse::parse(sess)?;
            fields.push(if let Some(comma_span) = sess.try_expect_sep(Separator::Comma) {
                TypeFieldDef::new(field_name.span + comma_span, field_name, colon_span, field_type)
            } else {
                TypeFieldDef::new(field_name.span + field_type.all_span, field_name, colon_span, field_type)
            });
        };

        Ok(TypeDef::new(starting_span + right_brace_span, name, fields))
    }
}

#[cfg(test)] #[test]
fn type_def_parse() {
    use super::super::make_node;

    //                                  01234567890123456
    assert_eq!{ make_node!("type x { x: i32 }" as TypeDef),
        TypeDef::new(Span::new(0, 16), SimpleName::new(1, Span::new(5, 5)), vec![
            TypeFieldDef::new(Span::new(9, 14), 
                SimpleName::new(1, Span::new(9, 9)),
                Span::new(10, 10),
                TypeUse::new_simple(2, Span::new(12, 14))
            )
        ])
    }
    assert_eq!{ make_node!("type x { x: i32,}" as TypeDef),
        TypeDef::new(Span::new(0, 16), SimpleName::new(1, Span::new(5, 5)), vec![
            TypeFieldDef::new(Span::new(9, 15), 
                SimpleName::new(1, Span::new(9, 9)),
                Span::new(10, 10),
                TypeUse::new_simple(2, Span::new(12, 14))
            )
        ])
    }
    //                                    0         1         2         3         4
    //                                    0123456789012345678901234567890123456789012345
    assert_eq!{ make_node!("type array { data: [u8], size: u64, cap: u64 }" as TypeDef, [], ["array", "data", "size", "cap", "u64", "u8"]),
        TypeDef::new(Span::new(0, 45), SimpleName::new(1, Span::new(5, 9)), vec![
            TypeFieldDef::new(Span::new(13, 23),
                SimpleName::new(2, Span::new(13, 16)),
                Span::new(17, 17),
                TypeUse::new_template(1, Span::new(0, 0), Span::new(19, 22), vec![
                    TypeUse::new_simple(6, Span::new(20, 21))
                ])
            ),
            TypeFieldDef::new(Span::new(25, 34), 
                SimpleName::new(3, Span::new(25, 28)),
                Span::new(29, 29),
                TypeUse::new_simple(5, Span::new(31, 33))
            ),
            TypeFieldDef::new(Span::new(36, 43), 
                SimpleName::new(4, Span::new(36, 38)),
                Span::new(39, 39),
                TypeUse::new_simple(5, Span::new(41, 43))
            )
        ])
    }
}

#[cfg(feature = "test_stdlib_parse")]
#[cfg(test)] #[test]
fn type_def_stdlib() {
    use std::io::Read;
    use std::fs::File;
    use super::super::WithTestInput;
    use super::super::SyntaxTree;

    let mut file = File::open("..\\tests\\syntax\\std.ff").expect("open std.ff failed");
    let mut src = String::new();
    file.read_to_string(&mut src).expect("read std.ff failed");

    let (tree, symbols) = SyntaxTree::with_test_str_ret_symbols(&src);
    panic!("result: {:?}{:?}", symbols, tree);
}
