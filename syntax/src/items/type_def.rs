///! fff-lang
///! 
///! syntax/type_def
///! required by removing hardcode macros in semantic typedef collection and fndef collections
///! type_def = 'type' (identifier | keyword_primitive_type)  '{' [ type_field_def { ',' type_field_def } [ ',' ] ] '}'
///! type_field_def = identifier ':' type_use

// TODO: support Keyword::PrimType in type name, for primitive declarations

use std::fmt;

use codemap::Span;
use lexical::Token;
use lexical::Keyword;
use lexical::Seperator;

use super::super::TypeUse;
use super::super::SimpleName;

use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxFormat;
use super::super::ISyntaxItemGrammar;

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
        
        let mut f = f.indent().header_text_or("TypeDef").space().span(self.all_span).endl().apply1(&self.name);
        for &TypeFieldDef{ ref name, ref colon_span, ref typeuse, ref all_span } in &self.fields {
            f = f.indent1().lit("Field").space().span(*all_span).endl()
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
impl ISyntaxItemGrammar for TypeDef {
    fn is_first_final(sess: &ParseSession) -> bool { sess.current_tokens()[0] == &Token::Keyword(Keyword::Type) }
}
impl ISyntaxItemParse for TypeDef {
    type Target = Self;

    fn parse(sess: &mut ParseSession) -> ParseResult<TypeDef> {

        let starting_span = sess.expect_keyword(Keyword::Type)?;
        let (symid, name_span) = sess.expect_ident_or_if(Keyword::is_primitive)?;
        let name = SimpleName::new(symid, name_span);
        let _left_brace_span = sess.expect_sep(Seperator::LeftBrace)?;

        let mut fields = Vec::new();
        let right_brace_span = loop { 
            if let Some(right_brace_span) = sess.try_expect_sep(Seperator::RightBrace) {
                break right_brace_span;     // rustc 1.19 stablize break-expr
            }

            let field_name = SimpleName::parse(sess)?;
            let colon_span = sess.expect_sep(Seperator::Colon)?;
            let field_type = TypeUse::parse(sess)?;
            fields.push(if let Some(comma_span) = sess.try_expect_sep(Seperator::Comma) {
                TypeFieldDef::new(field_name.span.merge(&comma_span), field_name, colon_span, field_type)
            } else {
                TypeFieldDef::new(field_name.span.merge(&field_type.all_span), field_name, colon_span, field_type)
            });
        };

        Ok(TypeDef::new(starting_span.merge(&right_brace_span), name, fields))
    }
}

#[cfg(test)] #[test]
fn type_def_parse() {
    use codemap::SymbolCollection;
    use super::super::TestInput;
    use super::super::WithTestInput;

    //                                  01234567890123456
    assert_eq!{ TypeDef::with_test_str("type x { x: i32 }"),
        TypeDef::new(make_span!(0, 16), SimpleName::new(make_id!(1), make_span!(5, 5)), vec![
            TypeFieldDef::new(make_span!(9, 14), 
                SimpleName::new(make_id!(1), make_span!(9, 9)),
                make_span!(10, 10),
                TypeUse::new_simple(make_id!(2), make_span!(12, 14))
            )
        ])
    }
    assert_eq!{ TypeDef::with_test_str("type x { x: i32,}"),
        TypeDef::new(make_span!(0, 16), SimpleName::new(make_id!(1), make_span!(5, 5)), vec![
            TypeFieldDef::new(make_span!(9, 15), 
                SimpleName::new(make_id!(1), make_span!(9, 9)),
                make_span!(10, 10),
                TypeUse::new_simple(make_id!(2), make_span!(12, 14))
            )
        ])
    }
    //                                    0         1         2         3         4
    //                                    0123456789012345678901234567890123456789012345
    TestInput::new("type array { data: [u8], size: u64, cap: u64 }")
        .set_syms(make_symbols!["array", "data", "size", "cap", "u64", "u8"])
        .apply::<TypeDef, _>()
        .expect_no_message()
        .expect_result(TypeDef::new(make_span!(0, 45), SimpleName::new(make_id!(1), make_span!(5, 9)), vec![
            TypeFieldDef::new(make_span!(13, 23),
                SimpleName::new(make_id!(2), make_span!(13, 16)),
                make_span!(17, 17),
                TypeUse::new_template(make_id!(1), Span::default(), make_span!(19, 22), vec![
                    TypeUse::new_simple(make_id!(6), make_span!(20, 21))
                ])
            ),
            TypeFieldDef::new(make_span!(25, 34), 
                SimpleName::new(make_id!(3), make_span!(25, 28)),
                make_span!(29, 29),
                TypeUse::new_simple(make_id!(5), make_span!(31, 33))
            ),
            TypeFieldDef::new(make_span!(36, 43), 
                SimpleName::new(make_id!(4), make_span!(36, 38)),
                make_span!(39, 39),
                TypeUse::new_simple(make_id!(5), make_span!(41, 43))
            )
        ]))
    .finish();
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