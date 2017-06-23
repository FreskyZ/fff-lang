///! fff-lang
///! 
///! syntax/type_def
///! required by removing hardcode macros in semantic typedef collection and fndef collections
///! type_def = 'type' identifier '{' [ type_field_def { ',' type_field_def } [ ',' ] ] '}'
///! type_field_def = identifier ':' type_use

// TODO: support Keyword::PrimType in type name, for primitive declarations

use std::fmt;

use codemap::Span;
use lexical::Token;
use lexical::Keyword;
use lexical::SeperatorKind;

use super::super::TypeUse;
use super::super::IdentExpr;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct TypeFieldDef {
    pub name: IdentExpr,
    pub colon_span: Span,
    pub typeuse: TypeUse,
    pub all_span: Span,   // ident to typeuse or ident to comma
}
impl TypeFieldDef {
    pub fn new(all_span: Span, name: IdentExpr, colon_span: Span, typeuse: TypeUse) -> TypeFieldDef {
        TypeFieldDef{ all_span, name, colon_span, typeuse }
    }
}
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct TypeDef {
    pub all_span: Span,
    pub name: IdentExpr,
    pub fields: Vec<TypeFieldDef>,
}
impl ISyntaxItemFormat for TypeDef {
    fn format(&self, indent: u32) -> String {
        
        let mut retval = format!("{}TypeDef <{:?}>\n{}", TypeDef::indent_str(indent), self.all_span, self.name.format(indent + 1));
        for &TypeFieldDef{ ref name, ref colon_span, ref typeuse, ref all_span } in &self.fields {
            retval.push_str(&format!("\n{}Field <{:?}>\n{}\n{}colon <{:?}>\n{}", 
                TypeDef::indent_str(indent + 1), all_span, 
                name.format(indent + 2), 
                TypeDef::indent_str(indent + 2), colon_span, 
                typeuse.format(indent + 2)
            ));
        }
        return retval;
    }
}
impl fmt::Debug for TypeDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl TypeDef {
    pub fn new(all_span: Span, name: IdentExpr, fields: Vec<TypeFieldDef>) -> TypeDef {
        TypeDef{ all_span, name, fields }
    }
}
impl ISyntaxItemGrammar for TypeDef {
    fn is_first_final(sess: &ParseSession) -> bool { sess.tk == &Token::Keyword(Keyword::Type) }
}
impl ISyntaxItemParse for TypeDef {
    type Target = Self;

    fn parse(sess: &mut ParseSession) -> ParseResult<TypeDef> {

        let starting_span = sess.expect_keyword(Keyword::Type)?;
        let name = IdentExpr::parse(sess)?;
        let _left_brace_span = sess.expect_sep(SeperatorKind::LeftBrace)?;

        let mut fields = Vec::new();
        let right_brace_span: Span;
        loop {
            if let (&Token::Sep(SeperatorKind::RightBrace), span) = (sess.tk, sess.pos) {
                right_brace_span = span;
                break;
            }

            let field_name = IdentExpr::parse(sess)?;
            let colon_span = sess.expect_sep(SeperatorKind::Colon)?;
            let field_type = TypeUse::parse(sess)?;
            fields.push(if let (&Token::Sep(SeperatorKind::Comma), ref comma_span) = (sess.tk, sess.pos) {
                sess.move_next();
                TypeFieldDef::new(field_name.span.merge(comma_span), field_name, colon_span, field_type)
            } else {
                TypeFieldDef::new(field_name.span.merge(&field_type.all_span), field_name, colon_span, field_type)
            });
        }

        Ok(TypeDef::new(starting_span.merge(&right_brace_span), name, fields))
    }
}

#[cfg(test)] #[test]
fn type_def_parse() {
    use codemap::SymbolCollection;
    use super::super::ISyntaxItemWithStr;

    //                                  01234567890123456
    assert_eq!{ TypeDef::with_test_str("type x { x: i32 }"),
        TypeDef::new(make_span!(0, 16), IdentExpr::new(make_id!(1), make_span!(5, 5)), vec![
            TypeFieldDef::new(make_span!(9, 14), 
                IdentExpr::new(make_id!(1), make_span!(9, 9)),
                make_span!(10, 10),
                TypeUse::new_simple(make_id!(2), make_span!(12, 14))
            )
        ])
    }
    assert_eq!{ TypeDef::with_test_str("type x { x: i32,}"),
        TypeDef::new(make_span!(0, 16), IdentExpr::new(make_id!(1), make_span!(5, 5)), vec![
            TypeFieldDef::new(make_span!(9, 15), 
                IdentExpr::new(make_id!(1), make_span!(9, 9)),
                make_span!(10, 10),
                TypeUse::new_simple(make_id!(2), make_span!(12, 14))
            )
        ])
    }
    //                                    0         1         2         3         4
    //                                    0123456789012345678901234567890123456789012345
    assert_eq!{ TypeDef::with_test_input("type array { data: [u8], size: u64, cap: u64 }", 
            &mut make_symbols!["array", "data", "size", "cap", "u64", "u8"]),
        TypeDef::new(make_span!(0, 45), IdentExpr::new(make_id!(1), make_span!(5, 9)), vec![
            TypeFieldDef::new(make_span!(13, 23),
                IdentExpr::new(make_id!(2), make_span!(13, 16)),
                make_span!(17, 17),
                TypeUse::new_template(make_id!(1), Span::default(), make_span!(19, 22), vec![
                    TypeUse::new_simple(make_id!(6), make_span!(20, 21))
                ])
            ),
            TypeFieldDef::new(make_span!(25, 34), 
                IdentExpr::new(make_id!(3), make_span!(25, 28)),
                make_span!(29, 29),
                TypeUse::new_simple(make_id!(5), make_span!(31, 33))
            ),
            TypeFieldDef::new(make_span!(36, 43), 
                IdentExpr::new(make_id!(4), make_span!(36, 38)),
                make_span!(39, 39),
                TypeUse::new_simple(make_id!(5), make_span!(41, 43))
            )
        ])
    }
}