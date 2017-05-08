///! fff-lang
///! 
///! syntax/var_decl
///! ConstDecl = fConst fIdentifier [fColon TypeUse] [fAssign BinaryExpr] fSemiColon
///! VarDecl = fVar fIdentifier [fColon TypeUse] [fAssign BinaryExpr] fSemiColon

use std::fmt;

use codepos::StringPosition;
use message::Message;
use lexical::Token;
use lexical::KeywordKind;
use lexical::SeperatorKind;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;
use super::super::BinaryExpr;
use super::super::TypeUse;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct VarDeclStatement {
    m_is_const: bool,
    name: String,
    name_strpos: StringPosition,
    typeuse: Option<TypeUse>,
    init_expr: Option<BinaryExpr>,
    all_strpos: StringPosition,
}
impl ISyntaxItemFormat for VarDeclStatement {
    fn format(&self, indent: u32) -> String {
        format!("{}VarDecl {}<{:?}>\n{}'{}' <{:?}>{}{}", 
            VarDeclStatement::indent_str(indent), if self.m_is_const { "const " } else { "var " }, self.all_strpos,
            VarDeclStatement::indent_str(indent + 1), self.name, self.name_strpos,
            match self.typeuse { Some(ref typeuse) => format!("\n{}", typeuse.format(indent + 1)), None => String::new() },
            match self.init_expr { Some(ref init_expr) => format!("\n{}", init_expr.format(indent + 1)), None => String::new() },
        )
    } 
}
impl fmt::Debug for VarDeclStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl VarDeclStatement {

    // And lazy to provide new_const_no_type, new_const_no_init_expr, new_const_no_type_no_init_expr and new_var version
    pub fn new(all_strpos: StringPosition, 
        is_const: bool, name: String, name_strpos: StringPosition, 
        typeuse: Option<TypeUse>, init_expr: Option<BinaryExpr>) -> VarDeclStatement {
        VarDeclStatement{ all_strpos, m_is_const: is_const, name, name_strpos, typeuse, init_expr }
    }
    pub fn new_const(all_strpos: StringPosition, 
        name: String, name_strpos: StringPosition, 
        typeuse: Option<TypeUse>, init_expr: Option<BinaryExpr>) -> VarDeclStatement {
        VarDeclStatement{ all_strpos, m_is_const: true, name, name_strpos, typeuse, init_expr }
    }
    pub fn new_var(all_strpos: StringPosition, 
        name: String, name_strpos: StringPosition, 
        typeuse: Option<TypeUse>, init_expr: Option<BinaryExpr>) -> VarDeclStatement {
        VarDeclStatement{ all_strpos, m_is_const: false, name, name_strpos, typeuse, init_expr }
    }

    pub fn is_const(&self) -> bool { self.m_is_const }
    pub fn get_name(&self) -> &String { &self.name }
    pub fn get_name_strpos(&self) -> StringPosition { self.name_strpos }
    pub fn get_decltype(&self) -> Option<&TypeUse> { self.typeuse.as_ref() }
    pub fn get_init_expr(&self) -> Option<&BinaryExpr> { self.init_expr.as_ref() }
    
    pub fn get_all_strpos(&self) -> StringPosition { self.all_strpos }
}
impl ISyntaxItemGrammar for VarDeclStatement {
    fn is_first_final(sess: &ParseSession) -> bool { sess.tk == &Token::Keyword(KeywordKind::Const) || sess.tk == &Token::Keyword(KeywordKind::Var) }
}
impl ISyntaxItemParse for VarDeclStatement {

    fn parse(sess: &mut ParseSession) -> ParseResult<VarDeclStatement> {

        let starting_strpos = sess.pos;
        let is_const = match sess.tk {
            &Token::Keyword(KeywordKind::Const) => true, 
            &Token::Keyword(KeywordKind::Var) => false,
            _ => unreachable!(), 
        };
        sess.move_next();

        let (name, name_strpos) = sess.expect_ident_or(vec![KeywordKind::Underscore])?;
        let maybe_decltype = if sess.tk == &Token::Sep(SeperatorKind::Colon) { sess.move_next(); Some(TypeUse::parse(sess)?) } else { None };
        let maybe_init_expr = if sess.tk == &Token::Sep(SeperatorKind::Assign) { sess.move_next(); Some(BinaryExpr::parse(sess)?) } else { None };
        if maybe_decltype.is_none() && maybe_init_expr.is_none() {
            sess.push_message(Message::with_help_by_str("Require type annotation", 
                vec![(name_strpos, "Variable declaration here")],
                vec!["cannot infer type without initialization expression"]
            ));
        }
        let ending_strpos = sess.expect_sep(SeperatorKind::SemiColon)?;

        let all_strpos = StringPosition::merge(starting_strpos, ending_strpos);
        return Ok(VarDeclStatement::new(all_strpos, is_const, name, name_strpos, maybe_decltype, maybe_init_expr));
    }
}

#[cfg(test)] #[test]
fn var_decl_stmt_parse() {
    use super::super::ISyntaxItemWithStr;
    use super::super::TypeUseF;
    use lexical::LitValue;
    
    //                                           12345678901234
    assert_eq!{ VarDeclStatement::with_test_str("const abc = 0;"),
        VarDeclStatement::new_const(make_strpos!(1, 1, 1, 14),
            "abc".to_owned(), make_strpos!(1, 7, 1, 9),
            None,
            Some(BinaryExpr::new_lit(LitValue::from(0), make_strpos!(1, 13, 1, 13)))
        )
    }

    //                                           0        1         
    //                                           12345678901234567890
    assert_eq!{ VarDeclStatement::with_test_str("var hij = [1, 3, 5];"),
        VarDeclStatement::new_var(make_strpos!(1, 1, 1, 20),
            "hij".to_owned(), make_strpos!(1, 5, 1, 7),
            None,
            Some(BinaryExpr::new_array(make_strpos!(1, 11, 1, 19), vec![
                BinaryExpr::new_lit(LitValue::from(1), make_strpos!(1, 12, 1, 12)),
                BinaryExpr::new_lit(LitValue::from(3), make_strpos!(1, 15, 1, 15)),
                BinaryExpr::new_lit(LitValue::from(5), make_strpos!(1, 18, 1, 18)),
            ]))
        )
    }
    
    //                                           1234567890123456789
    assert_eq!{ VarDeclStatement::with_test_str("const input: string;"),
        VarDeclStatement::new_const(make_strpos!(1, 1, 1, 20),
            "input".to_owned(), make_strpos!(1, 7, 1, 11),
            Some(TypeUseF::new_simple_test("string", make_strpos!(1, 14, 1, 19))),
            None
        )
    }
    
    //                                           1234567890123456789012
    assert_eq!{ VarDeclStatement::with_test_str("var buf: [(u8, char)];"),
        VarDeclStatement::new_var(make_strpos!(1, 1, 1, 22), 
            "buf".to_owned(), make_strpos!(1, 5, 1, 7),
            Some(TypeUseF::new_array(make_strpos!(1, 10, 1, 21), 
                TypeUseF::new_tuple(make_strpos!(1, 11, 1, 20), vec![
                    TypeUseF::new_simple_test("u8", make_strpos!(1, 12, 1, 13)),
                    TypeUseF::new_simple("char".to_owned(), make_strpos!(1, 16, 1, 19)),
                ])
            )),
            None
        )
    }

    // Attention: after bits type added, the `0x7u8` will have different type as before, this is the Option::unwrap failure in test
    // and after advanced type infer, change the 0x7u8 to 7, and try to infer it as 7u8, which requires 2 major changes
    //     do not infer i32 in num lit if no postfix provided
    //     desugar array primary expr to call array_tid::new() and array.push, which infer 7's type as array_tid' push method's parameter
    //                                           0        1         2         3         4
    //                                           123456789012345678901234567890123456789012345678
    assert_eq!{ VarDeclStatement::with_test_str("var buf: ([u8], u32) = ([1u8, 5u8, 0x7u8], abc);"),
        VarDeclStatement::new_var(make_strpos!(1, 1, 1, 48),
            "buf".to_owned(), make_strpos!(1, 5, 1, 7),
            Some(TypeUseF::new_tuple(make_strpos!(1, 10, 1, 20), vec![
                TypeUseF::new_array(make_strpos!(1, 11, 1, 14), 
                    TypeUseF::new_simple("u8".to_owned(), make_strpos!(1, 12, 1, 13))
                ),
                TypeUseF::new_simple("u32".to_owned(), make_strpos!(1, 17, 1, 19))
            ])),
            Some(BinaryExpr::new_tuple(make_strpos!(1, 24, 1, 47), vec![
                BinaryExpr::new_array(make_strpos!(1, 25, 1, 41), vec![
                    BinaryExpr::new_lit(LitValue::from(1u8), make_strpos!(1, 26, 1, 28)),
                    BinaryExpr::new_lit(LitValue::from(5u8), make_strpos!(1, 31, 1, 33)),
                    BinaryExpr::new_lit(LitValue::from(7u8), make_strpos!(1, 36, 1, 40))
                ]),
                BinaryExpr::new_ident("abc".to_owned(), make_strpos!(1, 44, 1, 46))
            ]))
        )
    }

    // Todo: errors test
}