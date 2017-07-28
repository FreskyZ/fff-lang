///! fff-lang
///! 
///! syntax/var_decl
///! ConstDecl = fConst fIdentifier [fColon TypeUse] [fAssign Expr] fSemiColon
///! VarDecl = fVar fIdentifier [fColon TypeUse] [fAssign Expr] fSemiColon

use std::fmt;

use codemap::Span;
use codemap::SymbolID;
use message::Message;
use lexical::Token;
use lexical::Keyword;
use lexical::Seperator;

use super::super::Expr;
use super::super::TypeUse;
use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct VarDeclStatement {
    pub is_const: bool,
    pub name: SymbolID,
    pub name_span: Span,
    pub typeuse: Option<TypeUse>,
    pub init_expr: Option<Expr>,
    pub all_span: Span,
}
impl ISyntaxFormat for VarDeclStatement {
    fn format(&self, f: Formatter) -> String {
        let f = f.indent().header_text_or(if self.is_const { "const-def" } else { "var-def" }).space().span(self.all_span).endl()
            .indent1().sym(self.name).space().span(self.name_span);
        let f = match self.typeuse { 
            Some(ref ty) => f.endl().apply1(ty), 
            None => f.endl().indent1().lit("auto-type"),
        };
        let f = match self.init_expr { 
            Some(ref expr) => f.endl().set_prefix_text("init-as").apply1(expr), 
            None => f.endl().indent1().lit("not-inited"),
        };
        f.finish()
    } 
}
impl fmt::Debug for VarDeclStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl VarDeclStatement {

    pub fn new(all_span: Span, 
        is_const: bool, name: SymbolID, name_span: Span, 
        typeuse: Option<TypeUse>, init_expr: Option<Expr>) -> VarDeclStatement {
        VarDeclStatement{ all_span, is_const, name, name_span, typeuse, init_expr }
    }
    pub fn new_const(all_span: Span, 
        name: SymbolID, name_span: Span, 
        typeuse: Option<TypeUse>, init_expr: Option<Expr>) -> VarDeclStatement {
        VarDeclStatement{ all_span, is_const: true, name, name_span, typeuse, init_expr }
    }
    pub fn new_var(all_span: Span, 
        name: SymbolID, name_span: Span, 
        typeuse: Option<TypeUse>, init_expr: Option<Expr>) -> VarDeclStatement {
        VarDeclStatement{ all_span, is_const: false, name, name_span, typeuse, init_expr }
    }
}
impl ISyntaxItemGrammar for VarDeclStatement {
    fn is_first_final(sess: &ParseSession) -> bool { sess.current_tokens()[0] == &Token::Keyword(Keyword::Const) || sess.current_tokens()[0] == &Token::Keyword(Keyword::Var) }
}
impl ISyntaxItemParse for VarDeclStatement {
    type Target = VarDeclStatement;

    fn parse(sess: &mut ParseSession) -> ParseResult<VarDeclStatement> {
        
        let (starting_kw, starting_span) = sess.expect_keywords(&[Keyword::Const, Keyword::Var])?;
        let is_const = match starting_kw { Keyword::Const => true, Keyword::Var => false, _ => unreachable!() };

        let (name, name_strpos) = sess.expect_ident_or(vec![Keyword::Underscore])?;
        let maybe_decltype = if let Some(_) = sess.try_expect_sep(Seperator::Colon) { Some(TypeUse::parse(sess)?) } else { None };
        let maybe_init_expr = if let Some(_) = sess.try_expect_sep(Seperator::Assign) { Some(Expr::parse(sess)?) } else { None };
        if maybe_decltype.is_none() && maybe_init_expr.is_none() {
            sess.push_message(Message::with_help_by_str("require type annotation", 
                vec![(name_strpos, "variable declaration here")],
                vec!["cannot infer type without both type annotation and initialization expression"]
            ));
        }
        let ending_span = sess.expect_sep(Seperator::SemiColon)?;

        return Ok(VarDeclStatement::new(starting_span.merge(&ending_span), is_const, name, name_strpos, maybe_decltype, maybe_init_expr));
    }
}

#[cfg(test)] #[test]
fn var_decl_stmt_parse() {
    use codemap::SymbolCollection;
    use message::MessageCollection;
    use lexical::LitValue;
    use super::super::SimpleName;
    use super::super::LitExpr;
    use super::super::TupleDef;
    use super::super::ArrayDef;
    use super::super::ExprList;
    use super::super::TypeUse;
    use super::super::TestInput;
    use super::super::WithTestInput;
    
    //                                           12345678901234
    assert_eq!{ VarDeclStatement::with_test_str("const abc = 0;"),
        VarDeclStatement::new_const(make_span!(0, 13),
            make_id!(1), make_span!(6, 8),
            None,
            Some(Expr::Lit(LitExpr::new(LitValue::from(0), make_span!(12, 12))))
        )
    }

    //                                           0        1         
    //                                           12345678901234567890
    assert_eq!{ VarDeclStatement::with_test_str("var hij = [1, 3, 5];"),
        VarDeclStatement::new_var(make_span!(0, 19),
            make_id!(1), make_span!(4, 6),
            None,
            Some(Expr::Array(ArrayDef::new(make_span!(10, 18), ExprList::new(vec![
                Expr::Lit(LitExpr::new(LitValue::from(1), make_span!(11, 11))),
                Expr::Lit(LitExpr::new(LitValue::from(3), make_span!(14, 14))),
                Expr::Lit(LitExpr::new(LitValue::from(5), make_span!(17, 17))),
            ]))))
        )
    }
    
    //                                           1234567890123456789
    assert_eq!{ VarDeclStatement::with_test_str("const input: string;"),
        VarDeclStatement::new_const(make_span!(0, 19),
            make_id!(1), make_span!(6, 10),
            Some(TypeUse::new_simple(make_id!(2), make_span!(13, 18))),
            None
        )
    }
    
    //              0123456789012345678901
    TestInput::new("var buf: [(u8, char)];")
        .set_syms(make_symbols!["buf", "array", "tuple", "u8", "char"])
        .apply::<VarDeclStatement, _>()
        .expect_no_message()
        .expect_result(VarDeclStatement::new_var(make_span!(0, 21), 
            make_id!(1), make_span!(4, 6),
            Some(TypeUse::new_template(make_id!(2), Span::default(), make_span!(9, 20), vec![
                TypeUse::new_template(make_id!(3), Span::default(), make_span!(10, 19), vec![
                    TypeUse::new_simple(make_id!(4), make_span!(11, 12)),
                    TypeUse::new_simple(make_id!(5), make_span!(15, 18)),
                ])
            ])),
            None
        ))
    .finish();

    // Future Attention: after bits type added, the `0x7u8` will have different type as before, this is the Option::unwrap failure in test
    // and after advanced type infer, change the 0x7u8 to 7, and try to infer it as 7u8, which requires 2 major changes
    //     do not infer i32 in num lit if no postfix provided
    //     desugar array primary expr to call array_tid::new() and array.push, which infer 7's type as array_tid' push method's parameter
    //             0        1         2         3         4
    //             012345678901234567890123456789012345678901234567
    TestInput::new("var buf: ([u8], u32) = ([1u8, 5u8, 0x7u8], abc);")
        //                       1      2     3      4      5        6
        .set_syms(make_symbols!["buf", "u8", "u32", "abc", "tuple", "array"])
        .apply::<VarDeclStatement, _>()
        .expect_no_message()
        .expect_result(VarDeclStatement::new_var(make_span!(0, 47),
            make_id!(1), make_span!(4, 6),
            Some(TypeUse::new_template(make_id!(5), Span::default(), make_span!(9, 19), vec![
                TypeUse::new_template(make_id!(6), Span::default(), make_span!(10, 13), vec![
                    TypeUse::new_simple(make_id!(2), make_span!(11, 12))
                ]),
                TypeUse::new_simple(make_id!(3), make_span!(16, 18))
            ])),
            Some(Expr::Tuple(TupleDef::new(make_span!(23, 46), make_exprs![
                ArrayDef::new(make_span!(24, 40), make_exprs![
                    LitExpr::new(LitValue::from(1u8), make_span!(25, 27)),
                    LitExpr::new(LitValue::from(5u8), make_span!(30, 32)),
                    LitExpr::new(LitValue::from(7u8), make_span!(35, 39))
                ]),
                SimpleName::new(make_id!(4), make_span!(43, 45))
            ])))
        ))
    .finish();

    TestInput::new("var a;")
        .apply::<VarDeclStatement, _>()
        .expect_result(VarDeclStatement::new_var(make_span!(0, 5), make_id!(1), make_span!(4, 4), None, None))
        .expect_messages(make_messages![
            Message::with_help_by_str("require type annotation", 
                vec![(make_span!(4, 4), "variable declaration here")],
                vec!["cannot infer type without both type annotation and initialization expression"]
            )
        ])
    .finish();
}