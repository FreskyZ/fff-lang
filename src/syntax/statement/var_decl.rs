///! fff-lang
///! 
///! syntax/var_decl
///! const-decl = 'const' identifier [ ':' type-use ] [ '=' expr ] ';'
///! var-decl = 'var' identifier [ ':' type-use ] [ '=' expr ] ';'

use crate::syntax::prelude::*;
use super::super::{Expr, TypeUse};

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct VarDeclStatement {
    pub is_const: bool,
    pub name: IsId,
    pub name_span: Span,
    pub typeuse: Option<TypeUse>,
    pub init_expr: Option<Expr>,
    pub all_span: Span,
}
impl ISyntaxFormat for VarDeclStatement {
    fn format(&self, f: Formatter) -> String {
        let f = f.indent().header_text_or(if self.is_const { "const-def" } else { "var-def" }).space().span(self.all_span).endl()
            .indent1().isid(self.name).space().span(self.name_span);
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
        is_const: bool, name: IsId, name_span: Span, 
        typeuse: Option<TypeUse>, init_expr: Option<Expr>) -> VarDeclStatement {
        VarDeclStatement{ all_span, is_const, name, name_span, typeuse, init_expr }
    }
    pub fn new_const(all_span: Span, 
        name: IsId, name_span: Span, 
        typeuse: Option<TypeUse>, init_expr: Option<Expr>) -> VarDeclStatement {
        VarDeclStatement{ all_span, is_const: true, name, name_span, typeuse, init_expr }
    }
    pub fn new_var(all_span: Span, 
        name: IsId, name_span: Span, 
        typeuse: Option<TypeUse>, init_expr: Option<Expr>) -> VarDeclStatement {
        VarDeclStatement{ all_span, is_const: false, name, name_span, typeuse, init_expr }
    }
}
impl ISyntaxGrammar for VarDeclStatement {
    fn matches_first(tokens: [&Token; 3]) -> bool { matches!(tokens[0], &Token::Keyword(Keyword::Const) | &Token::Keyword(Keyword::Var)) }
}
impl<'ecx, 'scx, F> ISyntaxParse<'ecx, 'scx, F> for VarDeclStatement where F: FileSystem {
    type Output = VarDeclStatement;

    fn parse(sess: &mut ParseSession<'ecx, 'scx, F>) -> ParseResult<VarDeclStatement> {
        
        let (starting_kw, starting_span) = sess.expect_keywords(&[Keyword::Const, Keyword::Var])?;
        let is_const = match starting_kw { Keyword::Const => true, Keyword::Var => false, _ => unreachable!() };

        let (name, name_strpos) = sess.expect_ident_or(&[Keyword::Underscore])?;
        let maybe_decltype = if let Some(_) = sess.try_expect_sep(Separator::Colon) { Some(TypeUse::parse(sess)?) } else { None };
        let maybe_init_expr = if let Some(_) = sess.try_expect_sep(Separator::Eq) { Some(Expr::parse(sess)?) } else { None };
        if maybe_decltype.is_none() && maybe_init_expr.is_none() {
            sess.push_message(Message::with_help_by_str("require type annotation", 
                vec![(name_strpos, "variable declaration here")],
                vec!["cannot infer type without both type annotation and initialization expression"]
            ));
        }
        let ending_span = sess.expect_sep(Separator::SemiColon)?;

        return Ok(VarDeclStatement::new(starting_span + ending_span, is_const, name, name_strpos, maybe_decltype, maybe_init_expr));
    }
}

#[cfg(test)] #[test]
fn var_decl_stmt_parse() {
    use crate::syntax::*;
    
    //                                           12345678901234
    assert_eq!{ make_node!("const abc = 0;"),
        VarDeclStatement::new_const(Span::new(0, 13),
            IsId::new(1), Span::new(6, 8),
            None,
            Some(Expr::Lit(LitExpr::new(LitValue::from(0i32), Span::new(12, 12))))
        )
    }

    //                                           0        1         
    //                                           12345678901234567890
    assert_eq!{ make_node!("var hij = [1, 3, 5];"),
        VarDeclStatement::new_var(Span::new(0, 19),
            1, Span::new(4, 6),
            None,
            Some(Expr::Array(ArrayDef::new(Span::new(10, 18), ExprList::new(vec![
                Expr::Lit(LitExpr::new(LitValue::from(1i32), Span::new(11, 11))),
                Expr::Lit(LitExpr::new(LitValue::from(3i32), Span::new(14, 14))),
                Expr::Lit(LitExpr::new(LitValue::from(5i32), Span::new(17, 17))),
            ]))))
        )
    }
    
    //                                           1234567890123456789
    assert_eq!{ make_node!("const input: string;"),
        VarDeclStatement::new_const(Span::new(0, 19),
            1, Span::new(6, 10),
            Some(TypeUse::new_simple(2, Span::new(13, 18))),
            None
        )
    }
    
    //              0123456789012345678901
    TestInput::new("var buf: [(u8, char)];")
        .set_syms(make_symbols!["buf", "array", "tuple", "u8", "char"])
        .apply::<VarDeclStatement, _>()
        .expect_no_message()
        .expect_result(VarDeclStatement::new_var(Span::new(0, 21), 
            1, Span::new(4, 6),
            Some(TypeUse::new_template(2, Span::new(0, 0), Span::new(9, 20), vec![
                TypeUse::new_template(3, Span::new(0, 0), Span::new(10, 19), vec![
                    TypeUse::new_simple(4, Span::new(11, 12)),
                    TypeUse::new_simple(5, Span::new(15, 18)),
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
        .expect_result(VarDeclStatement::new_var(Span::new(0, 47),
            1, Span::new(4, 6),
            Some(TypeUse::new_template(5, Span::new(0, 0), Span::new(9, 19), vec![
                TypeUse::new_template(6, Span::new(0, 0), Span::new(10, 13), vec![
                    TypeUse::new_simple(2, Span::new(11, 12))
                ]),
                TypeUse::new_simple(3, Span::new(16, 18))
            ])),
            Some(Expr::Tuple(TupleDef::new(Span::new(23, 46), make_exprs![
                ArrayDef::new(Span::new(24, 40), make_exprs![
                    LitExpr::new(LitValue::from(1u8), Span::new(25, 27)),
                    LitExpr::new(LitValue::from(5u8), Span::new(30, 32)),
                    LitExpr::new(LitValue::from(7u8), Span::new(35, 39))
                ]),
                SimpleName::new(4, Span::new(43, 45))
            ])))
        ))
    .finish();

    TestInput::new("var a;")
        .apply::<VarDeclStatement, _>()
        .expect_result(VarDeclStatement::new_var(Span::new(0, 5), 1, Span::new(4, 4), None, None))
        .expect_messages(make_messages![
            Message::with_help_by_str("require type annotation", 
                vec![(Span::new(4, 4), "variable declaration here")],
                vec!["cannot infer type without both type annotation and initialization expression"]
            )
        ])
    .finish();
}