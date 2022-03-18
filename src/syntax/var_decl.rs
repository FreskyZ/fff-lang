///! fff-lang
///! 
///! syntax/var_decl
///! const-decl = 'const' identifier [ ':' type-use ] [ '=' expr ] ';'
///! var-decl = 'var' identifier [ ':' type-use ] [ '=' expr ] ';'

use super::prelude::*;
use super::{Expr, TypeRef};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct VarDeclStatement {
    pub is_const: bool,
    pub name: IsId,
    pub name_span: Span,
    pub r#type: Option<TypeRef>,
    pub init_expr: Option<Expr>,
    pub all_span: Span,
}

impl VarDeclStatement {

    pub fn new(all_span: Span, 
        is_const: bool, name: impl Into<IsId>, name_span: Span, 
        r#type: Option<TypeRef>, init_expr: Option<Expr>) -> VarDeclStatement {
        VarDeclStatement{ all_span, is_const, name: name.into(), name_span, r#type, init_expr }
    }
    pub fn new_const(all_span: Span, 
        name: impl Into<IsId>, name_span: Span, 
        r#type: Option<TypeRef>, init_expr: Option<Expr>) -> VarDeclStatement {
        VarDeclStatement{ all_span, is_const: true, name: name.into(), name_span, r#type, init_expr }
    }
    pub fn new_var(all_span: Span, 
        name: impl Into<IsId>, name_span: Span, 
        r#type: Option<TypeRef>, init_expr: Option<Expr>) -> VarDeclStatement {
        VarDeclStatement{ all_span, is_const: false, name: name.into(), name_span, r#type, init_expr }
    }
}

impl Parser for VarDeclStatement {
    type Output = VarDeclStatement;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Keyword(Keyword::Const | Keyword::Var)) 
    }

    fn parse(cx: &mut ParseContext) -> Result<VarDeclStatement, Unexpected> {
        
        let (starting_kw, starting_span) = cx.expect_keywords(&[Keyword::Const, Keyword::Var])?;
        let is_const = match starting_kw { Keyword::Const => true, Keyword::Var => false, _ => unreachable!() };

        let (name, name_span) = cx.expect_ident_or(&[Keyword::Underscore])?;
        let maybe_decltype = if let Some(_) = cx.try_expect_sep(Separator::Colon) { Some(cx.expect::<TypeRef>()?) } else { None };
        let maybe_init_expr = if let Some(_) = cx.try_expect_sep(Separator::Eq) { Some(cx.expect::<Expr>()?) } else { None };
        if maybe_decltype.is_none() && maybe_init_expr.is_none() {
            cx.emit("require type annotation")
                .detail(name_span, "variable declaration here")
                .help("cannot infer type without both type annotation and initialization expression");
        }
        let ending_span = cx.expect_sep(Separator::SemiColon)?;

        return Ok(VarDeclStatement::new(starting_span + ending_span, is_const, name, name_span, maybe_decltype, maybe_init_expr));
    }
}

impl Node for VarDeclStatement {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_var_decl(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        if let Some(r#type) = &self.r#type {
            v.visit_type_ref(r#type)?;
        }
        if let Some(init_expr) = &self.init_expr {
            v.visit_expr(init_expr)?;
        }
        Ok(Default::default())
    }
}

#[cfg(feature = "todo")]
#[cfg(test)] #[test]
fn var_decl_stmt_parse() {
    use crate::diagnostics::*;
    use crate::syntax::*;
    
    //                                           12345678901234
    case!{ "const abc = 0;" as VarDeclStatement,
        VarDeclStatement::new_const(Span::new(0, 13),
            2, Span::new(6, 8),
            None,
            Some(Expr::Lit(make_lit!(0, 12, 12)))
        )
    }

    //                                           0        1         
    //                                           12345678901234567890
    case!{ "var hij = [1, 3, 5];" as VarDeclStatement,
        VarDeclStatement::new_var(Span::new(0, 19),
            2, Span::new(4, 6),
            None,
            Some(Expr::Array(ArrayDef::new(Span::new(10, 18), ExprList::new(vec![
                Expr::Lit(make_lit!(1, 11, 11)),
                Expr::Lit(make_lit!(3, 14, 14)),
                Expr::Lit(make_lit!(5, 17, 17)),
            ]))))
        )
    }
    
    //                                           1234567890123456789
    case!{ "const input: string;" as VarDeclStatement,
        VarDeclStatement::new_const(Span::new(0, 19),
            2, Span::new(6, 10),
            Some(TypeRef::new_simple(3, Span::new(13, 18))),
            None
        )
    }
    
    //              0123456789012345678901
    case!{ "var buf: [(u8, char)];" as VarDeclStatement,
        VarDeclStatement::new_var(Span::new(0, 21), 
            2, Span::new(4, 6),
            Some(TypeRef::new_template(6, Span::new(0, 0), Span::new(9, 20), vec![
                TypeRef::new_template(5, Span::new(0, 0), Span::new(10, 19), vec![
                    TypeRef::new_simple(3, Span::new(11, 12)),
                    TypeRef::new_simple(4, Span::new(15, 18)),
                ])
            ])),
            None
        ), strings ["buf", "u8", "char", "tuple", "array"]
    }

    // Future Attention: after bits type added, the `0x7u8` will have different type as before, this is the Option::unwrap failure in test
    // and after advanced type infer, change the 0x7u8 to 7, and try to infer it as 7u8, which requires 2 major changes
    //     do not infer i32 in num lit if no postfix provided
    //     desugar array primary expr to call array_tid::new() and array.push, which infer 7's type as array_tid' push method's parameter
    //             0        1         2         3         4
    //             012345678901234567890123456789012345678901234567
    case!{ "var buf: ([u8], u32) = ([1u8, 5u8, 0x7u8], abc);" as VarDeclStatement,
        VarDeclStatement::new_var(Span::new(0, 47),
            2, Span::new(4, 6),
            Some(TypeRef::new_template(6, Span::new(0, 0), Span::new(9, 19), vec![
                TypeRef::new_template(4, Span::new(0, 0), Span::new(10, 13), vec![
                    TypeRef::new_simple(3, Span::new(11, 12))
                ]),
                TypeRef::new_simple(5, Span::new(16, 18))
            ])),
            Some(Expr::Tuple(TupleDef::new(Span::new(23, 46), make_exprs![
                ArrayDef::new(Span::new(24, 40), make_exprs![
                    make_lit!(1: u8, 25, 27),
                    make_lit!(5: u8, 30, 32),
                    make_lit!(7: u8, 35, 39),
                ]),
                SimpleName::new(7, Span::new(43, 45))
            ])))
        ), strings ["buf", "u8", "array", "u32", "tuple", "abc"]
    }

    case!{ "var a;" as VarDeclStatement,
        VarDeclStatement::new_var(Span::new(0, 5), 2, Span::new(4, 4), None, None), errors make_errors!(
            e: e.emit("require type annotation")
                .detail(Span::new(4, 4), "variable declaration here")
                .help("cannot infer type without both type annotation and initialization expression")),
    }
}
