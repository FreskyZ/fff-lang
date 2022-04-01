///! fff-lang
///! 
///! syntax/var_decl
///! const-decl = 'const' identifier [ ':' type-use ] [ '=' expr ] ';'
///! var-decl = 'var' identifier [ ':' type-use ] [ '=' expr ] ';'

use super::prelude::*;

impl Parser for VarDeclStatement {
    type Output = VarDeclStatement;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Keyword(Keyword::Const | Keyword::Var)) 
    }

    fn parse(cx: &mut ParseContext) -> Result<VarDeclStatement, Unexpected> {
        
        let (starting_kw, starting_span) = cx.expect_keywords(&[Keyword::Const, Keyword::Var])?;
        let is_const = match starting_kw { Keyword::Const => true, Keyword::Var => false, _ => unreachable!() };

        let (name, name_span) = cx.expect_ident_or_keywords(&[Keyword::Underscore])?;
        let r#type = cx.try_expect_sep(Separator::Colon).map(|_| cx.expect::<TypeRef>()).transpose()?;
        let init_expr = cx.try_expect_sep(Separator::Eq).map(|_| cx.expect::<Expr>()).transpose()?;
        if r#type.is_none() && init_expr.is_none() {
            cx.emit("require type annotation")
                .detail(name_span, "variable declaration here")
                .help("cannot infer type without both type annotation and initialization expression");
        }
        let ending_span = cx.expect_sep(Separator::SemiColon)?;

        Ok(VarDeclStatement{ all_span: starting_span + ending_span, is_const, name, name_span, r#type, init_expr })
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

#[cfg(test)] #[test]
fn var_decl_stmt_parse() {    
    //                                           12345678901234
    case!{ "const abc = 0;" as VarDeclStatement,
        make_stmt!(const 0:13 #2 6:8
            None,
            Some(make_expr!(i32 0 12:12)))
    }

    //                                           0        1         
    //                                           12345678901234567890
    case!{ "var hij = [1, 3, 5];" as VarDeclStatement,
        make_stmt!(var 0:19 #2 4:6
            None,
            Some(make_expr!(array 10:18
                make_expr!(i32 1 11:11),
                make_expr!(i32 3 14:14),
                make_expr!(i32 5 17:17))))
    }
    
    //       1234567890123456789
    case!{ "const input: string;" as VarDeclStatement,
        make_stmt!(const 0:19 #2 6:10
            Some(make_type!(simple 13:18 #3)),
            None)
    }
    
    //      0         1         2
    //      012345678901234567890123
    case!{ "var buf: [(u8, char);1];" as VarDeclStatement,
        make_stmt!(var 0:23 #2 4:6
            Some(make_type!(array 9:22
                make_type!(tuple 10:19 [
                    make_type!(prim 11:12 U8),
                    make_type!(prim 15:18 Char),
                ]),
                make_expr!(i32 1 21:21).into())),
            None)
    }

    // Future Attention: after bits type added, the `0x7u8` will have different type as before, this is the Option::unwrap failure in test
    // and after advanced type infer, change the 0x7u8 to 7, and try to infer it as 7u8, which requires 2 major changes
    //     do not infer i32 in num lit if no postfix provided
    //     desugar array primary expr to call array_tid::new() and array.push, which infer 7's type as array_tid' push method's parameter
    //      0         1         2         3         4
    //      01234567890123456789012345678901234567890123456789
    case!{ "var buf: ([u8;3], u32) = ([1u8, 5u8, 0x7u8], abc);" as VarDeclStatement,
        make_stmt!(var 0:49 #2 4:6
            Some(make_type!(tuple 9:21 [
                make_type!(array 10:15 
                    make_type!(prim 11:12 U8), 
                    make_expr!(i32 3 14:14).into()), 
                make_type!(prim 18:20 U32)])),
            Some(make_expr!(tuple 25:48
                make_expr!(array 26:42
                    make_expr!(u8 1 27:29),
                    make_expr!(u8 5 32:34),
                    make_expr!(u8 7 37:41)),
                make_name!(simple 45:47 #3)))
        ), strings ["buf", "abc"]
    }

    case!{ "var a;" as VarDeclStatement,
        make_stmt!(var 0:5 #2 4:4 None, None), errors make_errors!(
            e: e.emit("require type annotation")
                .detail(Span::new(4, 4), "variable declaration here")
                .help("cannot infer type without both type annotation and initialization expression")),
    }
}
