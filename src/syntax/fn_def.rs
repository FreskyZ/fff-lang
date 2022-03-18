///! fff-lang
///!
///! syntax/fn_def
///! fn-def = 'fn' identifier '(' [ identifier ':' type-use { ',' identifier ':' type-use [ ',' ] } ] ')' [ '->' type-use ] block

use super::prelude::*;
use super::{Block, TypeRef};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct FnParam {
    pub name: IsId,
    pub name_span: Span,
    pub r#type: TypeRef,
}
impl FnParam {
    pub fn new(name: impl Into<IsId>, name_span: Span, r#type: TypeRef) -> Self { 
        Self{ r#type, name: name.into(), name_span } 
    }
}
impl Node for FnParam {
    
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_fn_param(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_type_ref(&self.r#type)
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct FnDef {
    pub name: IsId,
    pub name_span: Span,
    pub params: Vec<FnParam>,
    pub params_paren_span: Span,
    pub ret_type: Option<TypeRef>,
    pub body: Block,
    pub all_span: Span,   // fn_span = all_span.slice(0..2)
}

impl FnDef {

    pub fn new(all_span: Span, 
        name: impl Into<IsId>, name_span: Span,
        params_paren_span: Span, params: Vec<FnParam>, 
        ret_type: Option<TypeRef>, body: Block) -> FnDef {
        FnDef{ name: name.into(), name_span, params, params_paren_span, ret_type, body, all_span }
    }
}

impl Parser for FnDef {
    type Output = FnDef;

    fn matches(current: &Token) -> bool {
        matches!(current, Token::Keyword(Keyword::Fn))
    }

    fn parse(cx: &mut ParseContext) -> Result<FnDef, Unexpected> {
        #[cfg(feature = "trace_fn_def_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[FnDef: {}]", line!()); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_fn_def_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }
        
        let fn_span = cx.expect_keyword(Keyword::Fn)?;
        let (fn_name, fn_name_span) = cx.expect_ident()?;
        let mut params_paren_span = cx.expect_sep(Separator::LeftParen)?;
        trace!("fndef name span: {:?}", fn_name_span);

        let mut params = Vec::new();
        loop {
            if let Some((_comma_span, right_paren_span)) = cx.try_expect_2_sep(Separator::Comma, Separator::RightParen) {
                params_paren_span = params_paren_span + right_paren_span;
                if params.is_empty() {
                    cx.emit("Single comma in function definition argument list")
                        .detail(fn_name_span, "function definition here")
                        .detail(params_paren_span, "param list here");
                }
                break;
            } else if let Some(right_paren_span) = cx.try_expect_sep(Separator::RightParen) {
                params_paren_span = params_paren_span + right_paren_span;
                break;
            } else if let Some(_comma_span) = cx.try_expect_sep(Separator::Comma) {
                continue;
            }

            let (param_name, param_span) = cx.expect_ident_or(&[Keyword::Underscore, Keyword::This])?;
            let _ = cx.expect_sep(Separator::Colon)?;
            let decltype = cx.expect::<TypeRef>()?;
            params.push(FnParam::new(param_name, param_span, decltype));
        }

        let maybe_ret_type = if let Some(_right_arrow_span) = cx.try_expect_sep(Separator::Arrow) { Some(cx.expect::<TypeRef>()?) } else { None };
        let body = cx.expect::<Block>()?;

        return Ok(FnDef::new(fn_span + body.all_span, fn_name, fn_name_span, params_paren_span, params, maybe_ret_type, body));
    }
}

impl Node for FnDef {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_fn_def(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        for param in &self.params {
            v.visit_fn_param(param)?;
        }
        if let Some(ret_type) = &self.ret_type {
            v.visit_type_ref(ret_type)?;
        }
        v.visit_block(&self.body)
    }
}

#[cfg(test)] #[test]
fn fn_def_parse() {
    use super::{SimpleName, Statement, SimpleExprStatement, Expr, FnCallExpr};

    //                                012345678901
    case!{ "fn main() {}" as FnDef,
        FnDef::new(Span::new(0, 11),
            2, Span::new(3, 6), 
            Span::new(7, 8), vec![], 
            None,
            Block::new(Span::new(10, 11), vec![])
        )
    }

    //                      0        1
    //                      0123456789012345678
    case!{ "fn main(ac: i32) {}" as FnDef,
        FnDef::new(Span::new(0, 18), 
            2, Span::new(3, 6), 
            Span::new(7, 15), vec![
                FnParam::new(
                    3, Span::new(8, 9),
                    TypeRef::new_simple(4, Span::new(12, 14))
                ),
            ],
            None,
            Block::new(Span::new(17, 18), vec![])
        ), strings ["main", "ac", "i32"]
    }

    //                      0         1         2         3         4         5         6         7         8
    //                      012345678901234567890123456789012345678901234567890123456789012345678901234567890
    case!{ " fn mainxxx(argv:[[string] ]   ,this:i32, some_other: char, )  { println(this); }" as FnDef,
        FnDef::new(Span::new(1, 80),
            2, Span::new(4, 10),
            Span::new(11, 60), vec![
                FnParam::new(3, Span::new(12, 15),
                    TypeRef::new_template(5, Span::new(0, 0), Span::new(17, 27), vec![
                        TypeRef::new_template(5, Span::new(0, 0), Span::new(18, 25), vec![
                            TypeRef::new_simple(4, Span::new(19, 24))
                        ])
                    ])
                ),
                FnParam::new(6, Span::new(32, 35),
                    TypeRef::new_simple(8, Span::new(37, 39))
                ),
                FnParam::new(7, Span::new(42, 51),
                    TypeRef::new_simple(9, Span::new(54, 57))
                )
            ],
            None,
            Block::new(Span::new(63, 80), vec![
                Statement::SimpleExpr(SimpleExprStatement::new(Span::new(65, 78),
                    FnCallExpr::new(
                        Expr::SimpleName(SimpleName::new(10, Span::new(65, 71))),
                        Span::new(72, 77), make_exprs![
                            SimpleName::new(6, Span::new(73, 76))
                        ]
                    )
                ))
            ])
        ), strings ["mainxxx", "argv", "string", "array", "this", "some_other", "i32", "char", "println"]
    }

    //                                0        1               
    //                                1234567890123456789
    case!{ "fn main() -> i32 {}" as FnDef,
        FnDef::new(Span::new(0, 18),
            2, Span::new(3, 6), 
            Span::new(7, 8), vec![],
            Some(TypeRef::new_simple(3, Span::new(13, 15))),
            Block::new(Span::new(17, 18), vec![])
        )
    }
    //      0        1         2         3         4         5         6
    //      01234567890123456789012345678901234567890123456789012345678901234567
    case!{ "fn ffff(argc: i32, argv: [string], envv: [string],) -> [[string]] {}" as FnDef,
        FnDef::new(Span::new(0, 67), 2, Span::new(3, 6), Span::new(7, 50), vec![
            FnParam::new(3, Span::new(8, 11),
                TypeRef::new_simple(5, Span::new(14, 16))
            ),
            FnParam::new(4, Span::new(19, 22),
                TypeRef::new_template(8, Span::new(0, 0), Span::new(25, 32), vec![
                    TypeRef::new_simple(6, Span::new(26, 31))
                ])
            ),
            FnParam::new(7, Span::new(35, 38),
                TypeRef::new_template(8, Span::new(0, 0), Span::new(41, 48), vec![
                    TypeRef::new_simple(6, Span::new(42, 47))
                ])
            )
        ], Some(TypeRef::new_template(8, Span::new(0, 0), Span::new(55, 64), vec![   
            TypeRef::new_template(8, Span::new(0, 0), Span::new(56, 63), vec![
                TypeRef::new_simple(6, Span::new(57, 62))
            ])
        ])), Block::new(Span::new(66, 67), vec![])
            //       2       3       4       5      6         7       8
        ), strings ["ffff", "argc", "argv", "i32", "string", "envv", "array"]
    }
}
