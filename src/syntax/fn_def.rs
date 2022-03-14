///! fff-lang
///!
///! syntax/fn_def
///! fn-def = 'fn' identifier '(' [ identifier ':' type-use { ',' identifier ':' type-use [ ',' ] } ] ')' [ '->' type-use ] block

use super::prelude::*;
use super::{Block, TypeUse};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct FnParam {
    pub name: IsId,
    pub name_span: Span,
    pub decltype: TypeUse,
}
impl FnParam {
    pub fn new(name: impl Into<IsId>, name_span: Span, decltype: TypeUse) -> FnParam { FnParam{ decltype, name: name.into(), name_span } }
}
impl Node for FnParam {
    type ParseOutput = FnParam;
    
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_fn_param(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_type_use(&self.decltype)
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct FnDef {
    pub name: IsId,
    pub name_span: Span,
    pub params: Vec<FnParam>,
    pub params_paren_span: Span,
    pub ret_type: Option<TypeUse>,
    pub body: Block,
    pub all_span: Span,   // fn_span = all_span.slice(0..2)
}
impl FnDef {

    pub fn new(all_span: Span, 
        name: impl Into<IsId>, name_span: Span,
        params_paren_span: Span, params: Vec<FnParam>, 
        ret_type: Option<TypeUse>, body: Block) -> FnDef {
        FnDef{ name: name.into(), name_span, params, params_paren_span, ret_type, body, all_span }
    }
}
impl Node for FnDef {
    type ParseOutput = FnDef;

    fn matches(current: &Token) -> bool {
        matches!(current, Token::Keyword(Keyword::Fn))
    }

    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<FnDef> {
        #[cfg(feature = "trace_fn_def_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[FnDef: {}]", line!()); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_fn_def_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }
        
        let fn_span = sess.expect_keyword(Keyword::Fn)?;
        let (fn_name, fn_name_span) = sess.expect_ident()?;
        let mut params_paren_span = sess.expect_sep(Separator::LeftParen)?;
        trace!("fndef name span: {:?}", fn_name_span);

        let mut params = Vec::new();
        loop {
            if let Some((_comma_span, right_paren_span)) = sess.try_expect_2_sep(Separator::Comma, Separator::RightParen) {
                params_paren_span = params_paren_span + right_paren_span;
                if params.is_empty() {
                    sess.emit("Single comma in function definition argument list")
                        .detail(fn_name_span, "function definition here")
                        .detail(params_paren_span, "param list here");
                }
                break;
            } else if let Some(right_paren_span) = sess.try_expect_sep(Separator::RightParen) {
                params_paren_span = params_paren_span + right_paren_span;
                break;
            } else if let Some(_comma_span) = sess.try_expect_sep(Separator::Comma) {
                continue;
            }

            let (param_name, param_span) = sess.expect_ident_or(&[Keyword::Underscore, Keyword::This])?;
            let _ = sess.expect_sep(Separator::Colon)?;
            let decltype = TypeUse::parse(sess)?;
            params.push(FnParam::new(param_name, param_span, decltype));
        }

        let maybe_ret_type = if let Some(_right_arrow_span) = sess.try_expect_sep(Separator::Arrow) { Some(TypeUse::parse(sess)?) } else { None };
        let body = Block::parse(sess)?;

        return Ok(FnDef::new(fn_span + body.all_span, fn_name, fn_name_span, params_paren_span, params, maybe_ret_type, body));
    }

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_fn_def(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        for param in &self.params {
            v.visit_fn_param(param)?;
        }
        if let Some(ret_type) = &self.ret_type {
            v.visit_type_use(ret_type)?;
        }
        v.visit_block(&self.body)
    }
}

#[cfg(test)] #[test]
fn fn_def_parse() {
    use super::{make_node, make_exprs, SimpleName, TypeUse, Statement, SimpleExprStatement, Expr, FnCallExpr};

    //                                012345678901
    assert_eq!{ make_node!("fn main() {}" as FnDef),
        FnDef::new(Span::new(0, 11),
            2, Span::new(3, 6), 
            Span::new(7, 8), vec![], 
            None,
            Block::new(Span::new(10, 11), vec![])
        )
    }

    //                      0        1
    //                      0123456789012345678
    assert_node_eq!{ make_node!("fn main(ac: i32) {}" as FnDef, [Span::new(3, 6), Span::new(8, 9)], ["i32"]),
        FnDef::new(Span::new(0, 18), 
            2, Span::new(3, 6), 
            Span::new(7, 15), vec![
                FnParam::new(
                    3, Span::new(8, 9),
                    TypeUse::new_simple(4, Span::new(12, 14))
                ),
            ],
            None,
            Block::new(Span::new(17, 18), vec![])
        )
    }

    //                      0         1         2         3         4         5         6         7         8
    //                      012345678901234567890123456789012345678901234567890123456789012345678901234567890
    assert_node_eq!{ make_node!(" fn mainxxx(argv:[[string] ]   ,this:i32, some_other: char, )  { println(this); }" as FnDef, 
        [Span::new(4, 10), Span::new(12, 15), Span::new(19, 24), Span::new(42, 51), Span::new(65, 71)], ["this", "i32"]),
        FnDef::new(Span::new(1, 80),
            2, Span::new(4, 10),
            Span::new(11, 60), vec![
                FnParam::new(3, Span::new(12, 15),
                    TypeUse::new_template(9, Span::new(0, 0), Span::new(17, 27), vec![
                        TypeUse::new_template(9, Span::new(0, 0), Span::new(18, 25), vec![
                            TypeUse::new_simple(4, Span::new(19, 24))
                        ])
                    ])
                ),
                FnParam::new(7, Span::new(32, 35),
                    TypeUse::new_simple(8, Span::new(37, 39))
                ),
                FnParam::new(5, Span::new(42, 51),
                    TypeUse::new_simple(10, Span::new(54, 57))
                )
            ],
            None,
            Block::new(Span::new(63, 80), vec![
                Statement::SimpleExpr(SimpleExprStatement::new(Span::new(65, 78),
                    FnCallExpr::new(
                        Expr::SimpleName(SimpleName::new(6, Span::new(65, 71))),
                        Span::new(72, 77), make_exprs![
                            SimpleName::new(7, Span::new(73, 76))
                        ]
                    )
                ))
            ])
        )
    }

    //                                0        1               
    //                                1234567890123456789
    assert_node_eq!{ make_node!("fn main() -> i32 {}" as FnDef),
        FnDef::new(Span::new(0, 18),
            2, Span::new(3, 6), 
            Span::new(7, 8), vec![],
            Some(TypeUse::new_simple(3, Span::new(13, 15))),
            Block::new(Span::new(17, 18), vec![])
        )
    }
    //                      0        1         2         3         4         5         6
    //                      01234567890123456789012345678901234567890123456789012345678901234567
    assert_node_eq!{ make_node!("fn ffff(argc: i32, argv: [string], envv: [string],) -> [[string]] {}" as FnDef, [],  ["ffff", "argc", "i32", "argv", "array", "string", "envv"]),
        FnDef::new(Span::new(0, 67),
            2, Span::new(3, 6), 
            Span::new(7, 50), vec![
                FnParam::new(3, Span::new(8, 11),
                    TypeUse::new_simple(4, Span::new(14, 16))
                ),
                FnParam::new(5, Span::new(19, 22),
                    TypeUse::new_template(6, Span::new(0, 0), Span::new(25, 32), vec![
                        TypeUse::new_simple(7, Span::new(26, 31))
                    ])
                ),
                FnParam::new(8, Span::new(35, 38),
                    TypeUse::new_template(6, Span::new(0, 0), Span::new(41, 48), vec![
                        TypeUse::new_simple(7, Span::new(42, 47))
                    ])
                )
            ],
            Some(TypeUse::new_template(6, Span::new(0, 0), Span::new(55, 64), vec![   
                TypeUse::new_template(6, Span::new(0, 0), Span::new(56, 63), vec![
                    TypeUse::new_simple(7, Span::new(57, 62))
                ])
            ])),
            Block::new(Span::new(66, 67), vec![])
        )
    }
}
