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
            if let Some((right_paren_span, skipped_comma)) = cx.try_expect_closing_bracket(Separator::RightParen) {
                params_paren_span += right_paren_span;
                if skipped_comma && params.is_empty() {
                    cx.emit("Single comma in function definition argument list")
                        .detail(fn_name_span, "function definition here")
                        .detail(params_paren_span, "param list here");
                }
                break;
            } else if let Some(_comma_span) = cx.try_expect_sep(Separator::Comma) {
                continue;
            }

            let (param_name, param_span) = cx.expect_ident_or_keywords(&[Keyword::Underscore, Keyword::This, Keyword::Self_])?;
            let _ = cx.expect_sep(Separator::Colon)?;
            let decltype = cx.expect::<TypeRef>()?;
            params.push(FnParam::new(param_name, param_span, decltype));
        }

        let ret_type = cx.try_expect_seps(&[Separator::Arrow, Separator::Colon]).map(|(sep, span)| {
            if sep == Separator::Colon {
                cx.emit(strings::FunctionReturnTypeShouldUseArrow).detail(span, strings::FunctionReturnTypeExpectArrowMeetColon);
            }
            cx.try_expect::<TypeRef>()
        }).transpose()?.flatten();
        let body = cx.expect::<Block>()?;

        Ok(FnDef::new(fn_span + body.all_span, fn_name, fn_name_span, params_paren_span, params, ret_type, body))
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

#[cfg(test)]
#[test]
fn fn_def_parse() {
    use super::{Statement, SimpleExprStatement};

    //                                012345678901
    case!{ "fn main() {}" as FnDef,
        FnDef::new(Span::new(0, 11), 2, Span::new(3, 6), Span::new(7, 8), 
            vec![], 
            None,
            Block::new(Span::new(10, 11), vec![])
        )
    }

    //                      0        1
    //                      0123456789012345678
    case!{ "fn main(ac: i32) {}" as FnDef,
        FnDef::new(Span::new(0, 18), 2, Span::new(3, 6), Span::new(7, 15), 
            vec![
                FnParam::new(3, Span::new(8, 9), make_type!(prim 12:14 I32)),
            ],
            None,
            Block::new(Span::new(17, 18), vec![])
        ), strings ["main", "ac"]
    }

    //      0         1         2         3         4         5         6         7         8
    //      012345678901234567890123456789012345678901234567890123456789012345678901234567890
    case!{ " fn mainxxx(argv:&    string   ,this:i32, some_other: char, )  { println(this); }" as FnDef,
        FnDef::new(Span::new(1, 80), 2, Span::new(4, 10), Span::new(11, 60), 
            vec![
                FnParam::new(3, Span::new(12, 15), make_type!(ref 17:27 make_type!(simple 22:27 #4))),
                FnParam::new(5, Span::new(32, 35), make_type!(prim 37:39 I32)),
                FnParam::new(6, Span::new(42, 51), make_type!(prim 54:57 Char)),
            ],
            None,
            Block::new(Span::new(63, 80), vec![
                Statement::SimpleExpr(SimpleExprStatement::new(Span::new(65, 78),
                    make_expr!(fn 65:77 paren 72:77
                        make_name!(simple 65:71 #7),
                        make_name!(simple 73:76 #5))
                ))
            ]) //    2          3       4         5       6             7
        ), strings ["mainxxx", "argv", "string", "this", "some_other", "println"]
    }

    //                                0        1               
    //                                1234567890123456789
    case!{ "fn main() -> i32 {}" as FnDef,
        FnDef::new(Span::new(0, 18),
            2, Span::new(3, 6), 
            Span::new(7, 8), vec![],
            Some(make_type!(prim 13:15 I32)),
            Block::new(Span::new(17, 18), vec![])
        )
    }
    //      0         1         2         3         4         5         6
    //      0123456789012345678901234567890123456789012345678901234567890
    case!{ "fn ffff(argc: i32, argv: &&byte,   envv:  &string,) -> i32 {}" as FnDef,
        FnDef::new(Span::new(0, 60), 2, Span::new(3, 6), Span::new(7, 50), vec![
            FnParam::new(3, Span::new(8, 11), make_type!(prim 14:16 I32)),
            FnParam::new(4, Span::new(19, 22), make_type!(ref 25:30 make_type!(ref 26:30 make_type!(simple 27:30 #5)))),
            FnParam::new(6, Span::new(35, 38), make_type!(ref 42:48 make_type!(simple 43:48 #7))),
        ], 
            Some(make_type!(prim 55:57 I32)), 
            Block::new(Span::new(59, 60), vec![])
            //       2       3       4       5      6         7
        ), strings ["ffff", "argc", "argv", "byte", "envv", "string"]
    }

    //      0         1         2         3         4         5         6         7         8         9         0         1         2
    //      01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234
    case!{ "fn impl_visit(this: &This, node: &Node, title: string, span: Span, then: fn() -> Result<&This, fmt::Error>) -> fmt::Result {}" as FnDef,
        FnDef::new(Span::new(0, 124), 2, Span::new(3, 12), Span::new(13, 106), vec![
            FnParam::new(3, Span::new(14, 17), make_type!(ref 20:24 make_type!(simple 21:24 #4))),
            FnParam::new(5, Span::new(27, 30), make_type!(ref 33:37 make_type!(simple 34:37 #6))),
            FnParam::new(7, Span::new(40, 44), make_type!(simple 47:52 #8)),
            FnParam::new(9, Span::new(55, 58), make_type!(simple 61:64 #10)),
            FnParam::new(11, Span::new(67, 70), make_type!(fn ret 73:105 paren 75:76 [],
                make_type!(plain 81:105 false, None,
                    make_type!(segment generic 81:105 #12 81:86 quote 87:105
                        make_type!(ref 88:92 make_type!(simple 89:92 #4)),
                        make_type!(plain 95:104 false, None,
                            make_type!(segment 95:97 #13),
                            make_type!(segment 100:104 #14)))))),
        ],
            Some(make_type!(plain 111:121 false, None, 
                make_type!(segment 111:113 #13),
                make_type!(segment 116:121 #12))),
            Block::new(Span::new(123, 124), vec![]),
        //           2             3       4       5       6       7        8         9       10      11      12        13     14
        ), strings ["impl_visit", "this", "This", "node", "Node", "title", "string", "span", "Span", "then", "Result", "fmt", "Error"]
    }
}
