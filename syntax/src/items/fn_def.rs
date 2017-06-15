///! fff-lang
///!
///! syntax/fn_def
///! FnDef = fFn fIdentifier fLeftParen [Identifier fColon TypeUse [fComma Identifier fColon TypeUse]* [fComma] ] fRightParen [fNarrowRightArrow Type] Block

use std::fmt;

use codemap::Span;
use codemap::SymbolID;
use message::Message;
use lexical::Token;
use lexical::KeywordKind;
use lexical::SeperatorKind;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;
use super::super::TypeUse;
use super::super::Block;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct FnParam {
    pub name: SymbolID,
    pub name_span: Span,
    pub decltype: TypeUse,
}
impl FnParam {
    pub fn new(name: SymbolID, name_span: Span, decltype: TypeUse) -> FnParam { FnParam{ decltype, name, name_span } }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct FnDef {
    pub name: SymbolID,
    pub name_span: Span,
    pub params: Vec<FnParam>,
    pub params_paren_span: Span,
    pub ret_type: Option<TypeUse>,
    pub body: Block,
    pub all_span: Span,   // fn_span = all_span.slice(0..2)
}
impl ISyntaxItemFormat for FnDef {
    fn format(&self, indent: u32) -> String {

        let mut retval = String::new();
        retval.push_str(&format!("{}FnDef <{:?}>", FnDef::indent_str(indent), self.all_span));
        retval.push_str(&format!("\n{}Name {:?} <{:?}>", FnDef::indent_str(indent + 1), self.name, self.name_span));

        match self.ret_type { 
            Some(ref ret_type) => retval.push_str(&format!("\n{}", ret_type.format(indent + 1))),
            None => (), 
        }
        retval.push_str(&format!("\n{}Params <{:?}>", FnDef::indent_str(indent + 1), self.params_paren_span));
        for &FnParam{ ref decltype, ref name, ref name_span } in &self.params {
            retval.push_str(&format!("\n{}Param {:?} <{:?}>\n{}", FnDef::indent_str(indent + 1), name, name_span, decltype.format(indent + 2)));
        }
        retval.push_str(&format!("\n{}", self.body.format(indent + 1)));
        return retval;
    }
}
impl fmt::Debug for FnDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl FnDef {

    pub fn new(all_span: Span, 
        name: SymbolID, name_span: Span,
        params_paren_span: Span, params: Vec<FnParam>, 
        ret_type: Option<TypeUse>, body: Block) -> FnDef {
        FnDef{ name, name_span, params, params_paren_span, ret_type, body, all_span }
    }
}
impl ISyntaxItemGrammar for FnDef {   
    fn is_first_final(sess: &ParseSession) -> bool { sess.tk == &Token::Keyword(KeywordKind::FnDef) }
}
impl ISyntaxItemParse for FnDef {
    type Target = FnDef;

    fn parse(sess: &mut ParseSession) -> ParseResult<FnDef> {
        
        let fn_strpos = sess.expect_keyword(KeywordKind::FnDef)?;
        let (fn_name, fn_name_strpos) = sess.expect_ident()?;
        let mut params_paren_strpos = sess.expect_sep(SeperatorKind::LeftParenthenes)?;

        let mut params = Vec::new();
        loop {
            match (sess.tk, sess.pos, sess.next_tk, sess.next_pos) {
                (&Token::Sep(SeperatorKind::Comma), _,
                    &Token::Sep(SeperatorKind::RightParenthenes), ref right_paren_strpos) => {
                    sess.move_next2();
                    params_paren_strpos = params_paren_strpos.merge(right_paren_strpos);
                    if params.is_empty() {
                        sess.push_message(Message::new_by_str("Single comma in function definition argument list", vec![
                            (fn_name_strpos, "function definition here"),
                            (params_paren_strpos, "param list here")
                        ]));
                    }
                    break;
                }
                (&Token::Sep(SeperatorKind::RightParenthenes), ref right_paren_strpos, _, _) => {
                    sess.move_next();
                    params_paren_strpos = params_paren_strpos.merge(right_paren_strpos);
                    break;
                }
                (&Token::Sep(SeperatorKind::Comma), _, _, _) => {
                    sess.move_next();
                    continue;
                }
                _ => (),
            }

            let (param_name, param_strpos) = sess.expect_ident_or(vec![KeywordKind::Underscore, KeywordKind::This])?;
            let _ = sess.expect_sep(SeperatorKind::Colon)?;
            let decltype = TypeUse::parse(sess)?;
            params.push(FnParam::new(param_name, param_strpos, decltype));
        }

        let maybe_ret_type = if sess.tk == &Token::Sep(SeperatorKind::NarrowRightArrow) { sess.move_next(); Some(TypeUse::parse(sess)?) } else { None };
        let body = Block::parse(sess)?;

        let all_strpos = fn_strpos.merge(&body.all_span);
        return Ok(FnDef::new(all_strpos, fn_name, fn_name_strpos, params_paren_strpos, params, maybe_ret_type, body));
    }
}

#[cfg(test)] #[test]
fn fn_def_parse() {
    use codemap::SymbolCollection;
    use super::super::IdentExpr;
    use super::super::ISyntaxItemWithStr;
    use super::super::TypeUse;
    use super::super::Statement;
    use super::super::ExprStatement;
    use super::super::BinaryExpr;
    use super::super::PostfixExpr;
    use super::super::PrimaryExpr;

    //                                012345678901
    assert_eq!{ FnDef::with_test_str("fn main() {}"),
        FnDef::new(make_span!(0, 11),
            make_id!(1), make_span!(3, 6), 
            make_span!(7, 8), vec![], 
            None,
            Block::new(make_span!(10, 11), vec![])
        )
    }
    
    //                                  0        1
    //                                  0123456789012345678
    assert_eq!{ FnDef::with_test_input("fn main(ac: i32) {}", &mut make_symbols!["main", "ac", "i32"]),
        FnDef::new(make_span!(0, 18), 
            make_id!(1), make_span!(3, 6), 
            make_span!(7, 15), vec![
                FnParam::new(
                    make_id!(2), make_span!(8, 9),
                    TypeUse::new_simple(make_id!(3), make_span!(12, 14))
                ),
            ],
            None,
            Block::new(make_span!(17, 18), vec![])
        )
    }

    //                                  0        1         2         3         4         5         6         7         8
    //                                  012345678901234567890123456789012345678901234567890123456789012345678901234567890
    assert_eq!{ FnDef::with_test_input(" fn mainxxx(argv:[[string] ]   ,this:i32, some_other: char, )  { println(this); }", 
        //             1          2       3        4         5       6      7             8       9
        &mut make_symbols!["mainxxx", "argv", "array", "string", "this", "i32", "some_other", "char", "println"]), 
        FnDef::new(make_span!(1, 80),
            make_id!(1), make_span!(4, 10), 
            make_span!(11, 60), vec![
                FnParam::new(make_id!(2), make_span!(12, 15),
                    TypeUse::new_template(make_id!(3), Span::default(), make_span!(17, 27), vec![
                        TypeUse::new_template(make_id!(3), Span::default(), make_span!(18, 25), vec![
                            TypeUse::new_simple(make_id!(4), make_span!(19, 24))
                        ])
                    ])
                ),
                FnParam::new(make_id!(5), make_span!(32, 35),
                    TypeUse::new_simple(make_id!(6), make_span!(37, 39))
                ),
                FnParam::new(make_id!(7), make_span!(42, 51),
                    TypeUse::new_simple(make_id!(8), make_span!(54, 57))
                )
            ],
            None,
            Block::new(make_span!(63, 80), vec![
                Statement::Expr(ExprStatement::new_simple(make_span!(65, 78),
                    BinaryExpr::new_postfix(PostfixExpr::new_function_call(
                        PostfixExpr::new_primary(PrimaryExpr::Ident(IdentExpr::new(make_id!(9), make_span!(65, 71)))),
                        make_span!(72, 77), vec![
                            BinaryExpr::new_ident(IdentExpr::new(make_id!(5), make_span!(73, 76)))
                        ]
                    ))
                ))
            ])
        )
    }
    //                                0        1               
    //                                1234567890123456789
    assert_eq!{ FnDef::with_test_str("fn main() -> i32 {}"),
        FnDef::new(make_span!(0, 18),
            make_id!(1), make_span!(3, 6), 
            make_span!(7, 8), vec![],
            Some(TypeUse::new_simple(make_id!(2), make_span!(13, 15))),
            Block::new(make_span!(17, 18), vec![])
        )
    }
    //                                  0        1         2         3         4         5         6
    //                                  01234567890123456789012345678901234567890123456789012345678901234567
    assert_eq!{ FnDef::with_test_input("fn ffff(argc: i32, argv: [string], envv: [string],) -> [[string]] {}", 
        //             1       2       3      4       5        6         7
        &mut make_symbols!["ffff", "argc", "i32", "argv", "array", "string", "envv"]),
        FnDef::new(make_span!(0, 67),
            make_id!(1), make_span!(3, 6), 
            make_span!(7, 50), vec![
                FnParam::new(make_id!(2), make_span!(8, 11),
                    TypeUse::new_simple(make_id!(3), make_span!(14, 16))
                ),
                FnParam::new(make_id!(4), make_span!(19, 22),
                    TypeUse::new_template(make_id!(5), Span::default(), make_span!(25, 32), vec![
                        TypeUse::new_simple(make_id!(6), make_span!(26, 31))
                    ])
                ),
                FnParam::new(make_id!(7), make_span!(35, 38),
                    TypeUse::new_template(make_id!(5), Span::default(), make_span!(41, 48), vec![
                        TypeUse::new_simple(make_id!(6), make_span!(42, 47))
                    ])
                )
            ],
            Some(TypeUse::new_template(make_id!(5), Span::default(), make_span!(55, 64), vec![   
                TypeUse::new_template(make_id!(5), Span::default(), make_span!(56, 63), vec![
                    TypeUse::new_simple(make_id!(6), make_span!(57, 62))
                ])
            ])),
            Block::new(make_span!(66, 67), vec![])
        )
    }
}