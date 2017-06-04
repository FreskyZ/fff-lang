///! fff-lang
///!
///! syntax/fn_def
///! FnDef = fFn fIdentifier fLeftParen [Identifier fColon TypeUse [fComma Identifier fColon TypeUse]* [fComma] ] fRightParen [fNarrowRightArrow Type] Block

use std::fmt;

use codemap::Span;
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
    decltype: TypeUse,
    name: String,
    name_strpos: Span,
}
impl FnParam {

    pub fn new(name: String, name_strpos: Span, decltype: TypeUse) -> FnParam {
        FnParam{ decltype, name, name_strpos }
    }
    pub fn get_decltype(&self) -> &TypeUse { &self.decltype }
    pub fn get_name(&self) -> &String { &self.name }
    pub fn get_name_strpos(&self) -> Span { self.name_strpos }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct FnDef {
    name: String,
    name_strpos: Span,
    params: Vec<FnParam>,
    params_paren_strpos: Span,
    ret_type: Option<TypeUse>,
    body: Block,
    all_strpos: Span,
}
impl ISyntaxItemFormat for FnDef {
    fn format(&self, indent: u32) -> String {

        let mut retval = String::new();
        retval.push_str(&format!("{}FnDef <{:?}>", FnDef::indent_str(indent), self.all_strpos));
        retval.push_str(&format!("\n{}Name '{}' <{:?}>", FnDef::indent_str(indent + 1), self.name, self.name_strpos));

        match self.ret_type { 
            Some(ref ret_type) => retval.push_str(&format!("\n{}", ret_type.format(indent + 1))),
            None => (), 
        }
        retval.push_str(&format!("\n{}Params <{:?}>", FnDef::indent_str(indent + 1), self.params_paren_strpos));
        for &FnParam{ ref decltype, ref name, ref name_strpos } in &self.params {
            retval.push_str(&format!("\n{}Param '{}' <{:?}>\n{}", FnDef::indent_str(indent + 1), name, name_strpos, decltype.format(indent + 2)));
        }
        retval.push_str(&format!("\n{}", self.body.format(indent + 1)));
        return retval;
    }
}
impl fmt::Debug for FnDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl FnDef {

    pub fn new(all_strpos: Span, 
        name: String, name_strpos: Span,
        params_paren_strpos: Span, params: Vec<FnParam>, 
        ret_type: Option<TypeUse>, body: Block) -> FnDef {
        FnDef{ name, name_strpos, params, params_paren_strpos, ret_type, body, all_strpos }
    }

    pub fn get_name(&self) -> &String { &self.name }
    pub fn get_name_strpos(&self) -> Span { self.name_strpos }
    pub fn get_params(&self) -> &Vec<FnParam> { &self.params }
    pub fn get_ret_type(&self) -> Option<&TypeUse> { self.ret_type.as_ref() }
    pub fn get_body(&self) -> &Block { &self.body }
    pub fn get_all_strpos(&self) -> Span { self.all_strpos }
}
impl ISyntaxItemGrammar for FnDef {   
    fn is_first_final(sess: &ParseSession) -> bool { sess.tk == &Token::Keyword(KeywordKind::FnDef) }
}
impl ISyntaxItemParse for FnDef {

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

        let all_strpos = fn_strpos.merge(&body.get_all_strpos());
        return Ok(FnDef::new(all_strpos, fn_name, fn_name_strpos, params_paren_strpos, params, maybe_ret_type, body));
    }
}

#[cfg(test)] #[test]
fn fn_def_parse() {
    use super::super::ISyntaxItemWithStr;
    use super::super::TypeUseF;
    use super::super::Statement;
    use super::super::ExprStatement;
    use super::super::BinaryExpr;
    use super::super::PostfixExpr;
    use super::super::PrimaryExpr;

    //                                123456789012
    assert_eq!{ FnDef::with_test_str("fn main() {}"),
        FnDef::new(make_span!(0, 11),
            "main".to_owned(), make_span!(3, 6), 
            make_span!(7, 8), vec![], 
            None,
            Block::new(make_span!(10, 11), vec![])
        )
    }
    
    //                                0        1
    //                                1234567890123456789
    assert_eq!{ FnDef::with_test_str("fn main(ac: i32) {}"),
        FnDef::new(make_span!(0, 18), 
            "main".to_owned(), make_span!(3, 6), 
            make_span!(7, 15), vec![
                FnParam::new(
                    "ac".to_owned(), make_span!(8, 9),
                    TypeUseF::new_simple_test("i32", make_span!(12, 14))
                ),
            ],
            None,
            Block::new(make_span!(17, 18), vec![])
        )
    }

    //                                0        1         2         3         4         5         6         7         8
    //                                123456789012345678901234567890123456789012345678901234567890123456789012345678901
    assert_eq!{ FnDef::with_test_str(" fn mainxxx(argv:[[string] ]   ,this:i32, some_other: char, )  { println(this); }"), 
        FnDef::new(make_span!(1, 80),
            "mainxxx".to_owned(), make_span!(4, 10), 
            make_span!(11, 60), vec![
                FnParam::new(
                    "argv".to_owned(), make_span!(12, 15),
                    TypeUseF::new_array(make_span!(17, 27), 
                        TypeUseF::new_array(make_span!(18, 25), 
                            TypeUseF::new_simple_test("string", make_span!(19, 24))
                        )
                    )
                ),
                FnParam::new(
                    "this".to_owned(), make_span!(32, 35),
                    TypeUseF::new_simple_test("i32", make_span!(37, 39))
                ),
                FnParam::new(
                    "some_other".to_owned(), make_span!(42, 51),
                    TypeUseF::new_simple("char".to_owned(), make_span!(54, 57))
                )
            ],
            None,
            Block::new(make_span!(63, 80), vec![
                Statement::Expr(ExprStatement::new_simple(make_span!(65, 78),
                    BinaryExpr::new_postfix(PostfixExpr::new_function_call(
                        PostfixExpr::new_primary(PrimaryExpr::new_ident("println".to_owned(), make_span!(65, 71))),
                        make_span!(72, 77), vec![
                            BinaryExpr::new_ident("this".to_owned(), make_span!(73, 76))
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
            "main".to_owned(), make_span!(3, 6), 
            make_span!(7, 8), vec![],
            Some(TypeUseF::new_simple_test("i32", make_span!(13, 15))),
            Block::new(make_span!(17, 18), vec![])
        )
    }
    //                                0        1         2         3         4         5         6
    //                                12345678901234567890123456789012345678901234567890123456789012345678
    assert_eq!{ FnDef::with_test_str("fn ffff(argc: i32, argv: [string], envv: [string],) -> [[string]] {}"),
        FnDef::new(make_span!(0, 67),
            "ffff".to_owned(), make_span!(3, 6), 
            make_span!(7, 50), vec![
                FnParam::new(
                    "argc".to_owned(), make_span!(8, 11),
                    TypeUseF::new_simple("i32".to_owned(), make_span!(14, 16))
                ),
                FnParam::new(
                    "argv".to_owned(), make_span!(19, 22),
                    TypeUseF::new_array(make_span!(25, 32), 
                        TypeUseF::new_simple("string".to_owned(), make_span!(26, 31))
                    )
                ),
                FnParam::new(
                    "envv".to_owned(), make_span!(35, 38),
                    TypeUseF::new_array(make_span!(41, 48), 
                        TypeUseF::new_simple("string".to_owned(), make_span!(42, 47))
                    )
                )
            ],
            Some(TypeUseF::new_array(make_span!(55, 64), 
                TypeUseF::new_array(make_span!(56, 63), 
                    TypeUseF::new_simple("string".to_owned(), make_span!(57, 62))
                )
            )),
            Block::new(make_span!(66, 67), vec![])
        )
    }
}