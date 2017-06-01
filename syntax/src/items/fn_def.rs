///! fff-lang
///!
///! syntax/fn_def
///! FnDef = fFn fIdentifier fLeftParen [Identifier fColon TypeUse [fComma Identifier fColon TypeUse]* [fComma] ] fRightParen [fNarrowRightArrow Type] Block

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
use super::super::TypeUse;
use super::super::Block;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct FnParam {
    decltype: TypeUse,
    name: String,
    name_strpos: StringPosition,
}
impl FnParam {

    pub fn new(name: String, name_strpos: StringPosition, decltype: TypeUse) -> FnParam {
        FnParam{ decltype, name, name_strpos }
    }
    pub fn get_decltype(&self) -> &TypeUse { &self.decltype }
    pub fn get_name(&self) -> &String { &self.name }
    pub fn get_name_strpos(&self) -> StringPosition { self.name_strpos }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct FnDef {
    name: String,
    name_strpos: StringPosition,
    params: Vec<FnParam>,
    params_paren_strpos: StringPosition,
    ret_type: Option<TypeUse>,
    body: Block,
    all_strpos: StringPosition,
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

    pub fn new(all_strpos: StringPosition, 
        name: String, name_strpos: StringPosition,
        params_paren_strpos: StringPosition, params: Vec<FnParam>, 
        ret_type: Option<TypeUse>, body: Block) -> FnDef {
        FnDef{ name, name_strpos, params, params_paren_strpos, ret_type, body, all_strpos }
    }

    pub fn get_name(&self) -> &String { &self.name }
    pub fn get_name_strpos(&self) -> StringPosition { self.name_strpos }
    pub fn get_params(&self) -> &Vec<FnParam> { &self.params }
    pub fn get_ret_type(&self) -> Option<&TypeUse> { self.ret_type.as_ref() }
    pub fn get_body(&self) -> &Block { &self.body }
    pub fn get_all_strpos(&self) -> StringPosition { self.all_strpos }
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
                    params_paren_strpos = StringPosition::merge(params_paren_strpos, *right_paren_strpos);
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
                    params_paren_strpos = StringPosition::merge(params_paren_strpos, *right_paren_strpos);
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

        let all_strpos = StringPosition::merge(fn_strpos, body.get_all_strpos());
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
        FnDef::new(make_strpos!(1, 1, 1, 12),
            "main".to_owned(), make_strpos!(1, 4, 1, 7), 
            make_strpos!(1, 8, 1, 9), vec![], 
            None,
            Block::new(make_strpos!(1, 11, 1, 12), vec![])
        )
    }
    
    //                                0        1
    //                                1234567890123456789
    assert_eq!{ FnDef::with_test_str("fn main(ac: i32) {}"),
        FnDef::new(make_strpos!(1, 1, 1, 19), 
            "main".to_owned(), make_strpos!(1, 4, 1, 7), 
            make_strpos!(1, 8, 1, 16), vec![
                FnParam::new(
                    "ac".to_owned(), make_strpos!(1, 9, 1, 10),
                    TypeUseF::new_simple_test("i32", make_strpos!(1, 13, 1, 15))
                ),
            ],
            None,
            Block::new(make_strpos!(1, 18, 1, 19), vec![])
        )
    }

    //                                0        1         2         3         4         5         6         7         8
    //                                123456789012345678901234567890123456789012345678901234567890123456789012345678901
    assert_eq!{ FnDef::with_test_str(" fn mainxxx(argv:[[string] ]   ,this:i32, some_other: char, )  { println(this); }"), 
        FnDef::new(make_strpos!(1, 2, 1, 81),
            "mainxxx".to_owned(), make_strpos!(1, 5, 1, 11), 
            make_strpos!(1, 12, 1, 61), vec![
                FnParam::new(
                    "argv".to_owned(), make_strpos!(1, 13, 1, 16),
                    TypeUseF::new_array(make_strpos!(1, 18, 1, 28), 
                        TypeUseF::new_array(make_strpos!(1, 19, 1, 26), 
                            TypeUseF::new_simple_test("string", make_strpos!(1, 20, 1, 25))
                        )
                    )
                ),
                FnParam::new(
                    "this".to_owned(), make_strpos!(1, 33, 1, 36),
                    TypeUseF::new_simple_test("i32", make_strpos!(1, 38, 1, 40))
                ),
                FnParam::new(
                    "some_other".to_owned(), make_strpos!(1, 43, 1, 52),
                    TypeUseF::new_simple("char".to_owned(), make_strpos!(1, 55, 1, 58))
                )
            ],
            None,
            Block::new(make_strpos!(1, 64, 1, 81), vec![
                Statement::Expr(ExprStatement::new_simple(make_strpos!(1, 66, 1, 79),
                    BinaryExpr::new_postfix(PostfixExpr::new_function_call(
                        PostfixExpr::new_primary(PrimaryExpr::new_ident("println".to_owned(), make_strpos!(1, 66, 1, 72))),
                        make_strpos!(1, 73, 1, 78), vec![
                            BinaryExpr::new_ident("this".to_owned(), make_strpos!(1, 74, 1, 77))
                        ]
                    ))
                ))
            ])
        )
    }
    //                                0        1               
    //                                1234567890123456789
    assert_eq!{ FnDef::with_test_str("fn main() -> i32 {}"),
        FnDef::new(make_strpos!(1, 1, 1, 19),
            "main".to_owned(), make_strpos!(1, 4, 1, 7), 
            make_strpos!(1, 8, 1, 9), vec![],
            Some(TypeUseF::new_simple_test("i32", make_strpos!(1, 14, 1, 16))),
            Block::new(make_strpos!(1, 18, 1, 19), vec![])
        )
    }
    //                                0        1         2         3         4         5         6
    //                                12345678901234567890123456789012345678901234567890123456789012345678
    assert_eq!{ FnDef::with_test_str("fn ffff(argc: i32, argv: [string], envv: [string],) -> [[string]] {}"),
        FnDef::new(make_strpos!(1, 1, 1, 68),
            "ffff".to_owned(), make_strpos!(1, 4, 1, 7), 
            make_strpos!(1, 8, 1, 51), vec![
                FnParam::new(
                    "argc".to_owned(), make_strpos!(1, 9, 1, 12),
                    TypeUseF::new_simple("i32".to_owned(), make_strpos!(1, 15, 1, 17))
                ),
                FnParam::new(
                    "argv".to_owned(), make_strpos!(1, 20, 1, 23),
                    TypeUseF::new_array(make_strpos!(1, 26, 1, 33), 
                        TypeUseF::new_simple("string".to_owned(), make_strpos!(1, 27, 1, 32))
                    )
                ),
                FnParam::new(
                    "envv".to_owned(), make_strpos!(1, 36, 1, 39),
                    TypeUseF::new_array(make_strpos!(1, 42, 1, 49), 
                        TypeUseF::new_simple("string".to_owned(), make_strpos!(1, 43, 1, 48))
                    )
                )
            ],
            Some(TypeUseF::new_array(make_strpos!(1, 56, 1, 65), 
                TypeUseF::new_array(make_strpos!(1, 57, 1, 64), 
                    TypeUseF::new_simple("string".to_owned(), make_strpos!(1, 58, 1, 63))
                )
            )),
            Block::new(make_strpos!(1, 67, 1, 68), vec![])
        )
    }
}