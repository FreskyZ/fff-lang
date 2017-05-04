///! fff-lang
///!
///! syntax/fn_def
///! FnDef = fFn fIdentifier fLeftParen [Identifier fColon TypeUse [fComma Identifier fColon TypeUse]* [fComma] ] fRightParen [fNarrowRightArrow Type] Block

use std::fmt;

use codepos::StringPosition;
use message::Message;
use message::MessageCollection;

use lexical::Token;
use lexical::TokenStream;
use lexical::KeywordKind;
use lexical::SeperatorKind;

use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;
use super::super::TypeUse;
use super::super::Block;

#[cfg_attr(test, derive(Eq, PartialEq))]
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
        for &FnParam{ ref decltype, ref name, ref name_strpos } in &self.params {
            retval.push_str(&format!("\n{}Param '{}' <{:?}>\n{}", FnDef::indent_str(indent + 1), name, name_strpos, decltype.format(indent + 2)));
        }
        retval.push_str(&format!("\n{}", self.body.format(indent + 1)));
        return retval;
    }
}
impl fmt::Debug for FnDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(0)) }
}
impl FnDef {

    pub fn new(all_strpos: StringPosition, 
        name: String, name_strpos: StringPosition,
        params: Vec<FnParam>, ret_type: Option<TypeUse>, body: Block) -> FnDef {
        FnDef{ name, name_strpos, params, ret_type, body, all_strpos }
    }

    pub fn get_name(&self) -> &String { &self.name }
    pub fn get_name_strpos(&self) -> StringPosition { self.name_strpos }
    pub fn get_params(&self) -> &Vec<FnParam> { &self.params }
    pub fn get_ret_type(&self) -> Option<&TypeUse> { self.ret_type.as_ref() }
    pub fn get_body(&self) -> &Block { &self.body }
    pub fn get_all_strpos(&self) -> StringPosition { self.all_strpos }
}
impl ISyntaxItemGrammar for FnDef {   
    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool { tokens.nth(index) == &Token::Keyword(KeywordKind::FnDef) }
}
impl ISyntaxItemParse for FnDef {

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<FnDef>, usize) {
        assert!(tokens.nth(index) == &Token::Keyword(KeywordKind::FnDef));

        let mut current_length = 1;

        let (fn_name, fn_name_strpos) = match tokens.nth(index + current_length) {
            &Token::Ident(ref name) => (name.clone(), tokens.pos(index + current_length)),
            _ => return push_unexpect!(tokens, messages, "identifier", index + 1, 1),
        };
        current_length += 1;

        if tokens.nth(index + current_length) != &Token::Sep(SeperatorKind::LeftParenthenes) {
            return push_unexpect!(tokens, messages, "left parenthenes", index + 2, 2);
        }
        let param_list_left_paren_strpos = tokens.pos(index + current_length);
        current_length += 1;

        let mut params = Vec::new();
        loop {
            if tokens.nth(index + current_length) == &Token::Sep(SeperatorKind::RightParenthenes) {
                current_length += 1;
                break; 
            }
            if tokens.nth(index + current_length) == &Token::Sep(SeperatorKind::Comma)   // accept `fn main(i32 a,) {}`
                && tokens.nth(index + current_length + 1) == &Token::Sep(SeperatorKind::RightParenthenes) {
                if params.is_empty() {                // recoverable error on fn main(, ) {}
                    let params_strpos = StringPosition::merge(param_list_left_paren_strpos, tokens.pos(index + current_length - 1));
                    messages.push(Message::new_by_str("Single comma in function definition argument list", vec![
                        (fn_name_strpos, "function definition here"),
                        (params_strpos, "param list here")
                    ]));
                }   
                current_length += 2;
                break;
            }
            if tokens.nth(index + current_length) == &Token::Sep(SeperatorKind::Comma) {
                current_length += 1;
                continue;
            }
            
            let (param_name, param_strpos) = match tokens.nth(index + current_length) {
                &Token::Ident(ref param_name) => { current_length += 1; (param_name.clone(), tokens.pos(index + current_length - 1)) }
                &Token::Keyword(KeywordKind::This) => { current_length += 1; ("this".to_owned(), tokens.pos(index + current_length - 1)) }
                _ => return push_unexpect!(tokens, messages, ["identifier", "comma", "parenthenes",], index + current_length, current_length),
            };
            if tokens.nth(index + current_length) != &Token::Sep(SeperatorKind::Colon) {
                return push_unexpect!(tokens, messages, "colon", index + current_length, current_length);
            }
            current_length += 1;
            let decltype = match TypeUse::parse(tokens, messages, index + current_length) {
                (None, length) => return (None, current_length + length),
                (Some(decltype), decltype_length) => { current_length += decltype_length; decltype },
            };

            params.push(FnParam::new(param_name, param_strpos, decltype));
        }

        let maybe_ret_type = if tokens.nth(index + current_length) == &Token::Sep(SeperatorKind::NarrowRightArrow) {
            current_length += 1;
            match TypeUse::parse(tokens, messages, index + current_length) {
                (Some(ret_type), ret_type_len) => { current_length += ret_type_len; Some(ret_type) }
                // (None, _length) => { // other things expect Type, find next left brace to continue
                //     let _: (Option<i32>, _) = push_unexpect!(tokens, messages, "typedef", index + current_len, current_len);
                //     for i in (index + current_len)..tokens.len() {
                //         if tokens.nth(i) == &Token::Sep(SeperatorKind::LeftBrace) {
                //             current_length = i - index;
                //         }
                //     }
                // }
                (None, length) => return (None, current_length + length),
            }
        } else {
            None
        };

        let body = match Block::parse(tokens, messages, index + current_length) {
            (Some(block), block_length) => { current_length += block_length; block }
            (None, length) => return (None, current_length + length),
        };

        let all_strpos = StringPosition::merge(tokens.pos(index), tokens.pos(index + current_length));
        return (Some(FnDef::new(all_strpos, fn_name, fn_name_strpos, params, maybe_ret_type, body)), current_length);
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
            vec![],
            None,
            Block::new(make_strpos!(1, 11, 1, 12), vec![])
        )
    }
    
    //                                0        1
    //                                1234567890123456789
    assert_eq!{ FnDef::with_test_str("fn main(ac: i32) {}"),
        FnDef::new(make_strpos!(1, 1, 1, 19), 
            "main".to_owned(), make_strpos!(1, 4, 1, 7), vec![
                FnParam::new(
                    "abc".to_owned(), make_strpos!(1, 9, 1, 10),
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
        FnDef::new(make_strpos!(1, 1, 1, 81),
            "mainxxx".to_owned(), make_strpos!(1, 5, 1, 11), vec![
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
            vec![],
            Some(TypeUseF::new_simple_test("i32", make_strpos!(1, 14, 1, 16))),
            Block::new(make_strpos!(1, 18, 1, 19), vec![])
        )
    }
    //                                0        1         2         3         4         5         6
    //                                12345678901234567890123456789012345678901234567890123456789012345678
    assert_eq!{ FnDef::with_test_str("fn ffff(argc: i32, argv: [string], envv: [string],) -> [[string]] {}"),
        FnDef::new(make_strpos!(1, 1, 1, 68),
            "ffff".to_owned(), make_strpos!(1, 4, 1, 7), vec![
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