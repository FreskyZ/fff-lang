
// Type = PrimitiveType | LeftBracket PrimitiveType RightBracket 

use common::From2;
use common::StringPosition;
use message::Message;

use lexical::Lexer;
use lexical::IToken;
use lexical::KeywordKind;
use lexical::SeperatorKind;

use syntax::ast_item::IASTItem;

#[derive(Debug, Eq, PartialEq)]
pub enum SMTypeBase {
    Dummy, // None for syntax parser, means some error happened
    Unit,
    U8,
    I32,
    U32,
    U64,
    F32,
    F64,
    Char,
    SMString,
    Array(Box<SMType>),
}

#[derive(Debug, Eq, PartialEq)]
pub struct SMType(pub SMTypeBase, pub StringPosition);

fn check_primitive_type(keyword: &KeywordKind) -> Option<SMTypeBase> {
    use self::SMTypeBase::*;
    match *keyword {
        KeywordKind::PrimTypeU8 => Some(U8),
        KeywordKind::PrimTypeI32 => Some(I32),
        KeywordKind::PrimTypeU32 => Some(U32),
        KeywordKind::PrimTypeU64 => Some(U64),
        KeywordKind::PrimTypeF32 => Some(F32),
        KeywordKind::PrimTypeF64 => Some(F64),
        KeywordKind::PrimTypeChar => Some(Char),
        KeywordKind::PrimTypeString => Some(SMString),
        _ => None
    }
}

impl Default for SMType {
    
    fn default() -> SMType {
        SMType(SMTypeBase::Dummy, StringPosition::new())
    }
}

impl SMType {

    pub fn make_array(ty: SMType, pos: StringPosition) -> SMType {
        SMType(SMTypeBase::Array(Box::new(ty)), pos)
    }

    pub fn pos_all(&self) -> StringPosition { self.1 }
}

impl IASTItem for SMType {

    
    fn parse(lexer: &mut Lexer, index: usize) -> (Option<SMType>, usize) {

        if let Some(kind) = lexer.nth(index).get_keyword() { 
            match check_primitive_type(kind) {
                Some(prim) => return (Some(SMType(prim, lexer.pos(index))), 1),
                None => return lexer.push_expect("primitive type keyword", index, 0),
            }
        }

        if lexer.nth(index).is_seperator(SeperatorKind::LeftBracket) {
            match SMType::parse(lexer, index + 1) {
                (None, length) => { // TODO: recover by find paired right bracket
                    return lexer.push_expect("some typedef", index + length, length);
                }  
                (Some(inner), inner_length) => {
                    if lexer.nth(index + 1 + inner_length).is_seperator(SeperatorKind::RightBracket) {
                        return (
                            Some(SMType::make_array(
                                inner, 
                                StringPosition::from2(lexer.pos(index).start_pos, lexer.pos(index + 1 +inner_length).end_pos)
                            )), 
                            inner_length + 2
                        );
                    } else {
                        return lexer.push_expect("right bracket", index + 1 + inner_length, inner_length + 1);
                    }
                } 
            }
        }

        return lexer.push_expects(vec!["left bracket", "primitive type keyword"], index, 0);
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn ast_smtype_parse() {
        use message::MessageEmitter;
        use lexical::Lexer;
        use syntax::ast_item::IASTItem;
        use super::SMTypeBase;
        use super::SMType;
        use common::StringPosition;

        macro_rules! test_case {
            ($program_slice: expr, $expect: expr, $len: expr) => ({

                let messages = MessageEmitter::new();
                let lexer = &mut Lexer::new_test($program_slice, messages);
                let result = SMType::parse(lexer, 0);
                assert_eq!(result, (Some($expect), $len));
            });
            ($program_slice: expr, $len: expr) => ({

                let messages = MessageEmitter::new();
                let lexer = &mut Lexer::new_test($program_slice, messages);
                let result = SMType::parse(lexer, 0);
                assert_eq!(result, (None, $len));
            })
        }

        test_case!("", 0);
        test_case!("u8", SMType(SMTypeBase::U8, make_str_pos!(1, 1, 1, 2)), 1);
        test_case!("u32", SMType(SMTypeBase::U32, make_str_pos!(1, 1, 1, 3)), 1);
        test_case!("[string]", SMType::make_array(SMType(SMTypeBase::SMString, make_str_pos!(1, 2, 1, 7)), make_str_pos!(1, 1, 1, 8)), 3);
        test_case!("[f32]", SMType::make_array(SMType(SMTypeBase::F32, make_str_pos!(1, 2, 1, 4)), make_str_pos!(1, 1, 1, 5)), 3);
        test_case!("[[f64]]", SMType::make_array(SMType::make_array(SMType(SMTypeBase::F64, make_str_pos!(1, 3, 1, 5)), make_str_pos!(1, 2, 1, 6)), make_str_pos!(1, 1, 1, 7)), 5);
        test_case!(
            "[[[string]]]", 
            SMType::make_array(
                SMType::make_array(
                    SMType::make_array(
                        SMType(SMTypeBase::SMString, make_str_pos!(1, 4, 1, 9)),
                        make_str_pos!(1, 3, 1, 10)
                    ),
                    make_str_pos!(1, 2, 1, 11)
                ),
                make_str_pos!(1, 1, 1, 12)
            ),
            7
        );
        test_case!("char", SMType(SMTypeBase::Char, make_str_pos!(1, 1, 1, 4)), 1);
        test_case!("helloworld", 0);
        test_case!("[asd]", 1);
        test_case!("\"helloworld\"", 0);
    }
}