
// Type = PrimitiveType | LeftBracket PrimitiveType RightBracket 

use lexical::Lexer;
use lexical::IToken;
use lexical::KeywordKind;
use lexical::SeperatorKind;

use syntax::ast_item::IASTItem;

#[derive(Debug, Eq, PartialEq)]
pub enum SMType {
    Unit,
    U8,
    I32,
    U32,
    U64,
    F32,
    F64,
    Char,
    SMString,
    Array(Box<SMType>)
}

fn check_primitive_type(keyword: &KeywordKind) -> Option<SMType> {
    use self::SMType::*;
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

impl SMType {

    pub fn make_array(smt: SMType) -> SMType {
        SMType::Array(Box::new(smt))
    }
}

impl IASTItem for SMType {

    fn symbol_len(&self) -> usize {
        match *self {   
            SMType::Array(ref boxed) => boxed.as_ref().symbol_len() + 2,
            _ => 1,
        }
    }
    
    fn parse(lexer: &mut Lexer, index: usize) -> Option<SMType> {

        if let Some(kind) = lexer.nth(index).get_keyword() { 
            match check_primitive_type(kind) {
                Some(prim) => return Some(prim),
                None => return lexer.push_expect_symbol("primitive type keyword", index),
            }
        }

        if lexer.nth(index).is_seperator(SeperatorKind::LeftBracket) {
            match SMType::parse(lexer, index + 1) {
                Some(inner) => {
                    if lexer.nth(index + 1 + inner.symbol_len()).is_seperator(SeperatorKind::RightBracket) {
                        return Some(SMType::make_array(inner));
                    } else {
                        return lexer.push_expect_symbol("right bracket after typedef", index + 1 + inner.symbol_len());
                    }
                } 
                None => return lexer.push_expect_symbol("some typedef", index + 1), 
            }
        }

        return lexer.push_expect_symbol("type name or left bracket", index);
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn ast_smtype_parse() {
        use message::MessageEmitter;
        use lexical::Lexer;
        use syntax::ast_item::IASTItem;
        use super::SMType;

        macro_rules! test_case {
            ($program_slice: expr, $expect: expr, $len: expr) => ({

                let messages = MessageEmitter::new();
                let lexer = &mut Lexer::new_test($program_slice, messages);
                let result = SMType::parse(lexer, 0);
                assert_eq!(result,  Some($expect));
                assert_eq!(result.unwrap().symbol_len(), $len);
            });
            ($program_slice: expr) => ({

                let messages = MessageEmitter::new();
                let lexer = &mut Lexer::new_test($program_slice, messages);
                let result = SMType::parse(lexer, 0);
                assert_eq!(result, None);
            })
        }

        test_case!("");
        test_case!("u8", SMType::U8, 1);
        test_case!("u32", SMType::U32, 1);
        test_case!("[string]", SMType::make_array(SMType::SMString), 3);
        test_case!("[f32]", SMType::make_array(SMType::F32), 3);
        test_case!("[[f64]]", SMType::make_array(SMType::make_array(SMType::F64)), 5);
        test_case!("[[[string]]]", SMType::make_array(SMType::make_array(SMType::make_array(SMType::SMString))), 7);
        test_case!("char", SMType::Char, 1);
        test_case!("helloworld");
        test_case!("[asd]");
        test_case!("\"helloworld\"");
    }
}