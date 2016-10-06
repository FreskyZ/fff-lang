
// Type -> PrimitiveType | LeftBracket PrimitiveType RightBracket 

use common::Position;
use message::Message;
use message::MessageEmitter;
use lexical::Lexer;
use lexical::Token;
use lexical::Keyword;
use lexical::KeywordKind;
use lexical::Seperator;
use lexical::SeperatorKind;
use syntax::ast_item::ASTItem;
use syntax::ast_item::ASTParser;

#[derive(Debug, Eq, PartialEq)]
pub enum PrimitiveType {
    Unit,
    U8,
    I32,
    U32,
    U64,
    F32,
    F64,
    Char,
    SMString,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    Array(PrimitiveType),       // currently only one level supported
}

fn check_primitive_type(keyword: &KeywordKind) -> Option<PrimitiveType> {
    use self::PrimitiveType::*;
    use lexical::KeywordKind::*;
    match *keyword {
        PrimTypeU8 => Some(U8),
        PrimTypeI32 => Some(I32),
        PrimTypeU32 => Some(U32),
        PrimTypeU64 => Some(U64),
        PrimTypeF32 => Some(F32),
        PrimTypeF64 => Some(F64),
        PrimTypeChar => Some(Char),
        PrimTypeString => Some(SMString),
        _ => None
    }
}

impl ASTItem for Type {

    fn symbol_len(&self) -> usize {
        match *self {
            Type::Primitive(_) => 1,
            Type::Array(_) => 3,
        }
    }
}

impl ASTParser for Type {
    
    fn parse(lexer: &mut Lexer, messages: &mut MessageEmitter) -> Option<Type> {

        match (lexer.nth(0), lexer.nth(1), lexer.nth(2)) {
            (Some(&Token::Keyword( Keyword{ ref kind, ref pos })), _, _) => {
                match check_primitive_type(kind) {
                    Some(prim) => Some(Type::Primitive(prim)),
                    None => {
                        messages.push(Message::ExpectType{ pos: pos.start_pos });
                        return None;
                    }
                }
            }
            (
                Some(&Token::Seperator( Seperator{ kind: SeperatorKind::LeftBracket, pos: ref pos1 })),
                Some(&Token::Keyword( Keyword{ ref kind, pos: ref pos2 })), 
                Some(&Token::Seperator( Seperator{ kind: SeperatorKind::RightBracket, pos: ref pos3 } ))
            ) => {
                match check_primitive_type(kind) {
                    Some(prim) => Some(Type::Array(prim)),
                    None => {
                        messages.push(Message::ExpectType{ pos: pos1.start_pos });
                        return None;
                    }
                }
            }
            _ => {
                messages.push(Message::ExpectType{ pos: lexer.position().start_pos });
                return None;
            }
        }
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn ast_smtype_parse() {
        use message::MessageEmitter;
        use lexical::Lexer;
        use syntax::ast_item::ASTParser;
        use super::PrimitiveType;
        use super::Type;

        macro_rules! test_case {
            ($program_slice: expr, $expect: expr) => ({

                let messages = &mut MessageEmitter::new();
                let lexer = &mut Lexer::from($program_slice.to_owned(), messages);
                assert_eq!(Type::parse(lexer, messages), Some($expect));
            });
            ($program_slice: expr) => ({

                let messages = &mut MessageEmitter::new();
                let lexer = &mut Lexer::from($program_slice.to_owned(), messages);
                assert_eq!(Type::parse(lexer, messages), None);
            })
        }

        test_case!("");
        test_case!("u8", Type::Primitive(PrimitiveType::U8));
        test_case!("u32", Type::Primitive(PrimitiveType::U32));
        test_case!("[string]", Type::Array(PrimitiveType::SMString));
        test_case!("[f32]", Type::Array(PrimitiveType::F32));
        test_case!("char", Type::Primitive(PrimitiveType::Char));
        test_case!("helloworld");
        test_case!("[asd]");
        test_case!("\"helloworld\"");
    }
}