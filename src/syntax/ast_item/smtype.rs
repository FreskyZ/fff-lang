
// Type -> PrimitiveType | LeftBracket PrimitiveType RightBracket 

use message::Message;

use lexical::Lexer;
use lexical::IToken;
use lexical::KeywordKind;
use lexical::SeperatorKind;

use syntax::ast_item::IASTItem;

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

impl Type {

    pub fn unit_type() -> Type {
        Type::Primitive(PrimitiveType::Unit)
    }
}

impl IASTItem for Type {

    fn symbol_len(&self) -> usize {
        match *self {   
            Type::Primitive(_) => 1,
            Type::Array(_) => 3,
        }
    }
    
    fn parse(lexer: &mut Lexer, index: usize) -> Option<Type> {

        if let (Some(kind), pos) = (lexer.nth(index).get_keyword(), lexer.sym_pos(index)) { 
            match check_primitive_type(kind) {
                Some(prim) => return Some(Type::Primitive(prim)),
                None => return lexer.push_ret_none(Message::ExpectSymbol{ desc: "primitive type keyword".to_owned(), pos: pos.start_pos }),
            }
        }

        if let (true, Some(kind), true, pos) = (
                lexer.nth(index).is_seperator(SeperatorKind::LeftBracket), 
                lexer.nth(index + 1).get_keyword(), 
                lexer.nth(index + 2).is_seperator(SeperatorKind::RightBracket),
                lexer.sym_pos(index + 1)) {
            match check_primitive_type(kind) {
                Some(prim) => return Some(Type::Array(prim)),
                None => return lexer.push_ret_none(Message::ExpectSymbol{ desc: "primitive type keyword between brackets".to_owned(), pos: pos.start_pos })
            }
        }

        let pos = lexer.sym_pos(index).start_pos;
        return lexer.push_ret_none(Message::ExpectSymbol{ desc: "typedef".to_owned(), pos: pos });
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn ast_smtype_parse() {
        use message::MessageEmitter;
        use lexical::Lexer;
        use syntax::ast_item::IASTItem;
        use super::PrimitiveType;
        use super::Type;

        macro_rules! test_case {
            ($program_slice: expr, $expect: expr) => ({

                let messages = MessageEmitter::new();
                let lexer = &mut Lexer::from($program_slice.to_owned(), messages);
                assert_eq!(Type::parse(lexer, 0), Some($expect));
            });
            ($program_slice: expr) => ({

                let messages = MessageEmitter::new();
                let lexer = &mut Lexer::from($program_slice.to_owned(), messages);
                assert_eq!(Type::parse(lexer, 0), None);
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