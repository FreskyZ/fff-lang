
// Type -> PrimitiveType | LeftBracket PrimitiveType RightBracket 

use common::Position;
use message::Message;
use message::MessageEmitter;
use lexical::BufLexer as Lexer;
use lexical::BufToken;
use lexical::Token;
use lexical::KeywordKind;
use lexical::SeperatorKind;
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

impl ASTParser for Type {
    
    fn parse(lexer: &mut Lexer, messages: &mut MessageEmitter) -> Option<Type> {

        enum State {
            ExpectPrimTypeOrLeftBracket,
            ExpectPrimType,
            ExpectRightBracket(PrimitiveType), // array base
        }

        enum Interest {
            Keyword(KeywordKind, Position),
            Seperator(SeperatorKind, Position),
            Other(Position),
        }

        let mut state = State::ExpectPrimTypeOrLeftBracket;
        loop {
            let interest = match lexer.next(messages) {
                Some(BufToken{ token: Token::Keyword{ kind, pos }, next: _1 }) => Interest::Keyword(kind, pos.start_pos),
                Some(BufToken{ token: Token::Seperator{ kind, pos }, next: _1 }) => Interest::Seperator(kind, pos.start_pos),
                Some(BufToken{ token, next: _1 }) => Interest::Other(token.position().start_pos),
                None => Interest::Other(lexer.inner().position()),
            };
            
            match state {
                State::ExpectPrimTypeOrLeftBracket => {
                    match interest {
                        Interest::Keyword(kind, pos) => {
                            match check_primitive_type(&kind) {
                                Some(prim) => return Some(Type::Primitive(prim)),
                                None => {
                                    messages.push(Message::ExpectType{ pos: pos });
                                    return None;
                                }
                            }
                        }
                        Interest::Seperator(kind, pos) => {
                            if kind == SeperatorKind::LeftBracket {
                                state = State::ExpectPrimType;
                            } else {
                                messages.push(Message::ExpectType{ pos: pos });
                                return None;
                            }
                        }
                        Interest::Other(pos) => {
                            messages.push(Message::ExpectType{ pos: pos });
                            return None;
                        }
                    }
                }
                State::ExpectPrimType => {
                    match interest {
                        Interest::Keyword(kind, pos) => {
                            match check_primitive_type(&kind) {
                                Some(prim) => state = State::ExpectRightBracket(prim),
                                None => {
                                    messages.push(Message::ExpectType{ pos: pos });
                                    return None;
                                }
                            }
                        }
                        Interest::Seperator(_, pos) | Interest::Other(pos) => {
                            messages.push(Message::ExpectType{ pos: pos });
                            return None;
                        }
                    }
                }
                State::ExpectRightBracket(prim) => {
                    match interest {
                        Interest::Seperator(kind, pos) => {
                            if kind == SeperatorKind::RightBracket {
                                return Some(Type::Array(prim));
                            }
                            messages.push(Message::ExpectType{ pos: pos });
                            return None;
                        }
                        Interest::Keyword(_, pos) | Interest::Other(pos) => {
                            messages.push(Message::ExpectType{ pos: pos });
                            return None;
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn ast_smtype_parse() {
        use message::MessageEmitter;
        use lexical::BufLexer as Lexer;
        use syntax::ast_item::ASTParser;
        use super::PrimitiveType;
        use super::Type;

        macro_rules! test_case {
            ($program_slice: expr, $expect: expr) => ({

                let messages = &mut MessageEmitter::new();
                let lexer = &mut Lexer::from($program_slice.to_owned());
                assert_eq!(Type::parse(lexer, messages), Some($expect));
            });
            ($program_slice: expr) => ({

                let messages = &mut MessageEmitter::new();
                let lexer = &mut Lexer::from($program_slice.to_owned());
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