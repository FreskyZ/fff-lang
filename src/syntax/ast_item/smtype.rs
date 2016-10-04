
// Type -> PrimitiveType | LeftBracket PrimitiveType RightBracket 

use message::Message;
use message::MessageEmitter;
use lexical::Lexer;
use lexical::BufToken;
use lexical::Token;
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

fn check_primitive_type(name: &str) -> Option<PrimitiveType> {
    use self::PrimitiveType::*;
    match name {
        "u8" => Some(U8),
        "i32" => Some(I32),
        "u32" => Some(U32),
        "u64" => Some(U64),
        "f32" => Some(F32),
        "f64" => Some(F64),
        "char" => Some(Char),
        "string" => Some(SMString),
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

        let mut state = State::ExpectPrimTypeOrLeftBracket;
        loop {
            let bufv3 = lexer.next(messages);
            match state {
                State::ExpectPrimTypeOrLeftBracket => {
                    match bufv3 {
                        Some(BufToken{ token: Token::Identifier{ name, pos }, next: _1 }) => {
                            match check_primitive_type(&name) {
                                Some(prim) => return Some(Type::Primitive(prim)),
                                None => {
                                    messages.push(Message::ExpectType{ pos: pos.start_pos });
                                    return None;
                                }
                            }
                        }
                        Some(BufToken{ token: Token::Seperator{ kind, pos }, next: _1 }) => {
                            if kind == SeperatorKind::LeftBracket {
                                state = State::ExpectPrimType;
                            } else {
                                messages.push(Message::ExpectType{ pos: pos.start_pos });
                                return None;
                            }
                        }
                        Some(BufToken{ token, next: _1 }) => {
                            messages.push(Message::ExpectType{ pos: token.position().start_pos });
                            return None;
                        }
                        None => {
                            messages.push(Message::ExpectType{ pos: lexer.inner().position() });
                            return None;
                        }
                    }
                }
                State::ExpectPrimType => {
                    match bufv3 {
                        Some(BufToken{ token: Token::Identifier{ name, pos }, next:  _1 }) => {
                            match check_primitive_type(&name) {
                                Some(prim) => state = State::ExpectRightBracket(prim),
                                None => {
                                    messages.push(Message::ExpectType{ pos: pos.start_pos });
                                    return None;
                                }
                            }
                        }
                        Some(BufToken{ token, next: _1 }) => {
                            messages.push(Message::ExpectType{ pos: token.position().start_pos });
                            return None;
                        }
                        None => {
                            messages.push(Message::ExpectType{ pos: lexer.inner().position() });
                            return None;
                        }
                    }
                }
                State::ExpectRightBracket(prim) => {
                    match bufv3 {
                        Some(BufToken{ token: Token::Seperator{ kind, pos }, next: _1 }) => {
                            if kind == SeperatorKind::RightBracket {
                                return Some(Type::Array(prim));
                            }
                            messages.push(Message::ExpectType{ pos: pos.start_pos });
                            return None;
                        }
                        Some(BufToken{ token, next: _1 }) => {
                            messages.push(Message::ExpectType{ pos: token.position().start_pos });
                            return None;
                        }
                        None => {
                            messages.push(Message::ExpectType{ pos: lexer.inner().position() });
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
        use lexical::Lexer;
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