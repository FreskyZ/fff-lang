///! fff-lang
///!
///! Raw string literal parser

use crate::source::{Position, Span, EOF};
use crate::diagnostics::{Message, MessageCollection, strings};

#[cfg_attr(test, derive(Debug, Eq, PartialEq))]
pub enum RawStringLiteralParserResult { 
    WantMore, 
    Finished(Option<String>, Span),
}

pub struct RawStringLiteralParser {
    raw: String,
    start_pos: Position,
}
impl RawStringLiteralParser {

    pub fn new(start_pos: Position) -> RawStringLiteralParser {
        RawStringLiteralParser {
            raw: String::new(),
            start_pos,
        }
    }

    pub fn input(&mut self, ch: char, pos: Position, messages: &mut MessageCollection) -> RawStringLiteralParserResult {
        match (ch, pos) {
            ('"', pos) => {                                               // C1: in raw string, meet ", finish, return
                return RawStringLiteralParserResult::Finished(Some(self.raw.clone()), self.start_pos + pos);
            }
            (EOF, pos) => {                                                    // C3: in raw string, meet EOF, emit error, return  
                messages.push(Message::new_by_str(strings::UnexpectedEOF, vec![ 
                    (self.start_pos.into(), strings::StringLiteralStartHere),
                    (pos.into(), strings::EOFHere),
                ]));
                return RawStringLiteralParserResult::Finished(None, self.start_pos + pos);
            }
            (ch, _1) => {                                                 // C2: in raw string, meet other, continue
                self.raw.push(ch);
                return RawStringLiteralParserResult::WantMore;
            }
        }
    }
}

#[cfg(test)] #[test]
fn raw_string_lit_parser() {
    use self::RawStringLiteralParserResult::*;

    let dummy_pos = Position::new(0);
    let spec_pos1 = Position::new(34);
    let spec_pos2 = Position::new(78);
    let spec_pos4 = Position::new(1516);

    {   // r"hell\u\no", normal, C1, C2
        let mut parser = RawStringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new(); 
        let expect_messages = &mut MessageCollection::new();
        assert_eq!(parser.input('h', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('e', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('l', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('l', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('\\', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('u', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('\\', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('n', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('o', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('"', spec_pos4, messages), 
            Finished(Some(r"hell\u\no".to_owned()), spec_pos1 + spec_pos4));

        assert_eq!(messages, expect_messages);
    }

    {   // R"he$, normal, C2, C3
        let mut parser = RawStringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new(); 
        let expect_messages = &mut MessageCollection::new();
        assert_eq!(parser.input('h', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('e', dummy_pos, messages), WantMore);
        assert_eq!(parser.input(EOF, spec_pos2, messages), 
            Finished(None, spec_pos1 + spec_pos2));

        expect_messages.push(Message::new_by_str(strings::UnexpectedEOF, vec![ 
            (spec_pos1.into(), strings::StringLiteralStartHere),
            (spec_pos2.into(), strings::EOFHere),
        ]));
        assert_eq!(messages, expect_messages);
    }
}