///! fff-lang
///!
///! Raw string literal parser

use crate::codemap::{CharPos, Span, EOF_CHAR};
use crate::diagnostics::{Message, MessageCollection};
use super::error_strings;

#[cfg_attr(test, derive(Debug, Eq, PartialEq))]
pub enum RawStringLiteralParserResult { 
    WantMore, 
    Finished(Option<String>, Span),
}

pub struct RawStringLiteralParser {
    raw: String,
    start_pos: CharPos,
}
impl RawStringLiteralParser {

    pub fn new(start_pos: CharPos) -> RawStringLiteralParser {
        RawStringLiteralParser {
            raw: String::new(),
            start_pos,
        }
    }

    pub fn input(&mut self, ch: char, pos: CharPos, messages: &mut MessageCollection) -> RawStringLiteralParserResult {
        match (ch, pos) {
            ('"', pos) => {                                               // C1: in raw string, meet ", finish, return
                return RawStringLiteralParserResult::Finished(Some(self.raw.clone()), self.start_pos.merge(&pos));
            }
            (EOF_CHAR, pos) => {                                                    // C3: in raw string, meet EOF, emit error, return  
                messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![ 
                    (self.start_pos.as_span(), error_strings::StringLiteralStartHere),
                    (pos.as_span(), error_strings::EOFHere),
                ]));
                return RawStringLiteralParserResult::Finished(None, self.start_pos.merge(&pos));
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

    let dummy_pos = CharPos::default();
    let spec_pos1 = make_charpos!(34);
    let spec_pos2 = make_charpos!(78);
    let spec_pos4 = make_charpos!(1516);

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
            Finished(Some(r"hell\u\no".to_owned()), spec_pos1.merge(&spec_pos4)));

        assert_eq!(messages, expect_messages);
    }

    {   // R"he$, normal, C2, C3
        let mut parser = RawStringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new(); 
        let expect_messages = &mut MessageCollection::new();
        assert_eq!(parser.input('h', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('e', dummy_pos, messages), WantMore);
        assert_eq!(parser.input(EOF_CHAR, spec_pos2, messages), 
            Finished(None, spec_pos1.merge(&spec_pos2)));

        expect_messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![ 
                    (spec_pos1.as_span(), error_strings::StringLiteralStartHere),
                    (spec_pos2.as_span(), error_strings::EOFHere),
        ]));
        assert_eq!(messages, expect_messages);
    }
}