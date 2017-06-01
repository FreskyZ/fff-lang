
// Raw string literal parser

use codemap::Position;
use codemap::StringPosition;
use message::Message;
use message::MessageCollection;
use codemap::EOFCHAR;

use super::error_strings;

pub struct RawStringLiteralParser {
    raw: String,
    start_pos: Position,
}

#[cfg(test)]
#[derive(Debug, Eq, PartialEq)]
pub enum RawStringLiteralParserResult { 
    WantMore, 
    Finished(Option<String>, StringPosition),
}
#[cfg(not(test))]
pub enum RawStringLiteralParserResult { 
    WantMore, 
    Finished(Option<String>, StringPosition),
}

impl RawStringLiteralParser {

    pub fn new(start_pos: Position) -> RawStringLiteralParser {
        RawStringLiteralParser {
            raw: String::new(),
            start_pos: start_pos,
        }
    }

    pub fn input(&mut self, ch: char, pos: Position, messages: &mut MessageCollection) -> RawStringLiteralParserResult {
        match (ch, pos) {
            ('"', pos) => {                                               // C1: in raw string, meet ", finish, return
                return RawStringLiteralParserResult::Finished(Some(self.raw.clone()), StringPosition::from2(self.start_pos, pos));
            }
            (EOFCHAR, pos) => {                                                    // C3: in raw string, meet EOF, emit error, return  
                messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![ 
                    (StringPosition::double(self.start_pos), error_strings::StringLiteralStartHere),
                    (StringPosition::double(pos), error_strings::EOFHere),
                ]));
                return RawStringLiteralParserResult::Finished(None, StringPosition::from2(self.start_pos, pos));
            }
            (ch, _1) => {                                                 // C2: in raw string, meet other, continue
                self.raw.push(ch);
                return RawStringLiteralParserResult::WantMore;
            }
        }
    }
}

#[cfg(test)]    
#[test]
fn raw_string_lit_parser() {
    use self::RawStringLiteralParserResult::*;

    let dummy_pos = Position::new();
    let spec_pos1 = make_pos!(12, 34);
    let spec_pos2 = make_pos!(56, 78);
    let spec_pos4 = make_pos!(1314, 1516);

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
            Finished(Some(r"hell\u\no".to_owned()), StringPosition::from2(spec_pos1, spec_pos4)));

        assert_eq!(messages, expect_messages);
    }

    {   // R"he$, normal, C2, C3
        let mut parser = RawStringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new(); 
        let expect_messages = &mut MessageCollection::new();
        assert_eq!(parser.input('h', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('e', dummy_pos, messages), WantMore);
        assert_eq!(parser.input(EOFCHAR, spec_pos2, messages), 
            Finished(None, StringPosition::from2(spec_pos1, spec_pos2)));

        expect_messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![ 
                    (StringPosition::double(spec_pos1), error_strings::StringLiteralStartHere),
                    (StringPosition::double(spec_pos2), error_strings::EOFHere),
        ]));
        assert_eq!(messages, expect_messages);
    }
}