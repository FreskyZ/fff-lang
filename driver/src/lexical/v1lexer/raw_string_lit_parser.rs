
// Raw string literal parser

use codepos::Position;
use codepos::StringPosition;
use message::LexicalMessage as Message;
use message::MessageEmitter;
use lexical::symbol_type::string_literal::StringLiteral;

pub struct RawStringLiteralParser {
    raw: String,
    start_pos: Position,
}

#[cfg(test)]
#[derive(Debug, Eq, PartialEq)]
pub enum RawStringLiteralParserResult { 
    WantMore, 
    Finished(StringLiteral),
}
#[cfg(not(test))]
pub enum RawStringLiteralParserResult { 
    WantMore, 
    Finished(StringLiteral),
}

impl RawStringLiteralParser {

    pub fn new(start_pos: Position) -> RawStringLiteralParser {
        RawStringLiteralParser {
            raw: String::new(),
            start_pos: start_pos,
        }
    }

    pub fn input(&mut self, ch: Option<char>, pos: Position, messages: &mut MessageEmitter) -> RawStringLiteralParserResult {
        match (ch, pos) {
            (Some('"'), pos) => {                                               // C1: in raw string, meet ", finish, return
                return RawStringLiteralParserResult::Finished(StringLiteral::new(self.raw.clone(), StringPosition::from2(self.start_pos, pos), true));
            }
            (Some(ch), _1) => {                                                 // C2: in raw string, meet other, continue
                self.raw.push(ch);
                return RawStringLiteralParserResult::WantMore;
            }
            (None, pos) => {
                messages.push(Message::UnexpectedEndofFileInStringLiteral {     // C3: in raw string, meet EOF, emit error, return  
                    literal_start: self.start_pos, 
                    eof_pos: pos,
                    hint_escaped_quote_pos: None
                });
                return RawStringLiteralParserResult::Finished(StringLiteral::new(None, StringPosition::from2(self.start_pos, pos), true));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    
    #[test]
    fn raw_string_lit_parser_test() {
        use codepos::Position;
        use codepos::StringPosition;
        use message::LexicalMessage as Message;
        use message::MessageEmitter;
        use lexical::symbol_type::string_literal::StringLiteral;
        use super::RawStringLiteralParser;
        use super::RawStringLiteralParserResult::*;

        let dummy_pos = Position::new();
        let spec_pos1 = make_pos!(12, 34);
        let spec_pos2 = make_pos!(56, 78);
        let spec_pos4 = make_pos!(1314, 1516);

        {   // r"hell\u\no", normal, C1, C2
            let mut parser = RawStringLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new(); 
            let expect_messages = &mut MessageEmitter::new();
            assert_eq!(parser.input(Some('h'), dummy_pos, messages), WantMore);
            assert_eq!(parser.input(Some('e'), dummy_pos, messages), WantMore);
            assert_eq!(parser.input(Some('l'), dummy_pos, messages), WantMore);
            assert_eq!(parser.input(Some('l'), dummy_pos, messages), WantMore);
            assert_eq!(parser.input(Some('\\'), dummy_pos, messages), WantMore);
            assert_eq!(parser.input(Some('u'), dummy_pos, messages), WantMore);
            assert_eq!(parser.input(Some('\\'), dummy_pos, messages), WantMore);
            assert_eq!(parser.input(Some('n'), dummy_pos, messages), WantMore);
            assert_eq!(parser.input(Some('o'), dummy_pos, messages), WantMore);
            assert_eq!(parser.input(Some('"'), spec_pos4, messages), 
                Finished(StringLiteral::new(r"hell\u\no".to_owned(), StringPosition::from2(spec_pos1, spec_pos4), true)));

            assert_eq!(messages, expect_messages);
        }

        {   // R"he$, normal, C2, C3
            let mut parser = RawStringLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new(); 
            let expect_messages = &mut MessageEmitter::new();
            assert_eq!(parser.input(Some('h'), dummy_pos, messages), WantMore);
            assert_eq!(parser.input(Some('e'), dummy_pos, messages), WantMore);
            assert_eq!(parser.input(None, spec_pos2, messages), 
                Finished(StringLiteral::new(None, StringPosition::from2(spec_pos1, spec_pos2), true)));

            expect_messages.push(Message::UnexpectedEndofFileInStringLiteral{ 
                literal_start: spec_pos1, eof_pos: spec_pos2,  hint_escaped_quote_pos: None});
            assert_eq!(messages, expect_messages);
        }
    }
}