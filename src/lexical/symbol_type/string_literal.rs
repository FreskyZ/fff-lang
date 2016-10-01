
// String literal type
// string literal storage, escape and display

use std::fmt;
use common::Position;
use common::StringPosition;
use message::Message;
use message::MessageEmitter;
use lexical::symbol_type::char_escape::EscapeCharParser;
use lexical::symbol_type::char_escape::EscapeCharSimpleCheckResult;
use lexical::symbol_type::char_escape::EscapeCharParserInputResult;

// --- String literal symbol
#[cfg(test)]
#[derive(Eq, PartialEq, Clone)]
pub struct StringLiteral {
    pub value: String,
    pub pos: StringPosition,
    pub is_raw: bool,
    pub has_failed: bool
}
#[cfg(not(test))]
#[derive(Clone)]
pub struct StringLiteral {
    pub value: String,
    pub pos: StringPosition,
    pub is_raw: bool,
    pub has_failed: bool
}

impl StringLiteral {

    pub fn new(raw: String, pos: StringPosition, is_raw: bool, has_failed: bool) -> StringLiteral {
        StringLiteral {
            value: raw,
            pos: pos,
            is_raw: is_raw,
            has_failed: has_failed,
        }
    }
}

impl fmt::Debug for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}tring literal {:?} at {:?}{}", 
            if self.is_raw { "Raw s" } else { "S" }, 
            self.value, 
            self.pos, 
            if self.has_failed { ", has failed" } else { "" }
        )
    }
}

impl_display_by_debug!(StringLiteral);

// String literal parser, raw or not raw

#[cfg(test)]
#[derive(Debug)]
pub struct StringLiteralParser {
    raw: String, 
    start_pos: Position,
    last_escape_quote_pos: Option<Position>,
    has_failed: bool,
    escape_parser: Option<EscapeCharParser>, // Not none means is parsing escape
    escape_start_pos: Position,
}
#[cfg(not(test))]
pub struct StringLiteralParser {
    raw: String, 
    start_pos: Position,
    last_escape_quote_pos: Option<Position>,
    has_failed: bool,
    escape_parser: Option<EscapeCharParser>,
    escape_start_pos: Position,
}

#[cfg(test)]
pub fn str_lit_parser_visitor(parser: &StringLiteralParser) -> (&str, &Position, &Option<Position>, &bool, &Option<EscapeCharParser>) {
    (&parser.raw, &parser.start_pos, &parser.last_escape_quote_pos, &parser.has_failed, &parser.escape_parser)
}

// Escape issues about string literal and char literal
// all escapes: \t, \n, \r, \0, \\, \", \', \uxxxx, \Uxxxxxxxx, are all supported in char and string literal
// when meet \, start parsing escape char literal
// if meet EOF, string report unexpected EOF in string, char report unexpected EOF in char
// if meet end of literal, e.g.  '\uAA' or "123\U45678", report incorrect unicode escape error
// specially  
//     '\', char parser will find next is not ' and report too long error
//     "\", string parser will record this escape position and this will most probably cause unexpected EOF in string and string parser will report it

impl StringLiteralParser {

    /// new with start position
    pub fn new(start_pos: Position) -> StringLiteralParser {
        StringLiteralParser{
            raw: String::new(),
            start_pos: start_pos, 
            last_escape_quote_pos: None, 
            has_failed: false,
            escape_parser: None,
            escape_start_pos: Position::new(),
        }
    }

    /// Try get string literal, use in state machine of v1, return Some for successfully processed, None for want more
    /// ch is none means get none, next_ch is none means next is none
    pub fn try_get_string_literal(
        &mut self, 
        ch: Option<char>, 
        pos: Position, 
        next_ch: Option<char>, 
        messages: &mut MessageEmitter) 
        -> (Option<StringLiteral>, bool){ // ret_val, require_skip1

        match (ch, pos, next_ch) {
            (Some('\\'), slash_pos, Some(next_ch)) => {
                match EscapeCharParser::simple_check(next_ch) {
                    EscapeCharSimpleCheckResult::Normal(ch) => {                    // C1, normal escape
                        self.raw.push(ch);
                        if ch == '"' {
                            self.last_escape_quote_pos = Some(slash_pos);
                        }
                        return (None, true);
                    }
                    EscapeCharSimpleCheckResult::Invalid(ch) => {
                        messages.push(Message::UnrecognizedEscapeCharInStringLiteral {
                            literal_start: self.start_pos,                          // C2, error normal escape, emit error and continue
                            unrecogonize_pos: slash_pos, 
                            unrecogonize_escape: ch });
                        self.has_failed = true;
                        return (None, true);
                    }
                    EscapeCharSimpleCheckResult::Unicode(parser) => {               // C3, start unicode escape
                        self.escape_start_pos = slash_pos;
                        self.escape_parser = Some(parser);
                        return (None, true);
                    }
                }
            }
            (Some('\\'), _pos, None) => {                                            // C4, \EOF, ignore
                // Do nothing here, `"abc\udef$` reports EOF in string error, not end of string or EOF in escape error
                return (None, false);
            }
            (Some('"'), pos, _1) => {
                // String finished, check if is parsing escape
                match self.escape_parser {
                    Some(ref _parser) => {                                           // C5, \uxxx\EOL, emit error and return
                        // If still parsing, it is absolutely failed
                        messages.push(Message::UnexpectedStringLiteralEndInUnicodeCharEscape {
                            literal_start: self.start_pos, 
                            escape_start: self.escape_start_pos,
                            unexpected_end_pos: pos,
                        });
                        return (Some(StringLiteral::new(self.raw.clone(), StringPosition::from((self.start_pos, pos)), false, true)), false);
                    }
                    None => {                                                       // C7, normal EOL, return
                        return (Some(StringLiteral::new(self.raw.clone(), StringPosition::from((self.start_pos, pos)), false, self.has_failed)), false);
                    }
                }
            }
            (Some(ch), pos, _2) => {
                // Normal in string
                let mut need_reset_escape_parser = false;
                match self.escape_parser {
                    Some(ref mut parser) => {
                        match parser.input(ch, (self.escape_start_pos, pos), messages) {
                            EscapeCharParserInputResult::WantMore => (),            // C8, in unicode escape, (may be fail and) want more
                            EscapeCharParserInputResult::Failed => {
                                self.has_failed = true;
                                need_reset_escape_parser = true;                    // C9, in unicode escape, not hex char or last not unicode codepoint value, finish
                            }
                            EscapeCharParserInputResult::Success(ch) => {           // C10, in unicode escape, success, finish
                                need_reset_escape_parser = true;
                                self.raw.push(ch);
                            }
                        }
                    }
                    None => {
                        self.raw.push(ch);                                          // C11, most plain
                    }
                }
                if need_reset_escape_parser {  
                    self.escape_parser = None;
                }
                return (None, false);
            }
            (None, pos, _2) => {
                messages.push(Message::UnexpectedEndofFileInStringLiteral {         // C12: in string, meet EOF, emit error, return 
                    literal_start: self.start_pos,
                    eof_pos: pos,
                    hint_escaped_quote_pos: self.last_escape_quote_pos 
                });
                return (None, false);
            }
        }
    }

    /// Try get raw string literal, use in state machine of v1, return Some for successfully processed, None for want more
    pub fn try_get_raw_string_literal(&mut self, ch: Option<char>, pos: Position, messages: &mut MessageEmitter) -> Option<StringLiteral> {
        match (ch, pos) {
            (Some('"'), pos) => {                                               // C1: in raw string, meet ", finish, return
                return Some(StringLiteral::new(self.raw.clone(), StringPosition::from((self.start_pos, pos)), true, false));
            }
            (Some(ch), _1) => {                                                 // C2: in raw string, meet other, continue
                self.raw.push(ch);
                return None;
            }
            (None, pos) => {
                messages.push(Message::UnexpectedEndofFileInStringLiteral {     // C3: in raw string, meet EOF, emit error, return  
                    literal_start: self.start_pos, 
                    eof_pos: pos,
                    hint_escaped_quote_pos: None
                });
                return None;
            }
        }
    }
}

#[cfg(test)]
mod tests {

    fn use_skipper<T>(skipper: T)
        where T: FnOnce() -> () {

        skipper();
    }
    fn str_lit_feasibility_fnonce_as_param() {
        use lexical::lexer::v0::V0Lexer;
        use lexical::lexer::v0::BufV0Lexer;
        use message::MessageEmitter;

        let pos1 = {
            let mut bufv0 = BufV0Lexer::from(V0Lexer::from("123234345".to_owned()));
            let messages = &mut MessageEmitter::new();
            bufv0.skip1(messages);
            bufv0.inner().position()
        };
        
        let pos2 = {
            let mut bufv0 = BufV0Lexer::from(V0Lexer::from("123234345".to_owned()));
            let messages = &mut MessageEmitter::new();
            use_skipper(|| bufv0.skip1(messages));
            bufv0.inner().position()
        };

        assert_eq!(pos1, pos2);
    }

    #[test]
    fn str_lit_not_raw() {
        use common::Position;
        use common::StringPosition;
        use message::Message;
        use message::MessageEmitter;
        use super::StringLiteral;
        use super::StringLiteralParser;

        let dummy_pos = Position::new();
        let spec_pos1 = Position::from((12, 34));
        let spec_pos2 = Position::from((56, 78));
        let spec_pos3 = Position::from((910, 1112));
        let spec_pos4 = Position::from((1314, 1516));

        {   // "Hello, world!", most normal,                                    C11, C5, C7
            let mut parser = StringLiteralParser::new(Position::from((12, 34)));
            let messages = &mut MessageEmitter::new(); 
            assert_eq!(parser.try_get_string_literal(Some('H'), dummy_pos, Some('e'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('e'), dummy_pos, Some('l'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('l'), dummy_pos, Some('l'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('l'), dummy_pos, Some('o'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('o'), dummy_pos, Some('o'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some(','), dummy_pos, Some(','), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some(' '), dummy_pos, Some(' '), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('w'), dummy_pos, Some('w'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('o'), dummy_pos, Some('o'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('r'), dummy_pos, Some('r'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('l'), dummy_pos, Some('l'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('d'), dummy_pos, Some('d'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('!'), dummy_pos, None, messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('"'), Position::from((56, 78)), None, messages), 
                (Some(StringLiteral::new("Hello, world!".to_owned(), StringPosition::from((12, 34, 56, 78)), false, false)), false));

            assert_eq!(messages, &MessageEmitter::new());
        }

        {   // "He$, unexpected end, no last escaped quote hint                 C11, C12
            let mut parser = StringLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let expect_messages = &mut MessageEmitter::new();  
            assert_eq!(parser.try_get_string_literal(Some('H'), dummy_pos, Some('e'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('e'), dummy_pos, Some('l'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(None, spec_pos2, None, messages), (None, false));

            expect_messages.push(Message::UnexpectedEndofFileInStringLiteral { literal_start: spec_pos1, eof_pos: spec_pos2, hint_escaped_quote_pos: None });
            assert_eq!(messages, expect_messages);
        }

        {   // "He\"l\"lo$, unexpected EOF, last escaped quote recorded         C11, C1, C12
            let mut parser = StringLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let expect_messages = &mut MessageEmitter::new();
            assert_eq!(parser.try_get_string_literal(Some('H'), dummy_pos, Some('e'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('e'), dummy_pos, Some('\\'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('\\'), spec_pos2, Some('"'), messages), (None, true));
            assert_eq!(parser.try_get_string_literal(Some('l'), dummy_pos, Some('\\'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('\\'), spec_pos3, Some('"'), messages), (None, true));
            assert_eq!(parser.try_get_string_literal(Some('l'), dummy_pos, Some('o'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('o'), dummy_pos, None, messages), (None, false));
            assert_eq!(parser.try_get_string_literal(None, spec_pos4, None, messages), (None, false));

            expect_messages.push(Message::UnexpectedEndofFileInStringLiteral { 
                literal_start: spec_pos1, eof_pos: spec_pos4, hint_escaped_quote_pos: Some(spec_pos3) });
            assert_eq!(messages, expect_messages);

        }

        {   // "H\t\n\0\'\"llo", normal escape                                  C11, C1, C7
            let mut parser = StringLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let expect_messages = &mut MessageEmitter::new();
            assert_eq!(parser.try_get_string_literal(Some('H'), dummy_pos, Some('\\'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('\\'), dummy_pos, Some('t'), messages), (None, true));
            assert_eq!(parser.try_get_string_literal(Some('\\'), dummy_pos, Some('n'), messages), (None, true));
            assert_eq!(parser.try_get_string_literal(Some('\\'), dummy_pos, Some('0'), messages), (None, true));
            assert_eq!(parser.try_get_string_literal(Some('\\'), dummy_pos, Some('\''), messages), (None, true));
            assert_eq!(parser.try_get_string_literal(Some('\\'), dummy_pos, Some('"'), messages), (None, true));
            assert_eq!(parser.try_get_string_literal(Some('l'), dummy_pos, Some('l'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('l'), dummy_pos, Some('o'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('o'), dummy_pos, Some('"'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('"'), spec_pos4, Some('$'), messages), 
                (Some(StringLiteral::new("H\t\n\0\'\"llo".to_owned(), StringPosition::from((spec_pos1, spec_pos4)), false, false)), false));

            assert_eq!(messages, expect_messages);
        }

        {   // "h\a\d\e\u\f", error normal escape                               C11, C3, C2
            let mut parser = StringLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let expect_messages = &mut MessageEmitter::new();
            assert_eq!(parser.try_get_string_literal(Some('H'), dummy_pos, Some('\\'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('\\'), spec_pos2, Some('a'), messages), (None, true));
            assert_eq!(parser.try_get_string_literal(Some('\\'), spec_pos3, Some('d'), messages), (None, true));
            assert_eq!(parser.try_get_string_literal(Some('\\'), spec_pos2, Some('e'), messages), (None, true));
            assert_eq!(parser.try_get_string_literal(Some('\\'), dummy_pos, Some('n'), messages), (None, true));
            assert_eq!(parser.try_get_string_literal(Some('\\'), spec_pos3, Some('f'), messages), (None, true));
            assert_eq!(parser.try_get_string_literal(Some('"'), spec_pos4, Some('$'), messages),
                (Some(StringLiteral::new("H\n".to_owned(), StringPosition::from((spec_pos1, spec_pos4)), false, true)), false));
            
            expect_messages.push(Message::UnrecognizedEscapeCharInStringLiteral{ 
                literal_start: spec_pos1, unrecogonize_pos: spec_pos2, unrecogonize_escape: 'a' });
            expect_messages.push(Message::UnrecognizedEscapeCharInStringLiteral{ 
                literal_start: spec_pos1, unrecogonize_pos: spec_pos3, unrecogonize_escape: 'd' });
            expect_messages.push(Message::UnrecognizedEscapeCharInStringLiteral{ 
                literal_start: spec_pos1, unrecogonize_pos: spec_pos2, unrecogonize_escape: 'e' });
            expect_messages.push(Message::UnrecognizedEscapeCharInStringLiteral{ 
                literal_start: spec_pos1, unrecogonize_pos: spec_pos3, unrecogonize_escape: 'f' });
            assert_eq!(messages, expect_messages);
        }

        {   // "H\uABCHel\uABCg", unicode escape error                          C11, C3, C8, C9, C7
            let mut parser = StringLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let expect_messages = &mut MessageEmitter::new();
            assert_eq!(parser.try_get_string_literal(Some('H'), dummy_pos, Some('\\'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('\\'), spec_pos2, Some('u'), messages), (None, true));
            assert_eq!(parser.try_get_string_literal(Some('A'), dummy_pos, Some('B'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('B'), dummy_pos, Some('C'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('C'), dummy_pos, Some('H'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('H'), spec_pos3, Some('e'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('e'), dummy_pos, Some('l'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('l'), dummy_pos, Some('\\'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('\\'), spec_pos3, Some('u'), messages), (None, true));
            assert_eq!(parser.try_get_string_literal(Some('A'), dummy_pos, Some('B'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('B'), dummy_pos, Some('C'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('C'), dummy_pos, Some('g'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('g'), spec_pos4, Some('"'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('"'), spec_pos4, Some('$'), messages), 
                (Some(StringLiteral::new("Hel".to_owned(), StringPosition::from((spec_pos1, spec_pos4)), false, true)), false));
            
            expect_messages.push(Message::UnexpectedCharInUnicodeCharEscape{ 
                    escape_start: spec_pos2, unexpected_char_pos: spec_pos3, unexpected_char: 'H' });
            expect_messages.push(Message::UnexpectedCharInUnicodeCharEscape{ 
                    escape_start: spec_pos3, unexpected_char_pos: spec_pos4, unexpected_char: 'g' });
            assert_eq!(messages, expect_messages);
        }

        {   // "H\U0011ABCD", unicode escape error 2                            C11, C3, C8, C9, C7
            let mut parser = StringLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let expect_messages = &mut MessageEmitter::new();
            assert_eq!(parser.try_get_string_literal(Some('H'), dummy_pos, Some('\\'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('\\'), spec_pos2, Some('U'), messages), (None, true));
            assert_eq!(parser.try_get_string_literal(Some('0'), dummy_pos, Some('0'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('0'), dummy_pos, Some('1'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('1'), dummy_pos, Some('1'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('1'), dummy_pos, Some('A'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('A'), dummy_pos, Some('B'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('B'), dummy_pos, Some('C'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('C'), dummy_pos, Some('D'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('D'), dummy_pos, Some('"'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('"'), spec_pos3, Some('$'), messages),
                (Some(StringLiteral::new("H".to_owned(), StringPosition::from((spec_pos1, spec_pos3)), false, true)), false));
            
            expect_messages.push(Message::IncorrectUnicodeCharEscapeValue{ escape_start: spec_pos2, raw_value: "0011ABCD".to_owned() });
            assert_eq!(messages, expect_messages);
        }

        {   // "H\uABCDel", unicode escape                                      C11, C3, C8, C10, C7
            let mut parser = StringLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let expect_messages = &mut MessageEmitter::new();
            assert_eq!(parser.try_get_string_literal(Some('H'), dummy_pos, Some('\\'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('\\'), spec_pos2, Some('u'), messages), (None, true));
            assert_eq!(parser.try_get_string_literal(Some('A'), dummy_pos, Some('B'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('B'), dummy_pos, Some('C'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('C'), dummy_pos, Some('D'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('D'), dummy_pos, Some('e'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('e'), dummy_pos, Some('l'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('l'), dummy_pos, Some('"'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('"'), spec_pos3, Some('$'), messages), 
                (Some(StringLiteral::new("H\u{ABCD}el".to_owned(), StringPosition::from((spec_pos1, spec_pos3)), false, false)), false));
            
            assert_eq!(messages, expect_messages);
        }

        {   // "H\u", unexpected EOL in unicode escape                          C11, C3, C5
            let mut parser = StringLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let expect_messages = &mut MessageEmitter::new();
            assert_eq!(parser.try_get_string_literal(Some('H'), dummy_pos, Some('\\'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('\\'), spec_pos2, Some('u'), messages), (None, true));
            assert_eq!(parser.try_get_string_literal(Some('"'), spec_pos3, Some('$'), messages), 
                (Some(StringLiteral::new("H".to_owned(), StringPosition::from((spec_pos1, spec_pos3)), false, true)), false));
            
            expect_messages.push(Message::UnexpectedStringLiteralEndInUnicodeCharEscape{
                literal_start: spec_pos1, escape_start: spec_pos2, unexpected_end_pos: spec_pos3 });
            assert_eq!(messages, expect_messages);
        }

        {   // "h\U123$, unexpected EOF in unicode escape                       C11, C3, C8, C12
            let mut parser = StringLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let expect_messages = &mut MessageEmitter::new();
            assert_eq!(parser.try_get_string_literal(Some('h'), dummy_pos, Some('\\'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('\\'), spec_pos2, Some('U'), messages), (None, true));
            assert_eq!(parser.try_get_string_literal(Some('1'), dummy_pos, Some('2'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('2'), dummy_pos, Some('3'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('3'), dummy_pos, None, messages), (None, false));
            assert_eq!(parser.try_get_string_literal(None, spec_pos3, None, messages), (None, false));
            
            expect_messages.push(Message::UnexpectedEndofFileInStringLiteral { 
                literal_start: spec_pos1, eof_pos: spec_pos3, hint_escaped_quote_pos: None });
            assert_eq!(messages, expect_messages);
        }

        {   // "he\$, unexpected EOF exactly after \                            C11, C4
            let mut parser = StringLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let expect_messages = &mut MessageEmitter::new();
            assert_eq!(parser.try_get_string_literal(Some('h'), dummy_pos, Some('e'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('e'), dummy_pos, Some('\\'), messages), (None, false));
            assert_eq!(parser.try_get_string_literal(Some('\\'), spec_pos2, None, messages), (None, false));
            assert_eq!(parser.try_get_string_literal(None, spec_pos3, None, messages), (None, false));
            
            expect_messages.push(Message::UnexpectedEndofFileInStringLiteral { 
                literal_start: spec_pos1, eof_pos: spec_pos3, hint_escaped_quote_pos: None });
            assert_eq!(messages, expect_messages);
        }
    }

    #[test]
    fn str_lit_raw() {
        use common::Position;
        use common::StringPosition;
        use message::Message;
        use message::MessageEmitter;
        use super::StringLiteral;
        use super::StringLiteralParser;

        let dummy_pos = Position::new();
        let spec_pos1 = Position::from((12, 34));
        let spec_pos2 = Position::from((56, 78));
        // let spec_pos3 = Position::from((910, 1112));
        let spec_pos4 = Position::from((1314, 1516));

        {   // r"hell\u\no", normal, C1, C2
            let mut parser = StringLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new(); 
            let expect_messages = &mut MessageEmitter::new();
            assert_eq!(parser.try_get_raw_string_literal(Some('h'), dummy_pos, messages), None);
            assert_eq!(parser.try_get_raw_string_literal(Some('e'), dummy_pos, messages), None);
            assert_eq!(parser.try_get_raw_string_literal(Some('l'), dummy_pos, messages), None);
            assert_eq!(parser.try_get_raw_string_literal(Some('l'), dummy_pos, messages), None);
            assert_eq!(parser.try_get_raw_string_literal(Some('\\'), dummy_pos, messages), None);
            assert_eq!(parser.try_get_raw_string_literal(Some('u'), dummy_pos, messages), None);
            assert_eq!(parser.try_get_raw_string_literal(Some('\\'), dummy_pos, messages), None);
            assert_eq!(parser.try_get_raw_string_literal(Some('n'), dummy_pos, messages), None);
            assert_eq!(parser.try_get_raw_string_literal(Some('o'), dummy_pos, messages), None);
            assert_eq!(parser.try_get_raw_string_literal(Some('"'), spec_pos4, messages), 
                Some(StringLiteral::new(r"hell\u\no".to_owned(), StringPosition::from((spec_pos1, spec_pos4)), true, false)));

            assert_eq!(messages, expect_messages);
        }

        {   // R"he$, normal, C2, C3
            let mut parser = StringLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new(); 
            let expect_messages = &mut MessageEmitter::new();
            assert_eq!(parser.try_get_raw_string_literal(Some('h'), dummy_pos, messages), None);
            assert_eq!(parser.try_get_raw_string_literal(Some('e'), dummy_pos, messages), None);
            assert_eq!(parser.try_get_raw_string_literal(None, spec_pos2, messages), None);

            expect_messages.push(Message::UnexpectedEndofFileInStringLiteral{ 
                literal_start: spec_pos1, eof_pos: spec_pos2,  hint_escaped_quote_pos: None});
            assert_eq!(messages, expect_messages);
        }
    }
}