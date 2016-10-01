
// Character literal

use std::fmt;
use common::From2;
use common::Position;
use common::StringPosition;
use message::Message;
use message::MessageEmitter;
use lexical::symbol_type::char_escape::EscapeCharParser;
use lexical::symbol_type::char_escape::EscapeCharSimpleCheckResult;
use lexical::symbol_type::char_escape::EscapeCharParserResult;

#[cfg(test)]
use std::collections::HashSet;
#[cfg(test)]
pub type CoverageRecorder = HashSet<i32>;
#[cfg(not(test))]
pub struct CoverageRecorder{ pub dummy: i32 }
#[cfg(not(test))]
impl CoverageRecorder {
    pub fn new() -> CoverageRecorder { CoverageRecorder{ dummy: 0 } }
    pub fn insert(&mut self, v: i32) { self.dummy = v } 
}

// Char literal
#[cfg(test)]
#[derive(Eq, PartialEq, Clone)]
pub struct CharLiteral {
    pub value: Option<char>, // None for invalid
    pub pos: StringPosition,
}
#[cfg(not(test))]
#[derive(Clone)]
pub struct CharLiteral {
    pub value: Option<char>,
    pub pos: StringPosition,
}

impl fmt::Debug for CharLiteral {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.value {
            Some(value) => write!(f, "Char literal {:?} at {:?}", value, self.pos),
            None => write!(f, "Invalid char literal at {:?}", self.pos)
        }
    }
}

// Char literal parser
pub struct CharLiteralParser {
    start_pos: Position,
    raw: String,
    buf: Option<char>,    // has some means has processed one char, error or not error
    has_failed: bool,
    has_too_longed: bool,
    escape_parser: Option<EscapeCharParser>,
    escape_start_pos: Position,
    is_very_special_that_previous_is_single_quote_escape_and_not_require_skipped: bool,
}

#[cfg(test)]
#[derive(Eq, PartialEq, Debug)]
pub enum CharLiteralParserResult {
    WantMore,
    WantMoreWithSkip1,
    Finished(CharLiteral),
}
#[cfg(not(test))]
pub enum CharLiteralParserResult {
    WantMore,
    WantMoreWithSkip1,
    Finished(CharLiteral),
}

const INVALID_CHAR: char = '\u{FFFE}'; // for parser presenting failed value

impl CharLiteralParser {

    pub fn new(start_pos: Position) -> CharLiteralParser {
        CharLiteralParser{ 
            start_pos: start_pos, 
            raw: String::new(),
            buf: None,
            has_failed: false,
            has_too_longed: false,
            escape_parser: None, 
            escape_start_pos: Position::new(),
            is_very_special_that_previous_is_single_quote_escape_and_not_require_skipped: false,
        }
    }

    pub fn input(
        &mut self, 
        ch: Option<char>, 
        pos: Position, 
        next_ch: Option<char>, 
        messages: &mut MessageEmitter
        , coverage_recorder: &mut CoverageRecorder) -> CharLiteralParserResult {

        if self.is_very_special_that_previous_is_single_quote_escape_and_not_require_skipped {
            match next_ch {
                None => (), // continue to C14
                Some(next_ch) => {
                    if next_ch != '\'' {   // else continue to C15
                        messages.push(Message::InvalidEscapeInCharLiteral{ start_pos: self.start_pos });
                        return CharLiteralParserResult::Finished(CharLiteral{ value: None, pos: StringPosition::from2(self.start_pos, pos) });
                    }
                }
            }
        }

        match self.buf {
            None => {       // Waiting for first char
                let mut need_reset_parser = false;
                match self.escape_parser {
                    Some(ref mut parser) => {  // parsing unicode escape buf meet 
                        match ch {
                            Some(ch) => {
                                if ch == '\'' { // '\'' should report unexpected EOL
                                    coverage_recorder.insert(18);
                                    messages.push(Message::UnexpectedCharLiteralEndInUnicodeCharEscape{
                                        literal_start: self.start_pos, 
                                        escape_start: self.escape_start_pos,
                                        unexpected_end_pos: pos });
                                    return CharLiteralParserResult::Finished(CharLiteral{ value: None, pos: StringPosition::from2(self.start_pos, pos) });
                                }

                                match parser.input(ch, (self.escape_start_pos, pos), messages) {
                                    EscapeCharParserResult::WantMore => {      // continue,
                                        coverage_recorder.insert(1);                // C1, in first char as unicode escape, continue
                                    },    
                                    EscapeCharParserResult::Failed => {        // Invalid unicode escape, message already emitted, return INVALID_CHAR
                                        self.buf = Some(INVALID_CHAR);              // start waiting for '
                                        self.has_failed = true;
                                        need_reset_parser = true;
                                        coverage_recorder.insert(2);                // C2, in first char as unicode escape, 
                                    }
                                    EscapeCharParserResult::Success(ch) => {
                                        self.buf = Some(ch);                        // Success, waiting for '
                                        need_reset_parser = true;
                                        coverage_recorder.insert(3);                // C3, in first char as unicode escape, success
                                    }
                                }
                                // WantMore  // wait to reset until out of match self.escape_parser
                            }
                            None => {   // another '$
                                messages.push(Message::UnexpectedEndofFileInCharLiteral{ literal_start: self.start_pos, eof_pos: pos });
                                coverage_recorder.insert(4);                        // C4, first char is EOF
                                return CharLiteralParserResult::Finished(CharLiteral{ value: None, pos: StringPosition::from2(self.start_pos, pos) });
                            }
                        }
                    }
                    None => {   // Not parsing unicode escape char
                        match ch {
                            Some('\'') => { 
                                messages.push(Message::EmptyCharLiteral{ pos: self.start_pos });            
                                coverage_recorder.insert(5);                        // C5, empty
                                return CharLiteralParserResult::Finished(CharLiteral{ value: None, pos: StringPosition::from2(self.start_pos, pos) });
                            }
                            None => {
                                coverage_recorder.insert(6);                        // C6, '$, report EOF in char literal
                                messages.push(Message::UnexpectedEndofFileInCharLiteral{ literal_start: self.start_pos, eof_pos: pos });
                                return CharLiteralParserResult::Finished(CharLiteral{ value: None, pos: StringPosition::from2(self.start_pos, pos) });
                            }
                            Some(ch) => {   // other char, then
                                match (ch, pos, next_ch) {
                                    ('\\', slash_pos, Some(next_ch)) => {   // if is escape, try escape
                                        match EscapeCharParser::simple_check(next_ch) {
                                            EscapeCharSimpleCheckResult::Normal(ch) => {
                                                self.buf = Some(ch);
                                                if ch == '\'' {
                                                    self.is_very_special_that_previous_is_single_quote_escape_and_not_require_skipped = true;
                                                    return CharLiteralParserResult::WantMore;
                                                }
                                                coverage_recorder.insert(7);        // C7, normal simple escape
                                                return CharLiteralParserResult::WantMoreWithSkip1;
                                            }
                                            EscapeCharSimpleCheckResult::Invalid(ch) => {
                                                messages.push(Message::UnrecognizedEscapeCharInStringLiteral {
                                                    literal_start: self.start_pos, 
                                                    unrecogonize_pos: slash_pos, 
                                                    unrecogonize_escape: ch });
                                                self.buf = Some(INVALID_CHAR);
                                                self.has_failed = true;
                                                coverage_recorder.insert(8);        // C8, invalid simple escape
                                                return CharLiteralParserResult::WantMoreWithSkip1;
                                            }
                                            EscapeCharSimpleCheckResult::Unicode(parser) => { 
                                                self.escape_start_pos = slash_pos;
                                                self.escape_parser = Some(parser);
                                                coverage_recorder.insert(9);        // C9, start unicode parser for first char
                                                return CharLiteralParserResult::WantMoreWithSkip1;
                                            }
                                        }
                                    }
                                    ('\\', pos, None) => { 
                                        messages.push(Message::UnexpectedEndofFileInCharLiteral{ literal_start: self.start_pos, eof_pos: pos });
                                        coverage_recorder.insert(10);               // C10, '\$
                                        return CharLiteralParserResult::Finished(CharLiteral{ value: None, pos: StringPosition::from2(self.start_pos, pos) });
                                    }
                                    (ch, _pos, _1) => {
                                        self.buf = Some(ch);   
                                        coverage_recorder.insert(11);               // C11, most normal a char
                                        return CharLiteralParserResult::WantMore;
                                    }
                                }
                            }
                        }
                    }
                }
                if need_reset_parser {
                    self.escape_parser = None;
                    coverage_recorder.insert(12);                                   // C12, reset escape parser, only and must with C2, C3
                }
                coverage_recorder.insert(13);                                       // C13, only and must with C1, C2, C3
                return CharLiteralParserResult::WantMore;
            }
            Some(buf) => {  // Already processed first char
                // No possibility for a unicode parser here, just wait for a ', if not, report too long, too long, too long
                match ch {
                    None => {
                        if self.has_too_longed {
                            messages.pop();  // if previously there is too long, remove it
                        }
                        messages.push(Message::UnexpectedEndofFileInCharLiteral{ literal_start: self.start_pos, eof_pos: pos });
                        coverage_recorder.insert(14);                               // C14, 'ABCD$
                                        return CharLiteralParserResult::Finished(CharLiteral{ value: None, pos: StringPosition::from2(self.start_pos, pos) });
                    }
                    Some(ch) => {
                        if ch == '\'' { // Normally successed
                            coverage_recorder.insert(15);                           // C15, most normal finish
                            return CharLiteralParserResult::Finished(CharLiteral{ 
                                value: if !self.has_failed && !self.has_too_longed { Some(buf) } else { None }, 
                                pos: StringPosition::from2(self.start_pos, pos)   // any one of them is failed 
                            });
                        } else {
                            if !self.has_too_longed { // 'AB... is too long, report only once
                                coverage_recorder.insert(16);                       // C16, too long and report
                                messages.push(Message::CharLiteralTooLong{ start_pos: self.start_pos });
                                self.has_too_longed = true;
                                self.buf = Some(INVALID_CHAR); // if too longed, devalidate the buffer
                            }
                            coverage_recorder.insert(17);                           // C17, too long and return
                            return CharLiteralParserResult::WantMore;
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
    #[allow(unused_mut)]
    fn char_lit_parser() {
        use common::From2;
        use common::Position;
        use common::StringPosition;
        use message::Message;
        use message::MessageEmitter;
        use super::CharLiteral;
        use super::CharLiteralParser;
        use super::CharLiteralParserResult::*;

        use std::collections::HashSet;

        let spec_pos1 = Position::from2(12, 34);
        let spec_pos2 = Position::from2(56, 78);
        let spec_pos3 = Position::from2(910, 11);
        let spec_pos4 = Position::from2(12, 13);
        let spec_pos5 = Position::from2(14, 15);

        const INVALID_CHAR: char = '\u{FFFE}';
        let mut all_counter = HashSet::<i32>::new();

        macro_rules! counter_expect {
            ($current_counter: expr, $all_counter: expr, [$($c: expr)*]) => ({
                let mut expect_counter = HashSet::<i32>::new();
                $(
                    expect_counter.insert($c);
                )*
                assert!($current_counter.difference(&expect_counter).collect::<HashSet<&i32>>().is_empty(),
                    format!("Current counter: {:?}, expect_counter: {:?}", $current_counter, expect_counter));
                $all_counter = $all_counter.union(&expect_counter).map(|x| *x).collect::<HashSet<i32>>();
            })
        }

        macro_rules! messages_expect {
            ($result_messages: expr, [$($e: expr)*]) => ({
                let expect_messages = &mut MessageEmitter::new();
                $(
                    expect_messages.push($e);
                )*
                assert_eq!($result_messages, expect_messages);
            })
        }

        {   // 'A', normal, C11, C15
            let mut parser = CharLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let current_counter = &mut HashSet::<i32>::new();

            assert_eq!(parser.input(Some('A'), spec_pos2, Some('\''), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('\''), spec_pos3, Some('$'), messages, current_counter), 
                Finished(CharLiteral{ value: Some('A'), pos: StringPosition::from2(spec_pos1, spec_pos3) }));
            
            messages_expect!(messages, []);
            counter_expect!(current_counter, all_counter, [11 15]);
        }

        {   // '\t', normal escape
            let mut parser = CharLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let current_counter = &mut HashSet::<i32>::new();

            assert_eq!(parser.input(Some('\\'), spec_pos3, Some('t'), messages, current_counter), WantMoreWithSkip1);
            assert_eq!(parser.input(Some('\''), spec_pos4, Some('$'), messages, current_counter), 
                Finished(CharLiteral{ value: Some('\t'), pos: StringPosition::from2(spec_pos1, spec_pos4) }));
            
            messages_expect!(messages, []);
            counter_expect!(current_counter, all_counter, [7 15]);
        } 

        {   // '\uABCD', normal unicode escape
            let mut parser = CharLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let current_counter = &mut HashSet::<i32>::new();

            assert_eq!(parser.input(Some('\\'), spec_pos3, Some('u'), messages, current_counter), WantMoreWithSkip1);
            assert_eq!(parser.input(Some('A'), spec_pos3, Some('B'), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('B'), spec_pos3, Some('C'), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('C'), spec_pos5, Some('D'), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('D'), spec_pos3, Some('\''), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('\''), spec_pos4, Some('$'), messages, current_counter), 
                Finished(CharLiteral{ value: Some('\u{ABCD}'), pos: StringPosition::from2(spec_pos1, spec_pos4) }));
            
            messages_expect!(messages, []);
            counter_expect!(current_counter, all_counter, [9 1 3 12 13 15]);
        }

        {   // '', empty
            let mut parser = CharLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let current_counter = &mut HashSet::<i32>::new();

            assert_eq!(parser.input(Some('\''), spec_pos2, Some('$'), messages, current_counter), 
                Finished(CharLiteral{ value: None, pos: StringPosition::from2(spec_pos1, spec_pos2) }));

            messages_expect!(messages, [Message::EmptyCharLiteral{ pos: spec_pos1 }]);
            counter_expect!(current_counter, all_counter, [5]);
        }

        {   // 'ABC', normal too long, ask if is string literal
            let mut parser = CharLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let current_counter = &mut HashSet::<i32>::new();

            assert_eq!(parser.input(Some('A'), spec_pos3, Some('B'), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('B'), spec_pos3, Some('C'), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('C'), spec_pos5, Some('\''), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('\''), spec_pos4, Some('$'), messages, current_counter), 
                Finished(CharLiteral{ value: None, pos: StringPosition::from2(spec_pos1, spec_pos4) }));
            
            messages_expect!(messages, [Message::CharLiteralTooLong{ start_pos: spec_pos1 }]);
            counter_expect!(current_counter, all_counter, [11 15 16 17]);
        }

        {   // '\a', other simple escape
            let mut parser = CharLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let current_counter = &mut HashSet::<i32>::new();

            assert_eq!(parser.input(Some('\\'), spec_pos3, Some('a'), messages, current_counter), WantMoreWithSkip1);
            assert_eq!(parser.input(Some('\''), spec_pos4, Some('$'), messages, current_counter), 
                Finished(CharLiteral{ value: None, pos: StringPosition::from2(spec_pos1, spec_pos4) }));
            
            messages_expect!(messages, [Message::UnrecognizedEscapeCharInStringLiteral{ 
                                                    literal_start: spec_pos1, 
                                                    unrecogonize_pos: spec_pos3, 
                                                    unrecogonize_escape: 'a' }]);
            counter_expect!(current_counter, all_counter, [8 15]);
        }

        {   // '\uBG', unexpect char in unicode escape, unexpected EOL in unicode escape
            let mut parser = CharLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let current_counter = &mut HashSet::<i32>::new();

            assert_eq!(parser.input(Some('\\'), spec_pos3, Some('u'), messages, current_counter), WantMoreWithSkip1);
            assert_eq!(parser.input(Some('B'), spec_pos4, Some('G'), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('G'), spec_pos5, Some('\''), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('\''), spec_pos2, Some('$'), messages, current_counter), 
                Finished(CharLiteral{ value: None, pos: StringPosition::from2(spec_pos1, spec_pos2) }));
            
            messages_expect!(messages, [
                Message::UnexpectedCharInUnicodeCharEscape{ 
                    escape_start: spec_pos3,
                    unexpected_char_pos: spec_pos5,
                    unexpected_char: 'G' }
                Message::UnexpectedCharLiteralEndInUnicodeCharEscape {
                    literal_start: spec_pos1,
                    escape_start: spec_pos3,
                    unexpected_end_pos: spec_pos2 }
            ]);
            counter_expect!(current_counter, all_counter, [9 1 13 8 18]);
        }

        {   // '\U0011ABCD', unicode escape error 2
            let mut parser = CharLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let current_counter = &mut HashSet::<i32>::new();

            assert_eq!(parser.input(Some('\\'), spec_pos3, Some('U'), messages, current_counter), WantMoreWithSkip1);
            assert_eq!(parser.input(Some('0'), spec_pos4, Some('0'), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('0'), spec_pos5, Some('1'), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('1'), spec_pos4, Some('1'), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('1'), spec_pos5, Some('A'), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('A'), spec_pos4, Some('B'), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('B'), spec_pos5, Some('C'), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('C'), spec_pos4, Some('D'), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('D'), spec_pos5, Some('\''), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('\''), spec_pos2, Some('$'), messages, current_counter), 
                Finished(CharLiteral{ value: None, pos: StringPosition::from2(spec_pos1, spec_pos2) }));
            
            messages_expect!(messages, [
                Message::IncorrectUnicodeCharEscapeValue{ 
                    escape_start: spec_pos3,
                    raw_value: "0011ABCD".to_owned() }
            ]);
            counter_expect!(current_counter, all_counter, [9 1 13 2 12 15]);
        }

        {   // '\na', too long after simple escape
            let mut parser = CharLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let current_counter = &mut HashSet::<i32>::new();

            assert_eq!(parser.input(Some('\\'), spec_pos3, Some('n'), messages, current_counter), WantMoreWithSkip1);
            assert_eq!(parser.input(Some('a'), spec_pos4, Some('\''), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('\''), spec_pos2, Some('$'), messages, current_counter), 
                Finished(CharLiteral{ value: None, pos: StringPosition::from2(spec_pos1, spec_pos2) }));
            
            messages_expect!(messages, [Message::CharLiteralTooLong{ start_pos: spec_pos1 }]);
            counter_expect!(current_counter, all_counter, [7 16 17 15]);
        }

        {   // '\uABCDA', too long after unicode escape
            let mut parser = CharLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let current_counter = &mut HashSet::<i32>::new();

            assert_eq!(parser.input(Some('\\'), spec_pos3, Some('u'), messages, current_counter), WantMoreWithSkip1);
            assert_eq!(parser.input(Some('A'), spec_pos4, Some('B'), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('B'), spec_pos4, Some('C'), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('C'), spec_pos4, Some('D'), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('D'), spec_pos4, Some('A'), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('A'), spec_pos4, Some('\''), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('\''), spec_pos2, Some('$'), messages, current_counter), 
                Finished(CharLiteral{ value: None, pos: StringPosition::from2(spec_pos1, spec_pos2) }));
            
            messages_expect!(messages, [Message::CharLiteralTooLong{ start_pos: spec_pos1 }]);
            counter_expect!(current_counter, all_counter, [9 1 12 3 13 16 17 15]);
        }

        {   // '$, EOF 
            let mut parser = CharLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let current_counter = &mut HashSet::<i32>::new();

            assert_eq!(parser.input(None, spec_pos3, None, messages, current_counter),
                Finished(CharLiteral{ value: None, pos: StringPosition::from2(spec_pos1, spec_pos3) }));
            
            messages_expect!(messages, [Message::UnexpectedEndofFileInCharLiteral{ literal_start: spec_pos1, eof_pos: spec_pos3 }]);
            counter_expect!(current_counter, all_counter, [6]);
        }

        {   // '\$
            let mut parser = CharLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let current_counter = &mut HashSet::<i32>::new();

            assert_eq!(parser.input(Some('\\'), spec_pos3, None, messages, current_counter), 
                Finished(CharLiteral{ value: None, pos: StringPosition::from2(spec_pos1, spec_pos3) }));
            
            messages_expect!(messages, [Message::UnexpectedEndofFileInCharLiteral{ literal_start: spec_pos1, eof_pos: spec_pos3 }]);
            counter_expect!(current_counter, all_counter, [10]);
        }

        {   // '\u$', EOF in unicode escape
            let mut parser = CharLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let current_counter = &mut HashSet::<i32>::new();

            assert_eq!(parser.input(Some('\\'), spec_pos3, Some('u'), messages, current_counter), WantMoreWithSkip1);
            assert_eq!(parser.input(None, spec_pos4, None, messages, current_counter), 
                Finished(CharLiteral{ value: None, pos: StringPosition::from2(spec_pos1, spec_pos4) }));
            
            messages_expect!(messages, [Message::UnexpectedEndofFileInCharLiteral{ literal_start: spec_pos1, eof_pos: spec_pos4 }]);
            counter_expect!(current_counter, all_counter, [9 4]);
        }

        {   // 'A$, normal and EOF
            let mut parser = CharLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let current_counter = &mut HashSet::<i32>::new();

            assert_eq!(parser.input(Some('A'), spec_pos3, None, messages, current_counter), WantMore);
            assert_eq!(parser.input(None, spec_pos4, None, messages, current_counter), 
                Finished(CharLiteral{ value: None, pos: StringPosition::from2(spec_pos1, spec_pos4) }));
            
            messages_expect!(messages, [Message::UnexpectedEndofFileInCharLiteral{ literal_start: spec_pos1, eof_pos: spec_pos4 }]);
            counter_expect!(current_counter, all_counter, [11 14]); 
        }

        {   // 'ABC$, too long and EOF and remove too long, do no revert because don't known where to revert
            let mut parser = CharLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let current_counter = &mut HashSet::<i32>::new();

            assert_eq!(parser.input(Some('A'), spec_pos3, Some('B'), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('B'), spec_pos3, Some('C'), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('C'), spec_pos5, None, messages, current_counter), WantMore);
            assert_eq!(parser.input(None, spec_pos4, None, messages, current_counter), 
                Finished(CharLiteral{ value: None, pos: StringPosition::from2(spec_pos1, spec_pos4) }));
            
            messages_expect!(messages, [Message::UnexpectedEndofFileInCharLiteral{ literal_start: spec_pos1, eof_pos: spec_pos4 }]);
            counter_expect!(current_counter, all_counter, [11 16 17 14]); 
        }

        {   // '\'AB$, when meet too long A and find buf is \\, so return invalid char literal and continue other v1
            let mut parser = CharLiteralParser::new(spec_pos1);
            let messages = &mut MessageEmitter::new();
            let current_counter = &mut HashSet::<i32>::new();

            assert_eq!(parser.input(Some('\\'), spec_pos3, Some('\''), messages, current_counter), WantMore);
            assert_eq!(parser.input(Some('\''), spec_pos3, Some('A'), messages, current_counter), 
                Finished(CharLiteral{ value: None, pos: StringPosition::from2(spec_pos1, spec_pos3) }));
            
            messages_expect!(messages, [Message::InvalidEscapeInCharLiteral{ start_pos: spec_pos1 }]);
            counter_expect!(current_counter, all_counter, []); 
        }

        // counter_expect!(all_counter, all_counter, [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17]);
        let mut sorted_all_counter = all_counter.into_iter().collect::<Vec<i32>>();
        sorted_all_counter.sort();
        // perrorln!("All counter is {:?}", sorted_all_counter);
        assert_eq!(sorted_all_counter, vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18]);
    }
}
