
// Char literal parser

use std::cell::Cell;

use codepos::Position;
use codepos::StringPosition;

use message::Message;
use message::MessageCollection;

use codemap::EOFCHAR;

use super::error_strings;
use super::escape_char_parser::EscapeCharParser;
use super::escape_char_parser::EscapeCharSimpleCheckResult;
use super::escape_char_parser::EscapeCharParserResult;

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

pub struct CharLiteralParser {
    start_pos: Position,
    state: Cell<ParserState>,
    has_failed: bool,
    prepare_to_too_long: bool,
    escape_parser: Option<EscapeCharParser>,
    escape_start_pos: Position,
}

#[cfg(test)]
#[derive(Eq, PartialEq, Debug)]
pub enum CharLiteralParserResult {
    WantMore,
    WantMoreWithSkip1,
    Finished(Option<char>, StringPosition),
}
#[cfg(not(test))]
pub enum CharLiteralParserResult {
    WantMore,
    WantMoreWithSkip1,
    Finished(Option<char>, StringPosition),
}

#[derive(Copy, Clone)]
enum ParserState {
    ExpectFirst, 
    ExpectEnd(Option<char>),
}

impl CharLiteralParser {

    pub fn new(start_pos: Position) -> CharLiteralParser {
        CharLiteralParser{ 
            start_pos: start_pos, 
            state: Cell::new(ParserState::ExpectFirst),
            has_failed: false,
            prepare_to_too_long: false,
            escape_parser: None, 
            escape_start_pos: Position::new(),
        }
    }

    //            self, current char, current char pos, next char preview
    pub fn input(&mut self, ch: char, pos: Position, next_ch: char, messages: &mut MessageCollection, 
        coverage_recorder: &mut CoverageRecorder) -> CharLiteralParserResult {

        match self.state.get() {
            ParserState::ExpectFirst => {       // Waiting for first char
                let mut need_reset_parser = false;
                let mut need_set_parser = false;
                let mut need_set_parser_value = None;
                match (&mut self.escape_parser, ch, pos, next_ch) {
                    (&mut Some(_), EOFCHAR, _2, _3) => {  // another 'u123$
                        messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![
                            (StringPosition::double(self.start_pos), error_strings::CharLiteralStartHere),
                            (StringPosition::double(pos), error_strings::EOFHere)
                        ]));
                        coverage_recorder.insert(4);                        // C4, first char is EOF
                        return CharLiteralParserResult::Finished(None, StringPosition::from2(self.start_pos, pos));
                    }
                    (&mut Some(ref mut parser), ch, _2, _3) => {
                        if ch == '\'' { // '\'' should report unexpected EOL
                            coverage_recorder.insert(18);
                            messages.push(Message::with_help_by_str(error_strings::UnexpectedCharLiteralEnd, vec![
                                (StringPosition::double(self.start_pos), error_strings::CharLiteralStartHere),
                                (StringPosition::double(pos), error_strings::CharLiteralEndHere),
                            ], vec![
                                error_strings::UnicodeCharEscapeHelpSyntax
                            ]));
                            return CharLiteralParserResult::Finished(None, StringPosition::from2(self.start_pos, pos));
                        }

                        match parser.input(ch, (self.escape_start_pos, pos), messages) {
                            EscapeCharParserResult::WantMore => { 
                                coverage_recorder.insert(1);                // C1, in first char as unicode escape, continue
                            },    
                            EscapeCharParserResult::Failed => {        // Invalid unicode escape, message already emitted, return INVALID_CHAR    
                                self.state.set(ParserState::ExpectEnd(None));
                                self.has_failed = true;
                                need_reset_parser = true;
                                coverage_recorder.insert(2);                // C2, in first char as unicode escape, 
                            }
                            EscapeCharParserResult::Success(ch) => {
                                self.state.set(ParserState::ExpectEnd(Some(ch)));   // Success, waiting for '
                                need_reset_parser = true;
                                coverage_recorder.insert(3);                // C3, in first char as unicode escape, success
                            }
                        }
                        // WantMore  // wait to reset until out of match self.escape_parser
                    }
                    (&mut None, '\'', _2, _3) => { 
                        coverage_recorder.insert(5);                        // C5, empty
                        messages.push(Message::with_help_by_str(error_strings::EmptyCharLiteral, vec![
                            (StringPosition::double(self.start_pos), error_strings::CharLiteralStartHere),
                        ], vec![
                            error_strings::CharLiteralSyntaxHelp1
                        ]));
                        return CharLiteralParserResult::Finished(None, StringPosition::from2(self.start_pos, pos));
                    }
                    (&mut None, EOFCHAR, _2, _3) => {
                        coverage_recorder.insert(6);                        // C6, '$, report EOF in char literal
                        messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![
                            (StringPosition::double(self.start_pos), error_strings::CharLiteralStartHere),
                            (StringPosition::double(pos), error_strings::EOFHere)
                        ]));
                        return CharLiteralParserResult::Finished(None, StringPosition::from2(self.start_pos, pos));
                    }
                    (&mut None, '\\', pos, EOFCHAR) => { 
                        messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![
                            (StringPosition::double(self.start_pos), error_strings::CharLiteralStartHere),
                            (StringPosition::double(pos.next_col()), error_strings::EOFHere)
                        ]));
                        coverage_recorder.insert(10);               // C10, '\$
                        return CharLiteralParserResult::Finished(None, StringPosition::from2(self.start_pos, pos.next_col()));
                    }
                    (&mut None, '\\', slash_pos, next_ch) => {   // if is escape, try escape
                        match EscapeCharParser::simple_check(next_ch) {
                            EscapeCharSimpleCheckResult::Normal(ch) => {
                                self.state.set(ParserState::ExpectEnd(Some(ch)));
                                coverage_recorder.insert(7);        // C7, normal simple escape
                                return CharLiteralParserResult::WantMoreWithSkip1;
                            }
                            EscapeCharSimpleCheckResult::Invalid(ch) => {
                                messages.push(Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, ch), vec![
                                    (StringPosition::double(self.start_pos), error_strings::CharLiteralStartHere.to_owned()),
                                    (StringPosition::double(slash_pos), error_strings::UnknownCharEscapeHere.to_owned()),
                                ]));
                                self.state.set(ParserState::ExpectEnd(None));
                                self.has_failed = true;
                                coverage_recorder.insert(8);        // C8, invalid simple escape
                                return CharLiteralParserResult::WantMoreWithSkip1;
                            }
                            EscapeCharSimpleCheckResult::Unicode(parser) => { 
                                self.escape_start_pos = slash_pos;
                                need_set_parser = true;
                                need_set_parser_value = Some(parser);
                                coverage_recorder.insert(9);        // C9, start unicode parser for first char
                            }
                        }
                    }
                    (&mut None, ch, _2, _3) => {
                        self.state.set(ParserState::ExpectEnd(Some(ch)));
                        coverage_recorder.insert(11);               // C11, most normal a char
                        return CharLiteralParserResult::WantMore;
                    }
                }
                if need_reset_parser {
                    self.escape_parser = None;
                    coverage_recorder.insert(12);                                   // C12, reset escape parser, only and must with C2, C3
                }
                if need_set_parser {  // C9 fix 
                    self.escape_parser = need_set_parser_value;
                    return CharLiteralParserResult::WantMoreWithSkip1;
                }
                coverage_recorder.insert(13);                                       // C13, only and must with C1, C2, C3
                return CharLiteralParserResult::WantMore;
            }
            ParserState::ExpectEnd(maybe_result) => {  // Already processed first char
                // No possibility for a unicode parser here, just wait for a ', if not, report too long
                match ch {
                    EOFCHAR => {
                        messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![
                            (StringPosition::double(self.start_pos), error_strings::CharLiteralStartHere),
                            (StringPosition::double(pos), error_strings::EOFHere)
                        ]));
                        coverage_recorder.insert(14);                               // C14, 'ABCD$
                        return CharLiteralParserResult::Finished(None, StringPosition::from2(self.start_pos, pos));
                    }
                    '\'' => { // Normally successed
                        coverage_recorder.insert(15);                           // C15, most normal finish
                        if self.prepare_to_too_long {
                            coverage_recorder.insert(19);                       // C19, actual report too long
                            messages.push(Message::new_by_str(error_strings::CharLiteralTooLong, vec![
                                (StringPosition::double(self.start_pos), error_strings::CharLiteralStartHere),
                                (StringPosition::double(pos), error_strings::CharLiteralEndHere),
                            ]));
                        }
                        return CharLiteralParserResult::Finished(
                            if !self.has_failed && !self.prepare_to_too_long { maybe_result } else { None }, 
                            StringPosition::from2(self.start_pos, pos)   // any one of them is failed 
                        );
                    }
                    _ => {
                        if !self.prepare_to_too_long { // 'AB... is too long, report only once
                            coverage_recorder.insert(16);                       // C16, too long and prepare to report
                            self.prepare_to_too_long = true;
                            self.has_failed = true;     // if too longed, devalidate the buffer
                        }
                        coverage_recorder.insert(17);                           // C17, too long and return
                        return CharLiteralParserResult::WantMore;
                    }
                }
            }
        }
    } 
}


#[cfg(test)]
#[test]
#[allow(unused_mut)] // for all_counter, don't know what rustc is thinking
fn char_lit_parser() {
    use self::CharLiteralParserResult::*;

    let spec_pos1 = make_pos!(12, 34);
    let spec_pos2 = make_pos!(56, 78);
    let spec_pos3 = make_pos!(910, 11);
    let spec_pos4 = make_pos!(12, 13);
    let spec_pos5 = make_pos!(14, 15);

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
            let expect_messages = &mut MessageCollection::new();
            $(
                expect_messages.push($e);
            )*
            assert_eq!($result_messages, expect_messages);
        })
    }

    {   // 'A', normal, C11, C15
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let current_counter = &mut HashSet::<i32>::new();

        assert_eq!(parser.input('A', spec_pos2, '\'', messages, current_counter), WantMore);
        assert_eq!(parser.input('\'', spec_pos3, '$', messages, current_counter), 
            Finished(Some('A'), StringPosition::from2(spec_pos1, spec_pos3)));
        
        messages_expect!(messages, []);
        counter_expect!(current_counter, all_counter, [11 15]);
    }

    {   // '\t', normal escape
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let current_counter = &mut HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, 't', messages, current_counter), WantMoreWithSkip1);
        assert_eq!(parser.input('\'', spec_pos4, '$', messages, current_counter), 
            Finished(Some('\t'), StringPosition::from2(spec_pos1, spec_pos4)));
        
        messages_expect!(messages, []);
        counter_expect!(current_counter, all_counter, [7 15]);
    } 

    {   // '\uABCD', normal unicode escape
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let current_counter = &mut HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, 'u', messages, current_counter), WantMoreWithSkip1);
        assert_eq!(parser.input('A', spec_pos3, 'B', messages, current_counter), WantMore);
        assert_eq!(parser.input('B', spec_pos3, 'C', messages, current_counter), WantMore);
        assert_eq!(parser.input('C', spec_pos5, 'D', messages, current_counter), WantMore);
        assert_eq!(parser.input('D', spec_pos3, '\'', messages, current_counter), WantMore);
        assert_eq!(parser.input('\'', spec_pos4, '$', messages, current_counter), 
            Finished(Some('\u{ABCD}'), StringPosition::from2(spec_pos1, spec_pos4)));
        
        messages_expect!(messages, []);
        counter_expect!(current_counter, all_counter, [9 1 3 12 13 15]);
    }

    {   // '', empty
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let current_counter = &mut HashSet::<i32>::new();

        assert_eq!(parser.input('\'', spec_pos2, '$', messages, current_counter), 
            Finished(None, StringPosition::from2(spec_pos1, spec_pos2)));

        messages_expect!(messages, [Message::with_help_by_str(error_strings::EmptyCharLiteral, vec![
            (StringPosition::double(spec_pos1), error_strings::CharLiteralStartHere),
        ], vec![
            error_strings::CharLiteralSyntaxHelp1
        ])]);
        counter_expect!(current_counter, all_counter, [5]);
    }

    {   // 'ABC', normal too long, ask if is string literal
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let current_counter = &mut HashSet::<i32>::new();

        assert_eq!(parser.input('A', spec_pos3, 'B', messages, current_counter), WantMore);
        assert_eq!(parser.input('B', spec_pos3, 'C', messages, current_counter), WantMore);
        assert_eq!(parser.input('C', spec_pos5, '\'', messages, current_counter), WantMore);
        assert_eq!(parser.input('\'', spec_pos4, '$', messages, current_counter), 
            Finished(None, StringPosition::from2(spec_pos1, spec_pos4)));
                                 
        messages_expect!(messages, [Message::new_by_str(error_strings::CharLiteralTooLong, vec![
            (StringPosition::double(spec_pos1), error_strings::CharLiteralStartHere),
            (StringPosition::double(spec_pos4), error_strings::CharLiteralEndHere),
        ])]);
        counter_expect!(current_counter, all_counter, [11 15 16 17 19]);
    }

    {   // '\c', other simple escape
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let current_counter = &mut HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, 'c', messages, current_counter), WantMoreWithSkip1);
        assert_eq!(parser.input('\'', spec_pos4, '$', messages, current_counter), 
            Finished(None, StringPosition::from2(spec_pos1, spec_pos4)));
        
        messages_expect!(messages, [Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, 'c'), vec![
            (StringPosition::double(spec_pos1), error_strings::CharLiteralStartHere.to_owned()),
            (StringPosition::double(spec_pos3), error_strings::UnknownCharEscapeHere.to_owned()),
        ])]);
        counter_expect!(current_counter, all_counter, [8 15]);
    }

    {   // '\'', special normal escape
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let current_counter = &mut HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, '\'', messages, current_counter), WantMoreWithSkip1);
        assert_eq!(parser.input('\'', spec_pos4, '$', messages, current_counter), 
            Finished(Some('\''), StringPosition::from2(spec_pos1, spec_pos4)));
        
        messages_expect!(messages, []);
        counter_expect!(current_counter, all_counter, [7 15]);
    }

    {   // '\uBG', unexpect char in unicode escape, unexpected EOL in unicode escape
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let current_counter = &mut HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, 'u', messages, current_counter), WantMoreWithSkip1);
        assert_eq!(parser.input('B', spec_pos4, 'G', messages, current_counter), WantMore);
        assert_eq!(parser.input('G', spec_pos5, '\'', messages, current_counter), WantMore);
        assert_eq!(parser.input('\'', spec_pos2, '$', messages, current_counter), 
            Finished(None, StringPosition::from2(spec_pos1, spec_pos2)));
        
        messages_expect!(messages, [
            Message::with_help_by_str(error_strings::InvalidUnicodeCharEscape, vec![
                (StringPosition::double(spec_pos3), error_strings::UnicodeCharEscapeStartHere),
                (StringPosition::double(spec_pos5), error_strings::UnicodeCharEscapeInvalidChar)
            ], vec![
                error_strings::UnicodeCharEscapeHelpSyntax,
            ])
            Message::with_help_by_str(error_strings::UnexpectedCharLiteralEnd, vec![
                (StringPosition::double(spec_pos1), error_strings::CharLiteralStartHere),
                (StringPosition::double(spec_pos2), error_strings::CharLiteralEndHere)
            ], vec![
                error_strings::UnicodeCharEscapeHelpSyntax
            ])
        ]);
        counter_expect!(current_counter, all_counter, [9 1 13 8 18]);
    }

    {   // '\U0011ABCD', unicode escape error 2
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let current_counter = &mut HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, 'U', messages, current_counter), WantMoreWithSkip1);
        assert_eq!(parser.input('0', spec_pos4, '0', messages, current_counter), WantMore);
        assert_eq!(parser.input('0', spec_pos5, '1', messages, current_counter), WantMore);
        assert_eq!(parser.input('1', spec_pos4, '1', messages, current_counter), WantMore);
        assert_eq!(parser.input('1', spec_pos5, 'A', messages, current_counter), WantMore);
        assert_eq!(parser.input('A', spec_pos4, 'B', messages, current_counter), WantMore);
        assert_eq!(parser.input('B', spec_pos5, 'C', messages, current_counter), WantMore);
        assert_eq!(parser.input('C', spec_pos4, 'D', messages, current_counter), WantMore);
        assert_eq!(parser.input('D', spec_pos5, '\'', messages, current_counter), WantMore);
        assert_eq!(parser.input('\'', spec_pos2, '$', messages, current_counter), 
            Finished(None, StringPosition::from2(spec_pos1, spec_pos2)));
        
        messages_expect!(messages, [
            Message::with_help(error_strings::InvalidUnicodeCharEscape.to_owned(), vec![
                (StringPosition::double(spec_pos3), error_strings::UnicodeCharEscapeStartHere.to_owned()),
            ], vec![
                format!("{}{}", error_strings::UnicodeCharEscapeCodePointValueIs, "0011ABCD"),
                error_strings::UnicodeCharEscapeHelpValue.to_owned(),
            ])
        ]);
        counter_expect!(current_counter, all_counter, [9 1 13 2 12 15]);
    }

    {   // '\na', too long after simple escape
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let current_counter = &mut HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, 'n', messages, current_counter), WantMoreWithSkip1);
        assert_eq!(parser.input('a', spec_pos4, '\'', messages, current_counter), WantMore);
        assert_eq!(parser.input('\'', spec_pos2, '$', messages, current_counter), 
            Finished(None, StringPosition::from2(spec_pos1, spec_pos2)));
        
        messages_expect!(messages, [Message::new_by_str(error_strings::CharLiteralTooLong, vec![
            (StringPosition::double(spec_pos1), error_strings::CharLiteralStartHere),
            (StringPosition::double(spec_pos2), error_strings::CharLiteralEndHere),
        ])]);
        counter_expect!(current_counter, all_counter, [7 16 17 15 19]);
    }

    {   // '\uABCDA', too long after unicode escape
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let current_counter = &mut HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, 'u', messages, current_counter), WantMoreWithSkip1);
        assert_eq!(parser.input('A', spec_pos4, 'B', messages, current_counter), WantMore);
        assert_eq!(parser.input('B', spec_pos4, 'C', messages, current_counter), WantMore);
        assert_eq!(parser.input('C', spec_pos4, 'D', messages, current_counter), WantMore);
        assert_eq!(parser.input('D', spec_pos4, 'A', messages, current_counter), WantMore);
        assert_eq!(parser.input('A', spec_pos4, '\'', messages, current_counter), WantMore);
        assert_eq!(parser.input('\'', spec_pos2, '$', messages, current_counter), 
            Finished(None, StringPosition::from2(spec_pos1, spec_pos2)));
        
        messages_expect!(messages, [Message::new_by_str(error_strings::CharLiteralTooLong, vec![
            (StringPosition::double(spec_pos1), error_strings::CharLiteralStartHere),
            (StringPosition::double(spec_pos2), error_strings::CharLiteralEndHere),
        ])]);
        counter_expect!(current_counter, all_counter, [9 1 12 3 13 16 17 15 19]);
    }

    {   // '$, EOF 
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let current_counter = &mut HashSet::<i32>::new();

        assert_eq!(parser.input(EOFCHAR, spec_pos3, EOFCHAR, messages, current_counter),
            Finished(None, StringPosition::from2(spec_pos1, spec_pos3)));
        
        messages_expect!(messages, [Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (StringPosition::double(spec_pos1), error_strings::CharLiteralStartHere),
            (StringPosition::double(spec_pos3), error_strings::EOFHere)
        ])]);
        counter_expect!(current_counter, all_counter, [6]);
    }

    {   // '\$
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let current_counter = &mut HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, EOFCHAR, messages, current_counter), 
            Finished(None, StringPosition::from2(spec_pos1, spec_pos3.next_col())));
        
        messages_expect!(messages, [Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (StringPosition::double(spec_pos1), error_strings::CharLiteralStartHere),
            (StringPosition::double(spec_pos3.next_col()), error_strings::EOFHere)
        ])]);
        counter_expect!(current_counter, all_counter, [10]);
    }

    {   // '\u$', EOF in unicode escape
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let current_counter = &mut HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, 'u', messages, current_counter), WantMoreWithSkip1);
        assert_eq!(parser.input(EOFCHAR, spec_pos4, EOFCHAR, messages, current_counter), 
            Finished(None, StringPosition::from2(spec_pos1, spec_pos4)));
        
        messages_expect!(messages, [Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (StringPosition::double(spec_pos1), error_strings::CharLiteralStartHere),
            (StringPosition::double(spec_pos4), error_strings::EOFHere)
        ])]);
        counter_expect!(current_counter, all_counter, [9 4]);
    }

    {   // 'A$, normal and EOF
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let current_counter = &mut HashSet::<i32>::new();

        assert_eq!(parser.input('A', spec_pos3, EOFCHAR, messages, current_counter), WantMore);
        assert_eq!(parser.input(EOFCHAR, spec_pos4, EOFCHAR, messages, current_counter), 
            Finished(None, StringPosition::from2(spec_pos1, spec_pos4)));
        
        messages_expect!(messages, [Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (StringPosition::double(spec_pos1), error_strings::CharLiteralStartHere),
            (StringPosition::double(spec_pos4), error_strings::EOFHere)
        ])]);
        counter_expect!(current_counter, all_counter, [11 14]); 
    }

    {   // 'ABC$, too long and EOF and remove too long, do no revert because don't known where to revert
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let current_counter = &mut HashSet::<i32>::new();

        assert_eq!(parser.input('A', spec_pos3, 'B', messages, current_counter), WantMore);
        assert_eq!(parser.input('B', spec_pos3, 'C', messages, current_counter), WantMore);
        assert_eq!(parser.input('C', spec_pos5, EOFCHAR, messages, current_counter), WantMore);
        assert_eq!(parser.input(EOFCHAR, spec_pos4, EOFCHAR, messages, current_counter), 
            Finished(None, StringPosition::from2(spec_pos1, spec_pos4)));
        
        messages_expect!(messages, [Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (StringPosition::double(spec_pos1), error_strings::CharLiteralStartHere),
            (StringPosition::double(spec_pos4), error_strings::EOFHere)
        ])]);
        counter_expect!(current_counter, all_counter, [11 16 17 14]); 
    }

    {   // '\'AB$, this case is abandon: // when meet too long A and find buf is \\, so return invalid char literal and continue other v1
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let current_counter = &mut HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, '\'', messages, current_counter), WantMoreWithSkip1);
        assert_eq!(parser.input('A', spec_pos3, 'B', messages, current_counter), WantMore);
        assert_eq!(parser.input('B', spec_pos4, EOFCHAR, messages, current_counter), WantMore);
        assert_eq!(parser.input(EOFCHAR, spec_pos5, EOFCHAR, messages, current_counter),
            Finished(None, StringPosition::from2(spec_pos1, spec_pos5)));
        
        messages_expect!(messages, [Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (StringPosition::double(spec_pos1), error_strings::CharLiteralStartHere),
            (StringPosition::double(spec_pos5), error_strings::EOFHere),
        ])]);
        counter_expect!(current_counter, all_counter, [7 16 17 14]); 
    }

    let mut sorted_all_counter = all_counter.into_iter().collect::<Vec<i32>>();
    sorted_all_counter.sort();
    assert_eq!(sorted_all_counter, vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19]);
}