///! fff-lang
///!
///! Character literal parser

use std::cell::Cell;
use crate::codemap::{CharPos, Span, EOF_CHAR};
use crate::diagnostics::{Message, MessageCollection};
use super::error_strings;
use super::escape_char_parser::{EscapeCharParser, EscapeCharSimpleCheckResult, EscapeCharParserResult};

#[cfg(feature = "trace_char_lit_parse")]
use std::collections::HashSet;
#[cfg(feature = "trace_char_lit_parse")]
type CoverageRecorder = HashSet<i32>;

#[cfg(not(feature = "trace_char_lit_parse"))]
struct CoverageRecorder(i32);
#[cfg(not(feature = "trace_char_lit_parse"))]
impl CoverageRecorder {
    pub fn new() -> CoverageRecorder { CoverageRecorder(0) }
    pub fn insert(&mut self, v: i32) { self.0 = v } 
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub enum CharLiteralParserResult {
    WantMore,
    WantMoreWithSkip1,
    Finished(Option<char>, Span),
}
#[derive(Copy, Clone)]
enum ParserState {
    ExpectFirst, 
    ExpectEnd(Option<char>),
}

pub struct CharLiteralParser {
    current_span: Span,
    state: Cell<ParserState>,
    has_failed: bool,
    prepare_to_too_long: bool,
    escape_parser: Option<EscapeCharParser>,
    escape_start_pos: CharPos,
    coverage_recorder: CoverageRecorder,
}
impl CharLiteralParser {

    pub fn new(start_pos: CharPos) -> CharLiteralParser {
        CharLiteralParser{ 
            current_span: start_pos.as_span(),
            state: Cell::new(ParserState::ExpectFirst),
            has_failed: false,
            prepare_to_too_long: false,
            escape_parser: None, 
            escape_start_pos: CharPos::default(),
            coverage_recorder: CoverageRecorder::new(),
        }
    }

    //            self, current char, current char pos, next char preview
    pub fn input(&mut self, ch: char, pos: CharPos, next_ch: char, messages: &mut MessageCollection) -> CharLiteralParserResult {
        #[cfg(feature = "trace_char_lit_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[CharLitParser: {}] ", line!()); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_char_lit_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        match self.state.get() {
            ParserState::ExpectFirst => {       // Waiting for first char
                trace!("expecting first");
                let mut need_reset_parser = false;
                let mut need_set_parser = false;
                let mut need_set_parser_value = None;
                match (&mut self.escape_parser, ch, pos, next_ch) {
                    (&mut Some(_), EOF_CHAR, _2, _3) => {  // another 'u123$
                        trace!("expecting first but ch = EOF, current_span = {:?}", self.current_span);
                        self.coverage_recorder.insert(4);                        // C4, first char is EOF
                        messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![
                            (self.current_span, error_strings::CharLiteralHere),
                            (pos.as_span(), error_strings::EOFHere)
                        ]));
                        return CharLiteralParserResult::Finished(None, self.current_span);
                    }
                    (&mut Some(ref mut parser), ch, _2, _3) => {
                        trace!("expecting first, ch = {:?}", ch);
                        self.current_span = self.current_span.merge(&pos.as_span());
                        if ch == '\'' { // '\'' should report unexpected EOL
                            self.coverage_recorder.insert(18);
                            messages.push(Message::with_help_by_str(error_strings::UnexpectedCharLiteralEnd, vec![
                                (self.current_span, error_strings::CharLiteralHere),
                            ], vec![
                                error_strings::UnicodeCharEscapeHelpSyntax
                            ]));
                            return CharLiteralParserResult::Finished(None, self.current_span);
                        }

                        trace!("before parser.input, current_span is {:?}", self.current_span);
                        match parser.input(ch, (self.escape_start_pos, pos), messages) {
                            EscapeCharParserResult::WantMore => { 
                                trace!("parser want more");
                                self.coverage_recorder.insert(1);                // C1, in first char as unicode escape, continue
                            },    
                            EscapeCharParserResult::Failed => {        // Invalid unicode escape, message already emitted, return INVALID_CHAR    
                                trace!("parser failed, start expect end");
                                self.state.set(ParserState::ExpectEnd(None));
                                self.has_failed = true;
                                need_reset_parser = true;
                                self.coverage_recorder.insert(2);                // C2, in first char as unicode escape, 
                            }
                            EscapeCharParserResult::Success(ch) => {
                                trace!("parser success, result: {:?}", ch);
                                self.state.set(ParserState::ExpectEnd(Some(ch)));   // Success, waiting for '
                                need_reset_parser = true;
                                self.coverage_recorder.insert(3);                // C3, in first char as unicode escape, success
                            }
                        }
                        // WantMore  // wait to reset until out of match self.escape_parser
                    }
                    (&mut None, '\'', _2, _3) => { 
                        self.coverage_recorder.insert(5);                        // C5, empty
                        let all_span = self.current_span.merge(&pos.as_span());
                        messages.push(Message::with_help_by_str(error_strings::EmptyCharLiteral, vec![
                            (all_span, error_strings::CharLiteralHere),
                        ], vec![
                            error_strings::CharLiteralSyntaxHelp1
                        ]));
                        return CharLiteralParserResult::Finished(None, all_span);
                    }
                    (&mut None, EOF_CHAR, _2, _3) => {
                        self.coverage_recorder.insert(6);                        // C6, '$, report EOF in char literal
                        messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![
                            (self.current_span, error_strings::CharLiteralHere),
                            (pos.as_span(), error_strings::EOFHere)
                        ]));
                        return CharLiteralParserResult::Finished(None, self.current_span);
                    }
                    (&mut None, '\\', pos, EOF_CHAR) => { 
                        self.coverage_recorder.insert(10);               // C10, '\$
                        let all_span = self.current_span.merge(&pos.as_span());
                        let eof_pos = pos.offset(1); // in this case, `\` is always 1 byte wide
                        messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![
                            (all_span, error_strings::CharLiteralHere),
                            (eof_pos.as_span(), error_strings::EOFHere)
                        ]));
                        return CharLiteralParserResult::Finished(None, all_span);
                    }
                    (&mut None, '\\', slash_pos, next_ch) => {   // if is escape, try escape
                        self.current_span = self.current_span.merge(
                            &Span::new(self.current_span.get_file_id(), slash_pos.get_char_id() + 1, slash_pos.get_char_id() + 1)); // `\`
                        match EscapeCharParser::simple_check(next_ch) {
                            EscapeCharSimpleCheckResult::Normal(ch) => {
                                self.state.set(ParserState::ExpectEnd(Some(ch)));
                                self.coverage_recorder.insert(7);        // C7, normal simple escape
                                return CharLiteralParserResult::WantMoreWithSkip1;
                            }
                            EscapeCharSimpleCheckResult::Invalid(ch) => {
                                messages.push(Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, ch), vec![
                                    (self.current_span.get_start_pos().as_span(), error_strings::CharLiteralStartHere.to_owned()),
                                    (slash_pos.as_span(), error_strings::UnknownCharEscapeHere.to_owned()),
                                ]));
                                self.state.set(ParserState::ExpectEnd(None));
                                self.current_span = self.current_span.merge(&pos.as_span());
                                self.has_failed = true;
                                self.coverage_recorder.insert(8);        // C8, invalid simple escape
                                return CharLiteralParserResult::WantMoreWithSkip1;
                            }
                            EscapeCharSimpleCheckResult::Unicode(parser) => { 
                                self.escape_start_pos = slash_pos;
                                need_set_parser = true;
                                need_set_parser_value = Some(parser);
                                self.coverage_recorder.insert(9);        // C9, start unicode parser for first char
                            }
                        }
                    }
                    (&mut None, ch, pos, _3) => {
                        trace!("experienced normal char, expecting end");
                        self.state.set(ParserState::ExpectEnd(Some(ch)));
                        self.current_span = self.current_span.merge(&pos.as_span());
                        self.coverage_recorder.insert(11);               // C11, most normal a char
                        return CharLiteralParserResult::WantMore;
                    }
                }
                if need_reset_parser {
                    self.escape_parser = None;
                    self.coverage_recorder.insert(12);                                   // C12, reset escape parser, only and must with C2, C3
                }
                if need_set_parser {  // C9 fix 
                    self.escape_parser = need_set_parser_value;
                    return CharLiteralParserResult::WantMoreWithSkip1;
                }
                self.coverage_recorder.insert(13);                                       // C13, only and must with C1, C2, C3
                return CharLiteralParserResult::WantMore;
            }
            ParserState::ExpectEnd(maybe_result) => {  // Already processed first char
                // No possibility for a unicode parser here, just wait for a ', if not, report too long
                match ch {
                    EOF_CHAR => {
                        trace!("meet EOF when expecting end, current_span = {:?}", self.current_span);
                        messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![
                            (self.current_span, error_strings::CharLiteralHere),
                            (pos.as_span(), error_strings::EOFHere)
                        ]));
                        self.coverage_recorder.insert(14);                               // C14, 'ABCD$
                        return CharLiteralParserResult::Finished(None, self.current_span);
                    }
                    '\'' => { // Normally successed
                        self.coverage_recorder.insert(15);                           // C15, most normal finish
                        let all_span = self.current_span.merge(&pos.as_span());
                        if self.prepare_to_too_long {
                            
                            self.coverage_recorder.insert(19);                       // C19, actual report too long
                            messages.push(Message::new_by_str(error_strings::CharLiteralTooLong, vec![
                                (all_span, error_strings::CharLiteralHere),
                            ]));
                        }
                        return CharLiteralParserResult::Finished(
                            if !self.has_failed && !self.prepare_to_too_long { maybe_result } else { None }, 
                            all_span   // any one of them is failed 
                        );
                    }
                    _ => {
                        trace!("meet other when expecting end, start prepare too long");
                        if !self.prepare_to_too_long { // 'AB... is too long, report only once
                            self.coverage_recorder.insert(16);                       // C16, too long and prepare to report
                            self.prepare_to_too_long = true;
                            self.has_failed = true;     // if too longed, devalidate the buffer
                        }
                        self.current_span = self.current_span.merge(&pos.as_span());
                        self.coverage_recorder.insert(17);                           // C17, too long and return
                        return CharLiteralParserResult::WantMore;
                    }
                }
            }
        }
    } 
}


#[cfg(feature = "trace_char_lit_parse")] #[cfg(test)] #[test]
#[allow(unused_mut)] // for all_counter, don't know what rustc is thinking
fn char_lit_parser() {
    use self::CharLiteralParserResult::*;
    // TODO: any failure about position according to v1_base

    let spec_pos1 = make_charpos!(1);
    let spec_pos2 = make_charpos!(11);
    let spec_pos3 = make_charpos!(111);
    let spec_pos4 = make_charpos!(1111);
    let spec_pos5 = make_charpos!(11111);

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

    {   // 'A', normal, C11, C15
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        parser.coverage_recorder = HashSet::<i32>::new();

        assert_eq!(parser.input('A', spec_pos2, '\'', messages), WantMore);
        assert_eq!(parser.input('\'', spec_pos3, '$', messages), 
            Finished(Some('A'), spec_pos1.merge(&spec_pos3)));

        assert_eq!(messages.is_empty(), true);
        counter_expect!(&mut parser.coverage_recorder, all_counter, [11 15]);
    }

    {   // '\t', normal escape
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        parser.coverage_recorder = HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, 't', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('\'', spec_pos4, '$', messages), 
            Finished(Some('\t'), spec_pos1.merge(&spec_pos4)));
        
        assert_eq!(messages.is_empty(), true);
        counter_expect!(&mut parser.coverage_recorder, all_counter, [7 15]);
    } 

    {   // '\uABCD', normal unicode escape
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        parser.coverage_recorder = HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, 'u', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('A', spec_pos3, 'B', messages), WantMore);
        assert_eq!(parser.input('B', spec_pos3, 'C', messages), WantMore);
        assert_eq!(parser.input('C', spec_pos5, 'D', messages), WantMore);
        assert_eq!(parser.input('D', spec_pos3, '\'', messages), WantMore);
        assert_eq!(parser.input('\'', spec_pos4, '$', messages), 
            Finished(Some('\u{ABCD}'), spec_pos1.merge(&spec_pos4)));
        
        assert_eq!(messages.is_empty(), true);
        counter_expect!(&mut parser.coverage_recorder, all_counter, [9 1 3 12 13 15]);
    }

    {   // '', empty
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        parser.coverage_recorder = HashSet::<i32>::new();

        assert_eq!(parser.input('\'', spec_pos2, '$', messages), 
            Finished(None, spec_pos1.merge(&spec_pos2)));

        assert_eq!(messages, &make_messages![Message::with_help_by_str(error_strings::EmptyCharLiteral, vec![
            (spec_pos1.merge(&spec_pos2), error_strings::CharLiteralHere),
        ], vec![
            error_strings::CharLiteralSyntaxHelp1
        ])]);
        counter_expect!(&mut parser.coverage_recorder, all_counter, [5]);
    }

    {   // 'ABC', normal too long, ask if is string literal
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        parser.coverage_recorder = HashSet::<i32>::new();

        assert_eq!(parser.input('A', spec_pos3, 'B', messages), WantMore);
        assert_eq!(parser.input('B', spec_pos3, 'C', messages), WantMore);
        assert_eq!(parser.input('C', spec_pos5, '\'', messages), WantMore);
        assert_eq!(parser.input('\'', spec_pos4, '$', messages), 
            Finished(None, spec_pos1.merge(&spec_pos4)));
                                 
        assert_eq!(messages, &make_messages![Message::new_by_str(error_strings::CharLiteralTooLong, vec![
            (spec_pos1.merge(&spec_pos4), error_strings::CharLiteralHere),
        ])]);
        counter_expect!(&mut parser.coverage_recorder, all_counter, [11 15 16 17 19]);
    }

    {   // '\c', other simple escape
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        parser.coverage_recorder = HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, 'c', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('\'', spec_pos4, '$', messages), 
            Finished(None, spec_pos1.merge(&spec_pos4)));
        
        assert_eq!(messages, &make_messages![Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, 'c'), vec![
            (spec_pos1.as_span(), error_strings::CharLiteralStartHere.to_owned()),
            (spec_pos3.as_span(), error_strings::UnknownCharEscapeHere.to_owned()),
        ])]);
        counter_expect!(&mut parser.coverage_recorder, all_counter, [8 15]);
    }

    {   // '\'', special normal escape
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        parser.coverage_recorder = HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, '\'', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('\'', spec_pos4, '$', messages), 
            Finished(Some('\''), spec_pos1.merge(&spec_pos4)));
        
        assert_eq!(messages.is_empty(), true);
        counter_expect!(&mut parser.coverage_recorder, all_counter, [7 15]);
    }

    {   // '\uBG', unexpect char in unicode escape, unexpected EOL in unicode escape
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        parser.coverage_recorder = HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, 'u', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('B', spec_pos4, 'G', messages), WantMore);
        assert_eq!(parser.input('G', spec_pos5, '\'', messages), WantMore);
        assert_eq!(parser.input('\'', spec_pos2, '$', messages), 
            Finished(None, spec_pos1.merge(&spec_pos2)));
        
        assert_eq!(messages, &make_messages![
            Message::with_help_by_str(error_strings::InvalidUnicodeCharEscape, vec![
                (spec_pos3.as_span(), error_strings::UnicodeCharEscapeStartHere),
                (spec_pos5.as_span(), error_strings::UnicodeCharEscapeInvalidChar)
            ], vec![
                error_strings::UnicodeCharEscapeHelpSyntax,
            ]),
            Message::with_help_by_str(error_strings::UnexpectedCharLiteralEnd, vec![
                (spec_pos1.merge(&spec_pos2), error_strings::CharLiteralHere)
            ], vec![
                error_strings::UnicodeCharEscapeHelpSyntax
            ])
        ]);
        counter_expect!(&mut parser.coverage_recorder, all_counter, [9 1 13 8 18]);
    }

    {   // '\U0011ABCD', unicode escape error 2
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        parser.coverage_recorder = HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, 'U', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('0', spec_pos4, '0', messages), WantMore);
        assert_eq!(parser.input('0', spec_pos5, '1', messages), WantMore);
        assert_eq!(parser.input('1', spec_pos4, '1', messages), WantMore);
        assert_eq!(parser.input('1', spec_pos5, 'A', messages), WantMore);
        assert_eq!(parser.input('A', spec_pos4, 'B', messages), WantMore);
        assert_eq!(parser.input('B', spec_pos5, 'C', messages), WantMore);
        assert_eq!(parser.input('C', spec_pos4, 'D', messages), WantMore);
        assert_eq!(parser.input('D', spec_pos5, '\'', messages), WantMore);
        assert_eq!(parser.input('\'', spec_pos2, '$', messages), 
            Finished(None, spec_pos1.merge(&spec_pos2)));
        
        assert_eq!(messages, &make_messages![
            Message::with_help(error_strings::InvalidUnicodeCharEscape.to_owned(), vec![
                (spec_pos3.merge(&spec_pos5), error_strings::UnicodeCharEscapeHere.to_owned()),
            ], vec![
                format!("{}{}", error_strings::UnicodeCharEscapeCodePointValueIs, "0011ABCD"),
                error_strings::UnicodeCharEscapeHelpValue.to_owned(),
            ])
        ]);
        counter_expect!(&mut parser.coverage_recorder, all_counter, [9 1 13 2 12 15]);
    }

    {   // '\na', too long after simple escape
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        parser.coverage_recorder = HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, 'n', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('a', spec_pos4, '\'', messages), WantMore);
        assert_eq!(parser.input('\'', spec_pos2, '$', messages), 
            Finished(None, spec_pos1.merge(&spec_pos2)));
        
        assert_eq!(messages, &make_messages![Message::new_by_str(error_strings::CharLiteralTooLong, vec![
            (spec_pos1.merge(&spec_pos2), error_strings::CharLiteralHere),
        ])]);
        counter_expect!(&mut parser.coverage_recorder, all_counter, [7 16 17 15 19]);
    }

    {   // '\uABCDA', too long after unicode escape
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        parser.coverage_recorder = HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, 'u', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('A', spec_pos4, 'B', messages), WantMore);
        assert_eq!(parser.input('B', spec_pos4, 'C', messages), WantMore);
        assert_eq!(parser.input('C', spec_pos4, 'D', messages), WantMore);
        assert_eq!(parser.input('D', spec_pos4, 'A', messages), WantMore);
        assert_eq!(parser.input('A', spec_pos4, '\'', messages), WantMore);
        assert_eq!(parser.input('\'', spec_pos2, '$', messages), 
            Finished(None, spec_pos1.merge(&spec_pos2)));
        
        assert_eq!(messages, &make_messages![Message::new_by_str(error_strings::CharLiteralTooLong, vec![
            (spec_pos1.merge(&spec_pos2), error_strings::CharLiteralHere),
        ])]);
        counter_expect!(&mut parser.coverage_recorder, all_counter, [9 1 12 3 13 16 17 15 19]);
    }

    {   // '$, EOF 
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        parser.coverage_recorder = HashSet::<i32>::new();

        assert_eq!(parser.input(EOF_CHAR, spec_pos3, EOF_CHAR, messages),
            Finished(None, spec_pos1.as_span()));
        
        assert_eq!(messages, &make_messages![Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (spec_pos1.as_span(), error_strings::CharLiteralHere),
            (spec_pos3.as_span(), error_strings::EOFHere)
        ])]);
        counter_expect!(&mut parser.coverage_recorder, all_counter, [6]);
    }

    {   // '\$
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        parser.coverage_recorder = HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, EOF_CHAR, messages), 
            Finished(None, spec_pos1.merge(&spec_pos3)));
        
        assert_eq!(messages, &make_messages![Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (spec_pos1.merge(&spec_pos3), error_strings::CharLiteralHere),
            (spec_pos3.offset(1).as_span(), error_strings::EOFHere)
        ])]);
        counter_expect!(&mut parser.coverage_recorder, all_counter, [10]);
    }

    {   // '\u$', EOF in unicode escape
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        parser.coverage_recorder = HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, 'u', messages), WantMoreWithSkip1);
        assert_eq!(parser.input(EOF_CHAR, spec_pos4, EOF_CHAR, messages), 
            Finished(None, spec_pos1.merge(&spec_pos3.offset(1)))); // because `\` at spec_pos3, then the parser should guess `u` at spec_pos3.offset(1)
        
        assert_eq!(messages, &make_messages![Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (spec_pos1.merge(&spec_pos3.offset(1)), error_strings::CharLiteralHere),
            (spec_pos4.as_span(), error_strings::EOFHere)
        ])]);
        counter_expect!(&mut parser.coverage_recorder, all_counter, [9 4]);
    }

    {   // 'A$, normal and EOF
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        parser.coverage_recorder = HashSet::<i32>::new();

        assert_eq!(parser.input('A', spec_pos3, EOF_CHAR, messages), WantMore);
        assert_eq!(parser.input(EOF_CHAR, spec_pos4, EOF_CHAR, messages), 
            Finished(None, spec_pos1.merge(&spec_pos3)));
        
        assert_eq!(messages, &make_messages![Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (spec_pos1.merge(&spec_pos3), error_strings::CharLiteralHere),
            (spec_pos4.as_span(), error_strings::EOFHere)
        ])]);
        counter_expect!(&mut parser.coverage_recorder, all_counter, [11 14]); 
    }

    {   // 'ABC$, too long and EOF and remove too long, do no revert because don't known where to revert
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        parser.coverage_recorder = HashSet::<i32>::new();

        assert_eq!(parser.input('A', spec_pos3, 'B', messages), WantMore);
        assert_eq!(parser.input('B', spec_pos3, 'C', messages), WantMore);
        assert_eq!(parser.input('C', spec_pos5, EOF_CHAR, messages), WantMore);
        assert_eq!(parser.input(EOF_CHAR, spec_pos4, EOF_CHAR, messages), 
            Finished(None, spec_pos1.merge(&spec_pos5)));
        
        assert_eq!(messages, &make_messages![Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (spec_pos1.merge(&spec_pos5), error_strings::CharLiteralHere),
            (spec_pos4.as_span(), error_strings::EOFHere)
        ])]);
        counter_expect!(&mut parser.coverage_recorder, all_counter, [11 16 17 14]); 
    }

        // Update 17/6/4: what is 'this case is abandon' mean? if the feature is removed, why is the test still here and passed
    {   // '\'AB$, this case is abandon: // when meet too long A and find buf is \\, so return invalid char literal and continue other v1
        let mut parser = CharLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        parser.coverage_recorder = HashSet::<i32>::new();

        assert_eq!(parser.input('\\', spec_pos3, '\'', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('A', spec_pos3, 'B', messages), WantMore);
        assert_eq!(parser.input('B', spec_pos4, EOF_CHAR, messages), WantMore);
        assert_eq!(parser.input(EOF_CHAR, spec_pos5, EOF_CHAR, messages),
            Finished(None, spec_pos1.merge(&spec_pos4)));
        
        assert_eq!(messages, &make_messages![Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (spec_pos1.merge(&spec_pos4), error_strings::CharLiteralHere),
            (spec_pos5.as_span(), error_strings::EOFHere),
        ])]);
        counter_expect!(&mut parser.coverage_recorder, all_counter, [7 16 17 14]); 
    }

    let mut sorted_all_counter = all_counter.into_iter().collect::<Vec<i32>>();
    sorted_all_counter.sort();
    assert_eq!(sorted_all_counter, vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19]);
}