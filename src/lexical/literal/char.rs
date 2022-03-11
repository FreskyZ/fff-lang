///! fff-lang
///!
///! Character literal parser

use std::cell::Cell;
use crate::source::{Position, Span, EOF};
use crate::diagnostics::{Message, MessageCollection, strings};
use super::escape::{EscapeCharParser, EscapeCharSimpleCheckResult, EscapeCharParserResult};

#[cfg(not(feature = "trace_char_literal"))]
macro_rules! trace { ($($arg:tt)*) => () }
#[cfg(feature = "trace_char_literal")]
macro_rules! trace { ($($arg:tt)*) => ({ print!("[CharLitParser: {}] ", line!()); println!($($arg)*); }) }

#[cfg(not(feature = "trace_char_literal"))]
macro_rules! cover { ($self:expr, $id:literal) => () }
#[cfg(feature = "trace_char_literal")]
macro_rules! cover { ($self:expr, $id:literal) => ($self.coverage.insert($id)) }

#[cfg_attr(test, derive(Eq, PartialEq))]
#[derive(Debug)]
pub enum CharLiteralParserResult {
    WantMore,
    WantMoreWithSkip1,
    Finished(Option<char>, Span),
}
#[derive(Copy, Clone, Debug)]
enum ParserState {
    ExpectFirst, 
    ExpectEnd(Option<char>),
}

#[derive(Debug)]
pub struct CharLiteralParser {
    current_span: Span,
    state: Cell<ParserState>,
    has_failed: bool,
    prepare_to_too_long: bool,
    escape_parser: Option<EscapeCharParser>,
    escape_start_pos: Position,
    #[cfg(feature = "trace_char_literal")] coverage: std::collections::HashSet<i32>,
}
impl CharLiteralParser {

    pub fn new(start_pos: Position) -> CharLiteralParser {
        CharLiteralParser{ 
            current_span: start_pos.into(),
            state: Cell::new(ParserState::ExpectFirst),
            has_failed: false,
            prepare_to_too_long: false,
            escape_parser: None, 
            escape_start_pos: Position::new(0),
            #[cfg(feature = "trace_char_literal")] coverage: std::collections::HashSet::new(),
        }
    }

    //            self, current char, current char pos, next char preview
    pub fn input(&mut self, ch: char, pos: Position, next_ch: char, messages: &mut MessageCollection) -> CharLiteralParserResult {

        match self.state.get() {
            ParserState::ExpectFirst => {       // Waiting for first char
                trace!("expecting first");
                let mut need_reset_parser = false;
                let mut need_set_parser = false;
                let mut need_set_parser_value = None;
                match (&mut self.escape_parser, ch, pos, next_ch) {
                    (&mut Some(_), EOF, _2, _3) => {  // another 'u123$
                        trace!("expecting first but ch = EOF, current_span = {:?}", self.current_span);
                        cover!(self, 4);                        // C4, first char is EOF
                        messages.push(Message::new_by_str(strings::UnexpectedEOF, vec![
                            (self.current_span, strings::CharLiteralHere),
                            (pos.into(), strings::EOFHere)
                        ]));
                        return CharLiteralParserResult::Finished(None, self.current_span);
                    }
                    (&mut Some(ref mut parser), ch, _2, _3) => {
                        trace!("expecting first, ch = {:?}, pos = {:?}, current span = {:?}", ch, pos, self.current_span);
                        self.current_span += pos;
                        if ch == '\'' { // '\'' should report unexpected EOL
                            cover!(self, 18);
                            messages.push(Message::with_help_by_str(strings::UnexpectedCharLiteralEnd, vec![
                                (self.current_span, strings::CharLiteralHere),
                            ], vec![
                                strings::UnicodeCharEscapeHelpSyntax
                            ]));
                            return CharLiteralParserResult::Finished(None, self.current_span);
                        }

                        trace!("before parser.input, current_span is {:?}", self.current_span);
                        match parser.input(ch, (self.escape_start_pos, pos), messages) {
                            EscapeCharParserResult::WantMore => { 
                                trace!("parser want more");
                                cover!(self, 1);                // C1, in first char as unicode escape, continue
                            },    
                            EscapeCharParserResult::Failed => {        // Invalid unicode escape, message already emitted, return INVALID_CHAR    
                                trace!("parser failed, start expect end");
                                self.state.set(ParserState::ExpectEnd(None));
                                self.has_failed = true;
                                need_reset_parser = true;
                                cover!(self, 2);                // C2, in first char as unicode escape, 
                            }
                            EscapeCharParserResult::Success(ch) => {
                                trace!("parser success, result: {:?}", ch);
                                self.state.set(ParserState::ExpectEnd(Some(ch)));   // Success, waiting for '
                                need_reset_parser = true;
                                cover!(self, 3);                // C3, in first char as unicode escape, success
                            }
                        }
                        // WantMore  // wait to reset until out of match self.escape_parser
                    }
                    (&mut None, '\'', _2, _3) => { 
                        cover!(self, 5);                        // C5, empty
                        let all_span = self.current_span + pos;
                        messages.push(Message::with_help_by_str(strings::EmptyCharLiteral, vec![
                            (all_span, strings::CharLiteralHere),
                        ], vec![
                            strings::CharLiteralSyntaxHelp1
                        ]));
                        return CharLiteralParserResult::Finished(None, all_span);
                    }
                    (&mut None, EOF, _2, _3) => {
                        cover!(self, 6);                        // C6, '$, report EOF in char literal
                        messages.push(Message::new_by_str(strings::UnexpectedEOF, vec![
                            (self.current_span, strings::CharLiteralHere),
                            (pos.into(), strings::EOFHere)
                        ]));
                        return CharLiteralParserResult::Finished(None, self.current_span);
                    }
                    (&mut None, '\\', pos, EOF) => { 
                        cover!(self, 10);               // C10, '\$
                        let all_span = self.current_span + pos;
                        let eof_pos = pos.offset(1); // in this case, `\` is always 1 byte wide
                        messages.push(Message::new_by_str(strings::UnexpectedEOF, vec![
                            (all_span, strings::CharLiteralHere),
                            (eof_pos.into(), strings::EOFHere)
                        ]));
                        return CharLiteralParserResult::Finished(None, all_span);
                    }
                    (&mut None, '\\', slash_pos, next_ch) => {   // if is escape, try escape
                        self.current_span += slash_pos.offset(1); // `\`
                        match EscapeCharParser::simple_check(next_ch) {
                            EscapeCharSimpleCheckResult::Normal(ch) => {
                                self.state.set(ParserState::ExpectEnd(Some(ch)));
                                cover!(self, 7);        // C7, normal simple escape
                                return CharLiteralParserResult::WantMoreWithSkip1;
                            }
                            EscapeCharSimpleCheckResult::Invalid(ch) => {
                                messages.push(Message::new(format!("{} '\\{}'", strings::UnknownCharEscape, ch), vec![
                                    (self.current_span.start.into(), strings::CharLiteralStartHere.to_owned()),
                                    (slash_pos.into(), strings::UnknownCharEscapeHere.to_owned()),
                                ]));
                                self.state.set(ParserState::ExpectEnd(None));
                                self.has_failed = true;
                                cover!(self, 8);        // C8, invalid simple escape
                                return CharLiteralParserResult::WantMoreWithSkip1;
                            }
                            EscapeCharSimpleCheckResult::Unicode(parser) => { 
                                self.escape_start_pos = slash_pos;
                                need_set_parser = true;
                                need_set_parser_value = Some(parser);
                                cover!(self, 9);        // C9, start unicode parser for first char
                            }
                        }
                    }
                    (&mut None, ch, pos, _3) => {
                        trace!("experienced normal char, expecting end");
                        self.state.set(ParserState::ExpectEnd(Some(ch)));
                        self.current_span += pos;
                        cover!(self, 11);               // C11, most normal a char
                        return CharLiteralParserResult::WantMore;
                    }
                }
                if need_reset_parser {
                    self.escape_parser = None;
                    cover!(self, 12);                                   // C12, reset escape parser, only and must with C2, C3
                }
                if need_set_parser {  // C9 fix 
                    self.escape_parser = need_set_parser_value;
                    return CharLiteralParserResult::WantMoreWithSkip1;
                }
                cover!(self, 13);                                       // C13, only and must with C1, C2, C3
                return CharLiteralParserResult::WantMore;
            }
            ParserState::ExpectEnd(maybe_result) => {  // Already processed first char
                // No possibility for a unicode parser here, just wait for a ', if not, report too long
                match ch {
                    EOF => {
                        trace!("meet EOF when expecting end, current_span = {:?}", self.current_span);
                        messages.push(Message::new_by_str(strings::UnexpectedEOF, vec![
                            (self.current_span, strings::CharLiteralHere),
                            (pos.into(), strings::EOFHere)
                        ]));
                        cover!(self, 14);                               // C14, 'ABCD$
                        return CharLiteralParserResult::Finished(None, self.current_span);
                    }
                    '\'' => { // Normally successed
                        cover!(self, 15);                           // C15, most normal finish
                        let all_span = self.current_span + pos;
                        if self.prepare_to_too_long {
                            cover!(self, 19);                       // C19, actual report too long
                            messages.push(Message::new_by_str(strings::CharLiteralTooLong, vec![
                                (all_span, strings::CharLiteralHere),
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
                            cover!(self, 16);                       // C16, too long and prepare to report
                            self.prepare_to_too_long = true;
                            self.has_failed = true;     // if too longed, devalidate the buffer
                        }
                        self.current_span += pos;
                        cover!(self, 17);                           // C17, too long and return
                        return CharLiteralParserResult::WantMore;
                    }
                }
            }
        }
    } 
}

#[cfg(test)]
#[test]
fn char_lit_parser() {
    #[cfg(feature = "trace_char_literal")]
    use std::collections::HashSet;
    use self::CharLiteralParserResult::*;

    #[cfg(feature = "trace_char_literal")]
    let mut all_counter = HashSet::<i32>::new();

    #[cfg(not(feature = "trace_char_literal"))]
    macro_rules! counter_expect {
        ($($t:tt)*) => {}
    }
    #[cfg(feature = "trace_char_literal")]
    macro_rules! counter_expect {
        ($parser:expr, [$($c: expr)*]) => ({
            let mut expect_counter = HashSet::<i32>::new();
            $(
                expect_counter.insert($c);
            )*
            assert!($parser.coverage.difference(&expect_counter).collect::<HashSet<&i32>>().is_empty(),
                "Current counter: {:?}, expect_counter: {:?}", $parser.coverage, expect_counter);
            all_counter = all_counter.union(&expect_counter).map(|x| *x).collect::<HashSet<i32>>();
        })
    }

    {   // 'A', normal, C11, C15
        let mut parser = CharLiteralParser::new(Position::new(1));
        let messages = &mut MessageCollection::new();

        assert_eq!(parser.input('A', Position::new(2), '\'', messages), WantMore);
        assert_eq!(parser.input('\'', Position::new(3), '$', messages), Finished(Some('A'), Span::new(1, 3)));

        assert!(messages.is_empty());
        counter_expect!(parser, [11 15]);
    }

    {   // '\t', normal escape
        let mut parser = CharLiteralParser::new(Position::new(1));
        let messages = &mut MessageCollection::new();

        assert_eq!(parser.input('\\', Position::new(2), 't', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('\'', Position::new(4), '$', messages), Finished(Some('\t'), Span::new(1, 4)));
        
        assert!(messages.is_empty());
        counter_expect!(parser, [7 15]);
    } 

    {   // '\uABCD', normal unicode escape
        let mut parser = CharLiteralParser::new(Position::new(1));
        let messages = &mut MessageCollection::new();

        assert_eq!(parser.input('\\', Position::new(2), 'u', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('A', Position::new(4), 'B', messages), WantMore);
        assert_eq!(parser.input('B', Position::new(5), 'C', messages), WantMore);
        assert_eq!(parser.input('C', Position::new(6), 'D', messages), WantMore);
        assert_eq!(parser.input('D', Position::new(7), '\'', messages), WantMore);
        assert_eq!(parser.input('\'', Position::new(8), '$', messages), Finished(Some('\u{ABCD}'), Span::new(1, 8)));
        
        assert!(messages.is_empty());
        counter_expect!(parser, [9 1 3 12 13 15]);
    }

    {   // '', empty
        let mut parser = CharLiteralParser::new(Position::new(1));
        let messages = &mut MessageCollection::new();

        assert_eq!(parser.input('\'', Position::new(2), '$', messages), Finished(None, Span::new(1, 2)));

        assert_eq!(messages, &make_messages![Message::with_help_by_str(strings::EmptyCharLiteral, vec![
            (Span::new(1, 2), strings::CharLiteralHere),
        ], vec![
            strings::CharLiteralSyntaxHelp1
        ])]);
        counter_expect!(parser, [5]);
    }

    {   // 'ABC', normal too long, ask if is string literal
        let mut parser = CharLiteralParser::new(Position::new(1));
        let messages = &mut MessageCollection::new();

        assert_eq!(parser.input('A', Position::new(2), 'B', messages), WantMore);
        assert_eq!(parser.input('B', Position::new(3), 'C', messages), WantMore);
        assert_eq!(parser.input('C', Position::new(4), '\'', messages), WantMore);
        assert_eq!(parser.input('\'', Position::new(5), '$', messages), Finished(None, Span::new(1, 5)));
                                 
        assert_eq!(messages, &make_messages![Message::new_by_str(strings::CharLiteralTooLong, vec![
            (Span::new(1, 5), strings::CharLiteralHere),
        ])]);
        counter_expect!(parser, [11 15 16 17 19]);
    }

    {   // '\c', other simple escape
        let mut parser = CharLiteralParser::new(Position::new(1));
        let messages = &mut MessageCollection::new();

        assert_eq!(parser.input('\\', Position::new(2), 'c', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('\'', Position::new(3), '$', messages), Finished(None, Span::new(1, 3)));
        
        assert_eq!(messages, &make_messages![Message::new(format!("{} '\\{}'", strings::UnknownCharEscape, 'c'), vec![
            (Span::new(1, 1), strings::CharLiteralStartHere.to_owned()),
            (Span::new(2, 2), strings::UnknownCharEscapeHere.to_owned()),
        ])]);
        counter_expect!(parser, [8 15]);
    }

    {   // '\'', special normal escape
        let mut parser = CharLiteralParser::new(Position::new(1));
        let messages = &mut MessageCollection::new();

        assert_eq!(parser.input('\\', Position::new(2), '\'', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('\'', Position::new(4), '$', messages), Finished(Some('\''), Span::new(1, 4)));
        
        assert!(messages.is_empty());
        counter_expect!(parser, [7 15]);
    }

    {   // '\uBG', unexpect char in unicode escape, unexpected EOL in unicode escape
        let mut parser = CharLiteralParser::new(Position::new(1));
        let messages = &mut MessageCollection::new();

        assert_eq!(parser.input('\\', Position::new(2), 'u', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('B', Position::new(4), 'G', messages), WantMore);
        assert_eq!(parser.input('G', Position::new(5), '\'', messages), WantMore);
        assert_eq!(parser.input('\'', Position::new(6), '$', messages), Finished(None, Span::new(1, 6)));
        
        assert_eq!(messages, &make_messages![
            Message::with_help_by_str(strings::InvalidUnicodeCharEscape, vec![
                (Span::new(2, 2), strings::UnicodeCharEscapeStartHere),
                (Span::new(5, 5), strings::UnicodeCharEscapeInvalidChar)
            ], vec![
                strings::UnicodeCharEscapeHelpSyntax,
            ]),
            Message::with_help_by_str(strings::UnexpectedCharLiteralEnd, vec![
                (Span::new(1, 6), strings::CharLiteralHere)
            ], vec![
                strings::UnicodeCharEscapeHelpSyntax
            ])
        ]);
        counter_expect!(parser, [9 1 13 8 18]);
    }

    {   // '\U0011ABCD', unicode escape error 2
        let mut parser = CharLiteralParser::new(Position::new(1));
        let messages = &mut MessageCollection::new();

        assert_eq!(parser.input('\\', Position::new(2), 'U', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('0', Position::new(4), '0', messages), WantMore);
        assert_eq!(parser.input('0', Position::new(5), '1', messages), WantMore);
        assert_eq!(parser.input('1', Position::new(6), '1', messages), WantMore);
        assert_eq!(parser.input('1', Position::new(7), 'A', messages), WantMore);
        assert_eq!(parser.input('A', Position::new(8), 'B', messages), WantMore);
        assert_eq!(parser.input('B', Position::new(9), 'C', messages), WantMore);
        assert_eq!(parser.input('C', Position::new(10), 'D', messages), WantMore);
        assert_eq!(parser.input('D', Position::new(11), '\'', messages), WantMore);
        assert_eq!(parser.input('\'', Position::new(12), '$', messages),  Finished(None, Span::new(1, 12)));
        
        assert_eq!(messages, &make_messages![
            Message::with_help(strings::InvalidUnicodeCharEscape.to_owned(), vec![
                (Span::new(2, 11), strings::UnicodeCharEscapeHere.to_owned()),
            ], vec![
                format!("{}{}", strings::UnicodeCharEscapeCodePointValueIs, "0011ABCD"),
                strings::UnicodeCharEscapeHelpValue.to_owned(),
            ])
        ]);
        counter_expect!(parser, [9 1 13 2 12 15]);
    }

    {   // '\na', too long after simple escape
        let mut parser = CharLiteralParser::new(Position::new(1));
        let messages = &mut MessageCollection::new();

        assert_eq!(parser.input('\\', Position::new(2), 'n', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('a', Position::new(4), '\'', messages), WantMore);
        assert_eq!(parser.input('\'', Position::new(5), '$', messages), Finished(None, Span::new(1, 5)));
        
        assert_eq!(messages, &make_messages![Message::new_by_str(strings::CharLiteralTooLong, vec![
            (Span::new(1, 5), strings::CharLiteralHere),
        ])]);
        counter_expect!(parser, [7 16 17 15 19]);
    }

    {   // '\uABCDA', too long after unicode escape
        let mut parser = CharLiteralParser::new(Position::new(1));
        let messages = &mut MessageCollection::new();

        assert_eq!(parser.input('\\', Position::new(2), 'u', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('A', Position::new(4), 'B', messages), WantMore);
        assert_eq!(parser.input('B', Position::new(5), 'C', messages), WantMore);
        assert_eq!(parser.input('C', Position::new(6), 'D', messages), WantMore);
        assert_eq!(parser.input('D', Position::new(7), 'A', messages), WantMore);
        assert_eq!(parser.input('A', Position::new(8), '\'', messages), WantMore);
        assert_eq!(parser.input('\'', Position::new(9), '$', messages), Finished(None, Span::new(1, 9)));
        
        assert_eq!(messages, &make_messages![Message::new_by_str(strings::CharLiteralTooLong, vec![
            (Span::new(1, 9), strings::CharLiteralHere),
        ])]);
        counter_expect!(parser, [9 1 12 3 13 16 17 15 19]);
    }

    {   // '$, EOF 
        let mut parser = CharLiteralParser::new(Position::new(1));
        let messages = &mut MessageCollection::new();

        assert_eq!(parser.input(EOF, Position::new(2), EOF, messages), Finished(None, Span::new(1, 1)));
        
        assert_eq!(messages, &make_messages![Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(1, 1), strings::CharLiteralHere),
            (Span::new(2, 2), strings::EOFHere)
        ])]);
        counter_expect!(parser, [6]);
    }

    {   // '\$
        let mut parser = CharLiteralParser::new(Position::new(1));
        let messages = &mut MessageCollection::new();

        assert_eq!(parser.input('\\', Position::new(2), EOF, messages),  Finished(None, Span::new(1, 2)));
        
        assert_eq!(messages, &make_messages![Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(1, 2), strings::CharLiteralHere),
            (Span::new(3, 3), strings::EOFHere)
        ])]);
        counter_expect!(parser, [10]);
    }

    {   // '\u$, EOF in unicode escape
        let mut parser = CharLiteralParser::new(Position::new(1));
        let messages = &mut MessageCollection::new();

        assert_eq!(parser.input('\\', Position::new(2), 'u', messages), WantMoreWithSkip1);
        assert_eq!(parser.input(EOF, Position::new(4), EOF, messages), Finished(None, Span::new(1, 3))); // because `\` at spec_pos3, then the parser should guess `u` at spec_pos3.offset(1)
        
        assert_eq!(messages, &make_messages![Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(1, 3), strings::CharLiteralHere),
            (Span::new(4, 4), strings::EOFHere)
        ])]);
        counter_expect!(parser, [9 4]);
    }

    {   // 'A$, normal and EOF
        let mut parser = CharLiteralParser::new(Position::new(1));
        let messages = &mut MessageCollection::new();

        assert_eq!(parser.input('A', Position::new(2), EOF, messages), WantMore);
        assert_eq!(parser.input(EOF, Position::new(3), EOF, messages), Finished(None, Span::new(1, 2)));
        
        assert_eq!(messages, &make_messages![Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(1, 2), strings::CharLiteralHere),
            (Span::new(3, 3), strings::EOFHere)
        ])]);
        counter_expect!(parser, [11 14]); 
    }

    {   // 'ABC$, too long and EOF and remove too long, do no revert because don't known where to revert
        let mut parser = CharLiteralParser::new(Position::new(1));
        let messages = &mut MessageCollection::new();

        assert_eq!(parser.input('A', Position::new(2), 'B', messages), WantMore);
        assert_eq!(parser.input('B', Position::new(3), 'C', messages), WantMore);
        assert_eq!(parser.input('C', Position::new(4), EOF, messages), WantMore);
        assert_eq!(parser.input(EOF, Position::new(5), EOF, messages), Finished(None, Span::new(1, 4)));
        
        assert_eq!(messages, &make_messages![Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(1, 4), strings::CharLiteralHere),
            (Span::new(5, 5), strings::EOFHere)
        ])]);
        counter_expect!(parser, [11 16 17 14]); 
    }

        // Update 17/6/4: what is 'this case is abandon' mean? if the feature is removed, why is the test still here and passed
    {   // '\'AB$, this case is abandon: // when meet too long A and find buf is \\, so return invalid char literal and continue other v1
        let mut parser = CharLiteralParser::new(Position::new(1));
        let messages = &mut MessageCollection::new();

        assert_eq!(parser.input('\\', Position::new(2), '\'', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('A', Position::new(4), 'B', messages), WantMore);
        assert_eq!(parser.input('B', Position::new(5), EOF, messages), WantMore);
        assert_eq!(parser.input(EOF, Position::new(6), EOF, messages), Finished(None, Span::new(1, 5)));
        
        assert_eq!(messages, &make_messages![Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(1, 5), strings::CharLiteralHere),
            (Span::new(6, 6), strings::EOFHere),
        ])]);
        counter_expect!(parser, [7 16 17 14]); 
    }

    #[cfg(feature = "trace_char_literal")] {
        let mut sorted_all_counter = all_counter.into_iter().collect::<Vec<i32>>();
        sorted_all_counter.sort();
        assert_eq!(sorted_all_counter, vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19]);
    }
}
