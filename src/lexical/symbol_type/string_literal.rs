
// String literal type
// string literal storage, escape and display

use common::StringPosition;

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

use common::Position;
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

use std::fmt;
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

use lexical::symbol_type::char_literal::EscapeCharParser;
use lexical::symbol_type::char_literal::EscapeCharSimpleCheckResult;
use lexical::symbol_type::char_literal::EscapeCharParserInputResult;
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

use lexical::message::Message;
use lexical::message::MessageEmitter;

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
        -> (Option<StringLiteral>, bool) { // ret_val, require_skip1

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
                        messages.push(Message::UnrecogonizedEscapeCharInStringLiteral {
                            literal_start: self.start_pos,                          // C2, error normal escape, emit error and continue
                            unrecogonize_pos: slash_pos, 
                            unrecogonize_escape: ch });
                        self.has_failed = true;
                        return (None, true);
                    }
                    EscapeCharSimpleCheckResult::Unicode(parser) => {               // C3, start unicode escape
                        self.escape_start_pos = slash_pos;
                        self.escape_parser = Some(parser);
                        return (None, false);
                    }
                }
            }
            (Some('\\'), pos, None) => {                                            // C4, \EOF, ignore
                // Do nothing here, `"abc\udef$` reports EOF in string error, not end of string or EOF in escape error
                return (None, false);
            }
            (Some('"'), pos, _1) => {
                // String finished, check if is parsing escape
                match self.escape_parser {
                    Some(ref parser) => {                                           // C5, \uxxx\EOL, emit error and return
                        // If still parsing, it is absolutely failed
                        // Report not finished unicode escape here
                        // messages.push(Message::)
                        return (Some(StringLiteral::new(self.raw.clone(), StringPosition::from((self.start_pos, pos)), false, true)), false);
                    }
                    None => {                                                       // C7, normal EOL, return
                        return (Some(StringLiteral::new(self.raw.clone(), StringPosition::from((self.start_pos, pos)), false, self.has_failed)), false);
                    }
                }
            }
            (Some(ch), _1, _2) => {
                // Normal in string
                let mut need_reset_escape_parser = false;
                match self.escape_parser {
                    Some(ref mut parser) => {
                        match parser.input(ch, (self.start_pos, self.escape_start_pos), messages) {
                            EscapeCharParserInputResult::WantMore => (),            // C8, in unicode escape, more
                            EscapeCharParserInputResult::FailedAndWantMore => (),   // C9, in unicode escape, fail but more
                            EscapeCharParserInputResult::FailedAndFinish => {
                                need_reset_escape_parser = true;                    // C10, in unicode escape, failed and finish
                            }
                            EscapeCharParserInputResult::FailedAtLast => {
                                need_reset_escape_parser = true;                    // C11, in unicode escape, not unicode codepoint value, finish
                            }
                            EscapeCharParserInputResult::Success(ch) => {           // C12, in unicode escape, success, finish
                                need_reset_escape_parser = true;
                                self.raw.push(ch);
                            }
                        }
                    }
                    None => {
                        self.raw.push(ch);                                          // C13, most plain
                    }
                }
                if need_reset_escape_parser {  
                    self.escape_parser = None;
                }
                return (None, false);
            }
            (None, pos, _2) => {
                messages.push(Message::UnexpectedEndofFileInStringLiteral {         // C14: in string, meet EOF, emit error, return 
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
            (Some('"'), pos) => {                                               // C8: in raw string, meet ", finish, return
                return Some(StringLiteral::new(self.raw.clone(), StringPosition::from((self.start_pos, pos)), true, false));
            }
            (Some(ch), _1) => {                                                 // C9: in raw string, meet other, continue
                self.raw.push(ch);
                return None;
            }
            (None, pos) => {
                messages.push(Message::UnexpectedEndofFileInStringLiteral {     // C10: in raw string, meet EOF, emit error, return  
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
    use super::StringLiteralParser;

    #[test]
    fn str_lit_not_raw() {

    }
}