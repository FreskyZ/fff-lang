
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

#[cfg(test)]
#[derive(Debug)]
pub struct StringLiteralParser {
    raw: String, 
    start_pos: Position,
    last_escape_quote_pos: Option<Position>,
    has_failed: bool,
}
#[cfg(not(test))]
pub struct StringLiteralParser {
    raw: String, 
    start_pos: Position,
    last_escape_quote_pos: Option<Position>,
    has_failed: bool,
}

#[cfg(test)]
pub fn str_lit_parser_visitor(parser: &StringLiteralParser) -> (&str, &Position, &Option<Position>, &bool) {
    (&parser.raw, &parser.start_pos, &parser.last_escape_quote_pos, &parser.has_failed)
}

use lexical::message::Message;
use lexical::message::MessageEmitter;
impl StringLiteralParser {

    /// new with start position
    pub fn new(start_pos: Position) -> StringLiteralParser {
        StringLiteralParser{
            raw: String::new(),
            start_pos: start_pos, 
            last_escape_quote_pos: None, 
            has_failed: false,
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
                match next_ch {
                    '"' => {
                        // record escaped \" here to be error hint
                        self.raw.push('"');                                      // C1: meet \", record it
                        self.last_escape_quote_pos = Some(slash_pos);
                        return (None, true);
                    }
                    next_ch @ 'n' | next_ch @ '\\' | next_ch @ 't' | next_ch @ 'r' | next_ch @ '0'  => {
                        self.raw.push(match next_ch {'n' => '\n', 'r' => '\r', 't' => '\t', '\\' => '\\', '0' => '\0', ch => ch});
                        return (None, true);                                    // C2: in string, meet \\nrt0, escape
                    }
                    'u' => {                                                    // C3: in string, meet \u{}
                        self.raw.push('\\');
                        self.raw.push('u'); 
                        return (None, true);
                    }
                    other => {
                        messages.push(Message::UnrecogonizedEscapeCharInStringLiteral {
                            literal_start: self.start_pos, 
                            unrecogonize_pos: slash_pos, 
                            unrecogonize_escape: other });                      // C4: in string, meet \other, emit error, continue
                        self.has_failed = true;
                        return (None, true);
                    }
                }
            }
            (Some('\\'), _1, None) => {                                        // C5: in string, meet \EOF, continue
                self.has_failed = true;
                return (None, false);
            }
            (Some('"'), pos, _1) => {      // State conversion 18: in string, meet ", finish, return
                // String finished
                return (Some(StringLiteral::new(self.raw.clone(), StringPosition::from((self.start_pos, pos)), false, self.has_failed)), false);
            }
            (Some(ch), _1, _2) => {
                // Normal in string
                self.raw.push(ch);                                              // C6: in string, meet other, push, continue
                return (None, false);
            }
            (None, pos, _2) => {  // ATTENTION: when None, current pos is required here
                messages.push(Message::UnexpectedEndofFileInStringLiteral {     // C7: in string, meet EOF, emit error, return 
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