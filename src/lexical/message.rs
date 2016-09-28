
use std::io;
use position::Position;
pub enum Message {

    CannotOpenFile { file_name: String, e: io::Error },
    CannotReadFile { file_name: String, e: io::Error },

    UnexpectedEndofFileInBlockComment { 
        block_start: Position, 
        eof_pos: Position 
    },
    UnexpectedEndofFileInStringLiteral { 
        literal_start: Position, 
        eof_pos: Position, 
        hint_escaped_quote_pos: Option<Position> 
    },
    UnrecogonizedEscapeCharInStringLiteral {
        literal_start: Position,
        unrecogonize_pos: Position,
        unrecogonize_escape: char,
    },

    UnexpectedIdentifierCharInNumericLiteral {
        literal_start: Position,
        unexpected_char: char,
    },
    NumericLiteralTooLong {
        literal_start: Position,
    },
    NumericLiteralTooLarge {
        literal_start: Position,
    },
}

use std::fmt;
impl fmt::Debug for Message {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Message::*;
        match *self {
            CannotOpenFile { ref file_name, ref e } => {
                write!(f, "Cannot open input file `{}`: {}", file_name, e)
            }
            CannotReadFile { ref file_name, ref e } => {
                write!(f, "Cannot read input file `{}`: {}", file_name, e)
            }
            UnexpectedEndofFileInBlockComment { ref block_start, ref eof_pos } => {
                write!(f, "Unexpected end of file at {} in block comment starts from {}", eof_pos, block_start)
            }
            UnexpectedEndofFileInStringLiteral { ref literal_start, ref eof_pos, ref hint_escaped_quote_pos } => {
                try!(write!(f, "Unexpected end of file at {} in string literal starts from {}", eof_pos, literal_start));
                match hint_escaped_quote_pos {
                    &None => Ok(()),
                    &Some(ref hint) => write!(f, ", did you accidentally escape the quotation mark at {}?", hint),
                }
            }
            UnrecogonizedEscapeCharInStringLiteral { ref literal_start, ref unrecogonize_pos, ref unrecogonize_escape } => {
                write!(f, "Unrecogonized escape char {:?} at {} in string literal starts from {}", unrecogonize_escape, unrecogonize_pos, literal_start)
            }
            UnexpectedIdentifierCharInNumericLiteral { ref literal_start, ref unexpected_char } => {
                write!(f, "Unexpected character {:?} in numeric litral starts from {}", unexpected_char, literal_start)
            }
            NumericLiteralTooLong { ref literal_start } => {
                write!(f, "Numeric literal at {:?} too long", literal_start)
            }
            NumericLiteralTooLarge { ref literal_start } => {
                write!(f, "Numeric literal at {:?} too large", literal_start)
            }
        }
    }
}

impl_display_by_debug!(Message);

pub struct MessageEmitter {
    messages: Vec<Message>,
}

impl MessageEmitter {

    pub fn new() -> MessageEmitter {
        MessageEmitter { messages: Vec::new() }
    }

    pub fn is_empty(&self) -> bool { self.messages.is_empty() }

    pub fn push(&mut self, message: Message) {
        self.messages.push(message);
    }
}

impl fmt::Debug for MessageEmitter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for message in &self.messages {
            try!(writeln!(f, "{:?}", message));
            try!(writeln!(f, ""));
        }
        Ok(())
    }
}

impl_display_by_debug!(MessageEmitter);