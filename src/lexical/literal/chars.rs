///! lexical::literal::chars: char and string literal parsers, also shared escape parser

use crate::source::{FileSystem, Span, Position, IsId, EOF};
use crate::diagnostics::{strings};
use super::super::{Parser, Token, StringLiteralType};

enum EscapeResult {
    Ok(char, Span),
    UnknownSimpleEscape,
    EOF(Span), // meet EOF in simple escape or unicode escape
    UnexpectedUnicodeEscapeEnd(char), // unicode escape end by \, single quote or double quote
    InvalidUnicodeEscape, // other unexpected char in unicode escape
    InvalidCodePoint,
}

// char literal and string literal
impl<'e, 's, F> Parser<'e, 's, F> where F: FileSystem {

    // literal start: start position and error message for some error
    fn parse_escape(&mut self, literal_start: (Position, &'static str)) -> EscapeResult {
        #[cfg(feature = "trace_lexical_escape")] println!("[parse_escape] current = {:?}", self.current);
        let mut all_span = self.current_position.into();
        self.eat(); // eat initial \

        macro_rules! simple { ($v:expr) => {{ 
            all_span += self.current_position; 
            self.eat();
            #[cfg(feature = "trace_lexical_escape")] println!("[parse_escape] return (Some({:x}), {:?})", $v as u32, all_span);
            return EscapeResult::Ok($v, all_span); 
        }}}
        let expect_size = match self.current {
            't' => simple!('\t'),
            'n' => simple!('\n'),
            'r' => simple!('\r'),
            '0' => simple!('\0'),
            '\\' => simple!('\\'),
            '"' => simple!('"'),
            '\'' => simple!('\''),
            'a' => simple!('\u{7}'),
            'b' => simple!('\u{8}'),
            'f' => simple!('\u{C}'),
            'v' => simple!('\u{B}'),
            'u' => { all_span += self.current_position; self.eat(); 4 },
            'U' => { all_span += self.current_position; self.eat(); 8 },
            EOF => {
                self.diagnostics.emit(strings::UnexpectedEOF)
                    .detail(literal_start.0, literal_start.1)
                    .detail(self.current_position, strings::EOFHere);
                #[cfg(feature = "trace_lexical_escape")] println!("[parse_escape] return None because meet EOF in simple escape");
                return EscapeResult::EOF(all_span);
            },
            other => {
                self.diagnostics.emit(format!("{} '\\{}'", strings::UnknownCharEscape, other))
                    .detail(literal_start.0, literal_start.1)
                    .detail(all_span + self.current_position, strings::UnknownCharEscapeHere);
                self.eat();
                #[cfg(feature = "trace_lexical_escape")] println!("[parse_escape] return None because unknown simple escape");
                return EscapeResult::UnknownSimpleEscape;
            },
        };

        let mut eaten_size = 0;
        let mut code_point = 0;
        let mut already_invalid_char = false; // for non slash/quote invalid char, still consume expected size before return, also do not duplicate invalid char error
        loop {
            if let (Some(digit), false) = (self.current.to_digit(16), already_invalid_char) {
                code_point += digit << ((expect_size - eaten_size - 1) << 2);
                all_span += self.current_position;
                if eaten_size + 1 == expect_size {
                    if let Some(c) = char::from_u32(code_point) {
                        self.eat();
                        #[cfg(feature = "trace_lexical_escape")] println!("[parse_escape] return (Some({:x}), {:?})", c as u32, all_span);
                        return EscapeResult::Ok(c, all_span);
                    } else {
                        self.diagnostics.emit(strings::InvalidUnicodeCharEscape)
                            .detail(all_span, strings::UnicodeCharEscapeHere)
                            .help(if expect_size == 4 {
                                format!("{}{:04X}", strings::UnicodeCharEscapeCodePointValueIs, code_point)
                            } else {
                                format!("{}{:08X}", strings::UnicodeCharEscapeCodePointValueIs, code_point)
                            })
                            .help(strings::UnicodeCharEscapeHelpValue);
                        self.eat();
                        #[cfg(feature = "trace_lexical_escape")] println!("[parse_escape] return None because not a valid unicode code point");
                        return EscapeResult::InvalidCodePoint;
                    }
                }
                eaten_size += 1;
                self.eat();
            } else if already_invalid_char && eaten_size == expect_size {
                // other already consumed
                #[cfg(feature = "trace_lexical_escape")] println!("[parse_escape] return None because meet other invalid char in unicode escape");
                return EscapeResult::InvalidUnicodeEscape;
            } else { // this includes invalid hex char, EOF, unexpected ending quote
                match self.current {
                    // it seems the unicode ecsape is intended to be end, although incorrectly, do not consume and return
                    '\\' | '"' | '\'' => {
                        self.diagnostics.emit(strings::InvalidUnicodeCharEscape)
                            .detail(all_span, strings::UnicodeCharEscapeHere)
                            .help(strings::UnicodeCharEscapeHelpSyntax);
                        #[cfg(feature = "trace_lexical_escape")] println!("[parse_escape] return None because meet something like end in unicode escape");
                        return EscapeResult::UnexpectedUnicodeEscapeEnd(self.current);
                    },
                    EOF => {
                        self.diagnostics.emit(strings::UnexpectedEOF)
                            .detail(literal_start.0, literal_start.1)
                            .detail(self.current_position, strings::EOFHere);
                        #[cfg(feature = "trace_lexical_escape")] println!("[parse_escape] return None because meet EOF in unicode escape");
                        return EscapeResult::EOF(all_span);
                    },
                    _ => {
                        if !already_invalid_char {
                            self.diagnostics.emit(strings::InvalidUnicodeCharEscape)
                                .detail(all_span.start, strings::UnicodeCharEscapeStartHere)
                                .detail(self.current_position, strings::UnicodeCharEscapeInvalidChar)
                                .help(strings::UnicodeCharEscapeHelpSyntax);
                        }
                        all_span += self.current_position;
                        already_invalid_char = true;
                        eaten_size += 1;
                        self.eat();
                    }
                }
            }
        }
    }

    pub (in super::super) fn is_char_literal_start(&self) -> bool {
        self.current == '\''
    }

    pub (in super::super) fn parse_char_literal(&mut self) -> (Token, Span) {
        #[cfg(feature = "trace_lexical_char")] println!("[parse_char_literal] current = {:?}", self.current);

        let mut raw = Vec::new(); // allow arbitrary length char literal when parsing after raise error after that
        let mut all_span: Span = self.current_position.into();

        self.eat(); // eat initial quote
        loop {
            if let EOF = self.current {
                self.diagnostics.emit(strings::UnexpectedEOF)
                    .detail(all_span.start, strings::CharLiteralStartHere)
                    .detail(self.current_position, strings::EOFHere);
                #[cfg(feature = "trace_lexical_char")] println!("[parse_char_literal] return invalid because meet EOF");
                return (Token::Char('\0'), all_span);
            }

            if let '\\' = self.current {
                match self.parse_escape((all_span.start, strings::CharLiteralStartHere)) {
                    EscapeResult::Ok(c, span) => {
                        raw.push(c);
                        all_span += span;
                        #[cfg(feature = "trace_lexical_char")] println!("[parse_char_literal] append escape {:x}", c as u32);
                        continue;
                    },
                    EscapeResult::UnknownSimpleEscape 
                    | EscapeResult::InvalidCodePoint
                    | EscapeResult::InvalidUnicodeEscape
                    | EscapeResult::UnexpectedUnicodeEscapeEnd('\\') => {
                        // push 0 into raw to prevent empty error for normal errors, regard \ as unicode escape end, continue
                        raw.push('\0');
                        continue;
                    },
                    EscapeResult::EOF(span) => {
                        all_span += span;
                        // already raised error, direct return
                        #[cfg(feature = "trace_lexical_char")] println!("[parse_char_literal] return invalid because meet EOF in escape");
                        return (Token::Char('\0'), all_span);
                    },
                    EscapeResult::UnexpectedUnicodeEscapeEnd('\'') => {
                        // already raised invalid unicode escape, continue to normal literal end, push 0 to prevent empty error
                        raw.push('\0');
                    },
                    EscapeResult::UnexpectedUnicodeEscapeEnd('"') => {
                        // for char literal, unexpected double quote in unicode escape is simply regarded as normal char, 
                        // invalid unicode error already escaped so simply continue, and too long error will be raised later
                        raw.push('"');
                        all_span += self.current_position;
                        self.eat();
                        continue;
                    },
                    _ => unreachable!(),
                }
            }

            if let '\'' = self.current {
                all_span += self.current_position;
                self.eat();
                if raw.len() == 1 {
                    // normal end
                    return (Token::Char(raw[0]), all_span);
                } else if raw.is_empty() {
                    self.diagnostics.emit(strings::EmptyCharLiteral)
                        .detail(all_span, strings::CharLiteralHere)
                        .help(strings::CharLiteralSyntaxHelp1);
                    #[cfg(feature = "trace_lexical_char")] println!("[parse_char_literal] return invalid because empty");
                    return (Token::Char('\0'), all_span);
                } else {
                    self.diagnostics.emit(strings::CharLiteralTooLong)
                        .detail(all_span, strings::CharLiteralHere)
                        .help(strings::CharLiteralSyntaxHelp1);
                    #[cfg(feature = "trace_lexical_char")] println!("[parse_char_literal] return invalid because too long {:?}", raw);
                    return (Token::Char('\0'), all_span);
                }
            } else if self.current == '\n' {
                // \n in char literal (source code \n, not escape \n) is always error regardless of empty, one code point or too long
                self.diagnostics.emit(strings::UnexpectedEOL)
                    .detail(all_span.start, strings::CharLiteralStartHere)
                    .detail(self.current_position, strings::EOLHere);
                all_span += self.current_position;
                self.eat();
                #[cfg(feature = "trace_lexical_char")] println!("[parse_char_literal] return invalid because meet EOL");
                return (Token::Char('\0'), all_span);
            } else {
                // normal char in string
                raw.push(self.current);
                all_span += self.current_position;
                #[cfg(feature = "trace_lexical_char")] println!("[parse_char_literal] append normal {:x}", self.current as u32);
                self.eat();
            }
        }
    }

    pub (in super::super) fn is_normal_string_literal_start(&self) -> bool {
        self.current == '"'
    }

    pub (in super::super) fn parse_normal_string_literal(&mut self) -> (Token, Span) {

        let mut raw = String::new();
        let mut all_span: Span = self.current_position.into();
        let mut last_escape_quote_position: Option<Span> = None; // indicate if normal end is unexpectedly escaped

        self.eat(); // eat beginning double quote
        loop {
            if let EOF = self.current {
                if let Some(last_escape_quote_position) = last_escape_quote_position {
                    self.diagnostics.emit(strings::UnexpectedEOF)
                        .detail(all_span.start, strings::StringLiteralStartHere)
                        .detail(self.current_position, strings::EOFHere)
                        .detail(last_escape_quote_position, strings::LastEscapedQuoteHere);
                } else {
                    self.diagnostics.emit(strings::UnexpectedEOF)
                        .detail(all_span.start, strings::StringLiteralStartHere)
                        .detail(self.current_position, strings::EOFHere);
                }
                return (Token::Str(IsId::new(1), StringLiteralType::Normal), all_span);
            }

            if let '\\' = self.current {
                match self.parse_escape((all_span.start, strings::StringLiteralStartHere)) {
                    EscapeResult::Ok(c, span) => {
                        raw.push(c);
                        all_span += span;
                        if c == '"' {
                            last_escape_quote_position = Some(span);
                        }
                        continue;
                    },
                    EscapeResult::UnknownSimpleEscape 
                    | EscapeResult::InvalidCodePoint 
                    | EscapeResult::InvalidUnicodeEscape
                    | EscapeResult::UnexpectedUnicodeEscapeEnd('\\') => {
                        // error already raised for normal errors, regard \ as unicode escape end, continue
                        continue;
                    },
                    EscapeResult::EOF(span) => {
                        all_span += span;
                        // already raised error, direct return
                        return (Token::Str(IsId::new(1), StringLiteralType::Normal), all_span);
                    },
                    EscapeResult::UnexpectedUnicodeEscapeEnd('"') => {
                        // already raised invalid unicode escape, continue to normal literal end
                    },
                    EscapeResult::UnexpectedUnicodeEscapeEnd('\'') => {
                        // for string literal, unexpected single quote in unicode escape is simply regarded as normal char, invalid unicode error already escaped so simply continue
                        raw.push('\'');
                        all_span += self.current_position;
                        self.eat();
                        continue;
                    },
                    _ => unreachable!(),
                }
            }

            if let '"' = self.current {
                // normal end
                all_span += self.current_position;
                self.eat();
                #[cfg(feature = "trace_lexical_str")] println!("[parse_string_literal] return ({:?}, {:?})", raw.chars().map(|c| format!("{:x}", c as u32)).collect::<Vec<_>>().join(","), all_span);
                return (Token::Str(self.chars.intern(&raw), StringLiteralType::Normal), all_span);
            } else {
                // normal char in string
                raw.push(self.current);
                all_span += self.current_position;
                self.eat();
            }
        }
    }

    pub (in super::super) fn is_raw_string_literal_start(&self) -> bool {
        matches!((self.current, self.peek), ('r' | 'R', '"'))
    }

    pub (in super::super) fn parse_raw_string_literal(&mut self) -> (Token, Span) {
        let mut all_span = self.current_position.into();
        let mut raw = String::new();
        self.eat(); // eat beginning r
        self.eat(); // eat beginning quote
        loop {
            match self.current {
                '"' => {
                    all_span += self.current_position;
                    self.eat();
                    return (Token::Str(self.chars.intern(&raw), StringLiteralType::Raw), all_span);
                }
                EOF => {
                    self.diagnostics.emit(strings::UnexpectedEOF)
                        .detail(all_span.start, strings::StringLiteralStartHere)
                        .detail(self.current_position, strings::EOFHere);
                    self.eat();
                    return (Token::Str(IsId::new(1), StringLiteralType::Raw), all_span);
                }
                other => {
                    all_span += self.current_position;
                    raw.push(other);
                    self.eat();
                }
            }
        }
    }
}
