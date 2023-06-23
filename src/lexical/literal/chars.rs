///! lexical::literal::chars: char and string literal parsers

use crate::source::{Span, Position, IsId, EOF, IdSpan};
use crate::diagnostics::strings;
use super::super::{Parser, Token, StringLiteralType};

// enum PrefixKind {
//     None,
//     Binary,
//     Raw,
//     RawBinary,
//     Identifier,
//     RawIdentifier,
//     VeryRaw,
//     Format,
//     // Unknown(&str),
// }

enum EscapeResult {
    Ok(char, Span),
    UnknownSimpleEscape,
    EOF(Span), // meet EOF in simple escape or unicode escape
    UnexpectedUnicodeEscapeEnd(char), // unicode escape end by \, single quote or double quote
    InvalidUnicodeEscape, // other unexpected char in unicode escape
    InvalidCodePoint,
}

impl IdSpan {
    fn map_span(self, f: impl FnOnce(Span) -> Span) -> Self {
        Self{ id: self.id, span: f(self.span) }
    }
    fn map<T>(self, f: impl FnOnce(IsId) -> T) -> (T, Span) {
        (f(self.id), self.span)
    }
}

// char literal and string literal
impl<'ecx, 'scx> Parser<'ecx, 'scx> {

    // is_quoted_start => is_quote
    pub(in super::super) fn is_quote(&self) -> bool {
        matches!((self.inside_format_string, self.buf[0]), (false, '\'' | '"') | (true, ':' | '{'))
    }

    pub(in super::super) fn parse_quoted(&mut self, prefix: Option<(&str, Span)>) -> (Token, Span) {
        use StringLiteralType::*;
        
        // do not check confusable inside string
        self.check_confusable = false;
        match (prefix, self.buf[0]) {
            (None, '\'') => 
                self.parse_char(),
            // byte literal use similar syntax as char, but not allow unicode or unicode escape
            (Some(("b", prefix_span)), '\'') => 
                self.parse_byte(prefix_span),
            (Some((other, prefix_span)), '\'') => {
                self.diagnostics
                    .emit(format!("invalid literal prefix `{other}`"))
                    .span(prefix_span).help("expected none or `b`");
                self.parse_char()
            },
            (None, '"') =>
                self.parse_string().map(|v| Token::Str(v, Normal)).into(),
            // binary string does not allow unicode or unicode escape, item is byte (semantically, token is still IsId)
            (Some(("b", prefix_span)), '"') =>
                self.parse_binary_string().map_span(|s| prefix_span + s).map(|v| Token::Str(v, Binary)).into(),
            // raw string does not recognize any escape, first double quote is end
            (Some(("r", prefix_span)), '"') =>
                self.parse_raw_string(prefix_span).map(|v| Token::Str(v, Raw)).into(),
            // identifier literal processes content like raw string, but produce an identifier token
            (Some(("i", prefix_span)), '"') => 
                self.parse_raw_string(prefix_span).map(|v| Token::Ident(v)).into(),
            // very raw string use custom delimeter and allow double quote inside
            (Some(("v", prefix_span)), '"') => 
                self.parse_very_raw_string().map_span(|s| prefix_span + s).map(|v| Token::Str(v, Raw)).into(),
            // raw binary string does not recognize any escape, and item is byte
            (Some(("rb" | "br", prefix_span)), '"') => 
                self.parse_raw_binary_string().map_span(|s| prefix_span + s).map(|v| Token::Str(v, RawBinary)).into(),
            // format string
            (Some(("f", prefix_span)), '"') => 
                self.parse_string().map_span(|s| prefix_span + s).map(|v| Token::Str(v, FormatStart)).into(),
            (Some((other, prefix_span)), '"') => {
                self.diagnostics
                    .emit(format!("invalid literal prefix `{other}`"))
                    .span(prefix_span).help("expected none, `b`, `r`, `br`, `c`, `f` or `i`");
                self.parse_string().map_span(|s| prefix_span + s).map(|v| Token::Str(v, Normal)).into()
            },
            // format specifier processes content like raw string, but ends with first unpaired right brace
            (None, ':') =>
                self.parse_format_specifier().map(|v| Token::Str(v, FormatSpecifier)).into(),
            // format string segment processes content same as normal string, but can end with another left brace
            (None, '{') => {
                // TODO: intermediate or end
                self.parse_string().map(|v| Token::Str(v, FormatIntermdiate)).into()
            },
            _ => unreachable!(),
        }
    }

    // literal start: start position and error message for some error
    fn parse_escape(&mut self, literal_start: (Position, &'static str)) -> EscapeResult {
        trace!(scope "lexical_escape");
        trace!("buf[0] = {:?}", self.buf[0]);
        let mut all_span = self.pos[0].into();
        self.eat(); // eat initial \

        macro_rules! simple { ($v:expr) => {{ 
            all_span += self.pos[0]; 
            self.eat();
            trace!("return (Some({:x}), {:?})", $v as u32, all_span);
            return EscapeResult::Ok($v, all_span); 
        }}}
        let expect_size = match self.buf[0] {
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
            'x' => { all_span += self.pos[0]; self.eat(); 2 },
            'u' => { all_span += self.pos[0]; self.eat(); 4 },
            'U' => { all_span += self.pos[0]; self.eat(); 6 },
            EOF => {
                self.diagnostics.emit(strings::UnexpectedEOF)
                    .detail(literal_start.0, literal_start.1)
                    .detail(self.pos[0], strings::EOFHere);
                trace!("return None because meet EOF in simple escape");
                return EscapeResult::EOF(all_span);
            },
            other => {
                self.diagnostics.emit(format!("{} '\\{}'", strings::UnknownCharEscape, other))
                    .detail(literal_start.0, literal_start.1)
                    .detail(all_span + self.pos[0], strings::UnknownCharEscapeHere);
                self.eat();
                trace!("return None because unknown simple escape");
                return EscapeResult::UnknownSimpleEscape;
            },
        };

        let mut eaten_size = 0;
        let mut code_point = 0;
        let mut already_invalid_char = false; // for non slash/quote invalid char, still consume expected size before return, also do not duplicate invalid char error
        loop {
            if let (Some(digit), false) = (self.buf[0].to_digit(16), already_invalid_char) {
                code_point += digit << ((expect_size - eaten_size - 1) << 2);
                all_span += self.pos[0];
                if eaten_size + 1 == expect_size {
                    if let Some(c) = char::from_u32(code_point) {
                        self.eat();
                        trace!("return (Some({:x}), {:?})", c as u32, all_span);
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
                        trace!("return None because not a valid unicode code point");
                        return EscapeResult::InvalidCodePoint;
                    }
                }
                eaten_size += 1;
                self.eat();
            } else if already_invalid_char && eaten_size == expect_size {
                // other already consumed
                trace!("return None because meet other invalid char in unicode escape");
                return EscapeResult::InvalidUnicodeEscape;
            } else { // this includes invalid hex char, EOF, unexpected ending quote
                match self.buf[0] {
                    // it seems the unicode ecsape is intended to be end, although incorrectly, do not consume and return
                    '\\' | '"' | '\'' => {
                        self.diagnostics.emit(strings::InvalidUnicodeCharEscape)
                            .detail(all_span, strings::UnicodeCharEscapeHere)
                            .help(strings::UnicodeCharEscapeHelpSyntax);
                        trace!("return None because meet something like end in unicode escape");
                        return EscapeResult::UnexpectedUnicodeEscapeEnd(self.buf[0]);
                    },
                    EOF => {
                        self.diagnostics.emit(strings::UnexpectedEOF)
                            .detail(literal_start.0, literal_start.1)
                            .detail(self.pos[0], strings::EOFHere);
                        trace!("return None because meet EOF in unicode escape");
                        return EscapeResult::EOF(all_span);
                    },
                    _ => {
                        if !already_invalid_char {
                            self.diagnostics.emit(strings::InvalidUnicodeCharEscape)
                                .detail(all_span.start, strings::UnicodeCharEscapeStartHere)
                                .detail(self.pos[0], strings::UnicodeCharEscapeInvalidChar)
                                .help(strings::UnicodeCharEscapeHelpSyntax);
                        }
                        all_span += self.pos[0];
                        already_invalid_char = true;
                        eaten_size += 1;
                        self.eat();
                    }
                }
            }
        }
    }

    fn parse_char(&mut self) -> (Token, Span) {
        trace!(scope "char_literal");
        trace!("buf[0] = {:?}", self.buf[0]);

        let mut raw = Vec::new(); // allow arbitrary length char literal when parsing after raise error after that
        let mut all_span: Span = self.pos[0].into();

        self.eat(); // eat initial quote
        loop {
            if let EOF = self.buf[0] {
                self.diagnostics.emit(strings::UnexpectedEOF)
                    .detail(all_span.start, strings::CharLiteralStartHere)
                    .detail(self.pos[0], strings::EOFHere);
                trace!("return invalid because meet EOF");
                return (Token::Char('\0'), all_span)
            }

            if let '\\' = self.buf[0] {
                match self.parse_escape((all_span.start, strings::CharLiteralStartHere)) {
                    EscapeResult::Ok(c, span) => {
                        raw.push(c);
                        all_span += span;
                        trace!("append escape {:x}", c as u32);
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
                        trace!("return invalid because meet EOF in escape");
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
                        all_span += self.pos[0];
                        self.eat();
                        continue;
                    },
                    _ => unreachable!(),
                }
            }

            if let '\'' = self.buf[0] {
                all_span += self.pos[0];
                self.eat();
                if raw.len() == 1 {
                    // normal end
                    return (Token::Char(raw[0]), all_span);
                } else if raw.is_empty() {
                    self.diagnostics.emit(strings::EmptyCharLiteral)
                        .detail(all_span, strings::CharLiteralHere)
                        .help(strings::CharLiteralSyntaxHelp1);
                    trace!("return invalid because empty");
                    return (Token::Char('\0'), all_span);
                } else {
                    self.diagnostics.emit(strings::CharLiteralTooLong)
                        .detail(all_span, strings::CharLiteralHere)
                        .help(strings::CharLiteralSyntaxHelp1);
                    trace!("return invalid because too long {raw:?}");
                    return (Token::Char('\0'), all_span);
                }
            } else if self.buf[0] == '\n' {
                // \n in char literal (source code \n, not escape \n) is always error regardless of empty, one code point or too long
                self.diagnostics.emit(strings::UnexpectedEOL)
                    .detail(all_span.start, strings::CharLiteralStartHere)
                    .detail(self.pos[0], strings::EOLHere);
                all_span += self.pos[0];
                self.eat();
                trace!("return invalid because meet EOL");
                return (Token::Char('\0'), all_span);
            } else {
                // normal char in string
                raw.push(self.buf[0]);
                all_span += self.pos[0];
                trace!("append normal {:x}", self.buf[0] as u32);
                self.eat();
            }
        }
    }

    fn parse_byte(&mut self, prefix_span: Span) -> (Token, Span) {
        (Token::Byte(0), prefix_span)
    }

    fn parse_string(&mut self) -> IdSpan {
        trace!(scope "string_literal");

        let mut raw = String::new();
        let mut all_span: Span = self.pos[0].into();
        let mut last_escape_quote_position: Option<Span> = None; // indicate if normal end is unexpectedly escaped

        self.eat(); // eat beginning double quote
        loop {
            if let EOF = self.buf[0] {
                if let Some(last_escape_quote_position) = last_escape_quote_position {
                    self.diagnostics.emit(strings::UnexpectedEOF)
                        .detail(all_span.start, strings::StringLiteralStartHere)
                        .detail(self.pos[0], strings::EOFHere)
                        .detail(last_escape_quote_position, strings::LastEscapedQuoteHere);
                } else {
                    self.diagnostics.emit(strings::UnexpectedEOF)
                        .detail(all_span.start, strings::StringLiteralStartHere)
                        .detail(self.pos[0], strings::EOFHere);
                }
                return IdSpan::new(IsId::new(1), all_span);
            }

            if let '\\' = self.buf[0] {
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
                        return IdSpan::new(IsId::new(1), all_span);
                    },
                    EscapeResult::UnexpectedUnicodeEscapeEnd('"') => {
                        // already raised invalid unicode escape, continue to normal literal end
                    },
                    EscapeResult::UnexpectedUnicodeEscapeEnd('\'') => {
                        // for string literal, unexpected single quote in unicode escape is simply regarded as normal char, invalid unicode error already escaped so simply continue
                        raw.push('\'');
                        all_span += self.pos[0];
                        self.eat();
                        continue;
                    },
                    _ => unreachable!(),
                }
            }

            if let '"' = self.buf[0] {
                // normal end
                all_span += self.pos[0];
                self.eat();
                trace!("return ({:?}, {:?})", raw.chars().map(|c| format!("{:x}", c as u32)).collect::<Vec<_>>().join(","), all_span);
                return IdSpan::new(self.intern(&raw), all_span);
            } else {
                // normal char in string
                raw.push(self.buf[0]);
                all_span += self.pos[0];
                self.eat();
            }
        }
    }

    fn parse_binary_string(&mut self) -> IdSpan {
        IdSpan::new(IsId::new(1), Span::new(0, 0))
    }

    fn parse_raw_binary_string(&mut self) -> IdSpan {
        IdSpan::new(IsId::new(1), Span::new(0, 0))
    }

    fn parse_raw_string(&mut self, prefix_span: Span) -> IdSpan {
        let mut all_span = prefix_span;
        let mut raw = String::new();
        self.eat(); // eat beginning quote
        loop {
            match self.buf[0] {
                '"' => {
                    all_span += self.pos[0];
                    self.eat();
                    return IdSpan::new(self.intern(&raw), all_span);
                }
                EOF => {
                    self.diagnostics.emit(strings::UnexpectedEOF)
                        .detail(prefix_span, strings::StringLiteralStartHere)
                        .detail(self.pos[0], strings::EOFHere);
                    self.eat();
                    return IdSpan::new(IsId::new(1), all_span);
                }
                other => {
                    all_span += self.pos[0];
                    raw.push(other);
                    self.eat();
                }
            }
        }
    }

    fn parse_very_raw_string(&mut self) -> IdSpan {
        IdSpan::new(IsId::new(1), Span::new(0, 0))
    }

    // TODO: format_specifier ends with first unpaired right brace
    fn parse_format_specifier(&mut self) -> IdSpan {
        IdSpan::new(IsId::new(1), Span::new(0, 0))
    }
}
