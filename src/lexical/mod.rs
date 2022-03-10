///! lexical: the lexical parser

use crate::source::{SourceChars, FileSystem, DefaultFileSystem, EOF, Position, Span, IsId};
use crate::diagnostics::{Message, MessageCollection, strings};

#[cfg(test)]
mod tests;
mod token;
mod unicode;
mod literal {
    pub mod char;
    pub mod escape;
    pub mod numeric;
    pub mod string;
    pub mod raw_string;
}

use unicode::CharExt;
use literal::numeric::parse_numeric_literal;
use literal::string::{StringLiteralParser, StringLiteralParserResult};
use literal::raw_string::{RawStringLiteralParser, RawStringLiteralParserResult};
use literal::char::{CharLiteralParser, CharLiteralParserResult};
pub use token::{Separator, SeparatorKind, Keyword, KeywordKind};
pub use token::{Numeric, StringLiteralType, Token, TokenFormat};

#[derive(Debug)]
pub struct Parser<'ecx, 'scx, F = DefaultFileSystem> {
    diagnostics: &'ecx mut MessageCollection,
    chars: SourceChars<'scx, F>,
    current: (char, Position),
    peek1: (char, Position),
    peek2: (char, Position),
}
impl<'e, 's, F> Parser<'e, 's, F> where F: FileSystem {

    pub fn new(mut chars: SourceChars<'s, F>, diagnostics: &'e mut MessageCollection) -> Self {
        Self{ diagnostics, current: chars.next(), peek1: chars.next(), peek2: chars.next(), chars, }
    }
    fn move_next(&mut self) {
        std::mem::swap(&mut self.current, &mut self.peek1);
        std::mem::swap(&mut self.peek1, &mut self.peek2);
        self.peek2 = self.chars.next();
    }

    fn skip_line_comment(&mut self) {
        if let ('/', '/') = (self.current.0, self.peek1.0) {
            loop {
                self.move_next();
                if self.current.0 == '\n' {
                    self.move_next();
                    break;
                }
            }
        }
    }

    fn skip_block_comment(&mut self) {
        // TODO test recursive
        if let ('/', '*') = (self.current.0, self.peek1.0) {
            let mut level = 0;
            loop {
                self.move_next();
                if let ('/', '*') = (self.current.0, self.peek1.0) {
                    level += 1;
                } else if let ('*', '/') = (self.current.0, self.peek1.0) {
                    if level == 0 {
                        break;
                    } else {
                        level -= 1;
                    }
                }
            }
        }
    }

    fn parse_char_literal(&mut self) -> (Token, Span) {
        let mut parser = CharLiteralParser::new(self.current.1);
        loop {
            match parser.input(self.current.0, self.current.1, self.peek1.0, self.diagnostics) {
                CharLiteralParserResult::WantMore => {
                    self.move_next();
                },
                CharLiteralParserResult::WantMoreWithSkip1 => {
                    self.move_next();
                    self.move_next();
                },
                CharLiteralParserResult::Finished(value, span) => {
                    if self.current.0 != EOF {
                        self.move_next(); // should not consume EOF, or else EOF will not return
                    }
                    return (Token::Char(value.unwrap_or_default()), span);
                },
            }
        }
    }

    fn parse_string_literal(&mut self, literal_type: StringLiteralType) -> (Token, Span) {
        match literal_type {
            StringLiteralType::Normal => {
                let mut parser = StringLiteralParser::new(self.current.1);
                loop {
                    match parser.input(self.current.0, self.current.1, self.peek1.0, self.diagnostics) {
                        StringLiteralParserResult::WantMore => {
                            self.move_next();
                        },
                        StringLiteralParserResult::WantMoreWithSkip1 => {
                            self.move_next();
                            self.move_next();
                        }
                        StringLiteralParserResult::Finished(value, span) => {
                            if self.current.0 != EOF {
                                self.move_next(); // should not consume EOF, or else EOF will not return
                            }
                            return (Token::Str(value.map(|v| self.chars.intern(&v)).unwrap_or(IsId::new(1)), StringLiteralType::Normal), span);
                        }
                    }
                }
            },
            StringLiteralType::Raw => {
                let mut parser = RawStringLiteralParser::new(self.current.1);
                loop {
                    match parser.input(self.current.0, self.current.1, self.diagnostics) {
                        RawStringLiteralParserResult::WantMore => {
                            self.move_next();
                        },
                        RawStringLiteralParserResult::Finished(value, span) => {
                            if self.current.0 != EOF {
                                self.move_next(); // should not consume EOF, or else EOF will not return
                            }
                            return (Token::Str(value.map(|v| self.chars.intern(&v)).unwrap_or(IsId::new(1)), StringLiteralType::Normal), span);
                        }
                    }
                }
            },
            _ => unreachable!(),
        }
    }

    fn check_confusable(&mut self) {
        if let Some((unicode_char, unicode_name, ascii_char, ascii_name)) = unicode::check_confusable(self.current.0) {
            self.diagnostics.push(Message::with_help_by_str(strings::UnexpectedNonASCIIChar, vec![
                (self.current.1.into(), ""), 
            ], vec![
                &format!("Did you mean `{}`({}) by `{}`({})?", ascii_char, ascii_name, unicode_char, unicode_name),
            ]));
            self.current.0 = ascii_char;
        }
    }
    fn skip_whitespace(&mut self) {
        while self.current.0.is_whitespace() {
            self.move_next();
        }
    }

    pub fn next(&mut self) -> (Token, Span) {

        self.skip_line_comment();
        self.skip_block_comment();
        
        // put eof after skip comment or else eof after line/block comment will report unknown charactor
        if let EOF = self.current.0 {
            return (Token::EOF, self.current.1.into()); // no move next when meet EOF makes this iterator fuse
        }

        if let '\'' = self.current.0 { // TODO: change to self.is_char_start() after change CharLiteralParser to self.parse_char_literal
            return self.parse_char_literal();
        } else if let '"' = self.current.0 { // TODO: change to self.is_string_start after change StringLiteralParser to self.parse_string_literal 
            return self.parse_string_literal(StringLiteralType::Normal);
        } else if let ('r' | 'R', '"') = (self.current.0, self.peek1.0) {
            return self.parse_string_literal(StringLiteralType::Raw);
        }

        // call after skip comment and parse string/char literals to ignore confusables inside them
        self.check_confusable();
        // call skip whitespace after string literal because should not skip whitespace inside string literal
        self.skip_whitespace();

        if self.current.0.is_id_start() {
            let mut value = String::new();
            let mut span = self.current.1.into();
            while self.current.0.is_id_continue() {
                value.push(self.current.0);
                span += self.current.1;
                self.move_next();
            }
            return match Keyword::parse(&value) {
                Some(Keyword::True) => (Token::Bool(true), span),
                Some(Keyword::False) => (Token::Bool(false), span),
                Some(keyword) => {
                    if keyword.kind(KeywordKind::Reserved) {
                        self.diagnostics.push(Message::new(
                            format!("{}: {:?}", strings::UseReservedKeyword, keyword), 
                            vec![(span, String::new())]
                        ));
                    }
                    (Token::Keyword(keyword), span)
                },
                None => (Token::Ident(self.chars.intern(&value)), span),
            };
        } else if self.current.0.is_label_start() {
            let mut value = String::new();
            let mut span = self.current.1.into();
            self.move_next(); // skip leading @
            while self.current.0.is_label_continue() {
                value.push(self.current.0);
                span += self.current.1;
                self.move_next();
            }
            return (Token::Label(self.chars.intern(&value)), span);
            // TODO: move CharExt::is_numeric_{start|continue} into literal::numeric
            // this is_numeric_start rejects hyphen
            // numeric parser supports that but not used in lexical parser,
            // keep for generic numeric parser (e.g. the one in standard library)
        } else if self.current.0.is_numeric_start() {
            let mut string_value = String::new();
            let mut span = self.current.1.into();
            while self.current.0.is_numeric_continue() {
                // exclude 1..2 for range expression
                // numeric parser supports that but not used in lexical parser, keep for generic numeric parser
                if (self.current.0 == '.' && self.peek1.0 == '.')
                    // exclude 1.to_string()
                    // this also rejects 1._123, which is recognized as an error in 
                    // numeric parser but not used in lexical parser, keep for generic numeric parser
                    || (self.current.0 == '.' && self.peek1.0.is_id_start()) {
                    break;
                }
                string_value.push(self.current.0);
                span += self.current.1;
                self.move_next();
            }
            let (value, span) = parse_numeric_literal(string_value, span, self.diagnostics);
            return (Token::Num(value.unwrap_or(Numeric::I32(0))), span);
        }
        
        match Separator::parse3(self.current.0, self.peek1.0, self.peek2.0) {
            Some((separator, 1)) => {
                self.move_next();
                return (Token::Sep(separator), self.current.1.into());
            },
            Some((separator, 2)) => {
                self.move_next();
                self.move_next();
                return (Token::Sep(separator), self.current.1 + self.peek1.1);
            },
            Some((separator, 3)) => {
                self.move_next();
                self.move_next();
                self.move_next();
                return (Token::Sep(separator), self.current.1 + self.peek2.1);
            },
            _ => {
                self.diagnostics.push(Message::new_by_str(strings::UnknownCharactor, vec![
                    (self.current.1.into(), ""), 
                ]));
                // return empty string ident (which will not be created by parse ident) for unknown charactor
                return (Token::Ident(IsId::new(1)), self.current.1.into());
            }
        }
    }

    // finish self to return scx
    pub fn finish(self) {
        self.chars.finish();
    }
}
