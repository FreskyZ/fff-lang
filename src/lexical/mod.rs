///! lexical: the lexical parser

use crate::source::{SourceChars, FileSystem, DefaultFileSystem, EOF, Position, Span, IsId};
use crate::diagnostics::{Diagnostics, strings};

#[cfg(test)]
mod tests;
mod token;
mod unicode;
mod literal {
    pub mod chars;
    pub mod numeric;
}

use unicode::CharExt;
pub use token::{Separator, SeparatorKind, Keyword, KeywordKind};
pub use token::{Numeric, StringLiteralType, Token, TokenDisplay};

#[derive(Debug)]
pub struct Parser<'ecx, 'scx, F = DefaultFileSystem> {
    pub diagnostics: &'ecx mut Diagnostics,
    pub /* attention: temp pub for syntax */ chars: SourceChars<'scx, F>,
    current: char,
    current_position: Position,
    peek: char,
    peek_position: Position,
    peek2: char,
    peek2_position: Position,
    check_confusable: bool, // check confusable when consuming, off for comment/string/char (level 1), on for ident/label/numeric
}

impl<'e, 's, F> Parser<'e, 's, F> where F: FileSystem {

    pub fn new(mut chars: SourceChars<'s, F>, diagnostics: &'e mut Diagnostics) -> Self {
        let (current, current_position) = chars.next();
        let (peek, peek_position) = chars.next();
        let (peek2, peek2_position) = chars.next();
        Self{ diagnostics, current, current_position, peek, peek_position, peek2, peek2_position, chars, check_confusable: false }
    }

    // eat and check confusable
    fn eat(&mut self) {
        self.current = self.peek;
        self.current_position = self.peek_position;
        self.peek = self.peek2;
        self.peek_position = self.peek2_position;
        let (peek2, peek2_position) = self.chars.next();
        self.peek2 = peek2;
        self.peek2_position = peek2_position;
        // destructuring assignment is unstable
        // (self.peek2, self.peek2_position) = self.chars.next();

        if self.check_confusable && self.current != EOF {
            if let Some((unicode_char, unicode_name, ascii_char, ascii_name)) = unicode::check_confusable(self.current) {
                self.diagnostics.emit(strings::UnexpectedNonASCIIChar)
                    .span(self.current_position)
                    .help(format!("Did you mean `{}`({}) by `{}`({})?", ascii_char, ascii_name, unicode_char, unicode_name));
                self.current = ascii_char;
            }
        }
    }

    // return true for actually skipped something
    fn skip_whitespace(&mut self) -> bool {
        let start_position = self.current_position;
        // TODO: add this to unicode
        while self.current.is_whitespace() {
            self.eat();
        }
        self.current_position != start_position
    }

    // return true for actually skipped something
    fn skip_line_comment(&mut self) -> bool {
        if let ('/', '/') = (self.current, self.peek) {
            loop {
                self.eat();
                if self.current == EOF {
                    break true;
                }
                if self.current == '\n' {
                    self.eat();
                    break true;
                }
            }
        } else {
            false
        }
    }

    // return true for actually skipped something
    fn skip_block_comment(&mut self) -> bool {
        if let ('/', '*') = (self.current, self.peek) {
            let start_position = self.current_position;
            let mut level = 0;
            loop {
                self.eat();
                if self.current == EOF {
                    self.diagnostics.emit(strings::UnexpectedEOF)
                        .detail(start_position, strings::BlockCommentStartHere)
                        .detail(self.current_position, strings::EOFHere);
                    break true;
                }
                if let ('/', '*') = (self.current, self.peek) {
                    self.eat();
                    level += 1;
                } else if let ('*', '/') = (self.current, self.peek) {
                    self.eat();
                    if level == 0 {
                        self.eat();
                        break true;
                    } else {
                        level -= 1;
                    }
                }
            }
        } else {
            false
        }
    }

    fn parse_ident(&mut self) -> (Token, Span) {
        let mut value = String::new();
        let mut span = self.current_position.into();
        while self.current.is_id_continue() {
            value.push(self.current);
            span += self.current_position;
            self.eat();
        }
        return match Keyword::parse(&value) {
            Some(Keyword::True) => (Token::Bool(true), span),
            Some(Keyword::False) => (Token::Bool(false), span),
            Some(keyword) => {
                if keyword.kind(KeywordKind::Reserved) {
                    self.diagnostics.emit(format!("{}: {:?}", strings::UseReservedKeyword, keyword)).span(span);
                }
                (Token::Keyword(keyword), span)
            },
            None => (Token::Ident(self.chars.intern(&value)), span),
        };
    }

    fn parse_label(&mut self) -> (Token, Span) {
        let mut value = String::new();
        let mut span = self.current_position.into();
        self.eat(); // skip leading @
        while self.current.is_label_continue() {
            value.push(self.current);
            span += self.current_position;
            self.eat();
        }
        return (Token::Label(self.chars.intern(&value)), span);
    }

    fn parse_separator(&mut self) -> (Token, Span) {

        match Separator::parse3(self.current, self.peek, self.peek2) {
            Some((separator, 1)) => {
                let span = self.current_position.into();
                self.eat();
                (Token::Sep(separator), span)
            },
            Some((separator, 2)) => {
                let span = self.current_position + self.peek_position;
                self.eat();
                self.eat();
                (Token::Sep(separator), span)
            },
            Some((separator, 3)) => {
                let span = self.current_position + self.peek2_position;
                self.eat();
                self.eat();
                self.eat();
                (Token::Sep(separator), span)
            },
            _ => {
                self.diagnostics.emit(strings::UnknownCharactor).span(self.current_position);
                // return empty string ident (which will not be created by parse ident) for unknown charactor
                (Token::Ident(IsId::new(1)), self.current_position.into())
            }
        }
    }

    pub fn next(&mut self) -> (Token, Span) {

        self.check_confusable = false;
        // there may be multiple things to be skipped before an actual token
        while self.skip_whitespace()
            || self.skip_line_comment()
            || self.skip_block_comment() {}
        
        // put eof after skip comment or else eof after line/block comment will report unknown charactor
        if let EOF = self.current {
            return (Token::EOF, self.current_position.into()); // no move next when meet EOF makes this iterator fuse
        }

        if self.is_char_literal_start() {
            return self.parse_char_literal();
        } else if self.is_normal_string_literal_start() {
            return self.parse_normal_string_literal();
        } else if self.is_raw_string_literal_start() {
            return self.parse_raw_string_literal();
        }

        self.check_confusable = true;
        if self.current.is_id_start() {
            return self.parse_ident();
        } else if self.current.is_label_start() {
            return self.parse_label();
        } else if self.is_numeric_start() {
            return self.parse_numeric_literal();
        }
        
        self.parse_separator()
    }

    // finish self to return scx
    pub fn finish(self) {
        self.chars.finish();
    }
}
