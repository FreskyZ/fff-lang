///! lexical: the lexical parser

use crate::source::{SourceChars, EOF, Position, Span, IsId, FileId};
use crate::diagnostics::{Diagnostics, Diagnostic, strings};

#[cfg(test)]
mod tests;
mod token;
mod unicode;
mod literal {
    pub mod chars;
    pub mod numeric;
}

use unicode::CharExt;
pub use token::{Separator, SeparatorKind,
    Keyword, KeywordKind, Numeric, StringLiteralType, Token, TokenDisplay};

#[derive(Debug)]
pub struct Parser<'ecx, 'scx> {
    diagnostics: &'ecx mut Diagnostics,
    base: SourceChars<'scx>,

    // look ahead buffer and their positions
    // this is not LL1 because of shift assign ops, raw binary string and some others
    buf: [char; 3],
    pos: [Position; 3],

    // check confusable when consuming
    // off for comment/string/char content, on for ident/label/numeric/separator
    check_confusable: bool,
    // syntax parser also need to store this state so make this pub should be easier
    pub inside_format_string: bool,
}

impl<'ecx, 'scx> Parser<'ecx, 'scx> {

    pub fn new(mut base: SourceChars<'scx>, diagnostics: &'ecx mut Diagnostics) -> Self {
        let (ahead1, ahead1_position) = base.next();
        let (ahead2, ahead2_position) = base.next();
        let (ahead3, ahead3_position) = base.next();
        Self{
            diagnostics,
            base,
            buf: [ahead1, ahead2, ahead3],
            pos: [ahead1_position, ahead2_position, ahead3_position],
            check_confusable: false,
            inside_format_string: false,
        }
    }

    // eat and check confusable
    fn eat(&mut self) {
        self.buf[0] = self.buf[1];
        self.buf[1] = self.buf[2];
        self.pos[0] = self.pos[1];
        self.pos[1] = self.pos[2];
        (self.buf[2], self.pos[2]) = self.base.next();
        
        if self.check_confusable && self.buf[0] != EOF
            && let Some((unicode_char, unicode_name, ascii_char, ascii_name)) = unicode::check_confusable(self.buf[0]) {
            self.diagnostics.emit(strings::UnexpectedNonASCIIChar)
                .span(self.pos[0])
                .help(format!("Did you mean `{}`({}) by `{}`({})?", ascii_char, ascii_name, unicode_char, unicode_name));
            self.buf[0] = ascii_char;
        }
    }

    // fn reset(&mut self, span: Span) {}

    // return true for actually skipped something
    fn skip_whitespace(&mut self) -> bool {

        if self.buf[0].is_whitespace() {
            self.eat();
            true
        } else {
            false
        }
    }

    // return true for actually skipped something
    fn skip_line_comment(&mut self) -> bool {
        if let ['/', '/', ..] = self.buf {
            loop {
                self.eat();
                if self.buf[0] == EOF {
                    break true;
                }
                if self.buf[0] == '\n' {
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
        if let ['/', '*', ..] = self.buf {
            let start_position = self.pos[0];
            let mut level = 0;
            loop {
                self.eat();
                if self.buf[0] == EOF {
                    self.diagnostics.emit(strings::UnexpectedEOF)
                        .detail(start_position, strings::BlockCommentStartHere)
                        .detail(self.pos[0], strings::EOFHere);
                    break true;
                }
                if let ['/', '*', ..] = self.buf {
                    self.eat();
                    level += 1;
                } else if let ['*', '/', ..] = self.buf {
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

    fn parse_ident(&mut self) -> (Token, String, Span) {

        let mut value = String::new();
        let mut span = self.pos[0].into();
        while self.buf[0].is_id_continue() {
            value.push(self.buf[0]);
            span += self.pos[0];
            self.eat();
        }

        match Keyword::parse(&value) {
            Some(Keyword::True) => (Token::Bool(true), value, span),
            Some(Keyword::False) => (Token::Bool(false), value, span),
            Some(keyword) => {
                if keyword.kind(KeywordKind::Reserved) {
                    self.diagnostics.emit(format!("{}: {:?}", strings::UseReservedKeyword, keyword)).span(span);
                }
                if keyword.kind(KeywordKind::MaybeIdentifier) {
                    // intern maybe identifier keywords because 
                    // syntax parser may get string id from them, this makes string id more consist with appear order in source code
                    self.base.intern(keyword.display());
                }
                (Token::Keyword(keyword), value, span)
            },
            None => (Token::Ident(self.intern_span(span)), value, span),
        }
    }

    fn parse_label(&mut self) -> (Token, Span) {

        let mut value = String::new();
        let mut span: Span = self.pos[0].into();
        self.eat(); // eat leading @
        while self.buf[0].is_label_continue() {
            value.push(self.buf[0]);
            span += self.pos[0];
            self.eat();
        }

        if span.start == span.end {
            // valid span always points to non empty slice,
            // if label is empy, e.g. `@: loop { ... break @; }`, start.offset(1) + end will panic invalid position + position
            (Token::Label(self.intern("")), span)
        } else {
            (Token::Label(self.intern_span(span.start.offset(1) + span.end)), span)
        }
    }

    fn parse_separator(&mut self) -> (Token, Span) {

        match Separator::parse(self.buf) {
            Some((separator, 1)) => {
                let span = self.pos[0].into();
                self.eat();
                (Token::Sep(separator), span)
            },
            Some((separator, 2)) => {
                let span = self.pos[0] + self.pos[1];
                self.eat(); self.eat();
                (Token::Sep(separator), span)
            },
            Some((separator, 3)) => {
                let span = self.pos[0] + self.pos[2];
                self.eat(); self.eat(); self.eat();
                (Token::Sep(separator), span)
            },
            _ => {
                self.diagnostics.emit(strings::UnknownCharactor).span(self.pos[0]);
                // return empty string ident (which will not be created by parse ident) for unknown charactor
                (Token::Ident(IsId::new(1)), self.pos[0].into())
            }
        }
    }

    pub fn next(&mut self) -> (Token, Span) {
        // off by default
        self.check_confusable = false;

        // there may be multiple things to be skipped before an actual token
        while self.skip_whitespace()
            || self.skip_line_comment()
            || self.skip_block_comment() {}
        
        // put eof after skip comment or else eof after line/block comment will report unknown charactor
        if let EOF = self.buf[0] {
            return (Token::EOF, self.pos[0].into()); // no move next when meet EOF makes this iterator fuse
        }

        // most of following things need this
        self.check_confusable = true;
        if self.is_quote() {
            self.parse_quoted(None)
        } else if self.buf[0].is_id_start() {
            let (ident, ident_value, ident_span) = self.parse_ident();
            if self.is_quote() {
                self.parse_quoted(Some((&ident_value, ident_span)))
            } else {
                (ident, ident_span)
            }
        } else if self.buf[0].is_label_start() {
            self.parse_label()
        } else if self.is_numeric_start() {
            self.parse_numeric()
        } else {
            self.parse_separator()
        }
    }
}

// forward methods for syntax parser
impl<'ecx, 'scx> Parser<'ecx, 'scx> {

    pub fn emit(&mut self, name: impl Into<String>) -> &mut Diagnostic {
        self.diagnostics.emit(name)
    }

    pub fn intern(&mut self, v: &str) -> IsId {
        self.base.intern(v)
    }
    pub fn intern_span(&mut self, span: Span) -> IsId {
        self.base.intern_span(span)
    }

    pub fn get_file_id(&self) -> FileId {
        self.base.get_file_id()
    }

    pub fn finish(self) {
        self.base.finish()
    }
}
