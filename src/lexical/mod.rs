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
}

use unicode::CharExt;
use literal::numeric::parse_numeric_literal;
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

    // eat not care about whatever is next char, use in comment and string literal
    fn eatnc(&mut self) {
        std::mem::swap(&mut self.current, &mut self.peek1);
        std::mem::swap(&mut self.peek1, &mut self.peek2);
        self.peek2 = self.chars.next();
    }

    // eat and check confusable
    fn eat(&mut self) {
        std::mem::swap(&mut self.current, &mut self.peek1);
        std::mem::swap(&mut self.peek1, &mut self.peek2);
        self.peek2 = self.chars.next();

        if self.current.0 != EOF {
            if let Some((unicode_char, unicode_name, ascii_char, ascii_name)) = unicode::check_confusable(self.current.0) {
                self.diagnostics.push(Message::with_help_by_str(strings::UnexpectedNonASCIIChar, vec![
                    (self.current.1.into(), ""), 
                ], vec![
                    &format!("Did you mean `{}`({}) by `{}`({})?", ascii_char, ascii_name, unicode_char, unicode_name),
                ]));
                self.current.0 = ascii_char;
            }
        }
    }

    fn skip_line_comment(&mut self) {
        if let ('/', '/') = (self.current.0, self.peek1.0) {
            loop {
                self.eatnc();
                if self.current.0 == EOF {
                    break;
                }
                if self.current.0 == '\n' {
                    self.eatnc();
                    break;
                }
            }
        }
    }

    fn skip_block_comment(&mut self) {
        if let ('/', '*') = (self.current.0, self.peek1.0) {
            let start_position = self.current.1;
            let mut level = 0;
            loop {
                self.eatnc();
                if self.current.0 == EOF {
                    self.diagnostics.push(Message::new_by_str(strings::UnexpectedEOF, vec![
                        (start_position.into(), strings::BlockCommentStartHere),
                        (self.current.1.into(), strings::EOFHere),
                    ]));
                    break;
                }
                if let ('/', '*') = (self.current.0, self.peek1.0) {
                    self.eatnc();
                    level += 1;
                } else if let ('*', '/') = (self.current.0, self.peek1.0) {
                    self.eatnc();
                    if level == 0 {
                        self.eatnc();
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
        self.eat();
        loop {
            match parser.input(self.current.0, self.current.1, self.peek1.0, self.diagnostics) {
                CharLiteralParserResult::WantMore => {
                    self.eatnc();
                },
                CharLiteralParserResult::WantMoreWithSkip1 => {
                    self.eatnc();
                    self.eatnc();
                },
                CharLiteralParserResult::Finished(value, span) => {
                    self.eatnc();
                    return (Token::Char(value.unwrap_or_default()), span);
                },
            }
        }
    }

    fn skip_whitespace(&mut self) {
        while self.current.0.is_whitespace() {
            self.eat();
        }
    }

    pub fn next(&mut self) -> (Token, Span) {

        self.skip_whitespace();
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

        if self.current.0.is_id_start() {
            let mut value = String::new();
            let mut span = self.current.1.into();
            while self.current.0.is_id_continue() {
                value.push(self.current.0);
                span += self.current.1;
                self.eat();
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
            self.eat(); // skip leading @
            while self.current.0.is_label_continue() {
                value.push(self.current.0);
                span += self.current.1;
                self.eat();
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
                self.eat();
            }
            let (value, span) = parse_numeric_literal(string_value, span, self.diagnostics);
            return (Token::Num(value.unwrap_or(Numeric::I32(0))), span);
        }
        
        match Separator::parse3(self.current.0, self.peek1.0, self.peek2.0) {
            Some((separator, 1)) => {
                let span = self.current.1.into();
                self.eat();
                return (Token::Sep(separator), span);
            },
            Some((separator, 2)) => {
                let span = self.current.1 + self.peek1.1;
                self.eat();
                self.eat();
                return (Token::Sep(separator), span);
            },
            Some((separator, 3)) => {
                let span = self.current.1 + self.peek2.1;
                self.eat();
                self.eat();
                self.eat();
                return (Token::Sep(separator), span);
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
