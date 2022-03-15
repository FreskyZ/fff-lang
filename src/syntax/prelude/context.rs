///! fff-lang
///!
///! syntax/parse_sess, parse traits and helper struct

use crate::source::{FileSystem, Span, IsId};
use crate::diagnostics::Diagnostic;
use crate::lexical::{Parser, Token, Separator, SeparatorKind, Keyword, KeywordKind};
use super::node::Node;
use super::super::LitValue;

pub type ParseResult<T> = Result<T, ()>;

pub struct ParseSession<'ecx, 'scx, F> {
    pub /* attention: temp pub for syntax */ base: Parser<'ecx, 'scx, F>,
    pub(in super) current: Token,
    current_span: Span,
    pub(in super) peek: Token,
    peek_span: Span,
    pub(in super) peek2: Token,
    peek2_span: Span,
}
#[allow(dead_code)] // helper methods may not be used
impl<'ecx, 'scx, F> ParseSession<'ecx, 'scx, F> where F: FileSystem {

    pub fn new(mut base: Parser<'ecx, 'scx, F>) -> Self {
        let (current, current_span) = base.next();
        let (peek, peek_span) = base.next();
        let (peek2, peek2_span) = base.next();
        ParseSession{ base, current, current_span, peek, peek_span, peek2, peek2_span }
    }

    pub fn eof(&self) -> bool {
        matches!(self.current, Token::EOF)
    }

    // return previous current span
    fn move_next(&mut self) -> Span {
        let original_current_span = self.current_span;
        std::mem::swap(&mut self.current, &mut self.peek);
        self.current_span = self.peek_span;
        std::mem::swap(&mut self.peek, &mut self.peek2);
        self.peek_span = self.peek2_span;
        let (peek2, peek2_span) = self.base.next();
        self.peek2 = peek2;
        self.peek2_span = peek2_span;
        original_current_span
    }
    // return previous current span
    fn move_next2(&mut self) -> Span {
        let original_current_span = self.current_span;
        std::mem::swap(&mut self.current, &mut self.peek2);
        self.current_span = self.peek2_span;
        let (peek, peek_span) = self.base.next();
        self.peek = peek;
        self.peek_span = peek_span;
        let (peek2, peek2_span) = self.base.next();
        self.peek2 = peek2;
        self.peek2_span = peek2_span;
        original_current_span
    }

    /// Check current token is specified keyword
    /// 
    /// if so, move next and Ok(keyword_span),
    /// if not, push unexpect and Err(())
    ///
    /// example `let kw_span = sess.expect_keyword(Keyword::In)?;`
    pub fn expect_keyword(&mut self, expected_kw: Keyword) -> Result<Span, ()> {
        match self.current {
            Token::Keyword(kw) if kw == expected_kw => Ok(self.move_next()),
            _ => self.push_unexpect(expected_kw.display()),
        }
    }

    /// Check current token is specified Separator
    ///
    /// if so, move next and Ok(sep_span),
    /// if not, push unexpect and Err(())
    ///
    /// example `let sep_span = sess.expect_sep(Separator::Comma)?;`
    pub fn expect_sep(&mut self, expected_sep: Separator) -> Result<Span, ()> {
        match self.current {
            Token::Sep(sep) if sep == expected_sep => Ok(self.move_next()),
            _ => self.push_unexpect(expected_sep.display()),
        }
    }

    /// Check current token is a literal
    ///
    /// if so, move next and Ok((lit_value, lit_span)),
    /// if not, push unexpect and Err(())
    ///
    /// example `let (lit, lit_span) = sess.expect_lit()?;`
    pub fn expect_lit(&mut self) -> Result<(LitValue, Span), ()> {
        match self.current {
            Token::Bool(v) => Ok((LitValue::Bool(v), self.move_next())),
            Token::Char(v) => Ok((LitValue::Char(v), self.move_next())),
            Token::Num(v) => Ok((LitValue::Num(v), self.move_next())),
            Token::Str(v, _) => Ok((LitValue::Str(v), self.move_next())),
            _ => self.push_unexpect("literal"),
        }
    }

    /// Check current token is one of the specified Separators
    ///
    /// if so, move next and Ok((sep, sep_span)),
    /// if not, push unexpect and Err(())
    ///
    /// example `let (sep, sep_span) = sess.expect_seps(&[Separator::LeftBracket, Separator::LeftBrace])?;`
    pub fn expect_seps(&mut self, expected_seps: &[Separator]) -> Result<(Separator, Span), ()> {
        match self.current {
            Token::Sep(sep) if expected_seps.iter().any(|e| e == &sep) => Ok((sep, self.move_next())),
            _ => self.push_unexpect(&expected_seps.iter().map(|e| e.display()).collect::<Vec<_>>().join(", ")),
        }
    }

    /// Check current token is one of the specified keywords
    ///
    /// if so, move next and Ok((kw, kw_span)), 
    /// if not, push unexpect and Err(())
    ///
    /// example `let (kw, kw_span) = sess.expect_keywords(&[Const, Var])?;`
    pub fn expect_keywords(&mut self, expected_keywords: &[Keyword]) -> Result<(Keyword, Span), ()> {
        match self.current {
            Token::Keyword(kw) if expected_keywords.iter().any(|e| e == &kw) => Ok((kw, self.move_next())),
            _ => self.push_unexpect(&expected_keywords.iter().map(|e| e.display()).collect::<Vec<_>>().join(", ")),
        }
    }

    /// Check current token is an identifier
    ///
    /// if so, move next and Ok((id, ident_span)),
    /// if not, push unexpect and Err(())
    ///
    /// example `let (ident_id, ident_span) = sess.expect_ident()?;`
    pub fn expect_ident(&mut self) -> Result<(IsId, Span), ()> {
        match self.current {
            Token::Ident(id) => Ok((id, self.move_next())),
            _ => self.push_unexpect("identifier"),
        }
    }

    /// Check current token is a label
    /// 
    /// if so, move next and Some((id, id_span)), 
    /// if not, no move next and None
    ///
    /// example `if let Some((label_id, label_span)) = sess.try_expect_label() { ... }`
    pub fn try_expect_label(&mut self) -> Option<(IsId, Span)> {
        match self.current {
            Token::Label(id) => Some((id, self.move_next())),
            _ => None,
        }
    }

    /// Check current token is identifier or acceptable keywords
    /// 
    /// if so, move next and Ok((id, ident_span)),
    /// if not, push unexpect and Err(())
    ///
    /// example `let (ident_id, ident_span) = sess.expect_ident_or(&[Keyword::This, Keyword::Underscore])?;`
    pub fn expect_ident_or(&mut self, acceptable_keywords: &[Keyword]) -> Result<(IsId, Span), ()> {
        match self.current {
            Token::Ident(id) => Ok((id, self.move_next())),
            Token::Keyword(kw) if acceptable_keywords.iter().any(|a| a == &kw) => Ok((self.base.chars.intern(kw.display()), self.move_next())),
            _ => self.push_unexpect(&format!("identifier or {}", acceptable_keywords.iter().map(|a| a.display()).collect::<Vec<_>>().join(", "))),
        }
    }
    
    /// Check current token is identifier or meet the predict
    /// 
    /// if so, move next and Ok((id, ident_span)),
    /// if not, push unexpect and Err(())
    ///
    /// example `let (ident_id, ident_span) = sess.expect_ident_or_keyword_kind(KeywordKind::Primitive)?;`
    pub fn expect_ident_or_keyword_kind(&mut self, kind: KeywordKind) -> Result<(IsId, Span), ()> {
        match self.current {
            Token::Ident(id) => Ok((id, self.move_next())),
            Token::Keyword(kw) if kw.kind(kind) => Ok((self.base.chars.intern(kw.display()), self.move_next())),
            _ => self.push_unexpect(&format!("identifier or {:?}", kind)),
        }
    }

    /// Check current token is specified Separator
    ///
    /// if so, move next and Some(sep_span), 
    /// if not, no move next and None
    ///
    /// example `if let Some(sep_span) = sess.try_expect_sep(Separator::Comma) { ... }`
    pub fn try_expect_sep(&mut self, expected_sep: Separator) -> Option<Span> {
        match self.current {
            Token::Sep(sep) if sep == expected_sep => Some(self.move_next()),
            _ => None,
        }
    }

    /// Check current token is one of the specified Separators
    ///
    /// if so, move next and Some((sep, sep_span)),
    /// if not, no move next and None
    ///
    /// example `if let Some((sep, sep_span)) = sess.try_expect_seps(&[Separator::LeftBracket, Separator::LeftBrace])?;`
    pub fn try_expect_seps(&mut self, expected_seps: &[Separator]) -> Option<(Separator, Span)> {
        match self.current {
            Token::Sep(sep) if expected_seps.iter().any(|e| e == &sep) => Some((sep, self.move_next())),
            _ => None,
        }
    }

    /// Check current token is specified keyword
    ///
    /// if so, move next and Some(kw_span), 
    /// if not, no move next and None
    ///
    /// example `if let Some(kw_span) == sess.try_expect_keyword(Keyword::If) { ... }`
    pub fn try_expect_keyword(&mut self, expected_keyword: Keyword) -> Option<Span> {
        match self.current {
            Token::Keyword(kw) if kw == expected_keyword => Some(self.move_next()),
            _ => None,
        }
    }

    /// Check current token is of Keyword kind
    ///
    /// if so, move next and Some((kw, kw_span)), 
    /// if not, no move next and None
    ///
    /// example `if let Some((kw, kw_span)) = sess.try_expect_keyword_kind(Primitive) { ... }`
    pub fn try_expect_keyword_kind(&mut self, kind: KeywordKind) -> Option<(Keyword, Span)> {
        match self.current {
            Token::Keyword(keyword) if keyword.kind(kind) => Some((keyword, self.move_next())),
            _ => None,
        }
    }
    
    /// Check current and next token is specified Separators
    ///
    /// if so, move next and Some((sep1_span, sep2_span)),
    /// if not, no move next and None
    ///
    /// example `if let Some((comma_span, ending_span)) = sess.try_expect_2_spe(Separator::Comma, Separator::RParen) { ... }`
    pub fn try_expect_2_sep(&mut self, expected_sep1: Separator, expected_sep2: Separator) -> Option<(Span, Span)> {
        match (&self.current, &self.peek) {
            (Token::Sep(sep1), Token::Sep(sep2)) if sep1 == &expected_sep1 && sep2 == &expected_sep2 => { Some((self.move_next(), self.move_next())) },
            _ => None,
        }
    }

    /// Check current token is of Separator kind
    ///
    /// if so, move next and Some((sep, sep_span)), 
    /// if not, no move next and None
    ///
    /// example `if let Some((sep, sep_span)) = sess.try_expect_sep_kind(Unary) { ... }`
    pub fn try_expect_sep_kind(&mut self, kind: SeparatorKind) -> Option<(Separator, Span)> {
        match self.current {
            Token::Sep(sep) if sep.kind(kind) => Some((sep, self.move_next())),
            _ => None,
        }
    }

    /// if current token is shift right, regard it as 2 right angle brackets
    /// return location of first right angle bracket, and change current token to the second right angle bracket
    // is the shift right/double right angle bracket issue this simple?
    pub fn split_shift_right(&mut self) -> Option<Span> {
        match self.current {
            Token::Sep(Separator::GtGt) => {
                let first_location = self.current_span.start.into();
                self.current = Token::Sep(Separator::Gt);
                self.current_span = self.current_span.end.into();
                Some(first_location)
            },
            _ => None,
        }
    }

    // TODO: decide how to intern in syntax parser
    pub fn intern(&mut self, v: &str) -> IsId {
        self.base.chars.intern(v)
    }

    pub fn emit(&mut self, name: impl Into<String>) -> &mut Diagnostic { 
        self.base.diagnostics.emit(name)
    }

    pub fn push_unexpect<T>(&mut self, expect_desc: &str) -> ParseResult<T> {
        self.base.diagnostics.emit("Unexpect symbol")
            .detail(self.current_span, format!("Meet {:?}", self.current))
            .help(format!("Expect {}", expect_desc));
        return Err(());
    }

    pub fn matches<N: Node>(&self) -> bool {
        N::matches(&self.current) || N::matches3(&self.current, &self.peek, &self.peek2)
    } 
}
