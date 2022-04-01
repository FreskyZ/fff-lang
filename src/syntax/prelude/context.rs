///! fff-lang
///!
///! syntax/parse_cx, parse traits and helper struct

use crate::source::{Span, IsId, FileId};
use crate::diagnostics::Diagnostic;
use crate::lexical::{Parser as LexicalParser, Token, Numeric, Separator, SeparatorKind, Keyword, KeywordKind};
use super::{Parser, Unexpected};
use super::super::ast::LitValue;

pub struct ParseContext<'ecx, 'scx> {
    base: LexicalParser<'ecx, 'scx>,
    pub(in super) current: Token,
    current_span: Span,
    pub(in super) peek: Token,
    peek_span: Span,
    pub(in super) peek2: Token,
    peek2_span: Span,

    // special parser global states
    // // these states will make this parser non-context-free according to textbooks, but that's not important

    // 1. when expr is followed with block, e.g. if/for/while/swich,
    //    when meeting `{`, e.g. `if a < b { print(a); }` it should not expect an object literal
    //    only this can `if a < (b{ c: 1 }) {}`, or this `if (a < b{ c: 1 }) {}`
    // 2. that is, when expecting expr which is followed by block, a restriction flag should be pushed,
    //    while when expecting any expr, not lower level expr/higher priority expr, inside an expr node, a non restriction should be pushed
    // 3. currently the any-expr-expectations only include fn-call, index-call, array-def and tuple-def (the parened list or bracketed list)
    //    which is exactly same as ExprList usages // was not expecting ExprList have this kind of functionality
    // 4. actual object literal itself also expects any expr, but, 
    //    when you are parsing object literal, it is already allowed, so no need to push again
    // 5. currently there will not be push(true) after any push(false) is not popped, because if/for/while/switch is not inside expr
    //    but if future they will, it will happen so keep this a stack
    pub no_object_literals: Vec<bool>,
    // if allow format string inside format string, this is also a stack of flag not a single flag
    // pub inside_string_literal: Vec<bool>,
}
#[allow(dead_code)] // helper methods may not be used
impl<'ecx, 'scx> ParseContext<'ecx, 'scx> {

    pub fn new(mut base: LexicalParser<'ecx, 'scx>) -> Self {
        let (current, current_span) = base.next();
        let (peek, peek_span) = base.next();
        let (peek2, peek2_span) = base.next();
        ParseContext{ 
            base, 
            current, 
            current_span, 
            peek, 
            peek_span, 
            peek2, 
            peek2_span, 
            no_object_literals: Vec::with_capacity(128), // 128: arbitray value for max expression depth
        }
    }

    // forward base methods
    pub fn intern(&mut self, v: &str) -> IsId {
        self.base.intern(v)
    }
    pub fn emit(&mut self, name: impl Into<String>) -> &mut Diagnostic { 
        self.base.emit(name)
    }

    pub fn get_file_id(&self) -> FileId {
        self.base.get_file_id()
    }
    pub fn finish(self) {
        self.base.finish()
    }

    // invoke node
    pub fn matches<P: Parser>(&self) -> bool {
        P::matches(&self.current) || P::matches3(&self.current, &self.peek, &self.peek2)
    }

    pub fn expect<P: Parser>(&mut self) -> Result<P::Output, Unexpected> {
        P::parse(self)
    }

    pub fn try_expect<P: Parser>(&mut self) -> Result<Option<P::Output>, Unexpected> {
        P::try_parse(self)
    }

    // special method for root node
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

    /// Check current token is a literal
    ///
    /// if so, move next and Ok((lit_value, lit_span)),
    /// if not, push unexpect and Err(Unexpected)
    ///
    /// example `let (lit, lit_span) = cx.expect_lit()?;`
    pub fn expect_lit(&mut self) -> Result<(LitValue, Span), Unexpected> {
        match self.current {
            Token::Bool(v) => Ok((LitValue::Bool(v), self.move_next())),
            Token::Char(v) => Ok((LitValue::Char(v), self.move_next())),
            Token::Num(v) => Ok((LitValue::Num(v), self.move_next())),
            Token::Str(v, _) => Ok((LitValue::Str(v), self.move_next())),
            _ => self.push_unexpect("literal"),
        }
    }

    /// Check current token is a literal, used for import module path
    ///
    /// if so, move next and Some((lit_value, lit_span)),
    /// if not, no move next and None
    ///
    /// example `let (id, id_span) = cx.try_expect_str_lit()?;`
    pub fn try_expect_str_lit(&mut self) -> Option<(IsId, Span)> {
        match self.current {
            Token::Str(v, _) => Some((v, self.move_next())),
            _ => None,
        }
    }

    /// Check current token is a literal, used for member access number
    ///
    /// if so, move next and Some((lit_value, lit_span)),
    /// if not, no move next and None
    ///
    /// example `let (value, span) = cx.try_expect_num_lit()?;`
    pub fn try_expect_numeric(&mut self) -> Option<(Numeric, Span)> {
        match self.current {
            Token::Num(v) => Some((v, self.move_next())),
            _ => None,
        }
    }

    /// Check current token is specified keyword
    /// 
    /// if so, move next and Ok(keyword_span),
    /// if not, push unexpect and Err(Unexpected)
    ///
    /// example `let kw_span = cx.expect_keyword(Keyword::In)?;`
    pub fn expect_keyword(&mut self, expected_kw: Keyword) -> Result<Span, Unexpected> {
        match self.current {
            Token::Keyword(kw) if kw == expected_kw => Ok(self.move_next()),
            _ => self.push_unexpect(expected_kw.display()),
        }
    }

    /// Check current token is one of the specified keywords
    ///
    /// if so, move next and Ok((kw, kw_span)), 
    /// if not, push unexpect and Err(Unexpected)
    ///
    /// example `let (kw, kw_span) = cx.expect_keywords(&[Const, Var])?;`
    pub fn expect_keywords(&mut self, expected_keywords: &[Keyword]) -> Result<(Keyword, Span), Unexpected> {
        match self.current {
            Token::Keyword(kw) if expected_keywords.iter().any(|e| e == &kw) => Ok((kw, self.move_next())),
            _ => self.push_unexpect(&expected_keywords.iter().map(|e| e.display()).collect::<Vec<_>>().join(", ")),
        }
    }

    /// Check current token is specified keyword
    ///
    /// if so, move next and Some(kw_span), 
    /// if not, no move next and None
    ///
    /// example `if let Some(kw_span) == cx.try_expect_keyword(Keyword::If) { ... }`
    pub fn try_expect_keyword(&mut self, expected_keyword: Keyword) -> Option<Span> {
        match self.current {
            Token::Keyword(kw) if kw == expected_keyword => Some(self.move_next()),
            _ => None,
        }
    }

    /// Check current token is one of the specified keywords
    ///
    /// if so, move next and Ok((kw, kw_span)), 
    /// if not, no move next and None
    ///
    /// example `let (kw, kw_span) = cx.expect_keywords(&[Const, Var])?;`
    pub fn try_expect_keywords(&mut self, expected_keywords: &[Keyword]) -> Option<(Keyword, Span)> {
        match self.current {
            Token::Keyword(kw) if expected_keywords.iter().any(|e| e == &kw) => Some((kw, self.move_next())),
            _ => None,
        }
    }

    /// Check current token is of Keyword kind
    ///
    /// if so, move next and Some((kw, kw_span)), 
    /// if not, no move next and None
    ///
    /// example `if let Some((kw, kw_span)) = cx.try_expect_keyword_kind(Primitive) { ... }`
    pub fn try_expect_keyword_kind(&mut self, kind: KeywordKind) -> Option<(Keyword, Span)> {
        match self.current {
            Token::Keyword(keyword) if keyword.kind(kind) => Some((keyword, self.move_next())),
            _ => None,
        }
    }

    /// Check current token is of specified keyword kind
    /// 
    /// if so, move next and Ok(keyword, keyword_span),
    /// if not, push unexpect and Err(Unexpected)
    ///
    /// example `let kw_span = cx.expect_keyword(Keyword::In)?;`
    pub fn expect_keyword_kind(&mut self, kind: KeywordKind) -> Result<(Keyword, Span), Unexpected> {
        match self.current {
            Token::Keyword(kw) if kw.kind(kind) => Ok((kw, self.move_next())),
            _ => self.push_unexpect(&format!("{:?}", kind)),
        }
    }

    /// split current shift left token to 2 less than tokens
    /// caller to check current is shift left
    /// return span for first less than and logically move to next less than by directly changing cached current token
    fn split_shift_left(&mut self) -> Span {
        let result = self.current_span.start.into();
        self.current = Token::Sep(Separator::Lt);
        self.current_span = self.current_span.end.into();
        result
    }

    /// split current shift right token to 2 greater than tokens
    /// caller to check current is shift right
    /// return span for first greater than and logically move to next greater than by directly changing cached current token
    fn split_shift_right(&mut self) -> Span {
        let result = self.current_span.start.into();
        self.current = Token::Sep(Separator::Gt);
        self.current_span = self.current_span.end.into();
        result
    }


    /// split current logical and token to 2 bit and tokens
    /// caller to check current is logical and
    /// return span for first bit and than and logically move to next bit and by directly changing cached current token
    fn split_logical_and(&mut self) -> Span {
        let result = self.current_span.start.into();
        self.current = Token::Sep(Separator::And);
        self.current_span = self.current_span.end.into();
        result
    }

    /// Check current token is specified Separator, handles split shift left, shift right, logical and
    ///
    /// if so, move next and Ok(sep_span),
    /// if not, push unexpect and Err(Unexpected)
    ///
    /// example `let sep_span = cx.expect_sep(Separator::Comma)?;`
    pub fn expect_sep(&mut self, expected_sep: Separator) -> Result<Span, Unexpected> {
        match self.current {
            Token::Sep(sep) if sep == expected_sep => Ok(self.move_next()),
            Token::Sep(Separator::GtGt) if expected_sep == Separator::Gt => Ok(self.split_shift_right()),
            Token::Sep(Separator::LtLt) if expected_sep == Separator::Lt => Ok(self.split_shift_left()),
            Token::Sep(Separator::AndAnd) if expected_sep == Separator::And => Ok(self.split_logical_and()),
            // if it is GtGtEq, you need to split into Gt+Gt+Eq, but will that hapen?
            _ => self.push_unexpect(expected_sep.display()),
        }
    }

    /// Check current token is one of the specified Separators
    ///
    /// if so, move next and Ok((sep, sep_span)),
    /// if not, push unexpect and Err(Unexpected)
    ///
    /// example `let (sep, sep_span) = cx.expect_seps(&[Separator::LeftBracket, Separator::LeftBrace])?;`
    pub fn expect_seps(&mut self, expected_seps: &[Separator]) -> Result<(Separator, Span), Unexpected> {
        match self.current {
            Token::Sep(sep) if expected_seps.iter().any(|e| e == &sep) => Ok((sep, self.move_next())),
            _ => self.push_unexpect(&expected_seps.iter().map(|e| e.display()).collect::<Vec<_>>().join(", ")),
        }
    }

    /// Check current token is specified Separator, handles split shift right
    ///
    /// if so, move next and Some(sep_span)
    /// if not, no move next and None
    ///
    /// example `if let Some(sep_span) = cx.try_expect_sep(Separator::Comma) { ... }`
    pub fn try_expect_sep(&mut self, expected_sep: Separator) -> Option<Span> {
        match self.current {
            Token::Sep(sep) if sep == expected_sep => Some(self.move_next()),
            Token::Sep(Separator::GtGt) if expected_sep == Separator::Gt => Some(self.split_shift_right()),
            Token::Sep(Separator::LtLt) if expected_sep == Separator::Lt => Some(self.split_shift_left()),
            _ => None,
        }
    }

    /// Check current token is specified closing bracket, allows optional comma, handles split shift right
    ///
    /// if so, move next and Some((sep_span, skipped comma))
    /// if not, no move next and None
    ///
    /// example `if let Some(sep_span) = cx.try_expect_sep(Separator::Comma) { ... }`
    pub fn try_expect_closing_bracket(&mut self, expected_sep: Separator) -> Option<(Span, bool)> {
        debug_assert!(matches!(expected_sep, Separator::RightBrace | Separator::RightParen | Separator::RightBracket | Separator::Gt), "not a closing bracket");
        match (&self.current, &self.peek) {
            (Token::Sep(sep), _) if *sep == expected_sep => Some((self.move_next(), false)),
            (Token::Sep(Separator::GtGt), _) if expected_sep == Separator::Gt => Some((self.split_shift_right(), false)),
            (Token::Sep(Separator::Comma), Token::Sep(sep)) if *sep == expected_sep => { self.move_next(); Some((self.move_next(), true)) },
            (Token::Sep(Separator::Comma), Token::Sep(Separator::GtGt)) if expected_sep == Separator::Gt => { self.move_next(); Some((self.split_shift_right(), true)) },
            _ => None,
        }
    }

    /// Check current token is one of the specified Separators
    ///
    /// if so, move next and Some((sep, sep_span)),
    /// if not, no move next and None
    ///
    /// example `if let Some((sep, sep_span)) = cx.try_expect_seps(&[Separator::LeftBracket, Separator::LeftBrace])?;`
    pub fn try_expect_seps(&mut self, expected_seps: &[Separator]) -> Option<(Separator, Span)> {
        match self.current {
            Token::Sep(sep) if expected_seps.iter().any(|e| e == &sep) => Some((sep, self.move_next())),
            _ => None,
        }
    }
    
    /// Check current token is of Separator kind
    ///
    /// if so, move next and Some((sep, sep_span)), 
    /// if not, no move next and None
    ///
    /// example `if let Some((sep, sep_span)) = cx.try_expect_sep_kind(Unary) { ... }`
    pub fn try_expect_sep_kind(&mut self, kind: SeparatorKind) -> Option<(Separator, Span)> {
        match self.current {
            Token::Sep(sep) if sep.kind(kind) => Some((sep, self.move_next())),
            _ => None,
        }
    }

    /// Check current token is an identifier
    ///
    /// if so, move next and Ok((id, ident_span)),
    /// if not, push unexpect and Err(Unexpected)
    ///
    /// example `let (ident_id, ident_span) = cx.expect_ident()?;`
    pub fn expect_ident(&mut self) -> Result<(IsId, Span), Unexpected> {
        match self.current {
            Token::Ident(id) => Ok((id, self.move_next())),
            _ => self.push_unexpect("identifier"),
        }
    }

    /// Check current token is an identifier
    pub fn is_ident(&mut self) -> bool {
        matches!(self.current, Token::Ident(_))
    }

    /// Check current token is a identifier
    /// 
    /// if so, move next and Some((id, id_span)), 
    /// if not, no move next and None
    ///
    /// example `if let Some((ident_id, ident_span)) = cx.try_expect_ident() { ... }`
    pub fn try_expect_ident(&mut self) -> Option<(IsId, Span)> {
        match self.current {
            Token::Ident(id) => Some((id, self.move_next())),
            _ => None,
        }
    }

    /// Check current token is a label
    /// 
    /// if so, move next and Some((id, id_span)), 
    /// if not, no move next and None
    ///
    /// example `if let Some((label_id, label_span)) = cx.try_expect_label() { ... }`
    pub fn try_expect_label(&mut self) -> Option<(IsId, Span)> {
        match self.current {
            Token::Label(id) => Some((id, self.move_next())),
            _ => None,
        }
    }

    /// Check current token is identifier or acceptable keywords
    /// 
    /// if so, move next and Ok((id, ident_span)),
    /// if not, push unexpect and Err(Unexpected)
    ///
    /// example `let (ident_id, ident_span) = cx.expect_ident_or(&[Keyword::This, Keyword::Underscore])?;`
    pub fn expect_ident_or_keywords(&mut self, acceptable_keywords: &[Keyword]) -> Result<(IsId, Span), Unexpected> {
        match self.current {
            Token::Ident(id) => Ok((id, self.move_next())),
            Token::Keyword(kw) if acceptable_keywords.iter().any(|a| a == &kw) => Ok((self.base.intern(kw.display()), self.move_next())),
            _ => self.push_unexpect(&format!("identifier or {}", acceptable_keywords.iter().map(|a| a.display()).collect::<Vec<_>>().join(", "))),
        }
    }

    /// Check current token is identifier or acceptable keywords
    /// 
    /// if so, move next and Ok((id, ident_span)),
    /// if not, no move next and None
    ///
    /// example `if let Some((ident_id, ident_span)) = cx.try_expect_ident_or(&[Keyword::This, Keyword::Underscore]) { ... }`
    pub fn try_expect_ident_or_keywords(&mut self, acceptable_keywords: &[Keyword]) -> Option<(IsId, Span)> {
        match self.current {
            Token::Ident(id) => Some((id, self.move_next())),
            Token::Keyword(kw) if acceptable_keywords.iter().any(|a| a == &kw) => Some((self.base.intern(kw.display()), self.move_next())),
            _ => None,
        }
    }
    
    /// Check current token is identifier or meet the predict
    /// 
    /// if so, move next and Ok((id, ident_span)),
    /// if not, push unexpect and Err(Unexpected)
    ///
    /// example `let (ident_id, ident_span) = cx.expect_ident_or_keyword_kind(KeywordKind::Primitive)?;`
    pub fn expect_ident_or_keyword_kind(&mut self, kind: KeywordKind) -> Result<(IsId, Span), Unexpected> {
        match self.current {
            Token::Ident(id) => Ok((id, self.move_next())),
            Token::Keyword(kw) if kw.kind(kind) => Ok((self.base.intern(kw.display()), self.move_next())),
            _ => self.push_unexpect(&format!("identifier or {:?}", kind)),
        }
    }

    pub fn push_unexpect<T>(&mut self, expect_desc: &str) -> Result<T, Unexpected> {
        self.base.emit("unexpected token")
            .detail(self.current_span, format!("meet {:?}", self.current))
            .help(format!("expected {}", expect_desc));
        Err(Unexpected)
    }
}
