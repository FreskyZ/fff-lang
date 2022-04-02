
use crate::source::{Span, IsId, FileId};
use crate::diagnostics::{Diagnostic, strings};
use crate::lexical::{Parser as LexicalParser, Token, Numeric, Separator, SeparatorKind, Keyword, KeywordKind};
use super::visit::Node;
use super::ast::*;

mod expr;
mod stmt;
mod r#type;

/// unrecoverable unexpected for this parser, detail in diagnostics
// this should be more readable than previous Result<Self::Output, ()>
#[derive(Debug)]
pub struct Unexpected;

pub trait Parser: Sized {
    type Output: Node;
    fn parse(_cx: &mut ParseContext) -> Result<Self::Output, Unexpected>;
}

trait ThenTry {
    fn then_try<T, E>(self, f: impl FnOnce() -> Result<T, E>) -> Result<Option<T>, E>;
}
impl ThenTry for bool {
    fn then_try<T, E>(self, f: impl FnOnce() -> Result<T, E>) -> Result<Option<T>, E> {
        self.then(f).transpose()
    }
}

pub struct ParseContext<'ecx, 'scx> {
    base: LexicalParser<'ecx, 'scx>,
    current: Token,
    current_span: Span,
    peek: Token,
    peek_span: Span,
    peek2: Token,
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
    no_object_literals: Vec<bool>,
    // if allow format string inside format string, this is also a stack of flag not a single flag
    // pub inside_string_literal: Vec<bool>,
}

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
    fn intern(&mut self, v: &str) -> IsId {
        self.base.intern(v)
    }
    fn emit(&mut self, name: impl Into<String>) -> &mut Diagnostic { 
        self.base.emit(name)
    }

    fn get_file_id(&self) -> FileId {
        self.base.get_file_id()
    }
    pub fn finish(self) {
        self.base.finish()
    }

    fn expect<P: Parser>(&mut self) -> Result<P::Output, Unexpected> {
        P::parse(self)
    }

    // special method for root node
    fn eof(&self) -> bool {
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

    /// check current token is a literal
    fn is_lit(&self) -> bool {
        matches!(self.current, Token::Char(_) | Token::Bool(_) | Token::Str(..) | Token::Num(_)) 
    }

    /// Check current token is a literal
    ///
    /// if so, move next and Ok((lit_value, lit_span)),
    /// if not, push unexpect and Err(Unexpected)
    ///
    /// example `let (lit, lit_span) = cx.expect_lit()?;`
    fn expect_lit(&mut self) -> Result<(LitValue, Span), Unexpected> {
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
    fn try_expect_str_lit(&mut self) -> Option<(IsId, Span)> {
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
    fn try_expect_numeric(&mut self) -> Option<(Numeric, Span)> {
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
    fn expect_keyword(&mut self, expected_kw: Keyword) -> Result<Span, Unexpected> {
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
    fn expect_keywords(&mut self, expected_keywords: &[Keyword]) -> Result<(Keyword, Span), Unexpected> {
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
    fn try_expect_keyword(&mut self, expected_keyword: Keyword) -> Option<Span> {
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
    fn try_expect_keywords(&mut self, expected_keywords: &[Keyword]) -> Option<(Keyword, Span)> {
        match self.current {
            Token::Keyword(kw) if expected_keywords.iter().any(|e| e == &kw) => Some((kw, self.move_next())),
            _ => None,
        }
    }

    /// Check current token is of specified keyword kind
    /// 
    /// if so, move next and Ok(keyword, keyword_span),
    /// if not, push unexpect and Err(Unexpected)
    ///
    /// example `let kw_span = cx.expect_keyword(Keyword::In)?;`
    fn expect_keyword_kind(&mut self, kind: KeywordKind) -> Result<(Keyword, Span), Unexpected> {
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

    /// Check current token is specified Separator, recognizes shift left, shift right, logical and
    fn is_sep(&self, expected_sep: Separator) -> bool {
        match self.current {
            Token::Sep(sep) if sep == expected_sep => true,
            Token::Sep(Separator::GtGt) if expected_sep == Separator::Gt => true,
            Token::Sep(Separator::LtLt) if expected_sep == Separator::Lt => true,
            Token::Sep(Separator::AndAnd) if expected_sep == Separator::And => true,
            _ => false,
        }
    }

    /// Check current token is specified Separator, handles split shift left, shift right, logical and
    /// example `let sep_span = cx.expect_sep(Separator::Comma)?;`
    fn expect_sep(&mut self, expected_sep: Separator) -> Result<Span, Unexpected> {
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
    fn expect_seps(&mut self, expected_seps: &[Separator]) -> Result<(Separator, Span), Unexpected> {
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
    fn try_expect_sep(&mut self, expected_sep: Separator) -> Option<Span> {
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
    fn try_expect_closing_bracket(&mut self, expected_sep: Separator) -> Option<(Span, bool)> {
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
    fn try_expect_seps(&mut self, expected_seps: &[Separator]) -> Option<(Separator, Span)> {
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
    fn try_expect_sep_kind(&mut self, kind: SeparatorKind) -> Option<(Separator, Span)> {
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
    fn expect_ident(&mut self) -> Result<(IsId, Span), Unexpected> {
        match self.current {
            Token::Ident(id) => Ok((id, self.move_next())),
            _ => self.push_unexpect("identifier"),
        }
    }

    /// Check current token is a identifier
    /// 
    /// if so, move next and Some((id, id_span)), 
    /// if not, no move next and None
    ///
    /// example `if let Some((ident_id, ident_span)) = cx.try_expect_ident() { ... }`
    fn try_expect_ident(&mut self) -> Option<(IsId, Span)> {
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
    fn try_expect_label(&mut self) -> Option<(IsId, Span)> {
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
    fn expect_ident_or_keywords(&mut self, acceptable_keywords: &[Keyword]) -> Result<(IsId, Span), Unexpected> {
        match self.current {
            Token::Ident(id) => Ok((id, self.move_next())),
            Token::Keyword(kw) if acceptable_keywords.iter().any(|a| a == &kw) => Ok((self.base.intern(kw.display()), self.move_next())),
            _ => self.push_unexpect(&format!("identifier or {}", acceptable_keywords.iter().map(|a| a.display()).collect::<Vec<_>>().join(", "))),
        }
    }

    /// Check current token is identifier or meet the predict
    /// 
    /// if so, move next and Ok((id, ident_span)),
    /// if not, push unexpect and Err(Unexpected)
    ///
    /// example `let (ident_id, ident_span) = cx.expect_ident_or_keyword_kind(KeywordKind::Primitive)?;`
    fn expect_ident_or_keyword_kind(&mut self, kind: KeywordKind) -> Result<(IsId, Span), Unexpected> {
        match self.current {
            Token::Ident(id) => Ok((id, self.move_next())),
            Token::Keyword(kw) if kw.kind(kind) => Ok((self.base.intern(kw.display()), self.move_next())),
            _ => self.push_unexpect(&format!("identifier or {:?}", kind)),
        }
    }

    fn push_unexpect<T>(&mut self, expect_desc: &str) -> Result<T, Unexpected> {
        self.base.emit("unexpected token")
            .detail(self.current_span, format!("meet {:?}", self.current))
            .help(format!("expected {}", expect_desc));
        Err(Unexpected)
    }
}

impl<'ecx, 'scx> ParseContext<'ecx, 'scx> {

    // maybe implementations
    // checks current token (may peek) to see if it matches syntax node starting
    // // was called Parser::matches, ISyntaxParse::matches, ISyntaxItemParse::is_first_final, IASTItem::is_first_final before
    // // considered loops_like_xxx, seems_to_be_xxx, and maybe_xxx is shortest and consist with token check methods is_xxx

    fn maybe_array_type(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::LeftBracket))
    }

    fn maybe_fn_type(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Fn))
    }

    fn maybe_plain_type(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::Lt | Separator::ColonColon) | Token::Ident(_))
    }

    fn maybe_primitive_type(&self) -> bool {
        matches!(self.current, Token::Keyword(kw) if kw.kind(KeywordKind::Primitive))
    }

    fn maybe_ref_type(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::And | Separator::AndAnd))
    }

    fn maybe_tuple_type(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::LeftParen))
    }
}

// array_type = '[' type_ref ';' expr ']'
impl Parser for ArrayType {
    type Output = ArrayType;

    fn parse(cx: &mut ParseContext) -> Result<ArrayType, Unexpected> {

        let left_bracket_span = cx.expect_sep(Separator::LeftBracket)?;
        let base = Box::new(cx.expect::<TypeRef>()?);

        if let Some(right_bracket_span) = cx.try_expect_sep(Separator::RightBracket) {
            cx.emit(strings::InvalidArrayType)
                .detail(right_bracket_span, "expected semicolon, meet right bracket")
                .help(strings::ArrayTypeSyntaxHelp);
            return Ok(ArrayType{ base, size: Expr::dummy(), span: left_bracket_span + right_bracket_span });
        }

        let _semicolon_span = cx.expect_sep(Separator::SemiColon)?;

        if let Some(right_bracket_span) = cx.try_expect_sep(Separator::RightBracket) {
            cx.emit(strings::InvalidArrayType)
                .detail(right_bracket_span, "expected expr, meet right bracket")
                .help(strings::ArrayTypeSyntaxHelp);
            return Ok(ArrayType{ base, size: Expr::dummy(), span: left_bracket_span + right_bracket_span });
        }

        let size = cx.parse_expr()?;
        let right_bracket_span = cx.expect_sep(Separator::RightBracket)?;
        Ok(ArrayType{ base, size, span: left_bracket_span + right_bracket_span })
    }
}

// fn_type = 'fn' '(' [ ident ':' ] type_ref { ',' [ ident ':' ] type_ref } [ ',' ] ')' [ '->' type_ref ]
//
// - return type in fn type and fn def is not colon but arrow: https://mail.mozilla.org/pipermail/rust-dev/2013-July/005042.html
// - parameter name is optional and does not affect type identity
//   type ref may start with ident, actually most common type refs start with ident, 
//   so it is ambiguous and that may be the reason rust does not support that, but I always want to add parameter name to make function type more clear,
//   so they are distinguished by always parseing type ref and very simple result (only one identifier) followed with colon is regarded as parameter name
impl Parser for FnType {
    type Output = FnType;

    fn parse(cx: &mut ParseContext) -> Result<Self, Unexpected> {
        
        let fn_span = cx.expect_keyword(Keyword::Fn)?;
        let left_paren_span = cx.expect_sep(Separator::LeftParen)?;

        let mut parameters = Vec::new();
        let right_paren_span = loop {
            if let Some((right_paren_span, skipped_comma)) = cx.try_expect_closing_bracket(Separator::RightParen) {
                if skipped_comma && parameters.is_empty() {
                    // TODO: need comma span
                    cx.emit("unexpected token").detail(right_paren_span, "expected ident, type or right paren, meet comma");
                }
                break right_paren_span;
            } else if !parameters.is_empty() {
                cx.expect_sep(Separator::Comma)?;
            }
            // these can-regard-as-variable keywords are not expected by type ref, they are definitely parameter name
            let name = cx.try_expect_keywords(&[Keyword::This, Keyword::Self_, Keyword::Underscore]);
            if name.is_some() {
                cx.expect_sep(Separator::Colon)?;
            }
            let r#type = cx.expect::<TypeRef>()?;
            let (name, r#type) = if let TypeRef::Plain(PlainType{ type_as_segment: None, global: false, segments, .. }) = &r#type {
                if name.is_none() // this one should be before previous let r#type but that will make it 3 ifs are too more (None, r#type)s
                    && segments.len() == 1 && segments[0].parameters.is_empty() && cx.try_expect_sep(Separator::Colon).is_some() {
                    (Some((segments[0].ident, segments[0].ident_span)), cx.expect::<TypeRef>()?)
                } else {
                    (name.map(|(kw, span)| (cx.intern(kw.display()), span)), r#type)
                }
            } else {
                (name.map(|(kw, span)| (cx.intern(kw.display()), span)), r#type)
            };
            parameters.push(FnTypeParam{ name, all_span: name.map(|(_, name_span)| name_span).unwrap_or_else(|| r#type.get_all_span()) + r#type.get_all_span(), r#type });
        };

        let ret_type = cx.try_expect_seps(&[Separator::Arrow, Separator::Colon]).map(|(sep, span)| {
            if sep == Separator::Colon {
                cx.emit(strings::FunctionReturnTypeShouldUseArrow).detail(span, strings::FunctionReturnTypeExpectArrowMeetColon);
            }
            cx.expect::<TypeRef>()
        }).transpose()?;
        
        let all_span = fn_span + ret_type.as_ref().map(|t| t.get_all_span()).unwrap_or(right_paren_span);
        Ok(FnType{ paren_span: left_paren_span + right_paren_span, parameters, ret_type: ret_type.map(Box::new), all_span })
    }
}

// module = { item }
impl Parser for Module {
    type Output = Module;

    fn parse(cx: &mut ParseContext) -> Result<Module, Unexpected> {
        let mut items = Vec::new();
        while !cx.eof() {
            items.push(cx.parse_item()?);
        }
        Ok(Module{ items, file: cx.get_file_id() })
    }
}

// plain_type = [ type_as_segment | '::' ] plain_type_segment { '::' plain_type_segment }
// type_as_segment = '<' type_ref 'as' type_ref '>' '::'
// plain_type_segment = identifier [ '<' type_ref { ',' type_ref } [ ',' ] '>' ]
//
// most common type ref, plain means not special (array/tuple/fn) and not referenced (not directly a reference type)
// may be namespaced, segment may contain type parameter, does not need namespace separator `::` before type list angle bracket pair
// may contain a type_as_segment at beginning
// may contain a namespace separator at beginning, for referencing global items
impl Parser for PlainType {
    type Output = Self;

    fn parse(cx: &mut ParseContext) -> Result<Self, Unexpected> {

        let type_as_segment = cx.try_expect_sep(Separator::Lt).map(|lt_span| {
            let from = cx.expect::<TypeRef>()?;
            cx.expect_keyword(Keyword::As)?;
            let to = cx.expect::<TypeRef>()?;
            let gt_span = cx.expect_sep(Separator::Gt)?;
            Ok(TypeAsSegment{ from: Box::new(from), to: Box::new(to), span: lt_span + gt_span })
        }).transpose()?;

        let beginning_separator_span = cx.try_expect_sep(Separator::ColonColon);

        let mut segments = Vec::new();
        while let Some((ident, ident_span)) = cx.try_expect_ident() {
            if let Some(lt_span) = cx.try_expect_sep(Separator::Lt) {
                if let Some(gt_span) = cx.try_expect_sep(Separator::Gt) { // allow <> in syntax parse
                    segments.push(TypeSegment{ ident, ident_span, quote_span: lt_span + gt_span, parameters: Vec::new(), all_span: ident_span + gt_span });
                } else {
                    let mut parameters = vec![cx.expect::<TypeRef>()?];
                    let quote_span = lt_span + loop {
                        if let Some((gt_span, _)) = cx.try_expect_closing_bracket(Separator::Gt) {
                            break gt_span;
                        }
                        cx.expect_sep(Separator::Comma)?;
                        parameters.push(cx.expect::<TypeRef>()?);
                    };
                    segments.push(TypeSegment{ ident, ident_span, quote_span, parameters, all_span: ident_span + quote_span });
                }
            } else {
                segments.push(TypeSegment{ ident, ident_span, quote_span: Span::new(0, 0), parameters: Vec::new(), all_span: ident_span });
            }
            if cx.try_expect_sep(Separator::ColonColon).is_none() {
                break;
            }
        }

        let global = type_as_segment.is_none() && beginning_separator_span.is_some();
        let all_span = type_as_segment.as_ref().map(|s| s.span).or(beginning_separator_span)
            .unwrap_or_else(|| segments[0].all_span) + segments.last().unwrap().all_span; // [0] and last().unwrap(): matches() guarantees segments are not empty
        Ok(PlainType{ type_as_segment, global, segments, all_span })
    }
}

// primitive_type = primitive_keyword
impl Parser for PrimitiveType {
    type Output = PrimitiveType;

    fn parse(cx: &mut ParseContext) -> Result<PrimitiveType, Unexpected> {
        let (name, span) = cx.expect_keyword_kind(KeywordKind::Primitive)?;
        Ok(PrimitiveType{ name, span })
    }
}

// ref_type = '&' type_ref
impl Parser for RefType {
    type Output = RefType;

    fn parse(cx: &mut ParseContext) -> Result<RefType, Unexpected> {
        
        let and_span = cx.expect_sep(Separator::And)?;
        let base = cx.expect::<TypeRef>()?;
        Ok(RefType{ span: and_span + base.get_all_span(), base: Box::new(base) })
    }
}

// tuple_type = '(' type_ref { ',' type_ref } [ ',' ] ')'
//
// empty for unit type, one element tuple require ending comma
// type template name will be `tuple` when analysis, so user type `tuple` should be rejected by analysis
impl Parser for TupleType {
    type Output = TupleType;

    fn parse(cx: &mut ParseContext) -> Result<TupleType, Unexpected> {
        
        let left_paren_span = cx.expect_sep(Separator::LeftParen)?;
        if let Some(right_paren_span) = cx.try_expect_sep(Separator::RightParen) {
            return Ok(Self{ items: Vec::new(), span: left_paren_span + right_paren_span });
        }
        
        let mut items = vec![cx.expect::<TypeRef>()?];
        let span = left_paren_span + loop {
            if let Some((right_paren_span, skipped_comma)) = cx.try_expect_closing_bracket(Separator::RightParen) {
                if !skipped_comma && items.len() == 1 {
                    cx.emit(strings::SingleItemTupleType)
                        .detail(right_paren_span, strings::TupleTypeExpectCommaMeetRightParen);
                }
                break right_paren_span;
            } else {
                cx.expect_sep(Separator::Comma)?;
                items.push(cx.expect::<TypeRef>()?);
            }
        };

        Ok(TupleType{ items, span })
    }
}

impl Parser for TypeRef {
    type Output = TypeRef;

    fn parse(cx: &mut ParseContext) -> Result<TypeRef, Unexpected> {
        if cx.maybe_primitive_type() {
            Ok(TypeRef::Primitive(cx.expect::<PrimitiveType>()?))
        } else if cx.maybe_array_type() {
            Ok(TypeRef::Array(cx.expect::<ArrayType>()?))
        } else if cx.maybe_fn_type() {
            Ok(TypeRef::Fn(cx.expect::<FnType>()?))
        } else if cx.maybe_ref_type() {
            Ok(TypeRef::Ref(cx.expect::<RefType>()?))
        } else if cx.maybe_tuple_type() {
            Ok(TypeRef::Tuple(cx.expect::<TupleType>()?))
        } else if cx.maybe_plain_type() {
            Ok(TypeRef::Plain(cx.expect::<PlainType>()?))
        } else {
            cx.push_unexpect("[, fn, &, (, <, ::, ident or primitive type")
        }
    }
}
