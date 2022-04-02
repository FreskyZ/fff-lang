
use crate::source::{Span, IsId, FileId};
use crate::diagnostics::{Diagnostic, strings};
use crate::lexical::{Parser as LexicalParser, Token, Numeric, Separator, SeparatorKind, Keyword, KeywordKind};
use super::visit::Node;
use super::ast::*;

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

    /// Check current token is specified Separator, handles split shift left, shift right, logical and
    ///
    /// if so, move next and Ok(sep_span),
    /// if not, push unexpect and Err(Unexpected)
    ///
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
    // // was called Parser::matches, ISyntaxParse::matches, ISyntaxItemParse::is_first_final before
    // // considered loops_like_xxx, seems_to_be_xxx, and maybe_xxx is shortest and consist with token check methods is_xxx
    fn maybe_array_def(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::LeftBracket)) 
    }

    fn maybe_array_type(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::LeftBracket))
    }

    fn maybe_block_stmt(&self) -> bool {
        matches!((&self.current, &self.peek2), (Token::Label(_), Token::Sep(Separator::LeftBrace)) | (Token::Sep(Separator::LeftBrace), _))
    }

    fn maybe_enum_def(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Enum))
    }

    fn maybe_expr(&self) -> bool {
        self.maybe_lit()
        || self.maybe_name()
        || self.maybe_tuple_def()
        || self.maybe_array_def()
        || self.maybe_unary_expr()
        || matches!(self.current, Token::Sep(Separator::DotDot) | Token::Keyword(Keyword::This))
    }

    fn maybe_fn_call(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::LeftParen)) 
    }

    fn maybe_fn_def(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Fn))
    }

    fn maybe_fn_type(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Fn))
    }

    fn maybe_for_stmt(&self) -> bool {
        matches!((&self.current, &self.peek2), (Token::Label(_), Token::Keyword(Keyword::For)) | (Token::Keyword(Keyword::For), _))
    }

    fn maybe_if_stmt(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::If)) 
    }

    fn maybe_index_call(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::LeftBracket))
    }

    fn maybe_continue_stmt(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Continue)) 
    }

    fn maybe_break_stmt(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Break)) 
    }

    fn maybe_label(&self) -> bool {
        matches!(self.current, Token::Label(_))
    }

    fn maybe_lit(&self) -> bool {
        matches!(self.current, Token::Char(_) | Token::Bool(_) | Token::Str(..) | Token::Num(_)) 
    }

    fn maybe_loop_stmt(&self) -> bool {
        matches!((&self.current, &self.peek2), (Token::Label(_), Token::Keyword(Keyword::Loop)) | (Token::Keyword(Keyword::Loop), _))
    }

    fn maybe_member_access(&self) -> bool {
        matches!((&self.current, &self.peek), (Token::Sep(Separator::Dot), Token::Num(_) | Token::Ident(_))) 
    }

    fn maybe_module_stmt(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Module)) 
    }

    fn maybe_name(&self) -> bool {
        matches!(self.current, Token::Ident(_) | Token::Sep(Separator::Lt | Separator::ColonColon)) 
    }

    fn maybe_object_lit(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::LeftBrace))
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

    fn maybe_ret_stmt(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Return))
    }

    fn maybe_tuple_def(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::LeftParen)) 
    }

    fn maybe_tuple_type(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::LeftParen))
    }

    fn maybe_type_def(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Type)) 
    }

    fn maybe_unary_expr(&self) -> bool {
        matches!(self.current, Token::Sep(sep) if sep.kind(SeparatorKind::Unary))
    }

    fn maybe_use_stmt(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Use)) 
    }

    fn maybe_var_decl(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Const | Keyword::Var)) 
    }

    fn maybe_while_stmt(&self) -> bool {
        matches!((&self.current, &self.peek2), (Token::Label(_), Token::Keyword(Keyword::While)) | (Token::Keyword(Keyword::While), _))
    }
}

// array_def = '[' [ expr_list ] ']'
impl Parser for ArrayDef {
    type Output = Expr;

    fn parse(cx: &mut ParseContext) -> Result<Expr, Unexpected> {
        
        match cx.expect::<ExprList>()? {
            ExprListParseResult::Empty(span) =>
                Ok(Expr::Array(ArrayDef{ bracket_span: span, items: ExprList{ items: Vec::new() } })),
            ExprListParseResult::Normal(span, exprlist) 
            | ExprListParseResult::EndWithComma(span, exprlist) =>
                Ok(Expr::Array(ArrayDef{ bracket_span: span, items: exprlist })),
            ExprListParseResult::SingleComma(span) => {
                cx.emit(strings::UnexpectedSingleComma).detail(span, strings::ArrayDefHere);
                Ok(Expr::Array(ArrayDef{ bracket_span: span, items: ExprList{ items: Vec::new() } }))
            }
        }
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
            return Ok(ArrayType{ base, size: Default::default(), span: left_bracket_span + right_bracket_span });
        }

        let _semicolon_span = cx.expect_sep(Separator::SemiColon)?;

        if let Some(right_bracket_span) = cx.try_expect_sep(Separator::RightBracket) {
            cx.emit(strings::InvalidArrayType)
                .detail(right_bracket_span, "expected expr, meet right bracket")
                .help(strings::ArrayTypeSyntaxHelp);
            return Ok(ArrayType{ base, size: Default::default(), span: left_bracket_span + right_bracket_span });
        }

        let size = cx.expect::<Expr>()?;
        let right_bracket_span = cx.expect_sep(Separator::RightBracket)?;
        Ok(ArrayType{ base, size, span: left_bracket_span + right_bracket_span })
    }
}

// MultiplicativeExpression = UnaryExpr | MultiplicativeExpression MultiplicativeOperator UnaryExpr
// AdditiveExpression = MultiplicativeExpression | AdditiveExpression AdditiveOperator MultiplicativeExpression
// RelationalExpression = AdditiveExpression | RelationalExpression RelationalOperator AdditiveExpression
// ShiftExpression = RelationalExpression | ShiftExpression ShiftOperator RelationalExpression
// BitAndExpression = ShiftExpression | BitAndExpression BitAndOperator ShiftExpression
// BitXorExpression = BitAndExpression | BitXorExpression BitXorOperator BitAndExpression
// BitOrExpression = BitXorExpression | BitOrExpression BitOrOperator BitXorExpression
// EqualityExpression = BitOrExpression | EqualityExpression EqualityOperator BitOrExpression  // `==` and `!=` lower than `|` for `if (enum_var & enum_mem1 == enum_mem1)`
// LogicalAndExpression = EqualityExpression | LogicalAndExpression LogicalAndOperator EqualityExpression
// LogicalOrExpression = LogicalAndExpression | LogicalOrExpression LogicalOrOperator LogicalAndExpression
impl Parser for BinaryExpr {
    type Output = Expr;

    fn parse(cx: &mut ParseContext) -> Result<Expr, Unexpected> {
        #[cfg(feature = "trace_binary_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[PrimaryExpr] "); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_binary_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        return parse_logical_or(cx);

        fn check_relational_expr(cx: &mut ParseContext, expr: &Expr) {
            if let Expr::Binary(BinaryExpr{ operator: Separator::Gt, operator_span: gt_span, left_expr, .. }) = expr {
                if let Expr::Binary(BinaryExpr{ operator: Separator::Lt, operator_span: lt_span, .. }) = left_expr.as_ref() {
                    cx.emit(strings::MaybeGeneric).span(*lt_span).span(*gt_span).help(strings::MaybeGenericHelp);
                }
            }
        }

        macro_rules! impl_binary_parser {
            ($parser_name:ident, $previous_parser_name:path, $kind:ident $(,$check:path)?) => (
                fn $parser_name(cx: &mut ParseContext) -> Result<Expr, Unexpected> {
                    trace!("parsing {}", stringify!($parser_name));

                    let mut current_expr = $previous_parser_name(cx)?;
                    loop {
                        if let Some((sep, sep_span)) = cx.try_expect_sep_kind(SeparatorKind::$kind) {
                            let right_expr = $previous_parser_name(cx)?;
                            current_expr = Expr::Binary(BinaryExpr{
                                all_span: current_expr.get_all_span() + right_expr.get_all_span(),
                                left_expr: Box::new(current_expr),
                                operator: sep, 
                                operator_span: sep_span, 
                                right_expr: Box::new(right_expr)
                            });
                            $($check(cx, &current_expr))?
                        } else {
                            return Ok(current_expr);
                        }
                    }
                }
            )
        }
        impl_binary_parser! { parse_multiplicative, UnaryExpr::parse, Multiplicative }
        impl_binary_parser! { parse_additive, parse_multiplicative, Additive }
        impl_binary_parser! { parse_relational, parse_additive, Relational, check_relational_expr }
        impl_binary_parser! { parse_shift, parse_relational, Shift }
        impl_binary_parser! { parse_bitand, parse_shift, BitAnd }
        impl_binary_parser! { parse_bitxor, parse_bitand, BitXor }
        impl_binary_parser! { parse_bitor, parse_bitxor, BitOr }
        impl_binary_parser! { parse_equality, parse_bitor, Equality }
        impl_binary_parser! { parse_logical_and, parse_equality, LogicalAnd }
        impl_binary_parser! { parse_logical_or, parse_logical_and, LogicalOr }
    }
}

// block-stmt = [ label-def ] block
// block-stmt for explicit block definition in block and allow block label
impl Parser for BlockStatement {
    type Output = BlockStatement;

    fn parse(cx: &mut ParseContext) -> Result<BlockStatement, Unexpected> {
    
        let name = cx.maybe_label().then_try(|| cx.expect::<LabelDef>())?;
        let body = cx.expect::<Block>()?;
        let all_span = name.as_ref().map(|n| n.all_span).unwrap_or(body.all_span) + body.all_span;
        Ok(BlockStatement{ all_span, name, body })
    }
}

// block = '{' { statement } '}'
impl Parser for Block {
    type Output = Block;

    fn parse(cx: &mut ParseContext) -> Result<Block, Unexpected> {

        let starting_span = cx.expect_sep(Separator::LeftBrace)?;
        let mut items = Vec::new();
        loop {
            if let Some(ending_span) = cx.try_expect_sep(Separator::RightBrace) {
                return Ok(Block::new(starting_span + ending_span, items));
            }
            items.push(cx.expect::<Statement>()?);
        }
    }
}

// enum_def = 'enum' ident [ ':' primitive_type ] '{' { ident [ '=' expr ] ',' } '}'
impl Parser for EnumDef {
    type Output = Self;

    fn parse(cx: &mut ParseContext) -> Result<Self, Unexpected> {

        let enum_span = cx.expect_keyword(Keyword::Enum)?;
        let (enum_name, enum_name_span) = cx.expect_ident()?;
        let base_type = cx.try_expect_sep(Separator::Colon).map(|_| cx.expect::<PrimitiveType>()).transpose()?;
        let left_brace_span = cx.expect_sep(Separator::LeftBrace)?;

        let mut variants = Vec::new();
        let right_brace_span = if let Some(right_brace_span) = cx.try_expect_sep(Separator::RightBrace) {
            right_brace_span
        } else {
            loop {
                let (variant_name, variant_name_span) = cx.expect_ident()?;
                let init_value = cx.try_expect_sep(Separator::Eq).map(|_| cx.expect::<Expr>()).transpose()?;
                let variant_all_span = variant_name_span + init_value.as_ref().map(|e| e.get_all_span()).unwrap_or(variant_name_span);
                variants.push(EnumVariant{ name: variant_name, name_span: variant_name_span, value: init_value, all_span: variant_all_span });

                if let Some((right_brace_span, _)) = cx.try_expect_closing_bracket(Separator::RightBrace) {
                    break right_brace_span;
                } else {
                    cx.expect_sep(Separator::Comma)?;
                }
            }
        };

        let quote_span = left_brace_span + right_brace_span;
        let all_span = enum_span + right_brace_span;
        Ok(EnumDef{ name: enum_name, name_span: enum_name_span, base_type, quote_span, variants, all_span })
    }
}

// expr_list = expr { ',' expr } [ ',' ]
impl Parser for ExprList {
    type Output = ExprListParseResult;

    /// This is special, when calling `parse`, `cx.current` should point to the quote token
    /// Then the parser will check end token to determine end of parsing process
    fn parse(cx: &mut ParseContext) -> Result<ExprListParseResult, Unexpected> {

        let (starting_sep, starting_span) = cx.expect_seps(&[Separator::LeftBrace, Separator::LeftBracket, Separator::LeftParen])?;
        let expect_end_sep = match starting_sep { 
            Separator::LeftBrace => Separator::RightBrace, 
            Separator::LeftBracket => Separator::RightBracket,
            Separator::LeftParen => Separator::RightParen,
            _ => unreachable!(),
        };

        if let Some((ending_span, skipped_comma)) = cx.try_expect_closing_bracket(expect_end_sep) {
            return if skipped_comma {
                Ok(ExprListParseResult::SingleComma(starting_span + ending_span))
            } else {
                Ok(ExprListParseResult::Empty(starting_span + ending_span))
            };
        }

        cx.no_object_literals.push(false);
        let mut items = Vec::new();
        loop {
            items.push(cx.expect::<Expr>()?);
            if let Some((ending_span, skipped_comma)) = cx.try_expect_closing_bracket(expect_end_sep) {
                cx.no_object_literals.pop();
                return if skipped_comma {
                    Ok(ExprListParseResult::EndWithComma(starting_span + ending_span, ExprList{ items }))
                } else {
                    Ok(ExprListParseResult::Normal(starting_span + ending_span, ExprList{ items }))
                };
            }
            cx.expect_sep(Separator::Comma)?;
        }
    }
}

// dispatch them to convenience statement define macro
impl Parser for SimpleExprStatement {
    type Output = <AssignExprStatement as Parser>::Output;

    fn parse(cx: &mut ParseContext) -> Result<Self::Output, Unexpected> { 
        cx.expect::<AssignExprStatement>() 
    }
}

// expr_stmt = expr { assign_ops expr } ';'
impl Parser for AssignExprStatement {
    type Output = Statement;

    fn parse(cx: &mut ParseContext) -> Result<Statement, Unexpected> {

        let left_expr = cx.expect::<Expr>()?;
        let starting_span = left_expr.get_all_span();

        if let Some(semicolon_span) = cx.try_expect_sep(Separator::SemiColon) {
            Ok(Statement::SimpleExpr(SimpleExprStatement::new(starting_span + semicolon_span, left_expr)))
        } else if let Some((assign_op, assign_op_span)) = cx.try_expect_sep_kind(SeparatorKind::Assign) {
            let right_expr = cx.expect::<Expr>()?;
            let semicolon_span = cx.expect_sep(Separator::SemiColon)?;
            Ok(Statement::AssignExpr(
                AssignExprStatement::new(starting_span + semicolon_span, assign_op, assign_op_span, left_expr, right_expr)))
        } else {
            cx.push_unexpect("assign operators, semicolon")
        }
    }
}

impl Parser for Expr {
    type Output = Expr;

    fn parse(cx: &mut ParseContext) -> Result<Expr, Unexpected> { 
        cx.expect::<RangeExpr>()
    }
}

impl Default for Expr {
    fn default() -> Expr { 
        Expr::Lit(LitExpr{ value: LitValue::Num(Numeric::I32(0)), span: Span::new(0, 0) }) 
    }
}

// fn_call_expr = expr '(' [ expr_list ] ')'
impl Parser for FnCallExpr {
    type Output = FnCallExpr;

    fn parse(cx: &mut ParseContext) -> Result<FnCallExpr, Unexpected> {

        // this parse method return partial and priority proxy will fill base
        macro_rules! make_partial { ($span:expr, $params:expr) => (
            Ok(FnCallExpr{ all_span: Span::new(0, 0), base: Box::new(Expr::default()), paren_span: $span, params: $params })) }

        match cx.expect::<ExprList>()? {
            | ExprListParseResult::Normal(span, expr_list) 
            | ExprListParseResult::EndWithComma(span, expr_list) => make_partial!(span, expr_list),
            ExprListParseResult::Empty(span) => make_partial!(span, ExprList{ items: Vec::new() }),
            ExprListParseResult::SingleComma(span) => {
                cx.emit(strings::UnexpectedSingleComma).detail(span, strings::FnCallHere);
                make_partial!(span, ExprList{ items: Vec::new() })
            }
        }
    }
}

// fn-def = 'fn' identifier '(' [ identifier ':' type-use { ',' identifier ':' type-use [ ',' ] } ] ')' [ '->' type-use ] block
impl Parser for FnDef {
    type Output = FnDef;

    fn parse(cx: &mut ParseContext) -> Result<FnDef, Unexpected> {
        #[cfg(feature = "trace_fn_def_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[FnDef: {}]", line!()); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_fn_def_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }
        
        let fn_span = cx.expect_keyword(Keyword::Fn)?;
        let (fn_name, fn_name_span) = cx.expect_ident()?;
        let mut params_paren_span = cx.expect_sep(Separator::LeftParen)?;
        trace!("fndef name span: {:?}", fn_name_span);

        let mut params = Vec::new();
        loop {
            if let Some((right_paren_span, skipped_comma)) = cx.try_expect_closing_bracket(Separator::RightParen) {
                params_paren_span += right_paren_span;
                if skipped_comma && params.is_empty() {
                    cx.emit("Single comma in function definition argument list")
                        .detail(fn_name_span, "function definition here")
                        .detail(params_paren_span, "param list here");
                }
                break;
            } else if let Some(_comma_span) = cx.try_expect_sep(Separator::Comma) {
                continue;
            }

            let (param_name, param_span) = cx.expect_ident_or_keywords(&[Keyword::Underscore, Keyword::This, Keyword::Self_])?;
            let _ = cx.expect_sep(Separator::Colon)?;
            let decltype = cx.expect::<TypeRef>()?;
            params.push(FnParam::new(param_name, param_span, decltype));
        }

        let ret_type = cx.try_expect_seps(&[Separator::Arrow, Separator::Colon]).map(|(sep, span)| {
            if sep == Separator::Colon {
                cx.emit(strings::FunctionReturnTypeShouldUseArrow).detail(span, strings::FunctionReturnTypeExpectArrowMeetColon);
            }
            cx.expect::<TypeRef>()
        }).transpose()?;
        let body = cx.expect::<Block>()?;

        Ok(FnDef::new(fn_span + body.all_span, fn_name, fn_name_span, params_paren_span, params, ret_type, body))
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

// for_stmt = [ label_def ] 'for' identifier 'in' expr block
// TODO: add else for break, like python
impl Parser for ForStatement {
    type Output = ForStatement;

    fn parse(cx: &mut ParseContext) -> Result<ForStatement, Unexpected> {

        let loop_name = cx.maybe_label().then_try(|| cx.expect::<LabelDef>())?;
        let for_span = cx.expect_keyword(Keyword::For)?;

        // Accept _ as iter_name, _ do not declare iter var
        let (iter_name, iter_span) = cx.expect_ident_or_keywords(&[Keyword::Underscore])?; 
        cx.expect_keyword(Keyword::In)?;

        cx.no_object_literals.push(true);
        let iter_expr = cx.expect::<Expr>()?;
        cx.no_object_literals.pop();
        let body = cx.expect::<Block>()?;
        
        let all_span = loop_name.as_ref().map(|n| n.all_span).unwrap_or(for_span) + body.all_span;
        Ok(ForStatement{ loop_name, for_span, iter_name, iter_span, iter_expr, body, all_span })
    }
}

// if_stmt = 'if' expr block { 'else' 'if' expr block } [ 'else' block ]
impl Parser for IfStatement {
    type Output = IfStatement;

    fn parse(cx: &mut ParseContext) -> Result<IfStatement, Unexpected> {

        let mut all_span = cx.expect_keyword(Keyword::If)?;

        cx.no_object_literals.push(true);
        let if_expr = cx.expect::<Expr>()?;
        cx.no_object_literals.pop();
        let if_body = cx.expect::<Block>()?;
        all_span += if_body.all_span;
        let if_clause = IfClause{ all_span, condition: if_expr, body: if_body };

        let mut elseif_clauses = Vec::new();
        let mut else_clause = None;
        while let Some(else_span) = cx.try_expect_keyword(Keyword::Else) {
            if let Some(if_span) = cx.try_expect_keyword(Keyword::If) {
                let elseif_span = else_span + if_span;
                cx.no_object_literals.push(true);
                let elseif_expr = cx.expect::<Expr>()?;
                cx.no_object_literals.pop();
                let elseif_body = cx.expect::<Block>()?;
                all_span += elseif_body.all_span;
                elseif_clauses.push(IfClause{ all_span: elseif_span + elseif_body.all_span, condition: elseif_expr, body: elseif_body });
            } else {
                // 16/12/1, we lost TWO `+1`s for current_length here ... fixed
                // 17/5/6: When there is match Block::parse(tokens, messages, index + current_length), etc.
                // There was a bug fix here, now no more current_length handling!
                // 17/6/21: a new physical structure update makes it much more simple
                // 17/7/28: a new small update of parse_cx makes things even more simple
                let else_body = cx.expect::<Block>()?;
                all_span += else_body.all_span;
                else_clause = Some(ElseClause{ all_span: else_span + else_body.all_span, body: else_body });
            }
        }

        Ok(IfStatement{ all_span, if_clause, elseif_clauses, else_clause })
    }
}

// index_call_expr = expr '[' [ expr_list ] ']'
// renamed from postfix_expr::subscription to make it shorter
impl Parser for IndexCallExpr {
    type Output = IndexCallExpr;

    fn parse(cx: &mut ParseContext) -> Result<IndexCallExpr, Unexpected> {

        match cx.expect::<ExprList>()? {
            ExprListParseResult::Normal(span, expr_list) 
            | ExprListParseResult::EndWithComma(span, expr_list) => 
                Ok(IndexCallExpr{ all_span: Span::new(0, 0), base: Box::new(Expr::default()), bracket_span: span, params: expr_list }),
            ExprListParseResult::Empty(span) 
            | ExprListParseResult::SingleComma(span) => {
                // empty subscription is meaningless, refuse it here
                // update: but for trying to get more message in the rest program, make it not none
                cx.emit(strings::EmptyIndexCall).detail(span, strings::IndexCallHere);
                Ok(IndexCallExpr{ all_span: Span::new(0, 0), base: Box::new(Expr::default()), bracket_span: span, params: ExprList{ items: Vec::new() } })
            }
        }
    }
}

impl JumpStatement {
    fn parse(cx: &mut ParseContext, expect_first_kw: Keyword) -> Result<JumpStatement, Unexpected> {

        let starting_span = cx.expect_keyword(expect_first_kw)?;

        if let Some(label) = cx.try_expect_label() {
            let semicolon_span = cx.expect_sep(Separator::SemiColon)?;
            Ok(JumpStatement{ all_span: starting_span + semicolon_span, target: Some(label) })
        } else { 
            let semicolon_span = cx.expect_sep(Separator::SemiColon)?;
            Ok(JumpStatement{ all_span: starting_span + semicolon_span, target: None })
        }
    }
}

// continue_stmt = 'continue' [ label ] ';'
impl Parser for ContinueStatement {
    type Output = ContinueStatement;
    fn parse(cx: &mut ParseContext) -> Result<ContinueStatement, Unexpected> { 
        Ok(ContinueStatement(JumpStatement::parse(cx, Keyword::Continue)?))
    }
}

// break_stmt = 'break' [ label ] ';'
impl Parser for BreakStatement {
    type Output = BreakStatement;
    fn parse(cx: &mut ParseContext) -> Result<BreakStatement, Unexpected> {
        Ok(BreakStatement(JumpStatement::parse(cx, Keyword::Break)?))
    }
}

// label-def = label ':'
impl Parser for LabelDef {
    type Output = LabelDef;

    fn parse(cx: &mut ParseContext) -> Result<LabelDef, Unexpected> {

        if let Some((label_id, label_span)) = cx.try_expect_label() {
            let colon_span = cx.expect_sep(Separator::Colon)?;
            Ok(LabelDef::new(label_id, label_span + colon_span))
        } else {
            cx.push_unexpect("label")
        }
    }
}

impl Parser for LitExpr {
    type Output = Expr;

    fn parse(cx: &mut ParseContext) -> Result<Expr, Unexpected> {
        
        let (value, span) = cx.expect_lit()?;
        Ok(Expr::Lit(LitExpr{ value, span }))
    }
}

// loop_stmt = [ label_def ] 'loop' block
// NOTE: no else for break here because if control flow come to else it is always breaked
impl Parser for LoopStatement {
    type Output = LoopStatement;

    fn parse(cx: &mut ParseContext) -> Result<LoopStatement, Unexpected> {

        let name = cx.maybe_label().then_try(|| cx.expect::<LabelDef>())?;
        let loop_span = cx.expect_keyword(Keyword::Loop)?;
        let body = cx.expect::<Block>()?;
        let all_span = name.as_ref().map(|n| n.all_span).unwrap_or(loop_span) + body.all_span;
        Ok(LoopStatement{ all_span, name, loop_span, body })
    }
}

// member_access = expr '.' member
// member = num_lit | name
//
// name should start with ident,
// that is, type_as_segment or global (start with ::) is not allowed and first segment must be normal
// TODO: member = num_lit | name_segment
impl Parser for MemberAccessExpr {
    type Output = Self;

    // these 3 postfix exprs are special because
    // their node contains base expr, but their parser only expects token after that (dot for member access expr)
    // the postfix expr dispatcher is responsible for fullfilling the missing part
    fn parse(cx: &mut ParseContext) -> Result<MemberAccessExpr, Unexpected> {
        
        let dot_span = cx.expect_sep(Separator::Dot)?;
        let name = if let Some((numeric, span)) = cx.try_expect_numeric() {
            if let Numeric::I32(v) = numeric {
                Name{ type_as_segment: None, global: false, all_span: span, segments: vec![NameSegment::Normal(cx.intern(&format!("{}", v)), span)] }
            } else {
                cx.emit(strings::InvalidTupleIndex).span(span).help(strings::TupleIndexSyntaxHelp);
                Name{ type_as_segment: None, global: false, all_span: span, segments: Vec::new() }
            }
        } else {
            let name = cx.expect::<Name>()?;
            // first segment will not be generic and will not be type_as_segment and global, because matches3 checks for that
            if name.segments.len() == 2 && !matches!(name.segments[1], NameSegment::Generic(..)) {
                cx.emit(strings::InvalidMemberAccess).span(name.segments[1].get_span()).help(strings::GenericMemberAccessSyntaxHelp);
            }
            if name.segments.len() > 2 {
                let error_span = name.segments[1].get_span() + name.segments.last().unwrap().get_span();
                cx.emit(strings::InvalidMemberAccess).span(error_span).help(strings::GenericMemberAccessSyntaxHelp);
            }
            name
        };

        Ok(MemberAccessExpr{ base: Box::new(Expr::default()), dot_span, name, all_span: Span::new(0, 0) })
    }
}

// module_stmt = 'module' identifier [ str_lit ] ';'
impl Parser for ModuleStatement {
    type Output = ModuleStatement;

    fn parse(cx: &mut ParseContext) -> Result<ModuleStatement, Unexpected> {

        let starting_span = cx.expect_keyword(Keyword::Module)?;
        let (name, name_span) = cx.expect_ident()?;

        let path = cx.try_expect_str_lit(); 
        let semicolon_span = cx.expect_sep(Separator::SemiColon)?;
        let all_span = starting_span + semicolon_span;

        Ok(ModuleStatement{ all_span, name, name_span, path })
    }
}

// module = { item }
impl Parser for Module {
    type Output = Module;

    fn parse(cx: &mut ParseContext) -> Result<Module, Unexpected> {
        let mut items = Vec::new();
        while !cx.eof() {
            items.push(cx.expect::<Item>()?);
        }
        Ok(Module{ items, file: cx.get_file_id() })
    }
}

// name = [ type_as_segment ] [ '::' ] name_segment { '::' name_segment }
// name_segment = identifier | '<' type_ref { ',' type_ref } '>'
impl Parser for Name {
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
        loop {
            if let Some((ident, ident_span)) = cx.try_expect_ident() {
                segments.push(NameSegment::Normal(ident, ident_span));
            } else {
                let lt_span = cx.expect_sep(Separator::Lt)?;
                // none: first segment cannot be generic segment
                // some generic: generic segment cannot follow generic segment 
                if let None | Some(NameSegment::Generic(..)) = segments.last() {
                    cx.emit(strings::InvalidNameSegment).detail(lt_span, strings::NameSegmentExpect);
                }

                if let Some(gt_span) = cx.try_expect_sep(Separator::Gt) { // allow <> in syntax parse
                    segments.push(NameSegment::Generic(Vec::new(), lt_span + gt_span));
                } else {
                    let mut parameters = vec![cx.expect::<TypeRef>()?];
                    let quote_span = lt_span + loop {
                        if let Some((gt_span, _)) = cx.try_expect_closing_bracket(Separator::Gt) {
                            break gt_span;
                        }
                        cx.expect_sep(Separator::Comma)?;
                        parameters.push(cx.expect::<TypeRef>()?);
                    };
                    segments.push(NameSegment::Generic(parameters, quote_span));
                }
            }
            if cx.try_expect_sep(Separator::ColonColon).is_none() {
                break;
            }
        }

        let global = type_as_segment.is_none() && beginning_separator_span.is_some();
        let all_span = type_as_segment.as_ref().map(|s| s.span).or(beginning_separator_span)
            .unwrap_or_else(|| segments[0].get_span()) + segments.last().unwrap().get_span(); // [0] and last().unwrap(): matches() guarantees segments are not empty
        Ok(Name{ type_as_segment, global, segments, all_span })
    }
}

// object_literal = name '{' { ident ':' expr ',' } '}'
// last comma may omit
impl Parser for ObjectLiteral {
    type Output = Self;

    fn parse(cx: &mut ParseContext) -> Result<Self, Unexpected> {
        
        let left_brace_span = cx.expect_sep(Separator::LeftBrace)?;
        let mut fields = Vec::new();
        let right_brace_span = if let Some(right_brace_span) = cx.try_expect_sep(Separator::RightBrace) {
            right_brace_span
        } else { loop {
                let (field_name, field_name_span) = cx.expect_ident()?;
                let colon_span = cx.expect_sep(Separator::Colon)?;
                let value = cx.expect::<Expr>()?;
                fields.push(ObjectLiteralField{ all_span: field_name_span + value.get_all_span(), name: field_name, name_span: field_name_span, colon_span, value });

                if let Some((right_brace_span, _)) = cx.try_expect_closing_bracket(Separator::RightBrace) {
                    break right_brace_span;
                } else {
                    cx.expect_sep(Separator::Comma)?;
                }
            }
        };

        Ok(ObjectLiteral{ base: Box::new(Expr::default()), quote_span: left_brace_span + right_brace_span, fields, all_span: Span::new(0, 0) })
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

// primary_expr = ident_expr | lit_expr | unit_lit | paren_expr | tuple_def | array_def
struct PrimaryExpr;

impl Parser for PrimaryExpr {
    type Output = Expr;
    
    fn parse(cx: &mut ParseContext) -> Result<Expr, Unexpected> {

        if cx.maybe_lit() {
            return cx.expect::<LitExpr>();
        } else if cx.maybe_name() {
            return cx.expect::<Name>().map(Expr::Name);
        } else if cx.maybe_tuple_def() {
            return cx.expect::<TupleDef>();
        } else if cx.maybe_array_def() {
            return cx.expect::<ArrayDef>();
        }

        let (this_id, this_span) = cx.expect_ident_or_keywords(&[Keyword::This, Keyword::Self_])?;  // actually identifier is processed by Name, not here
        Ok(Expr::Name(Name{ type_as_segment: None, global: false, all_span: this_span, segments: vec![NameSegment::Normal(this_id, this_span)] }))
    }
}

// postfix_expr = expr { ( member_access | fn_call | indexer_call ) }
pub struct PostfixExpr;

impl Parser for PostfixExpr {
    type Output = Expr;

    fn parse(cx: &mut ParseContext) -> Result<Expr, Unexpected> {   
        #[cfg(feature = "trace_postfix_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ perror!("    [PostfixExpr:{}] ", line!()); perrorln!($($arg)*); }) }
        #[cfg(not(feature = "trace_postfix_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        let mut current_expr = cx.expect::<PrimaryExpr>()?;
        trace!("parsed primary, current is {:?}", current_expr);

        loop {
            if cx.maybe_member_access() {
                let mut postfix = cx.expect::<MemberAccessExpr>()?;
                postfix.all_span = current_expr.get_all_span() + postfix.name.all_span;
                postfix.base = Box::new(current_expr);
                current_expr = Expr::MemberAccess(postfix);
            } else if cx.maybe_fn_call() {
                let mut postfix = cx.expect::<FnCallExpr>()?;
                postfix.all_span = current_expr.get_all_span() + postfix.paren_span;
                postfix.base = Box::new(current_expr);
                current_expr = Expr::FnCall(postfix);
            } else if cx.maybe_index_call() {
                let mut postfix = cx.expect::<IndexCallExpr>()?;
                postfix.all_span = current_expr.get_all_span() + postfix.bracket_span;
                postfix.base = Box::new(current_expr);
                current_expr = Expr::IndexCall(postfix);
            } else if matches!(cx.no_object_literals.last(), None | Some(false)) && matches!(current_expr, Expr::Name(_)) && cx.maybe_object_lit() {
                let mut postfix = cx.expect::<ObjectLiteral>()?;
                postfix.all_span = current_expr.get_all_span() + postfix.quote_span;
                postfix.base = Box::new(current_expr);
                current_expr = Expr::Object(postfix);
            } else {
                break;
            }
        }

        trace!("parsing postfix finished, get retval: {:?}", current_expr);
        Ok(current_expr)
    }
}

// range_full = '..'
// range_left = binary_expr '..'
// range_right = '..' binary_expr
// range_both = binary_expr '..' binary_expr
pub struct RangeExpr;

impl Parser for RangeExpr {
    type Output = Expr;

    fn parse(cx: &mut ParseContext) -> Result<Expr, Unexpected> {
        match cx.try_expect_sep(Separator::DotDot) {
            Some(range_op_span) => {
                if cx.maybe_expr() {
                    let expr = cx.expect::<BinaryExpr>()?;
                    Ok(Expr::RangeRight(RangeRightExpr{ all_span: range_op_span + expr.get_all_span(), expr: Box::new(expr) }))
                } else {
                    Ok(Expr::RangeFull(RangeFullExpr{ all_span: range_op_span }))
                }
            }
            None => {
                let left_expr = cx.expect::<BinaryExpr>()?;
                if let Some(op_span) = cx.try_expect_sep(Separator::DotDot) {
                    if cx.maybe_expr() {
                        let right_expr = cx.expect::<BinaryExpr>()?;
                        let all_span = left_expr.get_all_span() + right_expr.get_all_span();
                        Ok(Expr::RangeBoth(RangeBothExpr{ all_span, op_span, left_expr: Box::new(left_expr), right_expr: Box::new(right_expr) }))
                    } else {
                        Ok(Expr::RangeLeft(RangeLeftExpr{ all_span: left_expr.get_all_span() + op_span, expr: Box::new(left_expr) }))
                    }
                } else {
                    Ok(left_expr)
                }
            }
        }
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

// ret_stmt = 'return' [ expr ] ';'
impl Parser for ReturnStatement {
    type Output = ReturnStatement;

    fn parse(cx: &mut ParseContext) -> Result<ReturnStatement, Unexpected> {

        let starting_span = cx.expect_keyword(Keyword::Return)?;

        if let Some(semicolon_span) = cx.try_expect_sep(Separator::SemiColon) {
            // 17/6/17: you forgot move_next here!
            // but I have never write some test cases like following something after ret stmt
            // so the bug is not propagated to be discovered
            // 17/7/28: now new features added to parse_cx and move_next is to be removed, no current position management bug any more!
            Ok(ReturnStatement::new_unit(starting_span + semicolon_span))
        } else {
            let expr = cx.expect::<Expr>()?;
            let semicolon_span = cx.expect_sep(Separator::SemiColon)?;
            Ok(ReturnStatement::new_expr(starting_span + semicolon_span, expr))
        }
    }
}

// tuple_def = '(' expr_list ')'
// paren_expr = '(' expr ')'
// unit_lit = '(' ')'
impl Parser for TupleDef {
    type Output = Expr;

    fn parse(cx: &mut ParseContext) -> Result<Expr, Unexpected> {

        match cx.expect::<ExprList>()? {
            ExprListParseResult::Empty(span) => {
                Ok(Expr::Lit(LitExpr{ value: LitValue::Unit, span }))
            }
            ExprListParseResult::SingleComma(span) => {
                cx.emit(strings::UnexpectedSingleComma).detail(span, strings::TupleDefHere);
                Ok(Expr::Tuple(TupleDef{ paren_span: span, items: ExprList{ items: Vec::new() } }))
            }
            ExprListParseResult::Normal(span, exprlist) => {
                if exprlist.items.len() == 1 {
                    Ok(Expr::Paren(ParenExpr{ span, expr: Box::new(exprlist.items.into_iter().last().unwrap()) }))
                } else {
                    Ok(Expr::Tuple(TupleDef{ paren_span: span, items: exprlist }))
                }
            }
            ExprListParseResult::EndWithComma(span, exprlist) => {
                Ok(Expr::Tuple(TupleDef{ paren_span: span, items: exprlist }))
            }
        }
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

// type_def = 'type' (identifier | keyword_primitive_type)  '{' [ type_field_def { ',' type_field_def } [ ',' ] ] '}'
// type_field_def = identifier ':' type_ref
impl Parser for TypeDef {
    type Output = Self;

    fn parse(cx: &mut ParseContext) -> Result<TypeDef, Unexpected> {

        let starting_span = cx.expect_keyword(Keyword::Type)?;
        let (name, name_span) = cx.expect_ident_or_keyword_kind(KeywordKind::Primitive)?;
        let _left_brace_span = cx.expect_sep(Separator::LeftBrace)?;

        let mut fields = Vec::new();
        let right_brace_span = loop { 
            if let Some(right_brace_span) = cx.try_expect_sep(Separator::RightBrace) {
                break right_brace_span;     // rustc 1.19 stablize break-expr
            }

            let (field_name, field_name_span) = cx.expect_ident()?;
            let colon_span = cx.expect_sep(Separator::Colon)?;
            let field_type = cx.expect::<TypeRef>()?;
            fields.push(if let Some(comma_span) = cx.try_expect_sep(Separator::Comma) {
                TypeFieldDef{ all_span: field_name_span + comma_span, name: field_name, name_span: field_name_span, colon_span, r#type: field_type }
            } else {
                TypeFieldDef{ all_span: field_name_span + field_type.get_all_span(), name: field_name, name_span: field_name_span, colon_span, r#type: field_type }
            });
        };

        Ok(TypeDef{ all_span: starting_span + right_brace_span, name, name_span, fields })
    }
}

// unary_expr = { unary_operator } postfix_expr
impl Parser for UnaryExpr {
    type Output = Expr;

    fn parse(cx: &mut ParseContext) -> Result<Expr, Unexpected> {
        
        let mut op_spans = Vec::new();
        loop {
            match cx.try_expect_sep_kind(SeparatorKind::Unary) {
                Some((sep, sep_span)) => op_spans.push((sep, sep_span)),
                None => {
                    let base = cx.expect::<PostfixExpr>()?;
                    return Ok(op_spans.into_iter().rev().fold(base, |base, (op, span)| { 
                        Expr::Unary(UnaryExpr{ all_span: span + base.get_all_span(), base: Box::new(base), operator: op, operator_span: span }) 
                    }));
                }
            }
        }
    }
}

// use_stmt = 'use' name [ 'as' identifier ] ';'
impl Parser for UseStatement {
    type Output = UseStatement;

    fn parse(cx: &mut ParseContext) -> Result<UseStatement, Unexpected> {

        let starting_span = cx.expect_keyword(Keyword::Use)?;
        let name = cx.expect::<Name>()?;

        let alias = cx.try_expect_keyword(Keyword::As).map(|_| cx.expect_ident()).transpose()?;
        let semicolon_span = cx.expect_sep(Separator::SemiColon)?;
        let all_span = starting_span + semicolon_span;

        Ok(UseStatement{ all_span, name, alias })
    }
}

// const-decl = 'const' identifier [ ':' type-use ] [ '=' expr ] ';'
// var-decl = 'var' identifier [ ':' type-use ] [ '=' expr ] ';'
impl Parser for VarDeclStatement {
    type Output = VarDeclStatement;

    fn parse(cx: &mut ParseContext) -> Result<VarDeclStatement, Unexpected> {
        
        let (starting_kw, starting_span) = cx.expect_keywords(&[Keyword::Const, Keyword::Var])?;
        let is_const = match starting_kw { Keyword::Const => true, Keyword::Var => false, _ => unreachable!() };

        let (name, name_span) = cx.expect_ident_or_keywords(&[Keyword::Underscore])?;
        let r#type = cx.try_expect_sep(Separator::Colon).map(|_| cx.expect::<TypeRef>()).transpose()?;
        let init_expr = cx.try_expect_sep(Separator::Eq).map(|_| cx.expect::<Expr>()).transpose()?;
        if r#type.is_none() && init_expr.is_none() {
            cx.emit("require type annotation")
                .detail(name_span, "variable declaration here")
                .help("cannot infer type without both type annotation and initialization expression");
        }
        let ending_span = cx.expect_sep(Separator::SemiColon)?;

        Ok(VarDeclStatement{ all_span: starting_span + ending_span, is_const, name, name_span, r#type, init_expr })
    }
}

// while-stmt = [ label-def ] 'while' expr block
// TODO: add else for break, like python
impl Parser for WhileStatement {
    type Output = WhileStatement;

    fn parse(cx: &mut ParseContext) -> Result<WhileStatement, Unexpected> {
        
        let name = cx.maybe_label().then_try(|| cx.expect::<LabelDef>())?;
        let while_span = cx.expect_keyword(Keyword::While)?;
        cx.no_object_literals.push(true);
        let expr = cx.expect::<Expr>()?;
        cx.no_object_literals.pop();
        let body = cx.expect::<Block>()?;
        let all_span = name.as_ref().map(|n| n.all_span).unwrap_or(while_span) + body.all_span;
        Ok(WhileStatement{ name, while_span, loop_expr: expr, body, all_span })
    }
}

impl Parser for Statement {
    type Output = Statement;

    fn parse(cx: &mut ParseContext) -> Result<Statement, Unexpected> {
        if cx.maybe_type_def() {
            Ok(Statement::Type(cx.expect::<TypeDef>()?))
        } else if cx.maybe_enum_def() {
            Ok(Statement::Enum(cx.expect::<EnumDef>()?))
        } else if cx.maybe_fn_def() {
            Ok(Statement::Fn(cx.expect::<FnDef>()?))
        } else if cx.maybe_block_stmt() {
            Ok(Statement::Block(cx.expect::<BlockStatement>()?))
        } else if cx.maybe_break_stmt() {
            Ok(Statement::Break(cx.expect::<BreakStatement>()?))
        } else if cx.maybe_continue_stmt() {
            Ok(Statement::Continue(cx.expect::<ContinueStatement>()?))
        } else if cx.maybe_expr() {
            Ok(cx.expect::<AssignExprStatement>()?)
        } else if cx.maybe_for_stmt() {
            Ok(Statement::For(cx.expect::<ForStatement>()?))
        } else if cx.maybe_if_stmt() {
            Ok(Statement::If(cx.expect::<IfStatement>()?))
        } else if cx.maybe_loop_stmt() {
            Ok(Statement::Loop(cx.expect::<LoopStatement>()?))
        } else if cx.maybe_ret_stmt() {
            Ok(Statement::Return(cx.expect::<ReturnStatement>()?))
        } else if cx.maybe_var_decl() {
            Ok(Statement::VarDecl(cx.expect::<VarDeclStatement>()?))
        } else if cx.maybe_while_stmt() {
            Ok(Statement::While(cx.expect::<WhileStatement>()?))
        } else {
            cx.push_unexpect("type, enum, fn, {, break, continue, for, if, loop, return, var, const, while, .., !, ~, &, <, ::, ident, [, (")
        }
    }
}

impl Parser for Item {
    type Output = Item;

    fn parse(cx: &mut ParseContext) -> Result<Item, Unexpected> {
        if cx.maybe_type_def() {
            Ok(Item::Type(cx.expect::<TypeDef>()?))
        } else if cx.maybe_enum_def() {
            Ok(Item::Enum(cx.expect::<EnumDef>()?))
        } else if cx.maybe_fn_def() {
            Ok(Item::Fn(cx.expect::<FnDef>()?))
        } else if cx.maybe_block_stmt() {
            Ok(Item::Block(cx.expect::<BlockStatement>()?))
        } else if cx.maybe_expr() {
            Ok(match cx.expect::<AssignExprStatement>()? {
                Statement::AssignExpr(a) => Item::AssignExpr(a),
                Statement::SimpleExpr(s) => Item::SimpleExpr(s),
                _ => unreachable!(),
            })
        } else if cx.maybe_for_stmt() {
            Ok(Item::For(cx.expect::<ForStatement>()?))
        } else if cx.maybe_if_stmt() {
            Ok(Item::If(cx.expect::<IfStatement>()?))
        } else if cx.maybe_loop_stmt() {
            Ok(Item::Loop(cx.expect::<LoopStatement>()?))
        } else if cx.maybe_var_decl() {
            Ok(Item::VarDecl(cx.expect::<VarDeclStatement>()?))
        } else if cx.maybe_while_stmt() {
            Ok(Item::While(cx.expect::<WhileStatement>()?))
        } else if cx.maybe_use_stmt() {
            Ok(Item::Use(cx.expect::<UseStatement>()?))
        } else if cx.maybe_module_stmt() {
            Ok(Item::Import(cx.expect::<ModuleStatement>()?))
        } else {
            cx.push_unexpect("type, enum, fn, {, for, if, loop, return, var, const, while, use, module, .., !, ~, &, <, ::, ident, [, (")
        }
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
