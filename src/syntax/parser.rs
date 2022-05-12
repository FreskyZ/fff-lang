
use crate::source::{Span, IdSpan};
use crate::diagnostics::strings;
use crate::lexical::{Parser as Scanner, Token, Numeric, Separator, SeparatorKind, Keyword, KeywordKind};
use crate::common::arena::{Arena, Index};
use super::ast::*;

#[cfg(test)]
mod tests;

/// unrecoverable unexpected for this parser, detail in diagnostics
// this should be more readable than previous Result<Self::Output, ()>
#[derive(Debug)]
pub struct Unexpected;

trait ThenTry {
    fn then_try<T, E>(self, f: impl FnOnce() -> std::result::Result<T, E>) -> std::result::Result<Option<T>, E>;
}
impl ThenTry for bool {
    fn then_try<T, E>(self, f: impl FnOnce() -> std::result::Result<T, E>) -> std::result::Result<Option<T>, E> {
        self.then(f).transpose()
    }
}

pub struct Parser<'ecx, 'scx, 'a> {
    base: Scanner<'ecx, 'scx>,
    arena: &'a Arena,

    // look ahead buffer and their span
    // this is not LL1 but currently one more is enough
    buf: [Token; 2],
    spans: [Span; 2],

    // 1. there is first/follow conflict for expr-postfix when meet left brace,
    //    or more concretely, when expr is followed with block, e.g. in if stmt or while stmt,
    //    `if a < b { print(a); }` should be parsed as `if (a < b) {}` but not `if a < (b {})`,
    //    only `if a < (b{ c: 1 }) {}` or `if (a < b { c: 1}) {}` can
    // 2. logically
    //    1. when expecting expr which is followed by rules with left brace in first set (or left brace terminal),
    //       object expr should not be allowed
    //    2. when expecting expr inside expr, that is, in some quote,
    //       (paren for array/fn-call, bracket for tuple/index, etc.), object literal should be allowed again
    // 3. in actual implementation,
    //    1. this stack is only modified by parse_expr and parse_expr_except_object_literal function
    //    2. when expecting expr followed by block, parse_expr_except_object_literal is called and no direct object literal is allowed
    //    3. when expecting expr in other places in statement parsers, parse_expr is called and object literal is allowed
    //    4. when expecting expr in expr, parse_expr is called and object literal is allowed
    //    5. when expecting expr in parse_object_literal, it should not be bothering this flag stack because itself is already allowed
    //       but still parse_expr is called and object literal is allowed
    // 4. currently there will not be push(false) after any push(true) before push(true) is popped,
    //    but the only-change-in-parse_expr{_no_object_literal} is convenient and easy to understand, so keep it
    //
    // // this does not make this language non context free, you can duplicate nearly all expr rules and add a "-no-object-expr" postfix,
    // // and the only difference between these 2 set of rules is "expr-postfix-no-object-expr" does not include an object-expr alternative,
    // // so this solution also can be regarded as parsing this (whether no-object-expr flag) through or expr parsers, but that's not convenient
    // // and not very related to expr parsers after postfix expr (unary expr, binary expr, etc.), so put a "global" state in self
    allow_object_expr: Vec<bool>,
}

impl<'ecx, 'scx, 'a> Parser<'ecx, 'scx, 'a> {

    pub fn new(mut base: Scanner<'ecx, 'scx>, arena: &'a Arena) -> Self {
        let (ahead1, ahead1_span) = base.next();
        let (ahead2, ahead2_span) = base.next();
        Self{
            base,
            arena,
            buf: [ahead1, ahead2],
            spans: [ahead1_span, ahead2_span],
            allow_object_expr: Vec::with_capacity(128), // 128: arbitray value for max expression depth
        }
    }

    // formal entry
    // it is required to pass arena as parameter but not use self.arena,
    // or else borrowck will think index is borrowing mutable self (index is pretending it's a reference)
    // and then you literally can do nothing because self is constantly borrowed after any call to parse_*,
    // by passing arena as parameter borrowck correctly knows return value is borrowing arena not self,
    // this is similar to source chars which is also forced to split struct to clarify borrow source
    pub fn parse_module(&mut self) -> Result<Index<Module>, Unexpected> {
        let mut items = Vec::new();
        while !matches!(self.buf[0], Token::EOF) {
            items.push(self.parse_item()?);
        }
        Ok(self.arena.emplace_module(self.base.get_file_id(), items))
    }

    pub fn finish(self) {
        self.base.finish()
    }

    // return previous current span
    fn move_next(&mut self) -> Span {
        let previous_span = self.spans[0];
        self.buf.swap(0, 1);
        self.spans[0] = self.spans[1];
        // // destructuring assignment is stablized at about 1.59, I'm amazing how this is parsed
        (self.buf[1], self.spans[1]) = self.base.next();
        previous_span
    }

    /// check current token is a literal
    fn is_lit(&self) -> bool {
        matches!(self.buf[0], Token::Char(_) | Token::Bool(_) | Token::Str(..) | Token::Num(_)) 
    }

    /// Check current token is a literal
    ///
    /// if so, move next and Ok((lit_value, lit_span)),
    /// if not, push unexpect and Err(Unexpected)
    ///
    /// example `let (lit, lit_span) = cx.expect_lit()?;`
    fn expect_lit(&mut self) -> Result<(LitValue, Span), Unexpected> {
        match self.buf[0] {
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
    /// example `let value = cx.try_expect_str_lit()?;`
    fn try_expect_str_lit(&mut self) -> Option<IdSpan> {
        match self.buf[0] {
            Token::Str(v, _) => Some(IdSpan::new(v, self.move_next())),
            _ => None,
        }
    }

    /// Check current token is a literal, used for tuple member expr
    ///
    /// if so, move next and Some((lit_value, lit_span)),
    /// if not, push unexpect and Err(Unexpected)
    ///
    /// example `let (value, span) = cx.expect_numeric()?;`
    fn expect_numeric(&mut self) -> Result<(Numeric, Span), Unexpected> {
        match self.buf[0] {
            Token::Num(v) => Ok((v, self.move_next())),
            _ => self.push_unexpect("tuple index"),
        }
    }

    /// Check current token is specified keyword
    /// 
    /// if so, move next and Ok(keyword_span),
    /// if not, push unexpect and Err(Unexpected)
    ///
    /// example `let kw_span = cx.expect_keyword(Keyword::In)?;`
    fn expect_keyword(&mut self, expected_kw: Keyword) -> Result<Span, Unexpected> {
        match self.buf[0] {
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
        match self.buf[0] {
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
        match self.buf[0] {
            Token::Keyword(kw) if kw == expected_keyword => Some(self.move_next()),
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
        match self.buf[0] {
            Token::Keyword(kw) if kw.kind(kind) => Ok((kw, self.move_next())),
            _ => self.push_unexpect(&format!("{:?}", kind)),
        }
    }

    /// split current shift left token to 2 less than tokens
    /// caller to check current is shift left
    /// return span for first less than and logically move to next less than by directly changing cached current token
    fn split_shift_left(&mut self) -> Span {
        let result = self.spans[0].start.into();
        self.buf[0] = Token::Sep(Separator::Lt);
        self.spans[0] = self.spans[0].end.into();
        result
    }

    /// split current shift right token to 2 greater than tokens
    /// caller to check current is shift right
    /// return span for first greater than and logically move to next greater than by directly changing cached current token
    fn split_shift_right(&mut self) -> Span {
        let result = self.spans[0].start.into();
        self.buf[0] = Token::Sep(Separator::Gt);
        self.spans[0] = self.spans[0].end.into();
        result
    }


    /// split current logical and token to 2 bit and tokens
    /// caller to check current is logical and
    /// return span for first bit and than and logically move to next bit and by directly changing cached current token
    fn split_logical_and(&mut self) -> Span {
        let result = self.spans[0].start.into();
        self.buf[0] = Token::Sep(Separator::And);
        self.spans[0] = self.spans[0].end.into();
        result
    }

    /// Check current token is specified Separator, recognizes shift left, shift right, logical and
    fn is_sep(&self, expected_sep: Separator) -> bool {
        match self.buf[0] {
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
        match self.buf[0] {
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
    /// example `let (sep, sep_span) = cx.expect_seps(&[Separator::LBracket, Separator::LBrace])?;`
    fn expect_seps(&mut self, expected_seps: &[Separator]) -> Result<(Separator, Span), Unexpected> {
        match self.buf[0] {
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
        match self.buf[0] {
            Token::Sep(sep) if sep == expected_sep => Some(self.move_next()),
            Token::Sep(Separator::GtGt) if expected_sep == Separator::Gt => Some(self.split_shift_right()),
            Token::Sep(Separator::LtLt) if expected_sep == Separator::Lt => Some(self.split_shift_left()),
            _ => None,
        }
    }

    /// Check current token is specified closing bracket, allows optional comma, handles split shift right
    ///
    /// if so, move next and Some((sep_span, comma span))
    /// if not, no move next and None
    ///
    /// example `if let Some((close_span, comma_span)) = cx.try_expect_closing_bracket(Separator::Comma) { ... }`
    fn try_expect_closing_bracket(&mut self, expected_sep: Separator) -> Option<(Span, Option<Span>)> {
        debug_assert!(matches!(expected_sep, 
            | Separator::RBrace 
            | Separator::RParen 
            | Separator::RBracket 
            | Separator::Gt
        ), "not a closing bracket");
        match (&self.buf[0], &self.buf[1]) {
            (Token::Sep(sep), _) if *sep == expected_sep => {
                Some((self.move_next(), None))
            },
            (Token::Sep(Separator::GtGt), _) if expected_sep == Separator::Gt => {
                Some((self.split_shift_right(), None))
            },
            (Token::Sep(Separator::Comma), Token::Sep(sep)) if *sep == expected_sep => { 
                let comma_span = self.move_next(); 
                Some((self.move_next(), Some(comma_span)))
            },
            (Token::Sep(Separator::Comma), Token::Sep(Separator::GtGt)) if expected_sep == Separator::Gt => { 
                let comma_span = self.move_next();
                Some((self.split_shift_right(), Some(comma_span)))
            },
            _ => None,
        }
    }

    /// Check current token is one of the specified Separators
    ///
    /// if so, move next and Some((sep, sep_span)),
    /// if not, no move next and None
    ///
    /// example `if let Some((sep, sep_span)) = cx.try_expect_seps(&[Separator::LBracket, Separator::LBrace])?;`
    fn try_expect_seps(&mut self, expected_seps: &[Separator]) -> Option<(Separator, Span)> {
        match self.buf[0] {
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
        match self.buf[0] {
            Token::Sep(sep) if sep.kind(kind) => Some((sep, self.move_next())),
            _ => None,
        }
    }

    /// Check current token is an identifier
    ///
    /// if so, move next and Ok(id_span),
    /// if not, push unexpect and Err(Unexpected)
    ///
    /// example `let name = cx.expect_ident()?;`
    fn expect_ident(&mut self) -> Result<IdSpan, Unexpected> {
        match self.buf[0] {
            Token::Ident(id) => Ok(IdSpan::new(id, self.move_next())),
            _ => self.push_unexpect("identifier"),
        }
    }

    /// Check current token is a label
    /// 
    /// if so, move next and Some(id_span),
    /// if not, no move next and None
    ///
    /// example `if let Some(label) = cx.try_expect_label() { ... }`
    fn try_expect_label(&mut self) -> Option<IdSpan> {
        match self.buf[0] {
            Token::Label(id) => Some(IdSpan::new(id, self.move_next())),
            _ => None,
        }
    }

    /// Check current token is identifier or acceptable keywords
    /// 
    /// if so, move next and Ok(id_span),
    /// if not, push unexpect and Err(Unexpected)
    ///
    /// example `let name = cx.expect_ident_or(&[Keyword::Self_, Keyword::Underscore])?;`
    fn expect_ident_or_keywords(&mut self, acceptable_keywords: &[Keyword]) -> Result<IdSpan, Unexpected> {
        match self.buf[0] {
            Token::Ident(id) => Ok(IdSpan::new(id, self.move_next())),
            Token::Keyword(kw) if acceptable_keywords.iter().any(|a| a == &kw) => Ok(IdSpan::new(self.base.intern(kw.display()), self.move_next())),
            _ => self.push_unexpect(&format!("identifier or {}", acceptable_keywords.iter().map(|a| a.display()).collect::<Vec<_>>().join(", "))),
        }
    }

    fn push_unexpect<T>(&mut self, expect_desc: &str) -> Result<T, Unexpected> {
        self.base.emit("unexpected token")
            .detail(self.spans[0], format!("meet {:?}", self.buf[0]))
            .help(format!("expected {}", expect_desc));
        Err(Unexpected)
    }
}

// expr parsers
// these functions are order logically
// "public" api parse_expr and maybe_expr
// common function parse_expr_list
// priority high to low: primary -> postfix -> unary -> binary -> range
// same priority inside primary: lit (inlined) -> name -> tuple -> array, this seems to be their occurance frequency order in my experience
// same priority inside postfix: member -> fn call -> index call -> object literal, this seems to be in occurance frequency too
impl<'ecx, 'scx, 'a> Parser<'ecx, 'scx, 'a> {

    // maybe_* functions: checks current token (may peek) to see if it matches syntax node starting
    // // was called Parser::matches, ISyntaxParse::matches, ISyntaxItemParse::is_first_final, IASTItem::is_first_final before
    // // considered loops_like_xxx, seems_to_be_xxx, and maybe_xxx is shortest and consist with token check methods is_xxx
    fn maybe_expr(&self) -> bool {
        self.is_lit()
        || self.maybe_path()
        || self.maybe_tuple_expr()
        || self.maybe_array_expr()
        || self.maybe_unary_expr()
        || matches!(self.buf[0], Token::Sep(Separator::DotDot))
    }

    fn parse_expr(&mut self) -> Result<Expr, Unexpected> {
        self.allow_object_expr.push(true);
        let result = self.parse_range_expr();
        self.allow_object_expr.pop();
        result
    }

    // disable top level object exprs,
    // until end of this expr, or call parse_expr again inside call_expr, e.g. array def and tuple def
    fn parse_expr_except_object_expr(&mut self) -> Result<Expr, Unexpected> {
        self.allow_object_expr.push(false);
        let result = self.parse_range_expr();
        self.allow_object_expr.pop();
        result
    }

    // expr_list = opening_bracket expr { ',' expr } [ ',' ] closing_bracket
    // return (span include bracket, exprs, end with comma)
    fn parse_expr_list(&mut self) -> Result<(Span, Vec<Expr>, bool), Unexpected> {

        let (open_bracket, start_span) = self.expect_seps(&[Separator::LBrace, Separator::LBracket, Separator::LParen])?;
        let close_bracket = match open_bracket { 
            Separator::LBrace => Separator::RBrace, 
            Separator::LBracket => Separator::RBracket,
            Separator::LParen => Separator::RParen,
            _ => unreachable!(),
        };

        if let Some((end_span, comma_span)) = self.try_expect_closing_bracket(close_bracket) {
            if let Some(comma_span) = comma_span {
                self.base.emit(format!("expect `{}`, meet `,`", close_bracket.display())).span(comma_span);
            }
            return Ok((start_span + end_span, Vec::new(), comma_span.is_some()));
        }

        let mut items = Vec::new();
        loop {
            items.push(self.parse_expr()?);
            if let Some((ending_span, comma_span)) = self.try_expect_closing_bracket(close_bracket) {
                return Ok((start_span + ending_span, items, comma_span.is_some()));
            }
            self.expect_sep(Separator::Comma)?;
        }
    }

    fn maybe_tuple_expr(&self) -> bool {
        matches!(self.buf[0], Token::Sep(Separator::LParen)) 
    }

    // tuple_expr = '(' expr_list ')'
    // paren_expr = '(' expr ')'
    // unit_lit = '(' ')'
    fn parse_tuple_expr(&mut self) -> Result<Expr, Unexpected> {

        let (span, mut items, end_with_comma) = self.parse_expr_list()?;
        if items.is_empty() {
            Ok(self.arena.emplace_lit_expr(span, LitValue::Unit).into())
        } else if items.len() == 1 && !end_with_comma {
            Ok(self.arena.emplace_paren_expr(span, items.pop().unwrap()).into())
        } else {
            Ok(self.arena.emplace_tuple_expr(span, items).into())
        }
    }

    fn maybe_array_expr(&self) -> bool {
        self.is_sep(Separator::LBracket)
    }

    // array_expr = '[' [ expr_list ] ']'
    fn parse_array_expr(&mut self) -> Result<Expr, Unexpected> {
        let (span, items, _) = self.parse_expr_list()?;
        Ok(self.arena.emplace_array_expr(span, items).into())
    }

    fn parse_primary_expr(&mut self) -> Result<Expr, Unexpected> {
        if self.is_lit() {
            // this is too short to put in one parse method
            // while it is actually tested more than hundred times in syntax test cases worldwide
            let (value, span) = self.expect_lit()?;
            return Ok(self.arena.emplace_lit_expr(span, value).into());
        } else if self.maybe_path() {
            // // this Into::into is amazing resolution
            return self.parse_path(PathContext::Value).map(Into::into);
        } else if self.maybe_tuple_expr() {
            return self.parse_tuple_expr();
        } else if self.maybe_array_expr() {
            return self.parse_array_expr();
        } else {
            self.push_unexpect("LITERAL, IDENT, `<`, `::`, `[`, `(`")
        }
    }

    fn maybe_member_expr(&self) -> bool {
        matches!((&self.buf[0], &self.buf[1]), (Token::Sep(Separator::Dot), Token::Ident(_))) 
    }

    // member_expr = primary_expr '.' ident [ ':' type_list ]
    // return dot span and member name, see parse_postfix_expr for the return type
    fn parse_member_expr(&mut self) -> Result<(Span, (IdSpan, Option<Index<TypeList>>)), Unexpected> {
        
        let dot_span = self.expect_sep(Separator::Dot)?;
        // // ? rust.await is really good design, but await is still currently reserved, put it here to indicate that it can be here
        let ident = self.expect_ident_or_keywords(&[Keyword::Await])?;
        let parameters = self.try_expect_sep(Separator::ColonColon).map(|_| self.parse_type_list()).transpose()?;
        Ok((dot_span, (ident, parameters)))
    }

    fn maybe_tuple_index_expr(&self) -> bool {
        matches!((&self.buf[0], &self.buf[1]), (Token::Sep(Separator::Dot), Token::Num(_)))
    }

    // tuple_member_expr = primary_expr '.' numeric
    // return dot span and tuple index and tuple index span, see parse_postfix_expr for the return type
    // TODO: `a.1.0` should be parsed as 2 tuple index expr, not a rational number, that require reset lexical parser like format string
    fn parse_tuple_index_expr(&mut self) -> Result<(Span, (i32, Span)), Unexpected> {
        
        let dot_span = self.expect_sep(Separator::Dot)?;
        let value = match self.expect_numeric()? {
            (Numeric::I32(v), numeric_span) /* && is unsuffixed and unprefixed */ => (v, numeric_span),
            (_, numeric_span) => {
                self.base.emit(strings::InvalidTupleIndex).span(numeric_span).help(strings::TupleIndexSyntaxHelp);
                (0, numeric_span)
            },
        };
        Ok((dot_span, value))
    }

    fn maybe_call_expr(&self) -> bool {
        matches!(self.buf[0], Token::Sep(Separator::LParen)) 
    }

    // call_expr = primary_expr '(' [ expr_list ] ')'
    // return quote span and expr list, see parse_postfix_expr for the return type
    fn parse_call_expr(&mut self) -> Result<(Span, Vec<Expr>), Unexpected> {
        let (span, items, _) = self.parse_expr_list()?;
        Ok((span, items))
    }

    fn maybe_array_index_expr(&self) -> bool {
        matches!(self.buf[0], Token::Sep(Separator::LBracket))
    }

    // index_call_expr = primary_expr '[' [ expr_list ] ']'
    // return quote span and expr list, see parse_postfix_expr for the return type
    // // was called postfix_expr::subscription, this one is shorter and similar to fn_call
    fn parse_array_index_expr(&mut self) -> Result<(Span, Vec<Expr>), Unexpected> {

        let (span, items, _) = self.parse_expr_list()?;
        if items.is_empty() {
            self.base.emit(strings::EmptyIndexCall).span(span);
        }
        Ok((span, items))
    }

    fn maybe_object_expr(&self) -> bool {
        matches!(self.buf[0], Token::Sep(Separator::LBrace))
    }

    // object_literal = name '{' { ident ':' expr ',' } '}'
    // last comma may omit
    // return quote span and field list, see parse_postfix_expr for the return type
    fn parse_object_expr(&mut self) -> Result<(Span, Vec<Index<ObjectExprField>>), Unexpected> {

        let left_brace_span = self.expect_sep(Separator::LBrace)?;
        let mut fields = Vec::new();
        let right_brace_span = if let Some(right_brace_span) = self.try_expect_sep(Separator::RBrace) {
            right_brace_span
        } else { 
            loop {
                let field_name = self.expect_ident()?;
                self.expect_sep(Separator::Colon)?;
                let value = self.parse_expr()?;
                fields.push(self.arena.emplace_object_expr_field(field_name.span + value.span(self.arena), field_name, value));

                if let Some((right_brace_span, _)) = self.try_expect_closing_bracket(Separator::RBrace) {
                    break right_brace_span;
                } else {
                    self.expect_sep(Separator::Comma)?;
                }
            }
        };

        Ok((left_brace_span + right_brace_span, fields))
    }

    fn parse_postfix_expr(&mut self) -> Result<Expr, Unexpected> {   
        #[cfg(feature = "trace_postfix_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ perror!("    [PostfixExpr:{}] ", line!()); eprintln!($($arg)*); }) }
        #[cfg(not(feature = "trace_postfix_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        let mut current_expr = self.parse_primary_expr()?;
        trace!("parsed primary, current is {:?}", current_expr);

        loop {
            let current_span = current_expr.span(self.arena);
            if self.maybe_member_expr() {
                let (op_span, (name, parameters)) = self.parse_member_expr()?;
                let span = current_span + parameters.as_ref().map(|p| self.arena.get(*p).span).unwrap_or(name.span);
                current_expr = self.arena.emplace_member_expr(span, current_expr, op_span, name, parameters).into();
            } else if self.maybe_call_expr() {
                let (quote_span, parameters) = self.parse_call_expr()?;
                let span = current_span + quote_span;
                current_expr = self.arena.emplace_call_expr(span, current_expr, quote_span, parameters).into();
            } else if self.maybe_tuple_index_expr() {
                let (op_span, (value, value_span)) = self.parse_tuple_index_expr()?;
                let span = current_span + value_span;
                current_expr = self.arena.emplace_tuple_index_expr(span, current_expr, op_span, value, value_span).into();
            } else if self.maybe_array_index_expr() {
                let (quote_span, parameters) = self.parse_array_index_expr()?;
                let span = current_span + quote_span;
                current_expr = self.arena.emplace_array_index_expr(span, current_expr, parameters, quote_span).into();
            } else if matches!({
                // // I carefully checked that only parse_expr and parse_name is called outside this file,
                // // and found the actual intruder is unit test, this will still work when compiler_test
                #[cfg(not(test))]
                debug_assert!(!self.allow_object_expr.is_empty(), "allow_object_expr unexpectedly empty");
                &self.allow_object_expr
            }.last(), None | Some(true)) && matches!(current_expr, Expr::Path(_)) && self.maybe_object_expr() {
                let (quote_span, fields) = self.parse_object_expr()?;
                let span = current_span + quote_span;
                current_expr = self.arena.emplace_object_expr(span, current_expr, quote_span, fields).into();
            } else {
                break;
            }
        }

        trace!("parsing postfix finished, get retval: {:?}", current_expr);
        Ok(current_expr)
    }

    fn maybe_unary_expr(&self) -> bool {
        matches!(self.buf[0], Token::Sep(sep) if sep.kind(SeparatorKind::Unary))
    }

    // unary_expr = { unary_operator } postfix_expr
    fn parse_unary_expr(&mut self) -> Result<Expr, Unexpected> {
        
        let mut op_spans = Vec::new();
        loop {
            match self.try_expect_sep_kind(SeparatorKind::Unary) {
                Some((sep, sep_span)) => op_spans.push((sep, sep_span)),
                None => {
                    let base = self.parse_postfix_expr()?;
                    return Ok(op_spans.into_iter().rev().fold(base, |base, (op, op_span)| { 
                        self.arena.emplace_unary_expr(op_span + base.span(self.arena), base, op, op_span).into()
                    }));
                }
            }
        }
    }

    // binary_expr = unary_expr binary_op unary_expr
    fn parse_binary_expr(&mut self) -> Result<Expr, Unexpected> {
        #[cfg(feature = "trace_binary_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[PrimaryExpr] "); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_binary_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        return parse_logical_or(self);

        fn parse_unary_expr_wrapper(cx: &mut Parser) -> Result<Expr, Unexpected> {
            cx.parse_unary_expr()
        }
        fn check_relational_expr(cx: &mut Parser, expr: &Expr) {
            if let Expr::Binary(expr) = expr {
                if let BinaryExpr{ op: Separator::Gt, op_span: gt_span, left, .. } = cx.arena.get(*expr) {
                    if let Expr::Binary(left) = left {
                        if let BinaryExpr{ op: Separator::Lt, op_span: lt_span, .. } = cx.arena.get(*left) {
                            cx.base.emit(strings::MaybeGeneric).span(*lt_span).span(*gt_span).help(strings::MaybeGenericHelp);
                        }
                    }
                }
            }
        }

        macro_rules! impl_binary_parser {
            ($parser_name:ident, $previous_parser_name:ident, $kind:ident $(,$check:path)?) => (
                fn $parser_name(cx: &mut Parser) -> Result<Expr, Unexpected> {
                    trace!("parsing {}", stringify!($parser_name));

                    let mut current_expr = $previous_parser_name(cx)?;
                    loop {
                        if let Some((op, op_span)) = cx.try_expect_sep_kind(SeparatorKind::$kind) {
                            let right_expr = $previous_parser_name(cx)?;
                            current_expr = cx.arena.emplace_binary_expr(
                                current_expr.span(cx.arena) + right_expr.span(cx.arena), current_expr, right_expr, op, op_span).into();
                            $($check(cx, &current_expr))?
                        } else {
                            return Ok(current_expr);
                        }
                    }
                }
            )
        }
        impl_binary_parser! { parse_multiplicative, parse_unary_expr_wrapper, Multiplicative }
        impl_binary_parser! { parse_additive, parse_multiplicative, Additive }
        impl_binary_parser! { parse_relational, parse_additive, Relational, check_relational_expr }
        impl_binary_parser! { parse_shift, parse_relational, Shift }
        impl_binary_parser! { parse_bitand, parse_shift, BitAnd }
        impl_binary_parser! { parse_bitxor, parse_bitand, BitXor }
        impl_binary_parser! { parse_bitor, parse_bitxor, BitOr } // `==` and `!=` lower than `|` for `if enum_var & enum_mem1 == enum_mem1`
        impl_binary_parser! { parse_equality, parse_bitor, Equality }
        impl_binary_parser! { parse_logical_and, parse_equality, LogicalAnd }
        impl_binary_parser! { parse_logical_or, parse_logical_and, LogicalOr }
    }
    
    // range_full = '..'
    // range_left = binary_expr '..'
    // range_right = '..' binary_expr
    // range_both = binary_expr '..' binary_expr
    fn parse_range_expr(&mut self) -> Result<Expr, Unexpected> {
        match self.try_expect_sep(Separator::DotDot) {
            Some(range_op_span) => {
                if self.maybe_expr() {
                    let expr = self.parse_binary_expr()?;
                    Ok(self.arena.emplace_range_right_expr(range_op_span + expr.span(self.arena), expr).into())
                } else {
                    Ok(self.arena.emplace_range_full_expr(range_op_span).into())
                }
            }
            None => {
                let left_expr = self.parse_binary_expr()?;
                if let Some(op_span) = self.try_expect_sep(Separator::DotDot) {
                    if self.maybe_expr() {
                        let right_expr = self.parse_binary_expr()?;
                        let span = left_expr.span(self.arena) + right_expr.span(self.arena);
                        Ok(self.arena.emplace_range_both_expr(span, left_expr, op_span, right_expr).into())
                    } else {
                        Ok(self.arena.emplace_range_left_expr(left_expr.span(self.arena) + op_span, left_expr).into())
                    }
                } else {
                    Ok(left_expr)
                }
            }
        }
    }
}

#[derive(PartialEq, Eq, Copy, Clone)]
enum PathContext {
    Type, // <>
    Value, // ::<>
}

// path parsers,
// type refs are also here because path is a kind of type ref and contains many type refs
impl<'ecx, 'scx, 'a> Parser<'ecx, 'scx, 'a> {
    
    fn maybe_path(&self) -> bool {
        matches!(self.buf[0], Token::Keyword(kw) if kw.kind(KeywordKind::MaybeIdentifier))
        || matches!(self.buf[0], Token::Sep(Separator::Lt | Separator::ColonColon) | Token::Ident(_))
    }

    // path = '::'? path-segment ('::' path-segment)*
    // path-segment = cast-segment | normal-segment
    // cast-segment = '<' type-ref ('as' type-ref)? '>' // TODO: this is called something like qualified in rust
    // normal-segment = (IDENT | UNDERSCORE | SELFL | SELFU) ('::'? '<' type-ref (COMMA type-ref)* COMMA? '>' )?
    //
    // - most common "name" (somewhat extented from "ident expr" in textbook)
    // - similar for type and value, except when expect value, there must be '::' before '<'
    // - cast-segment must be first segment, cast-segment not compatible with separator-started (global)
    // - ident in normal-segment maybe identifierable keyword except true/falses,
    //   self may be used in value context for current object pointer, may be used in type context for submodule type
    //   Self may be used in value context for static const or static method, may be used in type context for associate type 
    fn parse_path(&mut self, context: PathContext) -> Result<Index<Path>, Unexpected> {
        let mut segments = Vec::new();

        let global_span = self.try_expect_sep(Separator::ColonColon);
        if global_span.is_some() {
            segments.push(PathSegment::Global);
        } else if let Some(lt_span) = self.try_expect_sep(Separator::Lt) {
            let left = self.parse_type_ref()?;
            // TODO: optional as part
            self.expect_keyword(Keyword::As)?;
            let right = self.parse_type_ref()?;
            let gt_span = self.expect_sep(Separator::Gt)?;
            if let (Separator::Colon, colon_span) = self.expect_seps(&[Separator::Colon, Separator::ColonColon])? {
                self.base.emit(strings::ExpectDoubleColonMeetSingleColon).span(colon_span);
            }
            segments.push(self.arena.emplace_cast_segment(lt_span + gt_span, left, right).into());
        }
        
        loop {
            // this is also beyond current expect_* functions
            let (base, is_kw) = match self.buf[0] {
                Token::Ident(id) => (IdSpan::new(id, self.move_next()), false),
                // true/false is Token::Bool not here
                Token::Keyword(kw) if kw.kind(KeywordKind::MaybeIdentifier) => (IdSpan::new(self.base.intern(kw.display()), self.move_next()), true),
                _ => self.push_unexpect("IDENT, `_`, `self`, `Self`")?,
            };

            // this is beyond current expect_* functions
            if matches!((is_kw, context, &self.buf[0], &self.buf[1]),
                // at first, keyword base cannot be generic
                // then, colon is allowed when expecting coloncolon, ltlt is allowed when expecting lt
                // but, and ltlt is not allowed when expect value
                | (false, PathContext::Type, Token::Sep(Separator::Lt | Separator::LtLt), _)
                | (false, _, Token::Sep(Separator::Colon | Separator::ColonColon), Token::Sep(Separator::Lt | Separator::LtLt)))
            {
                if let Some((colon, colon_span)) = self.try_expect_seps(&[Separator::Colon, Separator::ColonColon]) {
                    if context == PathContext::Type {
                        self.base.emit(format!("{} `{}`", strings::ExpectLtMeet, colon.display())).span(colon_span);
                    }
                    if colon == Separator::Colon {
                        self.base.emit(strings::ExpectDoubleColonMeetSingleColon).span(colon_span);
                    }
                }
                // self.buf[0] now is the Lt (or LtLt)
                let parameters = self.parse_type_list()?;
                segments.push(self.arena.emplace_generic_segment(base.span + self.arena.get(parameters).span, base, parameters).into());
            } else {
                segments.push(self.arena.emplace_simple_segment(base.span, base.id).into());
            }
            if let Some(sep) = self.try_expect_seps(&[Separator::Colon, Separator::ColonColon]) {
                if let (Separator::Colon, colon_span) = sep {
                    self.base.emit(strings::ExpectDoubleColonMeetSingleColon).span(colon_span);
                }
            } else {
                break;
            }
        }

        // maybe_path and loop { expect_ident } should guarantee this
        debug_assert!(!segments.is_empty(), "unexpected empty path");
        debug_assert!(global_span.is_none() || segments.len() > 1, "unexpected empty path");
        let span = global_span.unwrap_or_else(|| segments[0].span(self.arena)) + segments.last().unwrap().span(self.arena);
        Ok(self.arena.emplace_path(span, segments))
    }

    // no maybe_type_ref because type ref is always after some colon or namespace separator
    // TODO add literal type = string/number literal (or string/number literal)*
    fn parse_type_ref(&mut self) -> Result<TypeRef, Unexpected> {
        if self.maybe_primitive_type() {
            self.parse_primitive_type().map(Into::into)
        } else if self.maybe_array_type() {
            self.parse_array_type().map(Into::into)
        } else if self.maybe_fn_type() {
            self.parse_fn_type().map(Into::into)
        } else if self.maybe_ref_type() {
            self.parse_ref_type().map(Into::into)
        } else if self.maybe_tuple_type() {
            self.parse_tuple_type().map(Into::into)
        } else if self.maybe_path() {
            self.parse_path(PathContext::Type).map(Into::into)
        } else {
            self.push_unexpect("[, fn, &, (, <, ::, ident or primitive type")
        }
    }

    // type_list = '<' type_ref { ',' type_ref } [ ',' ] '>'
    // angle bracket quoted type list use in many places
    fn parse_type_list(&mut self) -> Result<Index<TypeList>, Unexpected> {

        let lt_span = self.expect_sep(Separator::Lt)?;

        if let Some((gt_span, _)) = self.try_expect_closing_bracket(Separator::Gt) {
            self.base.emit(strings::EmptyTypeList).span(lt_span + gt_span);
            return Ok(self.arena.emplace_type_list(lt_span + gt_span, Vec::new()));
        }

        let mut items = Vec::new();
        loop {
            items.push(self.parse_type_ref()?);
            if let Some((gt_span, _)) = self.try_expect_closing_bracket(Separator::Gt) {
                return Ok(self.arena.emplace_type_list(lt_span + gt_span, items));
            }
            self.expect_sep(Separator::Comma)?;
        }
    }

    // type refs, also include path segment
    fn maybe_array_type(&self) -> bool {
        matches!(self.buf[0], Token::Sep(Separator::LBracket))
    }

    // array_type = '[' type_ref ';' expr ']'
    fn parse_array_type(&mut self) -> Result<Index<ArrayType>, Unexpected> {

        let left_bracket_span = self.expect_sep(Separator::LBracket)?;
        let base = self.parse_type_ref()?;

        if let Some(right_bracket_span) = self.try_expect_sep(Separator::RBracket) {
            self.base.emit(strings::InvalidArrayType)
                .detail(right_bracket_span, "expected semicolon, meet right bracket")
                .help(strings::ArrayTypeSyntaxHelp);
            return Ok(self.arena.emplace_array_type(left_bracket_span + right_bracket_span, base, Expr::dummy(self.arena)));
        }

        self.expect_sep(Separator::SemiColon)?;

        if let Some(right_bracket_span) = self.try_expect_sep(Separator::RBracket) {
            self.base.emit(strings::InvalidArrayType)
                .detail(right_bracket_span, "expected expr, meet right bracket")
                .help(strings::ArrayTypeSyntaxHelp);
            return Ok(self.arena.emplace_array_type(left_bracket_span + right_bracket_span, base, Expr::dummy(self.arena)));
        }

        let size = self.parse_expr()?;
        let right_bracket_span = self.expect_sep(Separator::RBracket)?;
        Ok(self.arena.emplace_array_type(left_bracket_span + right_bracket_span, base, size))
    }

    fn maybe_fn_type(&self) -> bool {
        matches!(self.buf[0], Token::Keyword(Keyword::Fn))
    }

    // fn_type = 'fn' '(' [ ident ':' ] type_ref { ',' [ ident ':' ] type_ref } [ ',' ] ')' [ '->' type_ref ]
    //
    // - return type in fn type and fn def is not colon but arrow: https://mail.mozilla.org/pipermail/rust-dev/2013-July/005042.html
    // - parameter name is optional and does not affect type identity
    //   type ref may start with ident, actually most common type refs start with ident, 
    //   so it is ambiguous and that may be the reason rust does not support that, but I always want to add parameter name to make function type more clear,
    //   so they are distinguished by always parseing type ref and very simple result (only one identifier) followed with colon is regarded as parameter name
    // - there is no `fn <T, U>(parameters)` because the normal solution is 
    //   declare these parameters outside: `fn accept_some_fn<T, U>(f: fn(T, U) -> T) -> U`,
    //   it's really hard (maybe impossible) to resolve the type parameters in bare (not in impl block etc.) `fn accept_some_template(f: fn<T, U>(T, U) -> T) -> U`
    fn parse_fn_type(&mut self) -> Result<Index<FnType>, Unexpected> {

        let fn_span = self.expect_keyword(Keyword::Fn)?;
        let left_paren_span = self.expect_sep(Separator::LParen)?;

        let mut parameters = Vec::new();
        let right_paren_span = loop {
            if let Some((right_paren_span, comma_span)) = self.try_expect_closing_bracket(Separator::RParen) {
                if let (0, Some(comma_span)) = (parameters.len(), comma_span) {
                    self.base.emit("unexpected token").detail(comma_span, "expected ident, type or right paren, meet comma");
                }
                break right_paren_span;
            } else if !parameters.is_empty() {
                self.expect_sep(Separator::Comma)?;
            }
            // ident/identifierable + single colon should be paramter name
            // // cannot assign to name here because cannot call self.move_next inside this match
            let name_id = match (&self.buf[0], &self.buf[1]) {
                (Token::Ident(ident), Token::Sep(Separator::Colon)) => {
                    Some(*ident)
                },
                (Token::Keyword(kw), Token::Sep(Separator::Colon)) if kw.kind(KeywordKind::MaybeIdentifier) => {
                    Some(self.base.intern(kw.display()))
                },
                _ => None,
            };
            let name = name_id.map(|i| IdSpan::new(i, self.move_next()));
            if name.is_some() {
                self.move_next(); // eat COLON
            }
            // parameter type
            let r#type = self.parse_type_ref()?;
            parameters.push(self.arena.emplace_fn_type_parameter(name.map(|n| n.span).unwrap_or_else(|| r#type.span(self.arena)) + r#type.span(self.arena), name, r#type));
        };

        let ret_type = self.try_expect_seps(&[Separator::SubGt, Separator::Colon]).map(|(sep, span)| {
            if sep == Separator::Colon {
                self.base.emit(strings::FunctionReturnTypeShouldUseArrow).detail(span, strings::FunctionReturnTypeExpectArrowMeetColon);
            }
            self.parse_type_ref()
        }).transpose()?;
        
        let span = fn_span + ret_type.as_ref().map(|t| t.span(self.arena)).unwrap_or(right_paren_span);
        Ok(self.arena.emplace_fn_type(span, left_paren_span + right_paren_span, parameters, ret_type))
    }

    fn maybe_primitive_type(&self) -> bool {
        matches!(self.buf[0], Token::Keyword(kw) if kw.kind(KeywordKind::Primitive))
    }

    // primitive_type = primitive_keyword
    fn parse_primitive_type(&mut self) -> Result<Index<PrimitiveType>, Unexpected> {
        let (base, span) = self.expect_keyword_kind(KeywordKind::Primitive)?;
        Ok(self.arena.emplace_primitive_type(span, base))
    }

    fn maybe_ref_type(&self) -> bool {
        matches!(self.buf[0], Token::Sep(Separator::And | Separator::AndAnd))
    }

    // ref_type = '&' type_ref
    fn parse_ref_type(&mut self) -> Result<Index<RefType>, Unexpected> {
        
        let and_span = self.expect_sep(Separator::And)?;
        let base = self.parse_type_ref()?;
        Ok(self.arena.emplace_ref_type(and_span + base.span(self.arena), base))
    }

    fn maybe_tuple_type(&self) -> bool {
        matches!(self.buf[0], Token::Sep(Separator::LParen))
    }

    // tuple_type = '(' type_ref { ',' type_ref } [ ',' ] ')'
    //
    // empty for unit type, one element tuple require ending comma
    // type template name will be `tuple` when analysis, so user type `tuple` should be rejected by analysis
    fn parse_tuple_type(&mut self) -> Result<Index<TupleType>, Unexpected> {
        
        let left_paren_span = self.expect_sep(Separator::LParen)?;
        if let Some(right_paren_span) = self.try_expect_sep(Separator::RParen) {
            return Ok(self.arena.emplace_tuple_type(left_paren_span + right_paren_span, Vec::new()));
        }
        
        let mut parameters = vec![self.parse_type_ref()?];
        let span = left_paren_span + loop {
            if let Some((right_paren_span, comma_span)) = self.try_expect_closing_bracket(Separator::RParen) {
                if comma_span.is_none() && parameters.len() == 1 {
                    self.base.emit(strings::SingleItemTupleType)
                        .detail(right_paren_span, strings::TupleTypeExpectCommaMeetRParen);
                }
                break right_paren_span;
            } else {
                self.expect_sep(Separator::Comma)?;
                parameters.push(self.parse_type_ref()?);
            }
        };

        Ok(self.arena.emplace_tuple_type(span, parameters))
    }
}

// statement and item (module level item) parsers
impl<'ecx, 'scx, 'a> Parser<'ecx, 'scx, 'a> {

    fn parse_stmt(&mut self) -> Result<Statement, Unexpected> {
        if self.maybe_struct_def() {
            self.parse_struct_def().map(Into::into)
        } else if self.maybe_enum_def() {
            self.parse_enum_def().map(Into::into)
        } else if self.maybe_fn_def() {
            self.parse_fn_def().map(Into::into)
        } else if self.maybe_impl_block() {
            self.parse_impl_block().map(Into::into)
        } else if self.maybe_type_def() {
            self.parse_type_def().map(Into::into)
        } else if self.maybe_class_def() {
            self.parse_class_def().map(Into::into)
        } else if self.maybe_labeled_stmt() {
            self.parse_labeled_stmt()
        } else if self.maybe_break_stmt() {
            self.parse_break_stmt().map(Into::into)
        } else if self.maybe_continue_stmt() {
            self.parse_continue_stmt().map(Into::into)
        } else if self.maybe_expr_stmt() {
            self.parse_expr_stmt()
        } else if self.maybe_if_stmt() {
            self.parse_if_stmt().map(Into::into)
        } else if self.maybe_ret_stmt() {
            self.parse_ret_stmt().map(Into::into)
        } else if self.maybe_var_decl() {
            self.parse_var_decl().map(Into::into)
        } else if self.maybe_use_stmt() {
            self.parse_use_stmt().map(Into::into)
        } else {
            self.push_unexpect("type, enum, fn, {, break, continue, for, if, loop, return, var, const, while, .., !, ~, &, <, ::, ident, [, (")
        }
    }

    fn parse_item(&mut self) -> Result<Item, Unexpected> {
        if self.maybe_struct_def() {
            self.parse_struct_def().map(Into::into)
        } else if self.maybe_enum_def() {
            self.parse_enum_def().map(Into::into)
        } else if self.maybe_fn_def() {
            self.parse_fn_def().map(Into::into)
        } else if self.maybe_impl_block() {
            self.parse_impl_block().map(Into::into)
        } else if self.maybe_type_def() {
            self.parse_type_def().map(Into::into)
        } else if self.maybe_class_def() {
            self.parse_class_def().map(Into::into)
        } else if self.maybe_labeled_stmt() {
            self.parse_labeled_stmt()
        } else if self.maybe_expr_stmt() {
            self.parse_expr_stmt()
        } else if self.maybe_if_stmt() {
            self.parse_if_stmt().map(Into::into)
        } else if self.maybe_var_decl() {
            self.parse_var_decl().map(Into::into)
        } else if self.maybe_use_stmt() {
            self.parse_use_stmt().map(Into::into)
        } else if self.maybe_module_stmt() {
            self.parse_module_stmt().map(Into::into)
        } else {
            self.push_unexpect("type, enum, fn, {, for, if, loop, return, var, const, while, use, module, .., !, ~, &, <, ::, ident, [, (")
        }
    }

    fn maybe_labeled_stmt(&self) -> bool {
        self.maybe_for_stmt()
        || self.maybe_loop_stmt()
        || self.maybe_while_stmt()
        || self.maybe_block_stmt()
        || matches!(self.buf[0], Token::Label(_))
    }

    fn parse_labeled_stmt<U>(&mut self) -> Result<U, Unexpected> where
        Index<BlockStatement>: Into<U>,
        Index<ForStatement>: Into<U>,
        Index<LoopStatement>: Into<U>,
        Index<WhileStatement>: Into<U>,
    {
        let label = if let Token::Label(label) = self.buf[0] {
            let label_span = self.move_next();
            if self.try_expect_sep(Separator::Colon).is_none() {
                self.base.emit(format!("expected `:`, meet {:?}", self.buf[0])).span(self.spans[0]);
            }
            Some(IdSpan::new(label, label_span))
        } else {
            None
        };

        if self.maybe_block_stmt() {
            self.parse_block_stmt(label).map(Into::into)
        } else if self.maybe_for_stmt() {
            self.parse_for_stmt(label).map(Into::into)
        } else if self.maybe_loop_stmt() {
            self.parse_loop_stmt(label).map(Into::into)
        } else if self.maybe_while_stmt() {
            self.parse_while_stmt(label).map(Into::into)
        } else {
            self.push_unexpect("`{`, `for`, `loop`, `while`")
        }
    }

    #[cfg(test)]
    fn parse_labeled_stmt_as_stmt(&mut self) -> Result<Statement, Unexpected> {
        self.parse_labeled_stmt()
    }

    // generic_name = ident [ '<' ident { ',' ident } [ ',' ] '>' ]
    fn parse_generic_name(&mut self) -> Result<Index<GenericName>, Unexpected> {
        
        let base = self.expect_ident()?;
        let mut quote_span = Span::new(0, 0);
        let mut parameters = Vec::new();
        if let Some(lt_span) = self.try_expect_sep(Separator::Lt) {
            quote_span = lt_span + if let Some((gt_span, _)) = self.try_expect_closing_bracket(Separator::Gt) {
                self.base.emit(strings::EmptyGenericParameterList).span(lt_span + gt_span);
                gt_span
            } else {
                let parameter = self.expect_ident()?;
                parameters.push(self.arena.emplace_generic_parameter(parameter.span, parameter));
                loop {
                    if let Some((gt_span, _)) = self.try_expect_closing_bracket(Separator::Gt) {
                        break gt_span;
                    } else {
                        self.expect_sep(Separator::Comma)?;
                    }
                    let parameter = self.expect_ident()?;
                    parameters.push(self.arena.emplace_generic_parameter(parameter.span, parameter));
                }
            };
        }

        let span = if quote_span == Span::new(0, 0) { base.span } else { base.span + quote_span };
        Ok(self.arena.emplace_generic_name(span, base, quote_span, parameters))
    }

    // block = '{' { statement } '}'
    // it is private because it's not used outside and it's included by their tests
    fn parse_block(&mut self) -> Result<Index<Block>, Unexpected> {

        let starting_span = self.expect_sep(Separator::LBrace)?;
        let mut items = Vec::new();
        loop {
            if let Some(ending_span) = self.try_expect_sep(Separator::RBrace) {
                return Ok(self.arena.emplace_block(starting_span + ending_span, items));
            }
            items.push(self.parse_stmt()?);
        }
    }

    // label handled in parse_labeled_stmt
    fn maybe_block_stmt(&self) -> bool {
        matches!(self.buf[0], Token::Sep(Separator::LBrace))
    }

    // block-stmt = (LABEL ':')? block
    // block-stmt for explicit block definition in block and allow block label
    // label handled in parse_labeled_stmt
    fn parse_block_stmt(&mut self, label: Option<IdSpan>) -> Result<Index<BlockStatement>, Unexpected> {

        let body = self.parse_block()?;
        let body_span = self.arena.get(body).span;
        let span = label.as_ref().map(|n| n.span).unwrap_or(body_span) + body_span;
        Ok(self.arena.emplace_block_stmt(span, label, body))
    }

    fn maybe_break_stmt(&self) -> bool {
        matches!(self.buf[0], Token::Keyword(Keyword::Break)) 
    }
    
    // break_stmt = 'break' [ label ] ';'
    fn parse_break_stmt(&mut self) -> Result<Index<BreakStatement>, Unexpected> {

        let starting_span = self.expect_keyword(Keyword::Break)?;

        if let Some(label) = self.try_expect_label() {
            let semicolon_span = self.expect_sep(Separator::SemiColon)?;
            Ok(self.arena.emplace_break_stmt(starting_span + semicolon_span, Some(label)))
        } else { 
            let semicolon_span = self.expect_sep(Separator::SemiColon)?;
            Ok(self.arena.emplace_break_stmt(starting_span + semicolon_span, None))
        }
    }

    fn maybe_class_def(&self) -> bool {
        matches!(self.buf[0], Token::Keyword(Keyword::Class))
    }

    // class_def = 'class' generic_name '{' { type_def | fn_def } '}'
    fn parse_class_def(&mut self) -> Result<Index<ClassDef>, Unexpected> {

        let start_span = self.expect_keyword(Keyword::Class)?;
        let name = self.parse_generic_name()?;
        let left_brace_span = self.expect_sep(Separator::LBrace)?;

        let mut items = Vec::new();
        let right_brace_span = loop {
            if let Some(right_brace_span) = self.try_expect_sep(Separator::RBrace) {
                break right_brace_span;
            } else if self.maybe_type_def() {
                items.push(self.parse_type_def()?.into());
            } else if self.maybe_fn_def() {
                items.push(self.parse_fn_def()?.into());
            }  else {
                self.push_unexpect("type, fn or `}`")?;
            }
        };

        Ok(self.arena.emplace_class_def(start_span + right_brace_span, name, left_brace_span + right_brace_span, items))
    }

    fn maybe_continue_stmt(&self) -> bool {
        matches!(self.buf[0], Token::Keyword(Keyword::Continue)) 
    }

    // continue_stmt = 'continue' [ label ] ';'
    fn parse_continue_stmt(&mut self) -> Result<Index<ContinueStatement>, Unexpected> {

        let starting_span = self.expect_keyword(Keyword::Continue)?;

        if let Some(label) = self.try_expect_label() {
            let semicolon_span = self.expect_sep(Separator::SemiColon)?;
            Ok(self.arena.emplace_continue_stmt(starting_span + semicolon_span, Some(label)))
        } else { 
            let semicolon_span = self.expect_sep(Separator::SemiColon)?;
            Ok(self.arena.emplace_continue_stmt(starting_span + semicolon_span, None))
        }
    }

    fn maybe_enum_def(&self) -> bool {
        matches!(self.buf[0], Token::Keyword(Keyword::Enum))
    }

    // enum_def = 'enum' ident [ ':' primitive_type ] '{' { ident [ '=' expr ] ',' } '}'
    // TODO allow variant to be struct
    fn parse_enum_def(&mut self) -> Result<Index<EnumDef>, Unexpected> {

        let enum_span = self.expect_keyword(Keyword::Enum)?;
        let enum_name = self.expect_ident()?;
        let base_type = self.try_expect_sep(Separator::Colon).map(|_| self.parse_primitive_type()).transpose()?;
        let left_brace_span = self.expect_sep(Separator::LBrace)?;

        let mut variants = Vec::new();
        let right_brace_span = if let Some(right_brace_span) = self.try_expect_sep(Separator::RBrace) {
            right_brace_span
        } else {
            loop {
                let variant_name = self.expect_ident()?;
                let init_value = self.try_expect_sep(Separator::Eq).map(|_| self.parse_expr()).transpose()?;
                let variant_all_span = variant_name.span + init_value.as_ref().map(|e| e.span(self.arena)).unwrap_or(variant_name.span);
                variants.push(self.arena.emplace_enum_def_variant(variant_all_span, variant_name, init_value));

                if let Some((right_brace_span, _)) = self.try_expect_closing_bracket(Separator::RBrace) {
                    break right_brace_span;
                } else {
                    self.expect_sep(Separator::Comma)?;
                }
            }
        };

        let quote_span = left_brace_span + right_brace_span;
        let span = enum_span + right_brace_span;
        Ok(self.arena.emplace_enum_def(span, enum_name, base_type, quote_span, variants))
    }

    fn maybe_expr_stmt(&self) -> bool {
        self.maybe_expr()
    }

    // expr_stmt = expr { assign_ops expr } ';'
    fn parse_expr_stmt<U>(&mut self) -> Result<U, Unexpected> where
        Index<AssignExprStatement>: Into<U>,
        Index<SimpleExprStatement>: Into<U>,
    {
        let left_expr = self.parse_expr()?;
        let starting_span = left_expr.span(self.arena);

        if let Some(semicolon_span) = self.try_expect_sep(Separator::SemiColon) {
            Ok(self.arena.emplace_simple_expr_stmt(starting_span + semicolon_span, left_expr).into())
        } else if let Some((op, op_span)) = self.try_expect_sep_kind(SeparatorKind::Assign) {
            let right_expr = self.parse_expr()?;
            let semicolon_span = self.expect_sep(Separator::SemiColon)?;
            Ok(self.arena.emplace_assign_expr_stmt(starting_span + semicolon_span, left_expr, right_expr, op, op_span).into())
        } else {
            self.push_unexpect("assign operators, semicolon")
        }
    }

    #[cfg(test)]
    fn parse_expr_stmt_as_stmt(&mut self) -> Result<Statement, Unexpected> {
        self.parse_expr_stmt()
    }

    fn maybe_fn_def(&self) -> bool {
        matches!(self.buf[0], Token::Keyword(Keyword::Fn))
    }

    // fn-def = 'fn' generic_name '(' [ identifier ':' type-use { ',' identifier ':' type-use [ ',' ] } ] ')' [ '->' type-use ] [ 'where' { where_clause ',' } ] [ block ]
    // where_clause = ident ':' type_ref { '+' type_ref }
    // TODO left of where clause is type_ref
    fn parse_fn_def(&mut self) -> Result<Index<FnDef>, Unexpected> {

        let fn_span = self.expect_keyword(Keyword::Fn)?;
        let fn_name = self.parse_generic_name()?;
        let mut quote_span = self.expect_sep(Separator::LParen)?;

        let mut parameters = Vec::new();
        loop {
            if let Some((right_paren_span, comma_span)) = self.try_expect_closing_bracket(Separator::RParen) {
                quote_span += right_paren_span;
                if let (0, Some(comma_span)) = (parameters.len(), comma_span) {
                    self.base.emit("Single comma in function definition argument list").span(comma_span);
                }
                break;
            } else if let Some(_comma_span) = self.try_expect_sep(Separator::Comma) {
                continue;
            }

            let parameter_name = self.expect_ident_or_keywords(&[Keyword::Underscore, Keyword::SelfL])?;
            self.expect_sep(Separator::Colon)?;
            let r#type = self.parse_type_ref()?;
            parameters.push(self.arena.emplace_fn_def_parameter(parameter_name.span + r#type.span(self.arena), parameter_name, r#type));
        }

        let ret_type = self.try_expect_seps(&[Separator::SubGt, Separator::Colon]).map(|(sep, span)| {
            if sep == Separator::Colon {
                self.base.emit(strings::FunctionReturnTypeShouldUseArrow).detail(span, strings::FunctionReturnTypeExpectArrowMeetColon);
            }
            self.parse_type_ref()
        }).transpose()?;

        macro_rules! parse_where { () => {{
            let name = self.expect_ident()?;
            self.expect_sep(Separator::Colon)?;
            let mut constraints = vec![self.parse_type_ref()?];
            while self.try_expect_sep(Separator::Add).is_some() { // no trailing add here
                constraints.push(self.parse_type_ref()?);
            }
            self.arena.emplace_where_clause(name.span + constraints.last().unwrap().span(self.arena), name, constraints)
        }};}

        let mut wheres = Vec::new();
        if self.try_expect_keyword(Keyword::Where).is_some() {
            wheres.push(parse_where!());
            loop {
                // left brace/semicolon is not closing bracket, and this does not move forward
                if self.is_sep(Separator::LBrace) || self.is_sep(Separator::SemiColon) {
                    break;
                } else if matches!((&self.buf[0], &self.buf[1]), (Token::Sep(Separator::Comma), Token::Sep(Separator::LBrace | Separator::SemiColon))) {
                    self.move_next();
                    break;
                } else {
                    self.expect_sep(Separator::Comma)?;
                }
                wheres.push(parse_where!());
            }
        }

        let (ending_span, body) = if self.is_sep(Separator::LBrace) {
            let body = self.parse_block()?;
            (self.arena.get(body).span, Some(body))
        } else {
            (self.expect_sep(Separator::SemiColon)?, None) 
        };
        Ok(self.arena.emplace_fn_def(fn_span + ending_span, fn_name, quote_span, parameters, ret_type, wheres, body))
    }

    // label handled in parse_labeled_stmt
    fn maybe_for_stmt(&self) -> bool {
        matches!(self.buf[0], Token::Keyword(Keyword::For))
    }

    // for-stmt = (LABEL ':')? 'for' IDENT 'in' expr block
    // label handled in parse_labeled_stmt
    // TODO: add else for break, like python
    fn parse_for_stmt(&mut self, label: Option<IdSpan>) -> Result<Index<ForStatement>, Unexpected> {

        let for_span = self.expect_keyword(Keyword::For)?;

        // Accept _ as iter_name, _ do not declare iter var
        let iter_name = self.expect_ident_or_keywords(&[Keyword::Underscore])?; 
        self.expect_keyword(Keyword::In)?;

        let iter_expr = self.parse_expr_except_object_expr()?;
        let body = self.parse_block()?;
        
        let span = label.as_ref().map(|n| n.span).unwrap_or(for_span) + self.arena.get(body).span;
        Ok(self.arena.emplace_for_stmt(span, label, iter_name, iter_expr, body))
    }

    fn maybe_if_stmt(&self) -> bool {
        matches!(self.buf[0], Token::Keyword(Keyword::If)) 
    }

    // if_stmt = 'if' expr block { 'else' 'if' expr block } [ 'else' block ]
    fn parse_if_stmt(&mut self) -> Result<Index<IfStatement>, Unexpected> {

        let mut all_span = self.expect_keyword(Keyword::If)?;

        let if_condition = self.parse_expr_except_object_expr()?;
        let if_body = self.parse_block()?;
        all_span += self.arena.get(if_body).span;
        let if_clause = self.arena.emplace_if_clause(all_span, if_condition, if_body);

        let mut elseif_clauses = Vec::new();
        let mut else_clause = None;
        while let Some(else_span) = self.try_expect_keyword(Keyword::Else) {
            if let Some(if_span) = self.try_expect_keyword(Keyword::If) {
                let elseif_span = else_span + if_span;
                let elseif_condition = self.parse_expr_except_object_expr()?;
                let elseif_body = self.parse_block()?;
                all_span += self.arena.get(elseif_body).span;
                elseif_clauses.push(self.arena.emplace_if_clause(elseif_span + self.arena.get(elseif_body).span, elseif_condition, elseif_body));
            } else {
                // 16/12/1, we lost TWO `+1`s for current_length here ... fixed
                // 17/5/6: When there is match Block::parse(tokens, messages, index + current_length), etc.
                // There was a bug fix here, now no more current_length handling!
                // 17/6/21: a new physical structure update makes it much more simple
                // 17/7/28: a new small update of parse_cx makes things even more simple
                // 22/5/12: it's interesting to see the Apr 2022 age "arena" feature under this 17s comment
                let else_body = self.parse_block()?;
                all_span += self.arena.get(else_body).span;
                else_clause = Some(self.arena.emplace_else_clause(else_span + self.arena.get(else_body).span, else_body));
            }
        }

        Ok(self.arena.emplace_if_stmt(all_span, if_clause, elseif_clauses, else_clause))
    }

    fn maybe_impl_block(&self) -> bool {
        matches!(self.buf[0], Token::Keyword(Keyword::Impl))
    }

    // impl = 'impl' generic_parameters [ type_ref 'for' ] type_ref where_clauses '{' { type_def | fn_def } '}'
    fn parse_impl_block(&mut self) -> Result<Index<Implementation>, Unexpected> {
        let start_span = self.expect_keyword(Keyword::Impl)?;

        let mut parameters = Vec::new();
        if let Some(lt_span) = self.try_expect_sep(Separator::Lt) {
            if let Some((gt_span, _)) = self.try_expect_closing_bracket(Separator::Gt) {
                self.base.emit(strings::EmptyGenericParameterList).span(lt_span + gt_span);
            } else {
                let parameter = self.expect_ident()?;
                parameters.push(self.arena.emplace_generic_parameter(parameter.span, parameter));
                loop {
                    if self.try_expect_closing_bracket(Separator::Gt).is_some() {
                        break;
                    } else {
                        self.expect_sep(Separator::Comma)?;
                    }
                    let parameter = self.expect_ident()?;
                    parameters.push(self.arena.emplace_generic_parameter(parameter.span, parameter));
                }
            }
        }

        let type1 = self.parse_type_ref()?;
        let type2 = self.try_expect_keyword(Keyword::For).map(|_| self.parse_type_ref()).transpose()?;
        let (class, r#type) = if let Some(type2) = type2 { (Some(type1), type2) } else { (None, type1) };

        let mut wheres = Vec::new();
        macro_rules! parse_where { () => {{
            let name = self.expect_ident()?;
            self.expect_sep(Separator::Colon)?;
            let mut constraints = vec![self.parse_type_ref()?];
            while self.try_expect_sep(Separator::Add).is_some() { // no trailing add here
                constraints.push(self.parse_type_ref()?);
            }
            wheres.push(self.arena.emplace_where_clause(name.span + constraints.last().unwrap().span(self.arena), name, constraints))
        }};}
        if self.try_expect_keyword(Keyword::Where).is_some() {
            parse_where!();
            loop {
                // left brace is not closing bracket, and this does not move forward
                if self.is_sep(Separator::LBrace) {
                    break;
                } else if matches!((&self.buf[0], &self.buf[1]), (Token::Sep(Separator::Comma), Token::Sep(Separator::LBrace))) {
                    self.move_next();
                    break;
                } else {
                    self.expect_sep(Separator::Comma)?;
                }
                parse_where!();
            }
        }

        let left_brace_span = self.expect_sep(Separator::LBrace)?;
        let mut items = Vec::new();
        let right_brace_span = loop {
            if let Some(right_brace_span) = self.try_expect_sep(Separator::RBrace) {
                break right_brace_span;
            } else if self.maybe_type_def() {
                items.push(self.parse_type_def()?.into());
            } else if self.maybe_fn_def() {
                items.push(self.parse_fn_def()?.into());
            }  else {
                self.push_unexpect("type, fn or `}`")?;
            }
        };

        let span = start_span + right_brace_span;
        let quote_span = left_brace_span + right_brace_span;
        Ok(self.arena.emplace_impl_block(span, parameters, class, r#type, wheres, quote_span, items))
    }

    // label handled in parse_labeled_stmt
    fn maybe_loop_stmt(&self) -> bool {
        matches!(self.buf[0], Token::Keyword(Keyword::Loop))
    }

    // loop-stmt = (LABEL ':')? 'loop' block
    // label handled in parse_labeled_stmt
    // NOTE: no else for break here because if control flow come to else it is always breaked
    fn parse_loop_stmt(&mut self, label: Option<IdSpan>) -> Result<Index<LoopStatement>, Unexpected> {

        let loop_span = self.expect_keyword(Keyword::Loop)?;
        let body = self.parse_block()?;
        let span = label.as_ref().map(|n| n.span).unwrap_or(loop_span) + self.arena.get(body).span;
        Ok(self.arena.emplace_loop_stmt(span, label, body))
    }

    fn maybe_module_stmt(&self) -> bool {
        matches!(self.buf[0], Token::Keyword(Keyword::Module)) 
    }

    // module_stmt = 'module' identifier [ str_lit ] ';'
    fn parse_module_stmt(&mut self) -> Result<Index<ModuleStatement>, Unexpected> {

        let starting_span = self.expect_keyword(Keyword::Module)?;
        let module_name = self.expect_ident()?;

        let path = self.try_expect_str_lit(); 
        let semicolon_span = self.expect_sep(Separator::SemiColon)?;
        let span = starting_span + semicolon_span;

        Ok(self.arena.emplace_module_stmt(span, module_name, path))
    }

    fn maybe_ret_stmt(&self) -> bool {
        matches!(self.buf[0], Token::Keyword(Keyword::Return))
    }

    // ret_stmt = 'return' [ expr ] ';'
    fn parse_ret_stmt(&mut self) -> Result<Index<ReturnStatement>, Unexpected> {

        let starting_span = self.expect_keyword(Keyword::Return)?;
        if let Some(semicolon_span) = self.try_expect_sep(Separator::SemiColon) {
            // 17/6/17: you forgot move_next here!
            // but I have never write some test cases like following something after ret stmt
            // so the bug is not propagated to be discovered
            // 17/7/28: now new features added to parse_cx and move_next is to be removed, no current position management bug any more!
            Ok(self.arena.emplace_ret_stmt(starting_span + semicolon_span, None))
        } else {
            let value = self.parse_expr()?;
            let semicolon_span = self.expect_sep(Separator::SemiColon)?;
            Ok(self.arena.emplace_ret_stmt(starting_span + semicolon_span, Some(value)))
        }
    }

    fn maybe_struct_def(&self) -> bool {
        matches!(self.buf[0], Token::Keyword(Keyword::Struct)) 
    }

    // struct_def = 'struct' generic_name  '{' [ field_def { ',' field_def } [ ',' ] ] '}'
    // field_def = identifier ':' type_ref
    fn parse_struct_def(&mut self) -> Result<Index<StructDef>, Unexpected> {

        let starting_span = self.expect_keyword(Keyword::Struct)?;
        let type_name = self.parse_generic_name()?;
        self.expect_sep(Separator::LBrace)?;

        let mut fields = Vec::new();
        macro_rules! parse_field {
            () => {{
                let field_name = self.expect_ident()?;
                let colon_span = self.expect_sep(Separator::Colon)?;
                let field_type = self.parse_type_ref()?;
                fields.push(self.arena.emplace_field_def(field_name.span + field_type.span(self.arena), field_name, colon_span, field_type));
            }}
        }

        let right_brace_span = if let Some((right_brace_span, _)) = self.try_expect_closing_bracket(Separator::RBrace) {
            right_brace_span
        } else {
            parse_field!();
            loop {
                if let Some((right_brace_span, _)) = self.try_expect_closing_bracket(Separator::RBrace) {
                    break right_brace_span;     // rustc 1.19 stablize break-expr
                } else {
                    self.expect_sep(Separator::Comma)?;
                }
                parse_field!();
            }
        };

        Ok(self.arena.emplace_struct_def(starting_span + right_brace_span, type_name, fields))
    }
    
    fn maybe_type_def(&self) -> bool {
        matches!(self.buf[0], Token::Keyword(Keyword::Type))
    }

    // type_alias = 'type' generic_name [ '=' type_ref ] ';'
    fn parse_type_def(&mut self) -> Result<Index<TypeDef>, Unexpected> {
        
        let start_span = self.expect_keyword(Keyword::Type)?;
        let name = self.parse_generic_name()?;
        let from = self.try_expect_sep(Separator::Eq).map(|_| self.parse_type_ref()).transpose()?;
        let end_span = self.expect_sep(Separator::SemiColon)?;
        Ok(self.arena.emplace_type_def(start_span + end_span, name, from))
    }

    fn maybe_use_stmt(&self) -> bool {
        matches!(self.buf[0], Token::Keyword(Keyword::Use)) 
    }

    // use_stmt = 'use' name [ 'as' identifier ] ';'
    fn parse_use_stmt(&mut self) -> Result<Index<UseStatement>, Unexpected> {

        let starting_span = self.expect_keyword(Keyword::Use)?;
        let path = self.parse_path(PathContext::Value)?;

        let alias = self.try_expect_keyword(Keyword::As).map(|_| self.expect_ident()).transpose()?;
        let semicolon_span = self.expect_sep(Separator::SemiColon)?;
        let span = starting_span + semicolon_span;

        Ok(self.arena.emplace_use_stmt(span, path, alias))
    }

    fn maybe_var_decl(&self) -> bool {
        matches!(self.buf[0], Token::Keyword(Keyword::Const | Keyword::Var)) 
    }

    // const-decl = 'const' identifier [ ':' type-use ] [ '=' expr ] ';'
    // var-decl = 'var' identifier [ ':' type-use ] [ '=' expr ] ';'
    fn parse_var_decl(&mut self) -> Result<Index<VarDeclStatement>, Unexpected> {
        
        let (starting_kw, starting_span) = self.expect_keywords(&[Keyword::Const, Keyword::Var])?;
        let r#const = match starting_kw { Keyword::Const => true, Keyword::Var => false, _ => unreachable!() };

        let var_name = self.expect_ident_or_keywords(&[Keyword::Underscore])?;
        let r#type = self.try_expect_sep(Separator::Colon).map(|_| self.parse_type_ref()).transpose()?;
        let init_value = self.try_expect_sep(Separator::Eq).map(|_| self.parse_expr()).transpose()?;
        let ending_span = self.expect_sep(Separator::SemiColon)?;

        if r#type.is_none() && init_value.is_none() {
            self.base.emit("require type annotation")
                .detail(var_name.span, "variable declaration here")
                .help("cannot infer type without both type annotation and initialization expression");
        }

        Ok(self.arena.emplace_var_decl_stmt(starting_span + ending_span, r#const, var_name, r#type, init_value))
    }

    // label handled in parse_labeled_stmt
    fn maybe_while_stmt(&self) -> bool {
        matches!(self.buf[0], Token::Keyword(Keyword::While))
    }

    // while-stmt = (LABEL ':')? 'while' expr block
    // label handled in parse_labeled_stmt
    fn parse_while_stmt(&mut self, label: Option<IdSpan>) -> Result<Index<WhileStatement>, Unexpected> {

        let while_span = self.expect_keyword(Keyword::While)?;
        let condition = self.parse_expr_except_object_expr()?;
        let body = self.parse_block()?;
        let span = label.as_ref().map(|n| n.span).unwrap_or(while_span) + self.arena.get(body).span;
        Ok(self.arena.emplace_while_stmt(span, label, condition, body))
    }
}
