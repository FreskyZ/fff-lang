use super::*;

impl<'ecx, 'scx> Parser<'ecx, 'scx> {
    
    // no maybe_type_ref because type ref is always after some colon or array separator

    pub fn parse_type_ref(&mut self) -> Result<TypeRef, Unexpected> {
        if self.maybe_primitive_type() {
            Ok(TypeRef::Primitive(self.parse_primitive_type()?))
        } else if self.maybe_array_type() {
            Ok(TypeRef::Array(self.parse_array_type()?))
        } else if self.maybe_fn_type() {
            Ok(TypeRef::Fn(self.parse_fn_type()?))
        } else if self.maybe_ref_type() {
            Ok(TypeRef::Ref(self.parse_ref_type()?))
        } else if self.maybe_tuple_type() {
            Ok(TypeRef::Tuple(self.parse_tuple_type()?))
        } else if self.maybe_plain_type() {
            Ok(TypeRef::Plain(self.parse_plain_type()?))
        } else {
            self.push_unexpect("[, fn, &, (, <, ::, ident or primitive type")
        }
    }

    // type refs, also include path segment
    pub fn maybe_array_type(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::LeftBracket))
    }

    // array_type = '[' type_ref ';' expr ']'
    pub fn parse_array_type(&mut self) -> Result<ArrayType, Unexpected> {

        let left_bracket_span = self.expect_sep(Separator::LeftBracket)?;
        let base = Box::new(self.parse_type_ref()?);

        if let Some(right_bracket_span) = self.try_expect_sep(Separator::RightBracket) {
            self.emit(strings::InvalidArrayType)
                .detail(right_bracket_span, "expected semicolon, meet right bracket")
                .help(strings::ArrayTypeSyntaxHelp);
            return Ok(ArrayType{ base, size: Expr::dummy(), span: left_bracket_span + right_bracket_span });
        }

        let _semicolon_span = self.expect_sep(Separator::SemiColon)?;

        if let Some(right_bracket_span) = self.try_expect_sep(Separator::RightBracket) {
            self.emit(strings::InvalidArrayType)
                .detail(right_bracket_span, "expected expr, meet right bracket")
                .help(strings::ArrayTypeSyntaxHelp);
            return Ok(ArrayType{ base, size: Expr::dummy(), span: left_bracket_span + right_bracket_span });
        }

        let size = self.parse_expr()?;
        let right_bracket_span = self.expect_sep(Separator::RightBracket)?;
        Ok(ArrayType{ base, size, span: left_bracket_span + right_bracket_span })
    }

    pub fn maybe_fn_type(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Fn))
    }

    // fn_type = 'fn' '(' [ ident ':' ] type_ref { ',' [ ident ':' ] type_ref } [ ',' ] ')' [ '->' type_ref ]
    //
    // - return type in fn type and fn def is not colon but arrow: https://mail.mozilla.org/pipermail/rust-dev/2013-July/005042.html
    // - parameter name is optional and does not affect type identity
    //   type ref may start with ident, actually most common type refs start with ident, 
    //   so it is ambiguous and that may be the reason rust does not support that, but I always want to add parameter name to make function type more clear,
    //   so they are distinguished by always parseing type ref and very simple result (only one identifier) followed with colon is regarded as parameter name
    pub fn parse_fn_type(&mut self) -> Result<FnType, Unexpected> {
        
        let fn_span = self.expect_keyword(Keyword::Fn)?;
        let left_paren_span = self.expect_sep(Separator::LeftParen)?;

        let mut parameters = Vec::new();
        let right_paren_span = loop {
            if let Some((right_paren_span, skipped_comma)) = self.try_expect_closing_bracket(Separator::RightParen) {
                if skipped_comma && parameters.is_empty() {
                    // TODO: need comma span
                    self.emit("unexpected token").detail(right_paren_span, "expected ident, type or right paren, meet comma");
                }
                break right_paren_span;
            } else if !parameters.is_empty() {
                self.expect_sep(Separator::Comma)?;
            }
            // these can-regard-as-variable keywords are not expected by type ref, they are definitely parameter name
            let name = self.try_expect_keywords(&[Keyword::This, Keyword::Self_, Keyword::Underscore]);
            if name.is_some() {
                self.expect_sep(Separator::Colon)?;
            }
            let r#type = self.parse_type_ref()?;
            let (name, r#type) = if let TypeRef::Plain(PlainType{ type_as_segment: None, global: false, segments, .. }) = &r#type {
                if name.is_none() // this one should be before previous let r#type but that will make it 3 ifs are too more (None, r#type)s
                    && segments.len() == 1 && segments[0].parameters.is_empty() && self.try_expect_sep(Separator::Colon).is_some() {
                    (Some((segments[0].ident, segments[0].ident_span)), self.parse_type_ref()?)
                } else {
                    (name.map(|(kw, span)| (self.intern(kw.display()), span)), r#type)
                }
            } else {
                (name.map(|(kw, span)| (self.intern(kw.display()), span)), r#type)
            };
            parameters.push(FnTypeParam{ name, span: name.map(|(_, name_span)| name_span).unwrap_or_else(|| r#type.span()) + r#type.span(), r#type });
        };

        let ret_type = self.try_expect_seps(&[Separator::Arrow, Separator::Colon]).map(|(sep, span)| {
            if sep == Separator::Colon {
                self.emit(strings::FunctionReturnTypeShouldUseArrow).detail(span, strings::FunctionReturnTypeExpectArrowMeetColon);
            }
            self.parse_type_ref()
        }).transpose()?;
        
        let span = fn_span + ret_type.as_ref().map(|t| t.span()).unwrap_or(right_paren_span);
        Ok(FnType{ quote_span: left_paren_span + right_paren_span, parameters, ret_type: ret_type.map(Box::new), span })
    }

    pub fn maybe_plain_type(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::Lt | Separator::ColonColon) | Token::Ident(_))
    }

    // plain_type = [ type_as_segment | '::' ] plain_type_segment { '::' plain_type_segment }
    // type_as_segment = '<' type_ref 'as' type_ref '>' '::'
    // plain_type_segment = identifier [ '<' type_ref { ',' type_ref } [ ',' ] '>' ]
    //
    // most common type ref, plain means not special (array/tuple/fn) and not referenced (not directly a reference type)
    // may be namespaced, segment may contain type parameter, does not need namespace separator `::` before type list angle bracket pair
    // may contain a type_as_segment at beginning
    // may contain a namespace separator at beginning, for referencing global items
    pub fn parse_plain_type(&mut self) -> Result<PlainType, Unexpected> {

        let type_as_segment = self.try_expect_sep(Separator::Lt).map(|lt_span| {
            let from = self.parse_type_ref()?;
            self.expect_keyword(Keyword::As)?;
            let to = self.parse_type_ref()?;
            let gt_span = self.expect_sep(Separator::Gt)?;
            Ok(TypeAsSegment{ from: Box::new(from), to: Box::new(to), span: lt_span + gt_span })
        }).transpose()?;

        let beginning_separator_span = self.try_expect_sep(Separator::ColonColon);

        let mut segments = Vec::new();
        while let Some((ident, ident_span)) = self.try_expect_ident() {
            if let Some(lt_span) = self.try_expect_sep(Separator::Lt) {
                if let Some(gt_span) = self.try_expect_sep(Separator::Gt) { // allow <> in syntax parse
                    segments.push(TypeSegment{ ident, ident_span, quote_span: lt_span + gt_span, parameters: Vec::new(), span: ident_span + gt_span });
                } else {
                    let mut parameters = vec![self.parse_type_ref()?];
                    let quote_span = lt_span + loop {
                        if let Some((gt_span, _)) = self.try_expect_closing_bracket(Separator::Gt) {
                            break gt_span;
                        }
                        self.expect_sep(Separator::Comma)?;
                        parameters.push(self.parse_type_ref()?);
                    };
                    segments.push(TypeSegment{ ident, ident_span, quote_span, parameters, span: ident_span + quote_span });
                }
            } else {
                segments.push(TypeSegment{ ident, ident_span, quote_span: Span::new(0, 0), parameters: Vec::new(), span: ident_span });
            }
            if self.try_expect_sep(Separator::ColonColon).is_none() {
                break;
            }
        }

        let global = type_as_segment.is_none() && beginning_separator_span.is_some();
        let span = type_as_segment.as_ref().map(|s| s.span).or(beginning_separator_span)
            .unwrap_or_else(|| segments[0].span) + segments.last().unwrap().span; // [0] and last().unwrap(): matches() guarantees segments are not empty
        Ok(PlainType{ type_as_segment, global, segments, span })
    }

    pub fn maybe_primitive_type(&self) -> bool {
        matches!(self.current, Token::Keyword(kw) if kw.kind(KeywordKind::Primitive))
    }

    // primitive_type = primitive_keyword
    pub fn parse_primitive_type(&mut self) -> Result<PrimitiveType, Unexpected> {
        let (name, span) = self.expect_keyword_kind(KeywordKind::Primitive)?;
        Ok(PrimitiveType{ name, span })
    }

    pub fn maybe_ref_type(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::And | Separator::AndAnd))
    }

    // ref_type = '&' type_ref
    pub fn parse_ref_type(&mut self) -> Result<RefType, Unexpected> {
        
        let and_span = self.expect_sep(Separator::And)?;
        let base = self.parse_type_ref()?;
        Ok(RefType{ span: and_span + base.span(), base: Box::new(base) })
    }

    pub fn maybe_tuple_type(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::LeftParen))
    }

    // tuple_type = '(' type_ref { ',' type_ref } [ ',' ] ')'
    //
    // empty for unit type, one element tuple require ending comma
    // type template name will be `tuple` when analysis, so user type `tuple` should be rejected by analysis
    pub fn parse_tuple_type(&mut self) -> Result<TupleType, Unexpected> {
        
        let left_paren_span = self.expect_sep(Separator::LeftParen)?;
        if let Some(right_paren_span) = self.try_expect_sep(Separator::RightParen) {
            return Ok(TupleType{ items: Vec::new(), span: left_paren_span + right_paren_span });
        }
        
        let mut items = vec![self.parse_type_ref()?];
        let span = left_paren_span + loop {
            if let Some((right_paren_span, skipped_comma)) = self.try_expect_closing_bracket(Separator::RightParen) {
                if !skipped_comma && items.len() == 1 {
                    self.emit(strings::SingleItemTupleType)
                        .detail(right_paren_span, strings::TupleTypeExpectCommaMeetRightParen);
                }
                break right_paren_span;
            } else {
                self.expect_sep(Separator::Comma)?;
                items.push(self.parse_type_ref()?);
            }
        };

        Ok(TupleType{ items, span })
    }
}