use super::*;

impl<'ecx, 'scx> ParseContext<'ecx, 'scx> {

    pub fn maybe_array_def(&self) -> bool {
        self.is_sep(Separator::LeftBracket)
    }

    // array_def = '[' [ expr_list ] ']'
    pub fn parse_array_def(&mut self) -> Result<Expr, Unexpected> {
        match self.parse_expr_list()? {
            ExprListParseResult::Empty(span) =>
                Ok(Expr::Array(ArrayDef{ bracket_span: span, items: ExprList{ items: Vec::new() } })),
            ExprListParseResult::Normal(span, exprlist) 
            | ExprListParseResult::EndWithComma(span, exprlist) =>
                Ok(Expr::Array(ArrayDef{ bracket_span: span, items: exprlist })),
            ExprListParseResult::SingleComma(span) => {
                self.emit(strings::UnexpectedSingleComma).detail(span, strings::ArrayDefHere);
                Ok(Expr::Array(ArrayDef{ bracket_span: span, items: ExprList{ items: Vec::new() } }))
            }
        }
    }

    // binary_expr = unary_expr binary_op unary_expr
    pub fn parse_binary_expr(&mut self) -> Result<Expr, Unexpected> {
        #[cfg(feature = "trace_binary_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[PrimaryExpr] "); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_binary_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        return parse_logical_or(self);

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
        impl_binary_parser! { parse_bitor, parse_bitxor, BitOr } // `==` and `!=` lower than `|` for `if enum_var & enum_mem1 == enum_mem1`
        impl_binary_parser! { parse_equality, parse_bitor, Equality }
        impl_binary_parser! { parse_logical_and, parse_equality, LogicalAnd }
        impl_binary_parser! { parse_logical_or, parse_logical_and, LogicalOr }
    }

    // expr_list = opening_sep expr { ',' expr } [ ',' ] closing_sep
    // TODO move emit message into this method not parse_array_def, parse_tuple_def, etc.
    pub fn parse_expr_list(&mut self) -> Result<ExprListParseResult, Unexpected> {

        let (starting_sep, starting_span) = self.expect_seps(&[Separator::LeftBrace, Separator::LeftBracket, Separator::LeftParen])?;
        let expect_end_sep = match starting_sep { 
            Separator::LeftBrace => Separator::RightBrace, 
            Separator::LeftBracket => Separator::RightBracket,
            Separator::LeftParen => Separator::RightParen,
            _ => unreachable!(),
        };

        if let Some((ending_span, skipped_comma)) = self.try_expect_closing_bracket(expect_end_sep) {
            return if skipped_comma {
                Ok(ExprListParseResult::SingleComma(starting_span + ending_span))
            } else {
                Ok(ExprListParseResult::Empty(starting_span + ending_span))
            };
        }

        self.no_object_literals.push(false);
        let mut items = Vec::new();
        loop {
            items.push(self.expect::<Expr>()?);
            if let Some((ending_span, skipped_comma)) = self.try_expect_closing_bracket(expect_end_sep) {
                self.no_object_literals.pop();
                return if skipped_comma {
                    Ok(ExprListParseResult::EndWithComma(starting_span + ending_span, ExprList{ items }))
                } else {
                    Ok(ExprListParseResult::Normal(starting_span + ending_span, ExprList{ items }))
                };
            }
            self.expect_sep(Separator::Comma)?;
        }
    }

    // fn_call_expr = primary_expr '(' [ expr_list ] ')'
    // return quote span and expr list, see parse_postfix_expr for the return type
    pub fn parse_fn_call(&mut self) -> Result<(Span, ExprList), Unexpected> {

        match self.parse_expr_list()? {
            ExprListParseResult::Normal(span, expr_list) => {
                Ok((span, expr_list))
            },
            ExprListParseResult::EndWithComma(span, expr_list) => {
                Ok((span, expr_list))
            },
            ExprListParseResult::Empty(span) => {
                Ok((span, ExprList{ items: Vec::new() }))
            },
            ExprListParseResult::SingleComma(span) => {
                self.emit(strings::UnexpectedSingleComma).detail(span, strings::FnCallHere);
                Ok((span, ExprList{ items: Vec::new() }))
            },
        }
    }
    
    // index_call_expr = primary_expr '[' [ expr_list ] ']'
    // return quote span and expr list, see parse_postfix_expr for the return type
    // // was called postfix_expr::subscription, this one is shorter and similar to fn_call
    pub fn parse_index_call(&mut self) -> Result<(Span, ExprList), Unexpected> {

        match self.parse_expr_list()? {
            ExprListParseResult::Normal(span, params) | ExprListParseResult::EndWithComma(span, params) => {
                Ok((span, params))
            },
            ExprListParseResult::Empty(span) | ExprListParseResult::SingleComma(span) => {
                self.emit(strings::EmptyIndexCall).detail(span, strings::IndexCallHere);
                Ok((span, ExprList{ items: Vec::new() }))
            },
        }
    }

    // member_access = expr '.' member
    // member = num_lit | name
    // TODO: member_access = primary_expr '.' name_segment, and add num lit to name_segment to prevent the intern, and control allow-num-lit-name-segment into context
    // return dot span and name, see parse_postfix_expr for the return type
    pub fn parse_member_access(&mut self) -> Result<(Span, Name), Unexpected> {
        
        let dot_span = self.expect_sep(Separator::Dot)?;
        let name = if let Some((numeric, span)) = self.try_expect_numeric() {
            if let Numeric::I32(v) = numeric {
                Name{ type_as_segment: None, global: false, all_span: span, segments: vec![NameSegment::Normal(self.intern(&format!("{}", v)), span)] }
            } else {
                self.emit(strings::InvalidTupleIndex).span(span).help(strings::TupleIndexSyntaxHelp);
                Name{ type_as_segment: None, global: false, all_span: span, segments: Vec::new() }
            }
        } else {
            let name = self.parse_name()?;
            // first segment will not be generic and will not be type_as_segment and global, because matches3 checks for that
            if name.segments.len() == 2 && !matches!(name.segments[1], NameSegment::Generic(..)) {
                self.emit(strings::InvalidMemberAccess).span(name.segments[1].get_span()).help(strings::GenericMemberAccessSyntaxHelp);
            }
            if name.segments.len() > 2 {
                let error_span = name.segments[1].get_span() + name.segments.last().unwrap().get_span();
                self.emit(strings::InvalidMemberAccess).span(error_span).help(strings::GenericMemberAccessSyntaxHelp);
            }
            name
        };

        Ok((dot_span, name))
    }

    // name = [ type_as_segment ] [ '::' ] name_segment { '::' name_segment }
    // name_segment = identifier | '<' type_ref { ',' type_ref } '>'
    // TODO: path_segment = 
    //    | num_lit
    //    | '<' type_ref 'as' type_ref '>'
    //    | [ '::' ] identifier [ [ '::' ] '<' type_ref { ',' type_ref } [ ',' ] '>' ]
    // enable num lit and require coloncolon control by context or parameter
    pub fn parse_name(&mut self) -> Result<Name, Unexpected> {
        let type_as_segment = self.try_expect_sep(Separator::Lt).map(|lt_span| {
            let from = self.expect::<TypeRef>()?;
            self.expect_keyword(Keyword::As)?;
            let to = self.expect::<TypeRef>()?;
            let gt_span = self.expect_sep(Separator::Gt)?;
            Ok(TypeAsSegment{ from: Box::new(from), to: Box::new(to), span: lt_span + gt_span })
        }).transpose()?;

        let beginning_separator_span = self.try_expect_sep(Separator::ColonColon);
        
        let mut segments = Vec::new();
        loop {
            if let Some((ident, ident_span)) = self.try_expect_ident() {
                segments.push(NameSegment::Normal(ident, ident_span));
            } else {
                let lt_span = self.expect_sep(Separator::Lt)?;
                // none: first segment cannot be generic segment
                // some generic: generic segment cannot follow generic segment 
                if let None | Some(NameSegment::Generic(..)) = segments.last() {
                    self.emit(strings::InvalidNameSegment).detail(lt_span, strings::NameSegmentExpect);
                }

                if let Some(gt_span) = self.try_expect_sep(Separator::Gt) { // allow <> in syntax parse
                    segments.push(NameSegment::Generic(Vec::new(), lt_span + gt_span));
                } else {
                    let mut parameters = vec![self.expect::<TypeRef>()?];
                    let quote_span = lt_span + loop {
                        if let Some((gt_span, _)) = self.try_expect_closing_bracket(Separator::Gt) {
                            break gt_span;
                        }
                        self.expect_sep(Separator::Comma)?;
                        parameters.push(self.expect::<TypeRef>()?);
                    };
                    segments.push(NameSegment::Generic(parameters, quote_span));
                }
            }
            if self.try_expect_sep(Separator::ColonColon).is_none() {
                break;
            }
        }

        let global = type_as_segment.is_none() && beginning_separator_span.is_some();
        let all_span = type_as_segment.as_ref().map(|s| s.span).or(beginning_separator_span)
            .unwrap_or_else(|| segments[0].get_span()) + segments.last().unwrap().get_span(); // [0] and last().unwrap(): matches() guarantees segments are not empty
        Ok(Name{ type_as_segment, global, segments, all_span })
    }
    
    // object_literal = name '{' { ident ':' expr ',' } '}'
    // last comma may omit
    // return quote span and field list, see parse_postfix_expr for the return type
    pub fn parse_object_literal(&mut self) -> Result<(Span, Vec<ObjectLiteralField>), Unexpected> {
        
        let left_brace_span = self.expect_sep(Separator::LeftBrace)?;
        let mut fields = Vec::new();
        let right_brace_span = if let Some(right_brace_span) = self.try_expect_sep(Separator::RightBrace) {
            right_brace_span
        } else { loop {
                let (field_name, field_name_span) = self.expect_ident()?;
                let colon_span = self.expect_sep(Separator::Colon)?;
                let value = self.expect::<Expr>()?;
                fields.push(ObjectLiteralField{ all_span: field_name_span + value.get_all_span(), name: field_name, name_span: field_name_span, colon_span, value });

                if let Some((right_brace_span, _)) = self.try_expect_closing_bracket(Separator::RightBrace) {
                    break right_brace_span;
                } else {
                    self.expect_sep(Separator::Comma)?;
                }
            }
        };

        Ok((left_brace_span + right_brace_span, fields))
    }
}
