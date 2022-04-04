use super::*;

impl<'ecx, 'scx> Parser<'ecx, 'scx> {

    // these functions are order logically
    // "public" api parse_expr and maybe_expr
    // common function parse_expr_list
    // priority high to low: primary -> postfix -> unary -> binary -> range
    // same priority inside primary: lit (inlined) -> name -> tuple -> array, this seems to be their occurance frequency order in my experience
    // same priority inside postfix: member -> fn call -> index call -> object literal, this seems to be in occurance frequency too

    // maybe_* functions: checks current token (may peek) to see if it matches syntax node starting
    // // was called Parser::matches, ISyntaxParse::matches, ISyntaxItemParse::is_first_final, IASTItem::is_first_final before
    // // considered loops_like_xxx, seems_to_be_xxx, and maybe_xxx is shortest and consist with token check methods is_xxx
    pub fn maybe_expr(&self) -> bool {
        self.is_lit()
        || self.maybe_name()
        || self.maybe_tuple_def()
        || self.maybe_array_def()
        || self.maybe_unary_expr()
        || matches!(self.current, Token::Sep(Separator::DotDot) | Token::Keyword(Keyword::This))
    }

    pub fn parse_expr(&mut self) -> Result<Expr, Unexpected> {
        self.allow_object_literal.push(true);
        let result = self.parse_range_expr();
        self.allow_object_literal.pop();
        result
    }

    // disable top level object literals,
    // until end of this expr, or call parse_expr again inside call_expr, e.g. array def and tuple def
    pub fn parse_expr_except_object_literal(&mut self) -> Result<Expr, Unexpected> {
        self.allow_object_literal.push(false);
        let result = self.parse_range_expr();
        self.allow_object_literal.pop();
        result
    }

    // expr_list = opening_sep expr { ',' expr } [ ',' ] closing_sep
    // TODO move emit message into this method not parse_array_def, parse_tuple_def, etc.
    // TODO confirm test covered by array_def_parse, tuple_def_parse, etc. and remove pub
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

        let mut items = Vec::new();
        loop {
            items.push(self.parse_expr()?);
            if let Some((ending_span, skipped_comma)) = self.try_expect_closing_bracket(expect_end_sep) {
                return if skipped_comma {
                    Ok(ExprListParseResult::EndWithComma(starting_span + ending_span, ExprList{ items }))
                } else {
                    Ok(ExprListParseResult::Normal(starting_span + ending_span, ExprList{ items }))
                };
            }
            self.expect_sep(Separator::Comma)?;
        }
    }

    pub fn maybe_name(&self) -> bool {
        matches!(self.current, Token::Ident(_) | Token::Sep(Separator::Lt | Separator::ColonColon)) 
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
            let from = self.parse_type_ref()?;
            self.expect_keyword(Keyword::As)?;
            let to = self.parse_type_ref()?;
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
                    let mut parameters = vec![self.parse_type_ref()?];
                    let quote_span = lt_span + loop {
                        if let Some((gt_span, _)) = self.try_expect_closing_bracket(Separator::Gt) {
                            break gt_span;
                        }
                        self.expect_sep(Separator::Comma)?;
                        parameters.push(self.parse_type_ref()?);
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
            .unwrap_or_else(|| segments[0].span()) + segments.last().unwrap().span(); // [0] and last().unwrap(): matches() guarantees segments are not empty
        Ok(Name{ type_as_segment, global, segments, span: all_span })
    }

    pub fn maybe_tuple_def(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::LeftParen)) 
    }

    // tuple_def = '(' expr_list ')'
    // paren_expr = '(' expr ')'
    // unit_lit = '(' ')'
    pub fn parse_tuple_def(&mut self) -> Result<Expr, Unexpected> {

        match self.parse_expr_list()? {
            ExprListParseResult::Empty(span) => {
                Ok(Expr::Lit(LitExpr{ value: LitValue::Unit, span }))
            }
            ExprListParseResult::SingleComma(span) => {
                self.emit(strings::UnexpectedSingleComma).detail(span, strings::TupleDefHere);
                Ok(Expr::Tuple(TupleDef{ span, items: ExprList{ items: Vec::new() } }))
            }
            ExprListParseResult::Normal(span, exprlist) => {
                if exprlist.items.len() == 1 {
                    Ok(Expr::Paren(ParenExpr{ span, expr: Box::new(exprlist.items.into_iter().last().unwrap()) }))
                } else {
                    Ok(Expr::Tuple(TupleDef{ span, items: exprlist }))
                }
            }
            ExprListParseResult::EndWithComma(span, exprlist) => {
                Ok(Expr::Tuple(TupleDef{ span, items: exprlist }))
            }
        }
    }

    pub fn maybe_array_def(&self) -> bool {
        self.is_sep(Separator::LeftBracket)
    }

    // array_def = '[' [ expr_list ] ']'
    pub fn parse_array_def(&mut self) -> Result<Expr, Unexpected> {
        match self.parse_expr_list()? {
            ExprListParseResult::Empty(span) =>
                Ok(Expr::Array(ArrayDef{ span, items: ExprList{ items: Vec::new() } })),
            ExprListParseResult::Normal(span, exprlist) 
            | ExprListParseResult::EndWithComma(span, exprlist) =>
                Ok(Expr::Array(ArrayDef{ span, items: exprlist })),
            ExprListParseResult::SingleComma(span) => {
                self.emit(strings::UnexpectedSingleComma).detail(span, strings::ArrayDefHere);
                Ok(Expr::Array(ArrayDef{ span, items: ExprList{ items: Vec::new() } }))
            }
        }
    }

    pub fn parse_primary_expr(&mut self) -> Result<Expr, Unexpected> {
        if self.is_lit() {
            // this is too short to put in one parse method
            // while it is actually tested more than hundred times in syntax test cases worldwide
            let (value, span) = self.expect_lit()?;
            return Ok(Expr::Lit(LitExpr{ value, span }));
        } else if self.maybe_name() {
            return self.parse_name().map(Expr::Name);
        } else if self.maybe_tuple_def() {
            return self.parse_tuple_def();
        } else if self.maybe_array_def() {
            return self.parse_array_def();
        }

        let (this_id, this_span) = self.expect_ident_or_keywords(&[Keyword::This, Keyword::Self_])?;  // actually identifier is processed by Name, not here
        Ok(Expr::Name(Name{ type_as_segment: None, global: false, span: this_span, segments: vec![NameSegment::Normal(this_id, this_span)] }))
    }

    pub fn maybe_member_access(&self) -> bool {
        matches!((&self.current, &self.peek), (Token::Sep(Separator::Dot), Token::Num(_) | Token::Ident(_))) 
    }

    // member_access = expr '.' member
    // member = num_lit | name
    // TODO: member_access = primary_expr '.' name_segment, and add num lit to name_segment to prevent the intern, and control allow-num-lit-name-segment into context
    // return dot span and name, see parse_postfix_expr for the return type
    pub fn parse_member_access(&mut self) -> Result<(Span, Name), Unexpected> {
        
        let dot_span = self.expect_sep(Separator::Dot)?;
        let name = if let Some((numeric, span)) = self.try_expect_numeric() {
            if let Numeric::I32(v) = numeric {
                Name{ type_as_segment: None, global: false, span, segments: vec![NameSegment::Normal(self.intern(&format!("{}", v)), span)] }
            } else {
                self.emit(strings::InvalidTupleIndex).span(span).help(strings::TupleIndexSyntaxHelp);
                Name{ type_as_segment: None, global: false, span, segments: Vec::new() }
            }
        } else {
            let name = self.parse_name()?;
            // first segment will not be generic and will not be type_as_segment and global, because matches3 checks for that
            if name.segments.len() == 2 && !matches!(name.segments[1], NameSegment::Generic(..)) {
                self.emit(strings::InvalidMemberAccess).span(name.segments[1].span()).help(strings::GenericMemberAccessSyntaxHelp);
            }
            if name.segments.len() > 2 {
                let error_span = name.segments[1].span() + name.segments.last().unwrap().span();
                self.emit(strings::InvalidMemberAccess).span(error_span).help(strings::GenericMemberAccessSyntaxHelp);
            }
            name
        };

        Ok((dot_span, name))
    }

    pub fn maybe_fn_call(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::LeftParen)) 
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

    pub fn maybe_index_call(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::LeftBracket))
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

    fn maybe_object_lit(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::LeftBrace))
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
                let value = self.parse_expr()?;
                fields.push(ObjectLiteralField{ span: field_name_span + value.span(), name: field_name, name_span: field_name_span, colon_span, value });

                if let Some((right_brace_span, _)) = self.try_expect_closing_bracket(Separator::RightBrace) {
                    break right_brace_span;
                } else {
                    self.expect_sep(Separator::Comma)?;
                }
            }
        };

        Ok((left_brace_span + right_brace_span, fields))
    }

    pub fn parse_postfix_expr(&mut self) -> Result<Expr, Unexpected> {   
        #[cfg(feature = "trace_postfix_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ perror!("    [PostfixExpr:{}] ", line!()); eprintln!($($arg)*); }) }
        #[cfg(not(feature = "trace_postfix_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        let mut current_expr = self.parse_primary_expr()?;
        trace!("parsed primary, current is {:?}", current_expr);

        loop {
            if self.maybe_member_access() {
                let (op_span, name) = self.parse_member_access()?;
                let span = current_expr.span() + name.span;
                let base = Box::new(current_expr);
                current_expr = Expr::Member(MemberAccessExpr{ span, base, op_span, name });
            } else if self.maybe_fn_call() {
                let (quote_span, params) = self.parse_fn_call()?;
                let span = current_expr.span() + quote_span;
                let base = Box::new(current_expr);
                current_expr = Expr::FnCall(FnCallExpr{ span, base, quote_span, params });
            } else if self.maybe_index_call() {
                let (quote_span, params) = self.parse_index_call()?;
                let span = current_expr.span() + quote_span;
                let base = Box::new(current_expr);
                current_expr = Expr::Index(IndexCallExpr{ span, base, quote_span, params });
            } else if matches!({
                // // I carefully checked that only parse_expr and parse_name is called outside this file,
                // // and found the actual intruder is unit test, this will still work when compiler_test
                #[cfg(not(test))]
                debug_assert!(!self.allow_object_literal.is_empty(), "allow_object_literal unexpectedly empty");
                &self.allow_object_literal
            }.last(), None | Some(true)) && matches!(current_expr, Expr::Name(_)) && self.maybe_object_lit() {
                let (quote_span, fields) = self.parse_object_literal()?;
                let span = current_expr.span() + quote_span;
                let base = Box::new(current_expr);
                current_expr = Expr::Object(ObjectLiteral{ span, base, quote_span, fields });
            } else {
                break;
            }
        }

        trace!("parsing postfix finished, get retval: {:?}", current_expr);
        Ok(current_expr)
    }

    pub fn maybe_unary_expr(&self) -> bool {
        matches!(self.current, Token::Sep(sep) if sep.kind(SeparatorKind::Unary))
    }

    // unary_expr = { unary_operator } postfix_expr
    pub fn parse_unary_expr(&mut self) -> Result<Expr, Unexpected> {
        
        let mut op_spans = Vec::new();
        loop {
            match self.try_expect_sep_kind(SeparatorKind::Unary) {
                Some((sep, sep_span)) => op_spans.push((sep, sep_span)),
                None => {
                    let base = self.parse_postfix_expr()?;
                    return Ok(op_spans.into_iter().rev().fold(base, |base, (op, span)| { 
                        Expr::Unary(UnaryExpr{ span: span + base.span(), base: Box::new(base), op, op_span: span }) 
                    }));
                }
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

        fn parse_unary_expr_wrapper(cx: &mut Parser) -> Result<Expr, Unexpected> {
            cx.parse_unary_expr()
        }
        fn check_relational_expr(cx: &mut Parser, expr: &Expr) {
            if let Expr::Binary(BinaryExpr{ op: Separator::Gt, op_span: gt_span, left_expr, .. }) = expr {
                if let Expr::Binary(BinaryExpr{ op: Separator::Lt, op_span: lt_span, .. }) = left_expr.as_ref() {
                    cx.emit(strings::MaybeGeneric).span(*lt_span).span(*gt_span).help(strings::MaybeGenericHelp);
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
                            current_expr = Expr::Binary(BinaryExpr{
                                span: current_expr.span() + right_expr.span(),
                                left_expr: Box::new(current_expr),
                                op, 
                                op_span, 
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
    pub fn parse_range_expr(&mut self) -> Result<Expr, Unexpected> {
        match self.try_expect_sep(Separator::DotDot) {
            Some(range_op_span) => {
                if self.maybe_expr() {
                    let expr = self.parse_binary_expr()?;
                    Ok(Expr::RangeRight(RangeRightExpr{ span: range_op_span + expr.span(), expr: Box::new(expr) }))
                } else {
                    Ok(Expr::RangeFull(RangeFullExpr{ span: range_op_span }))
                }
            }
            None => {
                let left_expr = self.parse_binary_expr()?;
                if let Some(op_span) = self.try_expect_sep(Separator::DotDot) {
                    if self.maybe_expr() {
                        let right_expr = self.parse_binary_expr()?;
                        let span = left_expr.span() + right_expr.span();
                        Ok(Expr::RangeBoth(RangeBothExpr{ span, op_span, left_expr: Box::new(left_expr), right_expr: Box::new(right_expr) }))
                    } else {
                        Ok(Expr::RangeLeft(RangeLeftExpr{ span: left_expr.span() + op_span, expr: Box::new(left_expr) }))
                    }
                } else {
                    Ok(left_expr)
                }
            }
        }
    }
}
