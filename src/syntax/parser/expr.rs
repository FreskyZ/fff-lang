use super::*;

// these functions are order logically
// "public" api parse_expr and maybe_expr
// common function parse_expr_list
// priority high to low: primary -> postfix -> unary -> binary -> range
// same priority inside primary: lit (inlined) -> name -> tuple -> array, this seems to be their occurance frequency order in my experience
// same priority inside postfix: member -> fn call -> index call -> object literal, this seems to be in occurance frequency too
impl<'ecx, 'scx> Parser<'ecx, 'scx> {

    // maybe_* functions: checks current token (may peek) to see if it matches syntax node starting
    // // was called Parser::matches, ISyntaxParse::matches, ISyntaxItemParse::is_first_final, IASTItem::is_first_final before
    // // considered loops_like_xxx, seems_to_be_xxx, and maybe_xxx is shortest and consist with token check methods is_xxx
    pub fn maybe_expr(&self) -> bool {
        self.is_lit()
        || self.maybe_path()
        || self.maybe_tuple_expr()
        || self.maybe_array_expr()
        || self.maybe_unary_expr()
        || matches!(self.current, Token::Sep(Separator::DotDot) | Token::Keyword(Keyword::This))
    }

    pub fn parse_expr(&mut self) -> Result<Expr, Unexpected> {
        self.allow_object_expr.push(true);
        let result = self.parse_range_expr();
        self.allow_object_expr.pop();
        result
    }

    // disable top level object exprs,
    // until end of this expr, or call parse_expr again inside call_expr, e.g. array def and tuple def
    pub fn parse_expr_except_object_expr(&mut self) -> Result<Expr, Unexpected> {
        self.allow_object_expr.push(false);
        let result = self.parse_range_expr();
        self.allow_object_expr.pop();
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

    fn parse_lit_expr(&mut self) -> Result<Expr, Unexpected> {
        
        let (value, span) = match self.current {
            Token::Bool(v) => Ok((LitValue::Bool(v), self.move_next())),
            Token::Char(v) => Ok((LitValue::Char(v), self.move_next())),
            Token::Num(v) => Ok((LitValue::Num(v), self.move_next())),
            Token::Str(v, _) => Ok((LitValue::Str(v), self.move_next())),
            _ => self.push_unexpect("literal"),
        }?;
        Ok(Expr::Lit(LitExpr{ value, span }))
    }

    fn maybe_tuple_expr(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::LeftParen)) 
    }

    // tuple_expr = '(' expr_list ')'
    // paren_expr = '(' expr ')'
    // unit_lit = '(' ')'
    pub fn parse_tuple_expr(&mut self) -> Result<Expr, Unexpected> {

        match self.parse_expr_list()? {
            ExprListParseResult::Empty(span) => {
                Ok(Expr::Lit(LitExpr{ value: LitValue::Unit, span }))
            }
            ExprListParseResult::SingleComma(span) => {
                self.emit(strings::UnexpectedSingleComma).detail(span, strings::TupleDefHere);
                Ok(Expr::Tuple(TupleExpr{ span, items: ExprList{ items: Vec::new() } }))
            }
            ExprListParseResult::Normal(span, exprlist) => {
                if exprlist.items.len() == 1 {
                    Ok(Expr::Paren(ParenExpr{ span, base: Box::new(exprlist.items.into_iter().last().unwrap()) }))
                } else {
                    Ok(Expr::Tuple(TupleExpr{ span, items: exprlist }))
                }
            }
            ExprListParseResult::EndWithComma(span, exprlist) => {
                Ok(Expr::Tuple(TupleExpr{ span, items: exprlist }))
            }
        }
    }

    fn maybe_array_expr(&self) -> bool {
        self.is_sep(Separator::LeftBracket)
    }

    // array_expr = '[' [ expr_list ] ']'
    pub fn parse_array_expr(&mut self) -> Result<Expr, Unexpected> {
        match self.parse_expr_list()? {
            ExprListParseResult::Empty(span) =>
                Ok(Expr::Array(ArrayExpr{ span, items: ExprList{ items: Vec::new() } })),
            ExprListParseResult::Normal(span, exprlist) 
            | ExprListParseResult::EndWithComma(span, exprlist) =>
                Ok(Expr::Array(ArrayExpr{ span, items: exprlist })),
            ExprListParseResult::SingleComma(span) => {
                self.emit(strings::UnexpectedSingleComma).detail(span, strings::ArrayDefHere);
                Ok(Expr::Array(ArrayExpr{ span, items: ExprList{ items: Vec::new() } }))
            }
        }
    }

    pub fn parse_primary_expr(&mut self) -> Result<Expr, Unexpected> {

        if self.is_lit() {
            return self.parse_lit_expr();
        } else if self.maybe_path() {
            return self.parse_value_path().map(Expr::Path);
        } else if self.maybe_tuple_expr() {
            return self.parse_tuple_expr();
        } else if self.maybe_array_expr() {
            return self.parse_array_expr();
        }

        let this = self.expect_ident_or_keywords(&[Keyword::This, Keyword::Self_])?;  // actually identifier is processed by Name, not here
        Ok(Expr::Path(Path{ span: this.span, segments: vec![PathSegment::Simple(this)] }))
    }

    fn maybe_member_expr(&self) -> bool {
        matches!((&self.current, &self.peek), (Token::Sep(Separator::Dot), Token::Ident(_))) 
    }

    // member_expr = primary_expr '.' ident [ ':' type_list ]
    // return dot span and member name, see parse_postfix_expr for the return type
    pub fn parse_member_expr(&mut self) -> Result<(Span, (IdSpan, Option<TypeList>)), Unexpected> {
        
        let dot_span = self.expect_sep(Separator::Dot)?;
        // // ? rust.await is really good design, but await is still currently reserved, put it here to indicate that it can be here
        let ident = self.expect_ident_or_keywords(&[Keyword::Await])?;
        let parameters = self.try_expect_sep(Separator::ColonColon).map(|_| self.parse_type_list()).transpose()?;
        Ok((dot_span, (ident, parameters)))
    }

    fn maybe_tuple_index_expr(&self) -> bool {
        matches!((&self.current, &self.peek), (Token::Sep(Separator::Dot), Token::Num(_)))
    }

    // tuple_member_expr = primary_expr '.' numeric
    // return dot span and tuple index and tuple index span, see parse_postfix_expr for the return type
    pub fn parse_tuple_index_expr(&mut self) -> Result<(Span, (Numeric, Span)), Unexpected> {
        
        let dot_span = self.expect_sep(Separator::Dot)?;
        let (numeric, numeric_span) = self.expect_numeric()?;
        if !matches!(numeric, Numeric::I32(_) /* && is unsuffixed and unprefixed */) {
            self.emit(strings::InvalidTupleIndex).span(numeric_span).help(strings::TupleIndexSyntaxHelp);
        }
        Ok((dot_span, (numeric, numeric_span)))
    }

    fn maybe_call_expr(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::LeftParen)) 
    }

    // call_expr = primary_expr '(' [ expr_list ] ')'
    // return quote span and expr list, see parse_postfix_expr for the return type
    pub fn parse_call_expr(&mut self) -> Result<(Span, ExprList), Unexpected> {

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

    fn maybe_array_index_expr(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::LeftBracket))
    }

    // index_call_expr = primary_expr '[' [ expr_list ] ']'
    // return quote span and expr list, see parse_postfix_expr for the return type
    // // was called postfix_expr::subscription, this one is shorter and similar to fn_call
    pub fn parse_array_index_expr(&mut self) -> Result<(Span, ExprList), Unexpected> {

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

    fn maybe_object_expr(&self) -> bool {
        matches!(self.current, Token::Sep(Separator::LeftBrace))
    }

    // object_literal = name '{' { ident ':' expr ',' } '}'
    // last comma may omit
    // return quote span and field list, see parse_postfix_expr for the return type
    pub fn parse_object_expr(&mut self) -> Result<(Span, Vec<ObjectExprField>), Unexpected> {
        
        let left_brace_span = self.expect_sep(Separator::LeftBrace)?;
        let mut fields = Vec::new();
        let right_brace_span = if let Some(right_brace_span) = self.try_expect_sep(Separator::RightBrace) {
            right_brace_span
        } else { 
            loop {
                let field_name = self.expect_ident()?;
                self.expect_sep(Separator::Colon)?;
                let value = self.parse_expr()?;
                fields.push(ObjectExprField{ span: field_name.span + value.span(), name: field_name, value });

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
            if self.maybe_member_expr() {
                let (op_span, (name, parameters)) = self.parse_member_expr()?;
                let span = current_expr.span() + parameters.as_ref().map(|p| p.span).unwrap_or(name.span);
                let base = Box::new(current_expr);
                current_expr = Expr::Member(MemberExpr{ span, base, op_span, name, parameters });
            } else if self.maybe_call_expr() {
                let (quote_span, parameters) = self.parse_call_expr()?;
                let span = current_expr.span() + quote_span;
                let base = Box::new(current_expr);
                current_expr = Expr::Call(CallExpr{ span, base, quote_span, parameters });
            } else if self.maybe_tuple_index_expr() {
                let (op_span, value) = self.parse_tuple_index_expr()?;
                let span = current_expr.span() + value.1;
                let base = Box::new(current_expr);
                current_expr = Expr::TupleIndex(TupleIndexExpr{ span, base, op_span, value });
            } else if self.maybe_array_index_expr() {
                let (quote_span, parameters) = self.parse_array_index_expr()?;
                let span = current_expr.span() + quote_span;
                let base = Box::new(current_expr);
                current_expr = Expr::ArrayIndex(ArrayIndexExpr{ span, base, quote_span, parameters });
            } else if matches!({
                // // I carefully checked that only parse_expr and parse_name is called outside this file,
                // // and found the actual intruder is unit test, this will still work when compiler_test
                #[cfg(not(test))]
                debug_assert!(!self.allow_object_expr.is_empty(), "allow_object_expr unexpectedly empty");
                &self.allow_object_expr
            }.last(), None | Some(true)) && matches!(current_expr, Expr::Path(_)) && self.maybe_object_expr() {
                let (quote_span, fields) = self.parse_object_expr()?;
                let span = current_expr.span() + quote_span;
                let base = Box::new(current_expr);
                current_expr = Expr::Object(ObjectExpr{ span, base, quote_span, fields });
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
            if let Expr::Binary(BinaryExpr{ op: Separator::Gt, op_span: gt_span, left, .. }) = expr {
                if let Expr::Binary(BinaryExpr{ op: Separator::Lt, op_span: lt_span, .. }) = left.as_ref() {
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
                                left: Box::new(current_expr),
                                op, 
                                op_span, 
                                right: Box::new(right_expr)
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
                    Ok(Expr::RangeRight(RangeRightExpr{ span: range_op_span + expr.span(), base: Box::new(expr) }))
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
                        Ok(Expr::RangeBoth(RangeBothExpr{ span, op_span, left: Box::new(left_expr), right: Box::new(right_expr) }))
                    } else {
                        Ok(Expr::RangeLeft(RangeLeftExpr{ span: left_expr.span() + op_span, base: Box::new(left_expr) }))
                    }
                } else {
                    Ok(left_expr)
                }
            }
        }
    }
}
