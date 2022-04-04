use super::*;

impl<'ecx, 'scx> Parser<'ecx, 'scx> {

    // statement and item (module level item) parsers

    pub fn parse_stmt(&mut self) -> Result<Statement, Unexpected> {
        if self.maybe_type_def() {
            Ok(Statement::Type(self.parse_type_def()?))
        } else if self.maybe_enum_def() {
            Ok(Statement::Enum(self.parse_enum_def()?))
        } else if self.maybe_fn_def() {
            Ok(Statement::Fn(self.parse_fn_def()?))
        } else if self.maybe_block_stmt() {
            Ok(Statement::Block(self.parse_block_stmt()?))
        } else if self.maybe_break_stmt() {
            Ok(Statement::Break(self.parse_break_stmt()?))
        } else if self.maybe_continue_stmt() {
            Ok(Statement::Continue(self.parse_continue_stmt()?))
        } else if self.maybe_expr_stmt() {
            Ok(self.parse_expr_stmt()?)
        } else if self.maybe_for_stmt() {
            Ok(Statement::For(self.parse_for_stmt()?))
        } else if self.maybe_if_stmt() {
            Ok(Statement::If(self.parse_if_stmt()?))
        } else if self.maybe_loop_stmt() {
            Ok(Statement::Loop(self.parse_loop_stmt()?))
        } else if self.maybe_ret_stmt() {
            Ok(Statement::Return(self.parse_ret_stmt()?))
        } else if self.maybe_var_decl() {
            Ok(Statement::VarDecl(self.parse_var_decl()?))
        } else if self.maybe_while_stmt() {
            Ok(Statement::While(self.parse_while_stmt()?))
        } else {
            self.push_unexpect("type, enum, fn, {, break, continue, for, if, loop, return, var, const, while, .., !, ~, &, <, ::, ident, [, (")
        }
    }

    pub fn parse_item(&mut self) -> Result<Item, Unexpected> {
        if self.maybe_type_def() {
            Ok(Item::Type(self.parse_type_def()?))
        } else if self.maybe_enum_def() {
            Ok(Item::Enum(self.parse_enum_def()?))
        } else if self.maybe_fn_def() {
            Ok(Item::Fn(self.parse_fn_def()?))
        } else if self.maybe_block_stmt() {
            Ok(Item::Block(self.parse_block_stmt()?))
        } else if self.maybe_expr_stmt() {
            Ok(match self.parse_expr_stmt()? {
                Statement::AssignExpr(a) => Item::AssignExpr(a),
                Statement::SimpleExpr(s) => Item::SimpleExpr(s),
                _ => unreachable!(),
            })
        } else if self.maybe_for_stmt() {
            Ok(Item::For(self.parse_for_stmt()?))
        } else if self.maybe_if_stmt() {
            Ok(Item::If(self.parse_if_stmt()?))
        } else if self.maybe_loop_stmt() {
            Ok(Item::Loop(self.parse_loop_stmt()?))
        } else if self.maybe_var_decl() {
            Ok(Item::VarDecl(self.parse_var_decl()?))
        } else if self.maybe_while_stmt() {
            Ok(Item::While(self.parse_while_stmt()?))
        } else if self.maybe_use_stmt() {
            Ok(Item::Use(self.parse_use_stmt()?))
        } else if self.maybe_module_stmt() {
            Ok(Item::Import(self.parse_module_stmt()?))
        } else {
            self.push_unexpect("type, enum, fn, {, for, if, loop, return, var, const, while, use, module, .., !, ~, &, <, ::, ident, [, (")
        }
    }
    
    // label_def = label ':'
    // label can be specified before for stmt, loop stmt, while stmt and block stmt
    // return result<option because it's always optional
    // it is private because it's not used outside and it's included by their tests
    fn parse_label(&mut self) -> Result<Option<LabelDef>, Unexpected> {
        if let Token::Label(id) = self.current {
            let label_span = self.move_next();
            // TODO allow if colon missing, note that maybe_for_stmt, maybe_loop_stmt and maybe_while_stmt also need change
            let colon_span = self.expect_sep(Separator::Colon)?;
            Ok(Some(LabelDef::new(id, label_span + colon_span)))
        } else {
            Ok(None)
        }
    }

    // block = '{' { statement } '}'
    // it is private because it's not used outside and it's included by their tests
    fn parse_block(&mut self) -> Result<Block, Unexpected> {

        let starting_span = self.expect_sep(Separator::LeftBrace)?;
        let mut items = Vec::new();
        loop {
            if let Some(ending_span) = self.try_expect_sep(Separator::RightBrace) {
                return Ok(Block::new(starting_span + ending_span, items));
            }
            items.push(self.parse_stmt()?);
        }
    }

    pub fn maybe_block_stmt(&self) -> bool {
        matches!((&self.current, &self.peek2), (Token::Label(_), Token::Sep(Separator::LeftBrace)) | (Token::Sep(Separator::LeftBrace), _))
    }

    // block-stmt = [ label-def ] block
    // block-stmt for explicit block definition in block and allow block label
    pub fn parse_block_stmt(&mut self) -> Result<BlockStatement, Unexpected> {
    
        let name = self.parse_label()?;
        let body = self.parse_block()?;
        let all_span = name.as_ref().map(|n| n.all_span).unwrap_or(body.all_span) + body.all_span;
        Ok(BlockStatement{ all_span, name, body })
    }

    pub fn maybe_break_stmt(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Break)) 
    }
    
    // break_stmt = 'break' [ label ] ';'
    pub fn parse_break_stmt(&mut self) -> Result<BreakStatement, Unexpected> {

        let starting_span = self.expect_keyword(Keyword::Break)?;

        if let Some(label) = self.try_expect_label() {
            let semicolon_span = self.expect_sep(Separator::SemiColon)?;
            Ok(BreakStatement{ all_span: starting_span + semicolon_span, target: Some(label) })
        } else { 
            let semicolon_span = self.expect_sep(Separator::SemiColon)?;
            Ok(BreakStatement{ all_span: starting_span + semicolon_span, target: None })
        }
    }

    pub fn maybe_continue_stmt(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Continue)) 
    }

    // continue_stmt = 'continue' [ label ] ';'
    pub fn parse_continue_stmt(&mut self) -> Result<ContinueStatement, Unexpected> {

        let starting_span = self.expect_keyword(Keyword::Continue)?;

        if let Some(label) = self.try_expect_label() {
            let semicolon_span = self.expect_sep(Separator::SemiColon)?;
            Ok(ContinueStatement{ all_span: starting_span + semicolon_span, target: Some(label) })
        } else { 
            let semicolon_span = self.expect_sep(Separator::SemiColon)?;
            Ok(ContinueStatement{ all_span: starting_span + semicolon_span, target: None })
        }
    }

    pub fn maybe_enum_def(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Enum))
    }

    // enum_def = 'enum' ident [ ':' primitive_type ] '{' { ident [ '=' expr ] ',' } '}'
    pub fn parse_enum_def(&mut self) -> Result<EnumDef, Unexpected> {

        let enum_span = self.expect_keyword(Keyword::Enum)?;
        let (enum_name, enum_name_span) = self.expect_ident()?;
        let base_type = self.try_expect_sep(Separator::Colon).map(|_| self.parse_primitive_type()).transpose()?;
        let left_brace_span = self.expect_sep(Separator::LeftBrace)?;

        let mut variants = Vec::new();
        let right_brace_span = if let Some(right_brace_span) = self.try_expect_sep(Separator::RightBrace) {
            right_brace_span
        } else {
            loop {
                let (variant_name, variant_name_span) = self.expect_ident()?;
                let init_value = self.try_expect_sep(Separator::Eq).map(|_| self.parse_expr()).transpose()?;
                let variant_all_span = variant_name_span + init_value.as_ref().map(|e| e.get_all_span()).unwrap_or(variant_name_span);
                variants.push(EnumVariant{ name: variant_name, name_span: variant_name_span, value: init_value, all_span: variant_all_span });

                if let Some((right_brace_span, _)) = self.try_expect_closing_bracket(Separator::RightBrace) {
                    break right_brace_span;
                } else {
                    self.expect_sep(Separator::Comma)?;
                }
            }
        };

        let quote_span = left_brace_span + right_brace_span;
        let all_span = enum_span + right_brace_span;
        Ok(EnumDef{ name: enum_name, name_span: enum_name_span, base_type, quote_span, variants, all_span })
    }

    pub fn maybe_expr_stmt(&self) -> bool {
        self.maybe_expr()
    }

    // expr_stmt = expr { assign_ops expr } ';'
    pub fn parse_expr_stmt(&mut self) -> Result<Statement, Unexpected> {

        let left_expr = self.parse_expr()?;
        let starting_span = left_expr.get_all_span();

        if let Some(semicolon_span) = self.try_expect_sep(Separator::SemiColon) {
            Ok(Statement::SimpleExpr(SimpleExprStatement::new(starting_span + semicolon_span, left_expr)))
        } else if let Some((assign_op, assign_op_span)) = self.try_expect_sep_kind(SeparatorKind::Assign) {
            let right_expr = self.parse_expr()?;
            let semicolon_span = self.expect_sep(Separator::SemiColon)?;
            Ok(Statement::AssignExpr(
                AssignExprStatement::new(starting_span + semicolon_span, assign_op, assign_op_span, left_expr, right_expr)))
        } else {
            self.push_unexpect("assign operators, semicolon")
        }
    }

    pub fn maybe_fn_def(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Fn))
    }

    // fn-def = 'fn' identifier '(' [ identifier ':' type-use { ',' identifier ':' type-use [ ',' ] } ] ')' [ '->' type-use ] block
    // TODO: fn name (and type name) should be a "GenericName" where type parameters only allow identifier
    pub fn parse_fn_def(&mut self) -> Result<FnDef, Unexpected> {

        let fn_span = self.expect_keyword(Keyword::Fn)?;
        let (fn_name, fn_name_span) = self.expect_ident()?;
        let mut params_paren_span = self.expect_sep(Separator::LeftParen)?;

        let mut params = Vec::new();
        loop {
            if let Some((right_paren_span, skipped_comma)) = self.try_expect_closing_bracket(Separator::RightParen) {
                params_paren_span += right_paren_span;
                if skipped_comma && params.is_empty() {
                    self.emit("Single comma in function definition argument list")
                        .detail(fn_name_span, "function definition here")
                        .detail(params_paren_span, "param list here");
                }
                break;
            } else if let Some(_comma_span) = self.try_expect_sep(Separator::Comma) {
                continue;
            }

            let (param_name, param_span) = self.expect_ident_or_keywords(&[Keyword::Underscore, Keyword::This, Keyword::Self_])?;
            let _ = self.expect_sep(Separator::Colon)?;
            let decltype = self.parse_type_ref()?;
            params.push(FnParam::new(param_name, param_span, decltype));
        }

        let ret_type = self.try_expect_seps(&[Separator::Arrow, Separator::Colon]).map(|(sep, span)| {
            if sep == Separator::Colon {
                self.emit(strings::FunctionReturnTypeShouldUseArrow).detail(span, strings::FunctionReturnTypeExpectArrowMeetColon);
            }
            self.parse_type_ref()
        }).transpose()?;
        let body = self.parse_block()?;

        Ok(FnDef::new(fn_span + body.all_span, fn_name, fn_name_span, params_paren_span, params, ret_type, body))
    }

    pub fn maybe_for_stmt(&self) -> bool {
        matches!((&self.current, &self.peek2), (Token::Label(_), Token::Keyword(Keyword::For)) | (Token::Keyword(Keyword::For), _))
    }

    // for_stmt = [ label_def ] 'for' identifier 'in' expr block
    // TODO: add else for break, like python
    pub fn parse_for_stmt(&mut self) -> Result<ForStatement, Unexpected> {

        let loop_name = self.parse_label()?;
        let for_span = self.expect_keyword(Keyword::For)?;

        // Accept _ as iter_name, _ do not declare iter var
        let (iter_name, iter_span) = self.expect_ident_or_keywords(&[Keyword::Underscore])?; 
        self.expect_keyword(Keyword::In)?;

        self.no_object_literals.push(true);
        let iter_expr = self.parse_expr()?;
        self.no_object_literals.pop();
        let body = self.parse_block()?;
        
        let all_span = loop_name.as_ref().map(|n| n.all_span).unwrap_or(for_span) + body.all_span;
        Ok(ForStatement{ loop_name, for_span, iter_name, iter_span, iter_expr, body, all_span })
    }

    pub fn maybe_if_stmt(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::If)) 
    }

    // if_stmt = 'if' expr block { 'else' 'if' expr block } [ 'else' block ]
    pub fn parse_if_stmt(&mut self) -> Result<IfStatement, Unexpected> {

        let mut all_span = self.expect_keyword(Keyword::If)?;

        self.no_object_literals.push(true);
        let if_expr = self.parse_expr()?;
        self.no_object_literals.pop();
        let if_body = self.parse_block()?;
        all_span += if_body.all_span;
        let if_clause = IfClause{ all_span, condition: if_expr, body: if_body };

        let mut elseif_clauses = Vec::new();
        let mut else_clause = None;
        while let Some(else_span) = self.try_expect_keyword(Keyword::Else) {
            if let Some(if_span) = self.try_expect_keyword(Keyword::If) {
                let elseif_span = else_span + if_span;
                self.no_object_literals.push(true);
                let elseif_expr = self.parse_expr()?;
                self.no_object_literals.pop();
                let elseif_body = self.parse_block()?;
                all_span += elseif_body.all_span;
                elseif_clauses.push(IfClause{ all_span: elseif_span + elseif_body.all_span, condition: elseif_expr, body: elseif_body });
            } else {
                // 16/12/1, we lost TWO `+1`s for current_length here ... fixed
                // 17/5/6: When there is match Block::parse(tokens, messages, index + current_length), etc.
                // There was a bug fix here, now no more current_length handling!
                // 17/6/21: a new physical structure update makes it much more simple
                // 17/7/28: a new small update of parse_cx makes things even more simple
                let else_body = self.parse_block()?;
                all_span += else_body.all_span;
                else_clause = Some(ElseClause{ all_span: else_span + else_body.all_span, body: else_body });
            }
        }

        Ok(IfStatement{ all_span, if_clause, elseif_clauses, else_clause })
    }

    pub fn maybe_loop_stmt(&self) -> bool {
        matches!((&self.current, &self.peek2), (Token::Label(_), Token::Keyword(Keyword::Loop)) | (Token::Keyword(Keyword::Loop), _))
    }

    // loop_stmt = [ label_def ] 'loop' block
    // NOTE: no else for break here because if control flow come to else it is always breaked
    pub fn parse_loop_stmt(&mut self) -> Result<LoopStatement, Unexpected> {

        let name = self.parse_label()?;
        let loop_span = self.expect_keyword(Keyword::Loop)?;
        let body = self.parse_block()?;
        let all_span = name.as_ref().map(|n| n.all_span).unwrap_or(loop_span) + body.all_span;
        Ok(LoopStatement{ all_span, name, loop_span, body })
    }

    pub fn maybe_module_stmt(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Module)) 
    }

    // module_stmt = 'module' identifier [ str_lit ] ';'
    pub fn parse_module_stmt(&mut self) -> Result<ModuleStatement, Unexpected> {

        let starting_span = self.expect_keyword(Keyword::Module)?;
        let (name, name_span) = self.expect_ident()?;

        let path = self.try_expect_str_lit(); 
        let semicolon_span = self.expect_sep(Separator::SemiColon)?;
        let all_span = starting_span + semicolon_span;

        Ok(ModuleStatement{ all_span, name, name_span, path })
    }

    pub fn maybe_ret_stmt(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Return))
    }

    // ret_stmt = 'return' [ expr ] ';'
    pub fn parse_ret_stmt(&mut self) -> Result<ReturnStatement, Unexpected> {

        let starting_span = self.expect_keyword(Keyword::Return)?;
        if let Some(semicolon_span) = self.try_expect_sep(Separator::SemiColon) {
            // 17/6/17: you forgot move_next here!
            // but I have never write some test cases like following something after ret stmt
            // so the bug is not propagated to be discovered
            // 17/7/28: now new features added to parse_cx and move_next is to be removed, no current position management bug any more!
            Ok(ReturnStatement::new_unit(starting_span + semicolon_span))
        } else {
            let expr = self.parse_expr()?;
            let semicolon_span = self.expect_sep(Separator::SemiColon)?;
            Ok(ReturnStatement::new_expr(starting_span + semicolon_span, expr))
        }
    }

    pub fn maybe_type_def(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Type)) 
    }

    // type_def = 'type' (identifier | keyword_primitive_type)  '{' [ type_field_def { ',' type_field_def } [ ',' ] ] '}'
    // type_field_def = identifier ':' type_ref
    pub fn parse_type_def(&mut self) -> Result<TypeDef, Unexpected> {

        let starting_span = self.expect_keyword(Keyword::Type)?;
        let (name, name_span) = self.expect_ident_or_keyword_kind(KeywordKind::Primitive)?;
        let _left_brace_span = self.expect_sep(Separator::LeftBrace)?;

        let mut fields = Vec::new();
        let right_brace_span = loop { 
            if let Some(right_brace_span) = self.try_expect_sep(Separator::RightBrace) {
                break right_brace_span;     // rustc 1.19 stablize break-expr
            }

            let (field_name, field_name_span) = self.expect_ident()?;
            let colon_span = self.expect_sep(Separator::Colon)?;
            let field_type = self.parse_type_ref()?;
            fields.push(if let Some(comma_span) = self.try_expect_sep(Separator::Comma) {
                TypeFieldDef{ all_span: field_name_span + comma_span, name: field_name, name_span: field_name_span, colon_span, r#type: field_type }
            } else {
                TypeFieldDef{ all_span: field_name_span + field_type.get_all_span(), name: field_name, name_span: field_name_span, colon_span, r#type: field_type }
            });
        };

        Ok(TypeDef{ all_span: starting_span + right_brace_span, name, name_span, fields })
    }
    
    pub fn maybe_use_stmt(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Use)) 
    }

    // use_stmt = 'use' name [ 'as' identifier ] ';'
    pub fn parse_use_stmt(&mut self) -> Result<UseStatement, Unexpected> {

        let starting_span = self.expect_keyword(Keyword::Use)?;
        let name = self.parse_name()?;

        let alias = self.try_expect_keyword(Keyword::As).map(|_| self.expect_ident()).transpose()?;
        let semicolon_span = self.expect_sep(Separator::SemiColon)?;
        let all_span = starting_span + semicolon_span;

        Ok(UseStatement{ all_span, name, alias })
    }

    pub fn maybe_var_decl(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Const | Keyword::Var)) 
    }

    // const-decl = 'const' identifier [ ':' type-use ] [ '=' expr ] ';'
    // var-decl = 'var' identifier [ ':' type-use ] [ '=' expr ] ';'
    pub fn parse_var_decl(&mut self) -> Result<VarDeclStatement, Unexpected> {
        
        let (starting_kw, starting_span) = self.expect_keywords(&[Keyword::Const, Keyword::Var])?;
        let is_const = match starting_kw { Keyword::Const => true, Keyword::Var => false, _ => unreachable!() };

        let (name, name_span) = self.expect_ident_or_keywords(&[Keyword::Underscore])?;
        let r#type = self.try_expect_sep(Separator::Colon).map(|_| self.parse_type_ref()).transpose()?;
        let init_expr = self.try_expect_sep(Separator::Eq).map(|_| self.parse_expr()).transpose()?;
        if r#type.is_none() && init_expr.is_none() {
            self.emit("require type annotation")
                .detail(name_span, "variable declaration here")
                .help("cannot infer type without both type annotation and initialization expression");
        }
        let ending_span = self.expect_sep(Separator::SemiColon)?;

        Ok(VarDeclStatement{ all_span: starting_span + ending_span, is_const, name, name_span, r#type, init_expr })
    }

    pub fn maybe_while_stmt(&self) -> bool {
        matches!((&self.current, &self.peek2), (Token::Label(_), Token::Keyword(Keyword::While)) | (Token::Keyword(Keyword::While), _))
    }

    // while-stmt = [ label-def ] 'while' expr block
    // TODO: add else for break, like python
    pub fn parse_while_stmt(&mut self) -> Result<WhileStatement, Unexpected> {
        
        let name = self.parse_label()?;
        let while_span = self.expect_keyword(Keyword::While)?;
        self.no_object_literals.push(true);
        let expr = self.parse_expr()?;
        self.no_object_literals.pop();
        let body = self.parse_block()?;
        let all_span = name.as_ref().map(|n| n.all_span).unwrap_or(while_span) + body.all_span;
        Ok(WhileStatement{ name, while_span, loop_expr: expr, body, all_span })
    }
}
