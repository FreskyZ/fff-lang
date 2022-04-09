use super::*;

// statement and item (module level item) parsers
impl<'ecx, 'scx> Parser<'ecx, 'scx> {

    pub fn parse_stmt(&mut self) -> Result<Statement, Unexpected> {
        if self.maybe_struct_def() {
            Ok(Statement::Struct(self.parse_struct_def()?))
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
        } else if self.maybe_use_stmt() {
            Ok(Statement::Use(self.parse_use_stmt()?))
        } else {
            self.push_unexpect("type, enum, fn, {, break, continue, for, if, loop, return, var, const, while, .., !, ~, &, <, ::, ident, [, (")
        }
    }

    pub fn parse_item(&mut self) -> Result<Item, Unexpected> {
        if self.maybe_struct_def() {
            Ok(Item::Struct(self.parse_struct_def()?))
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

    // generic_name = ident [ '<' ident { ',' ident } [ ',' ] '>' ]
    fn parse_generic_name(&mut self) -> Result<GenericName, Unexpected> {
        
        let base = self.expect_ident()?;
        let mut quote_span = Span::new(0, 0);
        let mut parameters = Vec::new();
        if let Some(lt_span) = self.try_expect_sep(Separator::Lt) {
            quote_span = lt_span + if let Some((gt_span, _)) = self.try_expect_closing_bracket(Separator::Gt) {
                self.emit(strings::EmptyGenericParameterList).span(lt_span + gt_span);
                gt_span
            } else {
                let parameter = self.expect_ident()?;
                parameters.push(GenericParameter{ span: parameter.span, name: parameter });
                loop {
                    if let Some((gt_span, _)) = self.try_expect_closing_bracket(Separator::Gt) {
                        break gt_span;
                    } else {
                        self.expect_sep(Separator::Comma)?;
                    }
                    let parameter = self.expect_ident()?;
                    parameters.push(GenericParameter{ span: parameter.span, name: parameter });
                }
            };
        }

        let span = if quote_span == Span::new(0, 0) { base.span } else { base.span + quote_span };
        Ok(GenericName{ span, base, quote_span, parameters })
    }
    
    // label_def = label ':'
    // label can be specified before for stmt, loop stmt, while stmt and block stmt
    // return result<option because it's always optional
    // it is private because it's not used outside and it's included by their tests
    fn parse_label(&mut self) -> Result<Option<IdSpan>, Unexpected> {
        if let Token::Label(id) = self.current {
            let label_span = self.move_next();
            // TODO allow if colon missing, note that maybe_for_stmt, maybe_loop_stmt and maybe_while_stmt also need change
            self.expect_sep(Separator::Colon)?;
            Ok(Some(IdSpan::new(id, label_span)))
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
                return Ok(Block{ span: starting_span + ending_span, items });
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
    
        let label = self.parse_label()?;
        let body = self.parse_block()?;
        let span = label.as_ref().map(|n| n.span).unwrap_or(body.span) + body.span;
        Ok(BlockStatement{ span, label, body })
    }

    pub fn maybe_break_stmt(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Break)) 
    }
    
    // break_stmt = 'break' [ label ] ';'
    pub fn parse_break_stmt(&mut self) -> Result<BreakStatement, Unexpected> {

        let starting_span = self.expect_keyword(Keyword::Break)?;

        if let Some(label) = self.try_expect_label() {
            let semicolon_span = self.expect_sep(Separator::SemiColon)?;
            Ok(BreakStatement{ span: starting_span + semicolon_span, label: Some(label) })
        } else { 
            let semicolon_span = self.expect_sep(Separator::SemiColon)?;
            Ok(BreakStatement{ span: starting_span + semicolon_span, label: None })
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
            Ok(ContinueStatement{ span: starting_span + semicolon_span, label: Some(label) })
        } else { 
            let semicolon_span = self.expect_sep(Separator::SemiColon)?;
            Ok(ContinueStatement{ span: starting_span + semicolon_span, label: None })
        }
    }

    pub fn maybe_enum_def(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Enum))
    }

    // enum_def = 'enum' ident [ ':' primitive_type ] '{' { ident [ '=' expr ] ',' } '}'
    pub fn parse_enum_def(&mut self) -> Result<EnumDef, Unexpected> {

        let enum_span = self.expect_keyword(Keyword::Enum)?;
        let enum_name = self.expect_ident()?;
        let base_type = self.try_expect_sep(Separator::Colon).map(|_| self.parse_primitive_type()).transpose()?;
        let left_brace_span = self.expect_sep(Separator::LeftBrace)?;

        let mut variants = Vec::new();
        let right_brace_span = if let Some(right_brace_span) = self.try_expect_sep(Separator::RightBrace) {
            right_brace_span
        } else {
            loop {
                let variant_name = self.expect_ident()?;
                let init_value = self.try_expect_sep(Separator::Eq).map(|_| self.parse_expr()).transpose()?;
                let variant_all_span = variant_name.span + init_value.as_ref().map(|e| e.span()).unwrap_or(variant_name.span);
                variants.push(EnumDefVariant{ name: variant_name, value: init_value, span: variant_all_span });

                if let Some((right_brace_span, _)) = self.try_expect_closing_bracket(Separator::RightBrace) {
                    break right_brace_span;
                } else {
                    self.expect_sep(Separator::Comma)?;
                }
            }
        };

        let quote_span = left_brace_span + right_brace_span;
        let span = enum_span + right_brace_span;
        Ok(EnumDef{ name: enum_name, base_type, quote_span, variants, span })
    }

    pub fn maybe_expr_stmt(&self) -> bool {
        self.maybe_expr()
    }

    // expr_stmt = expr { assign_ops expr } ';'
    pub fn parse_expr_stmt(&mut self) -> Result<Statement, Unexpected> {

        let left_expr = self.parse_expr()?;
        let starting_span = left_expr.span();

        if let Some(semicolon_span) = self.try_expect_sep(Separator::SemiColon) {
            Ok(Statement::SimpleExpr(SimpleExprStatement{ span: starting_span + semicolon_span, expr: left_expr }))
        } else if let Some((op, op_span)) = self.try_expect_sep_kind(SeparatorKind::Assign) {
            let right_expr = self.parse_expr()?;
            let semicolon_span = self.expect_sep(Separator::SemiColon)?;
            Ok(Statement::AssignExpr(
                AssignExprStatement{ span: starting_span + semicolon_span, op, op_span, left: left_expr, right: right_expr }))
        } else {
            self.push_unexpect("assign operators, semicolon")
        }
    }

    pub fn maybe_fn_def(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Fn))
    }

    // fn-def = 'fn' generic_name '(' [ identifier ':' type-use { ',' identifier ':' type-use [ ',' ] } ] ')' [ '->' type-use ] [ 'where' { where_clause ',' } ] [ block ]
    // where_clause = ident ':' type_ref { '+' type_ref }
    pub fn parse_fn_def(&mut self) -> Result<FnDef, Unexpected> {

        let fn_span = self.expect_keyword(Keyword::Fn)?;
        let fn_name = self.parse_generic_name()?;
        let mut quote_span = self.expect_sep(Separator::LeftParen)?;

        let mut parameters = Vec::new();
        loop {
            if let Some((right_paren_span, skipped_comma)) = self.try_expect_closing_bracket(Separator::RightParen) {
                quote_span += right_paren_span;
                if skipped_comma && parameters.is_empty() {
                    self.emit("Single comma in function definition argument list")
                        .detail(fn_name.span, "function definition here")
                        .detail(quote_span, "param list here");
                }
                break;
            } else if let Some(_comma_span) = self.try_expect_sep(Separator::Comma) {
                continue;
            }

            let parameter_name = self.expect_ident_or_keywords(&[Keyword::Underscore, Keyword::This, Keyword::Self_])?;
            self.expect_sep(Separator::Colon)?;
            let r#type = self.parse_type_ref()?;
            parameters.push(FnDefParameter{ span: parameter_name.span + r#type.span(), name: parameter_name, r#type });
        }

        let ret_type = self.try_expect_seps(&[Separator::Arrow, Separator::Colon]).map(|(sep, span)| {
            if sep == Separator::Colon {
                self.emit(strings::FunctionReturnTypeShouldUseArrow).detail(span, strings::FunctionReturnTypeExpectArrowMeetColon);
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
            WhereClause{ span: name.span + constraints.last().unwrap().span(), name, constraints }
        }};}

        let mut wheres = Vec::new();
        if self.try_expect_keyword(Keyword::Where).is_some() {
            wheres.push(parse_where!());
            loop {
                // left brace/semicolon is not closing bracket, and this does not move forward
                if self.is_sep(Separator::LeftBrace) || self.is_sep(Separator::SemiColon) {
                    break;
                } else if matches!((&self.current, &self.peek), (Token::Sep(Separator::Comma), Token::Sep(Separator::LeftBrace | Separator::SemiColon))) {
                    self.move_next();
                    break;
                } else {
                    self.expect_sep(Separator::Comma)?;
                }
                wheres.push(parse_where!());
            }
        }

        let (ending_span, body) = if self.is_sep(Separator::LeftBrace) {
            let body = self.parse_block()?;
            (body.span, Some(body))
        } else {
            (self.expect_sep(Separator::SemiColon)?, None) 
        };
        Ok(FnDef{ span: fn_span + ending_span, name: fn_name, quote_span, parameters, ret_type, wheres, body })
    }

    pub fn maybe_for_stmt(&self) -> bool {
        matches!((&self.current, &self.peek2), (Token::Label(_), Token::Keyword(Keyword::For)) | (Token::Keyword(Keyword::For), _))
    }

    // for_stmt = [ label_def ] 'for' identifier 'in' expr block
    // TODO: add else for break, like python
    pub fn parse_for_stmt(&mut self) -> Result<ForStatement, Unexpected> {

        let label = self.parse_label()?;
        let for_span = self.expect_keyword(Keyword::For)?;

        // Accept _ as iter_name, _ do not declare iter var
        let iter_name = self.expect_ident_or_keywords(&[Keyword::Underscore])?; 
        self.expect_keyword(Keyword::In)?;

        let iter_expr = self.parse_expr_except_object_expr()?;
        let body = self.parse_block()?;
        
        let span = label.as_ref().map(|n| n.span).unwrap_or(for_span) + body.span;
        Ok(ForStatement{ label, iter_name, iter_expr, body, span })
    }

    pub fn maybe_if_stmt(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::If)) 
    }

    // if_stmt = 'if' expr block { 'else' 'if' expr block } [ 'else' block ]
    pub fn parse_if_stmt(&mut self) -> Result<IfStatement, Unexpected> {

        let mut all_span = self.expect_keyword(Keyword::If)?;

        let if_condition = self.parse_expr_except_object_expr()?;
        let if_body = self.parse_block()?;
        all_span += if_body.span;
        let if_clause = IfClause{ span: all_span, condition: if_condition, body: if_body };

        let mut elseif_clauses = Vec::new();
        let mut else_clause = None;
        while let Some(else_span) = self.try_expect_keyword(Keyword::Else) {
            if let Some(if_span) = self.try_expect_keyword(Keyword::If) {
                let elseif_span = else_span + if_span;
                let elseif_condition = self.parse_expr_except_object_expr()?;
                let elseif_body = self.parse_block()?;
                all_span += elseif_body.span;
                elseif_clauses.push(IfClause{ span: elseif_span + elseif_body.span, condition: elseif_condition, body: elseif_body });
            } else {
                // 16/12/1, we lost TWO `+1`s for current_length here ... fixed
                // 17/5/6: When there is match Block::parse(tokens, messages, index + current_length), etc.
                // There was a bug fix here, now no more current_length handling!
                // 17/6/21: a new physical structure update makes it much more simple
                // 17/7/28: a new small update of parse_cx makes things even more simple
                let else_body = self.parse_block()?;
                all_span += else_body.span;
                else_clause = Some(ElseClause{ span: else_span + else_body.span, body: else_body });
            }
        }

        Ok(IfStatement{ span: all_span, if_clause, elseif_clauses, else_clause })
    }

    pub fn maybe_loop_stmt(&self) -> bool {
        matches!((&self.current, &self.peek2), (Token::Label(_), Token::Keyword(Keyword::Loop)) | (Token::Keyword(Keyword::Loop), _))
    }

    // loop_stmt = [ label_def ] 'loop' block
    // NOTE: no else for break here because if control flow come to else it is always breaked
    pub fn parse_loop_stmt(&mut self) -> Result<LoopStatement, Unexpected> {

        let label = self.parse_label()?;
        let loop_span = self.expect_keyword(Keyword::Loop)?;
        let body = self.parse_block()?;
        let span = label.as_ref().map(|n| n.span).unwrap_or(loop_span) + body.span;
        Ok(LoopStatement{ span, label, body })
    }

    pub fn maybe_module_stmt(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Module)) 
    }

    // module_stmt = 'module' identifier [ str_lit ] ';'
    pub fn parse_module_stmt(&mut self) -> Result<ModuleStatement, Unexpected> {

        let starting_span = self.expect_keyword(Keyword::Module)?;
        let module_name = self.expect_ident()?;

        let path = self.try_expect_str_lit(); 
        let semicolon_span = self.expect_sep(Separator::SemiColon)?;
        let span = starting_span + semicolon_span;

        Ok(ModuleStatement{ span, name: module_name, path })
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
            Ok(ReturnStatement{ span: starting_span + semicolon_span, value: None })
        } else {
            let value = self.parse_expr()?;
            let semicolon_span = self.expect_sep(Separator::SemiColon)?;
            Ok(ReturnStatement{ span: starting_span + semicolon_span, value: Some(value) })
        }
    }

    pub fn maybe_struct_def(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Struct)) 
    }

    // struct_def = 'struct' generic_name  '{' [ field_def { ',' field_def } [ ',' ] ] '}'
    // field_def = identifier ':' type_ref
    pub fn parse_struct_def(&mut self) -> Result<StructDef, Unexpected> {

        let starting_span = self.expect_keyword(Keyword::Struct)?;
        let type_name = self.parse_generic_name()?;
        self.expect_sep(Separator::LeftBrace)?;

        let mut fields = Vec::new();
        macro_rules! parse_field {
            () => {{
                let field_name = self.expect_ident()?;
                let colon_span = self.expect_sep(Separator::Colon)?;
                let field_type = self.parse_type_ref()?;
                fields.push(FieldDef{ span: field_name.span + field_type.span(), name: field_name, colon_span, r#type: field_type });
            }}
        }

        let right_brace_span = if let Some((right_brace_span, _)) = self.try_expect_closing_bracket(Separator::RightBrace) {
            right_brace_span
        } else {
            parse_field!();
            loop {
                if let Some((right_brace_span, _)) = self.try_expect_closing_bracket(Separator::RightBrace) {
                    break right_brace_span;     // rustc 1.19 stablize break-expr
                } else {
                    self.expect_sep(Separator::Comma)?;
                }
                parse_field!();
            }
        };

        Ok(StructDef{ span: starting_span + right_brace_span, name: type_name, fields })
    }
    
    pub fn maybe_type_def(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Type))
    }

    // type_alias = 'type' generic_name [ '=' type_ref ] ';'
    pub fn parse_type_def(&mut self) -> Result<TypeDef, Unexpected> {
        
        let start_span = self.expect_keyword(Keyword::Type)?;
        let name = self.parse_generic_name()?;
        let from = self.try_expect_sep(Separator::Eq).map(|_| self.parse_type_ref()).transpose()?;
        let end_span = self.expect_sep(Separator::SemiColon)?;
        Ok(TypeDef{ span: start_span + end_span, name, from })
    }

    pub fn maybe_use_stmt(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Use)) 
    }

    // use_stmt = 'use' name [ 'as' identifier ] ';'
    pub fn parse_use_stmt(&mut self) -> Result<UseStatement, Unexpected> {

        let starting_span = self.expect_keyword(Keyword::Use)?;
        let path = self.parse_value_path()?;

        let alias = self.try_expect_keyword(Keyword::As).map(|_| self.expect_ident()).transpose()?;
        let semicolon_span = self.expect_sep(Separator::SemiColon)?;
        let span = starting_span + semicolon_span;

        Ok(UseStatement{ span, path, alias })
    }

    pub fn maybe_var_decl(&self) -> bool {
        matches!(self.current, Token::Keyword(Keyword::Const | Keyword::Var)) 
    }

    // const-decl = 'const' identifier [ ':' type-use ] [ '=' expr ] ';'
    // var-decl = 'var' identifier [ ':' type-use ] [ '=' expr ] ';'
    pub fn parse_var_decl(&mut self) -> Result<VarDeclStatement, Unexpected> {
        
        let (starting_kw, starting_span) = self.expect_keywords(&[Keyword::Const, Keyword::Var])?;
        let r#const = match starting_kw { Keyword::Const => true, Keyword::Var => false, _ => unreachable!() };

        let var_name = self.expect_ident_or_keywords(&[Keyword::Underscore])?;
        let r#type = self.try_expect_sep(Separator::Colon).map(|_| self.parse_type_ref()).transpose()?;
        let init_value = self.try_expect_sep(Separator::Eq).map(|_| self.parse_expr()).transpose()?;
        let ending_span = self.expect_sep(Separator::SemiColon)?;

        if r#type.is_none() && init_value.is_none() {
            self.emit("require type annotation")
                .detail(var_name.span, "variable declaration here")
                .help("cannot infer type without both type annotation and initialization expression");
        }

        Ok(VarDeclStatement{ span: starting_span + ending_span, r#const, name: var_name, r#type, init_value })
    }

    pub fn maybe_while_stmt(&self) -> bool {
        matches!((&self.current, &self.peek2), (Token::Label(_), Token::Keyword(Keyword::While)) | (Token::Keyword(Keyword::While), _))
    }

    // while-stmt = [ label-def ] 'while' expr block
    // TODO: add else for break, like python
    pub fn parse_while_stmt(&mut self) -> Result<WhileStatement, Unexpected> {
        
        let label = self.parse_label()?;
        let while_span = self.expect_keyword(Keyword::While)?;
        let condition = self.parse_expr_except_object_expr()?;
        let body = self.parse_block()?;
        let span = label.as_ref().map(|n| n.span).unwrap_or(while_span) + body.span;
        Ok(WhileStatement{ label, condition, body, span })
    }
}
