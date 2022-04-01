///! syntax::member_access
///! member_access = expr '.' member
///! member = num_lit | name
///!
///! name should start with ident,
///! that is, type_as_segment or global (start with ::) is not allowed and first segment must be normal

use super::prelude::*;

impl Parser for MemberAccessExpr {
    type Output = Self;

    fn matches3(current: &Token, peek: &Token, _peek2: &Token) -> bool {
        matches!((current, peek), (Token::Sep(Separator::Dot), Token::Num(_) | Token::Ident(_))) 
    }

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
