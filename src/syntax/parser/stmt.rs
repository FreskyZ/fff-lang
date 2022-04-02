use super::*;

impl<'ecx, 'scx> ParseContext<'ecx, 'scx> {
    
    pub fn maybe_block_stmt(&self) -> bool {
        matches!((&self.current, &self.peek2), (Token::Label(_), Token::Sep(Separator::LeftBrace)) | (Token::Sep(Separator::LeftBrace), _))
    }

    // block-stmt = [ label-def ] block
    // block-stmt for explicit block definition in block and allow block label
    pub fn parse_block_stmt(&mut self) -> Result<BlockStatement, Unexpected> {
    
        let name = self.maybe_label().then_try(|| self.expect::<LabelDef>())?;
        let body = self.expect::<Block>()?;
        let all_span = name.as_ref().map(|n| n.all_span).unwrap_or(body.all_span) + body.all_span;
        Ok(BlockStatement{ all_span, name, body })
    }
}
