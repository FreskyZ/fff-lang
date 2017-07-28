///! fff-lang
///!
///! syntax/parse_sess, to be ISyntaxItem::parse's parameter

use codemap::Span;
use codemap::SymbolID;
use codemap::SymbolCollection;
use message::Message;
use message::MessageCollection;
use lexical::Token;
use lexical::TokenStream;
use lexical::Seperator;
use lexical::Keyword;
use lexical::LitValue;

pub type ParseResult<T> = Result<T, ()>;

pub struct ParseSession<'tokens, 'msgs, 'syms> {
    tokens: &'tokens TokenStream,
    messages: &'msgs mut MessageCollection,
    pub symbols: &'syms mut SymbolCollection,
    current_index: usize,
    current_tokens: [&'tokens Token; 3],
}
#[allow(dead_code)] // helper methods may not be used
impl<'a, 'b, 'c> ParseSession<'a, 'b, 'c> {

    pub fn new(tokens: &'a TokenStream, messages: &'b mut MessageCollection, symbols: &'c mut SymbolCollection) -> ParseSession<'a, 'b, 'c> {
        ParseSession{ 
            tokens, messages, symbols,
            current_index: 0,
            current_tokens: [tokens.nth_token(0), tokens.nth_token(1), tokens.nth_token(2)],
        }
    }

    fn move_next(&mut self) { 
        self.current_index += 1;
        self.current_tokens = [
            self.tokens.nth_token(self.current_index), 
            self.tokens.nth_token(self.current_index + 1), 
            self.tokens.nth_token(self.current_index + 2)
        ];
    }
    fn move_next2(&mut self) {
        self.current_index += 2;
        self.current_tokens = [
            self.tokens.nth_token(self.current_index), 
            self.tokens.nth_token(self.current_index + 1), 
            self.tokens.nth_token(self.current_index + 2)
        ];
    }

    /// Check current token is specified keyword
    /// 
    /// if so, move next and Ok(keyword_span),
    /// if not, push unexpect and Err(())
    ///
    /// example `let kw_span = sess.expect_keyword(Keyword::In)?;`
    pub fn expect_keyword(&mut self, expected_kw: Keyword) -> Result<Span, ()> {

        let current_index = self.current_index;
        self.move_next();
        match (self.tokens.nth_token(current_index), self.tokens.nth_span(current_index)) {
            (&Token::Keyword(ref actual_kw), ref kw_span) if actual_kw == &expected_kw => Ok(*kw_span),
            _ => self.push_unexpect(&format!("{:?}", expected_kw)),
        }
    }

    /// Check current token is specified seperator
    ///
    /// if so, move next and Ok(sep_span),
    /// if not, push unexpect and Err(())
    ///
    /// example `let sep_span = sess.expect_sep(Seperator::Comma)?;`
    pub fn expect_sep(&mut self, expected_sep: Seperator) -> Result<Span, ()> {

        let current_index = self.current_index;
        self.move_next();
        match (self.tokens.nth_token(current_index), self.tokens.nth_span(current_index)) {
            (&Token::Sep(ref actual_sep), ref sep_span) if actual_sep == &expected_sep => Ok(*sep_span),
            _ => self.push_unexpect(&format!("{:?}", expected_sep)),
        }
    }

    /// Check current token is a literal
    ///
    /// if so, move next and Ok((lit_value, lit_span)),
    /// if not, push unexpect and Err(())
    ///
    /// example `let (lit, lit_span) = sess.expect_lit()?;`
    pub fn expect_lit(&mut self) -> Result<(LitValue, Span), ()> {

        let current_index = self.current_index;
        self.move_next();
        match (self.tokens.nth_token(current_index), self.tokens.nth_span(current_index)) {
            (&Token::Lit(ref lit), ref lit_span) => Ok((*lit, *lit_span)),
            _ => self.push_unexpect("literal"),
        }
    }

    /// Check current token is one of the specified seperators
    ///
    /// if so, move next and Ok((sep, sep_span)),
    /// if not, push unexpect and Err(())
    ///
    /// example `let (sep, sep_span) = sess.expect_seps(&[Seperator::LeftBracket, Seperator::LeftBrace])?;`
    pub fn expect_seps<'e, T: IntoIterator<Item = &'e Seperator>>(&mut self, expected_seps: T) -> Result<(Seperator, Span), ()> {
        
        let current_index = self.current_index;
        self.move_next();
        match (self.tokens.nth_token(current_index), self.tokens.nth_span(current_index)) {
            (&Token::Sep(ref actual_sep), ref sep_span) => {
                if expected_seps.into_iter().any(|expected_sep| expected_sep == actual_sep) {
                    Ok((*actual_sep, *sep_span))
                } else {
                    self.push_unexpect("some seperators")           // FIXME: format expected_seps
                }
            }
            _ => self.push_unexpect("some seperators"),             // FIXME: format expected_seps
        }
    }

    /// Check current token is one of the specified keywords
    ///
    /// if so, move next and Ok((kw, kw_span)), 
    /// if not, push unexpect and Err(())
    ///
    /// example `let (kw, kw_span) = sess.expect_keywords(&[Const, Var])?;`
    pub fn expect_keywords<'e, T: IntoIterator<Item = &'e Keyword>>(&mut self, expected_keywords: T) -> Result<(Keyword, Span), ()> {
        
        let current_index = self.current_index;
        self.move_next();
        match (self.tokens.nth_token(current_index), self.tokens.nth_span(current_index)) {
            (&Token::Keyword(ref actual_kw), ref kw_span) => {
                if expected_keywords.into_iter().any(|expected_kw| expected_kw == actual_kw) {
                    Ok((*actual_kw, *kw_span))
                } else {
                    self.push_unexpect("some keywords")           // FIXME: format expected_keywords
                }
            }
            _ => self.push_unexpect("some keywords"),             // FIXME: format expected_keywords
        }
    }

    /// Check current token is an identifier
    ///
    /// if so, move next and Ok((symbol_id, ident_span)),
    /// if not, push unexpect and Err(())
    ///
    /// example `let (ident_id, ident_span) = sess.expect_ident()?;`
    pub fn expect_ident(&mut self) -> Result<(SymbolID, Span), ()> {

        let current_index = self.current_index;
        self.move_next();
        match (self.tokens.nth_token(current_index), self.tokens.nth_span(current_index)) {
            (&Token::Ident(ref ident), ref ident_strpos) => Ok((*ident, *ident_strpos)),
            _ => self.push_unexpect::<(SymbolID, Span)>("identifier"),
        }
    }

    /// Check current token is a label
    /// 
    /// if so, move next and Some((symid, sym_span)), 
    /// if not, no move next and None
    ///
    /// example `if let Some((symid, label_span)) = sess.try_expect_label() { ... }`
    pub fn try_expect_label(&mut self) -> Option<(SymbolID, Span)> {
        
        let current_index = self.current_index;
        match (self.tokens.nth_token(current_index), self.tokens.nth_span(current_index)) {
            (&Token::Label(ref id), ref label_span) => { self.move_next(); Some((*id, *label_span)) },
            _ => None,
        }
    }

    /// Check current token is identifier or acceptable keywords
    /// 
    /// if so, move next and Ok((symbol_id, ident_span)),
    /// if not, push unexpect and Err(())
    ///
    /// example `let (symbol_id, ident_span) = sess.expect_ident_or([Keyword::This, Keyword::Underscore])?;`
    pub fn expect_ident_or<T: IntoIterator<Item = Keyword>>(&mut self, acceptable_keywords: T) -> Result<(SymbolID, Span), ()> {

        let current_index = self.current_index;
        self.move_next();
        match (self.tokens.nth_token(current_index), self.tokens.nth_span(current_index)) {
            (&Token::Ident(ref ident), ref ident_span) => Ok((*ident, *ident_span)),
            (&Token::Keyword(ref actual_kw), ref kw_span) => {
                if acceptable_keywords.into_iter().any(|accept_kw| actual_kw == &accept_kw) {
                    Ok((self.symbols.intern(format!("{:?}", actual_kw)), *kw_span))
                } else {
                    self.push_unexpect("identifier")  // FIXME: add keywords desc here
                }
            }
            _ => self.push_unexpect("identifier"),    // FIXME: add keywords desc here
        }
    }
    
    /// Check current token is identifier or meet the predict
    /// 
    /// if so, move next and Ok((symbol_id, ident_span)),
    /// if not, push unexpect and Err(())
    ///
    /// example `let (symbold_id, ident_span) = sess.expect_ident_or_if(|kw| kw.is_primitive_type())?;`
    pub fn expect_ident_or_if<P: Fn(&Keyword) -> bool>(&mut self, predict: P) -> Result<(SymbolID, Span), ()> {
        
        let current_index = self.current_index;
        self.move_next();
        match (self.tokens.nth_token(current_index), self.tokens.nth_span(current_index)) {
            (&Token::Ident(ref ident), ref ident_span) => Ok((*ident, *ident_span)),
            (&Token::Keyword(ref actual_kw), ref kw_span) if predict(actual_kw) => Ok((self.symbols.intern(format!("{:?}", actual_kw)), *kw_span)),
            _ => self.push_unexpect("identifier"),  // TODO: may require Keywords::meet_predict(p: P) -> Vec<Keyword> or custom description
        }
    }

    /// Check current token is specified seperator
    ///
    /// if so, move next and Some(sep_span), 
    /// if not, no move next and None
    ///
    /// example `if let Some(sep_span) = sess.try_expect_sep(Seperator::Comma) { ... }`
    pub fn try_expect_sep(&mut self, expected_sep: Seperator) -> Option<Span> {

        let current_index = self.current_index;
        match (self.tokens.nth_token(current_index), self.tokens.nth_span(current_index)) {
            (&Token::Sep(ref actual_sep), ref sep_span) if actual_sep == &expected_sep => { self.move_next(); Some(*sep_span) }
            _ => None,
        }
    }

    /// Check current token is specified keyword
    ///
    /// if so, move next and Some(kw_span), 
    /// if not, no move next and None
    ///
    /// example `if let Some(kw_span) == sess.try_expect_keyword(Keyword::If) { ... }`
    pub fn try_expect_keyword(&mut self, expected_keyword: Keyword) -> Option<Span> {

        let current_index = self.current_index;
        match (self.tokens.nth_token(current_index), self.tokens.nth_span(current_index)) {
            (&Token::Keyword(ref actual_kw), ref kw_span) if actual_kw == &expected_keyword => { self.move_next(); Some(*kw_span) },
            _ => None,
        }
    }
    
    /// Check current and next token is specified seperators
    ///
    /// if so, move next and Some((sep1_span, sep2_span)),
    /// if not, no move next and None
    ///
    /// example `if let Some((comma_span, ending_span)) = sess.try_expect_2_spe(Seperator::Comma, Seperator::RParen) { ... }`
    pub fn try_expect_2_sep(&mut self, expected_sep1: Seperator, expected_sep2: Seperator) -> Option<(Span, Span)> {

        let current_index = self.current_index;
        match (self.tokens.nth_token(current_index), self.tokens.nth_span(current_index), 
            self.tokens.nth_token(current_index + 1), self.tokens.nth_span(current_index + 1)) {
            (&Token::Sep(ref sep1), sep1_span, &Token::Sep(ref sep2), sep2_span) 
                if sep1 == &expected_sep1 && sep2 == &expected_sep2 => { self.move_next2(); Some((sep1_span, sep2_span)) },
            _ => None,
        }
    }

    /// Check current token is of seperator category
    ///
    /// if so, move next and Some((sep, sep_span)), 
    /// if not, no move next and None
    ///
    /// example `if let Some((sep, sep_span)) = sess.try_expect_sep_cat(Unary) { ... }`
    pub fn try_expect_sep_cat(&mut self, expected_cat: u16) -> Option<(Seperator, Span)> {

        let current_index = self.current_index;
        match (self.tokens.nth_token(current_index), self.tokens.nth_span(current_index)) {
            (&Token::Sep(ref sep), ref sep_span) if sep.is_category(expected_cat) => { self.move_next(); Some((*sep, *sep_span)) },
            _ => None,
        }
    }

    pub fn push_message(&mut self, message: Message) { self.messages.push(message); }
    pub fn push_unexpect<T>(&mut self, expect_desc: &str) -> ParseResult<T> {

        self.messages.push(Message::with_help("Unexpect symbol".to_owned(), 
            vec![(self.tokens.nth_span(self.current_index), format!("Meet {:?}", self.tokens.nth_token(self.current_index)))],
            vec![format!("Expect {}", expect_desc)]
        ));

        return Err(());
    }

    pub fn current_tokens(&self) -> &[&Token] {
        &self.current_tokens
    }
}

pub trait ISyntaxGrammar {
    fn matches_first(tokens: &[&Token]) -> bool;
}
pub trait ISyntaxParse {
    type Output;
    
    fn parse(sess: &mut ParseSession) -> ParseResult<Self::Output>;

    // check is_first_final, if pass, parse, return Ok(Some(T)) or Err(()), else return None
    fn try_parse(sess: &mut ParseSession) -> ParseResult<Option<Self::Output>> where Self: ISyntaxGrammar {
        if Self::matches_first(sess.current_tokens()) { Ok(Some(Self::parse(sess)?)) } else { Ok(None) }
    }
}

// TODO: consider better unexpected descriptions
