#![allow(dead_code)] // leave it here
///! fff-lang
///!
///! syntax/parse_sess, to be ISyntaxItem::parse's parameter

use std::cell::Cell;

use codemap::Span;
use codemap::SymbolID;
use codemap::SymbolCollection;
use message::Message;
use message::MessageCollection;
use lexical::Token;
use lexical::TokenStream;
use lexical::SeperatorKind;
use lexical::Keyword;

pub type ParseResult<T> = Result<T, ()>;

pub struct ParseSession<'tokens, 'msgs, 'syms> {
    tokens: &'tokens TokenStream,
    messages: &'msgs mut MessageCollection,
    pub symbols: &'syms mut SymbolCollection,
    current_index: Cell<usize>,
    pub tk: &'tokens Token,
    pub next_tk: &'tokens Token,
    pub nextnext_tk: &'tokens Token,
    pub pos: Span,
    pub next_pos: Span,
    pub nextnext_pos: Span,
}
impl<'a, 'b, 'c> ParseSession<'a, 'b, 'c> {

    pub fn new(tokens: &'a TokenStream, messages: &'b mut MessageCollection, symbols: &'c mut SymbolCollection) -> ParseSession<'a, 'b, 'c> {
        ParseSession{ 
            tokens, 
            messages, 
            symbols,
            current_index: Cell::new(0),
            tk: tokens.nth_token(0),
            next_tk: tokens.nth_token(1),
            nextnext_tk: tokens.nth_token(2),
            pos: tokens.nth_span(0),
            next_pos: tokens.nth_span(1),
            nextnext_pos: tokens.nth_span(2),
        }
    }

    pub fn move_next(&mut self) { 
        self.current_index.set(self.current_index.get() + 1);
        self.tk = self.tokens.nth_token(self.current_index.get());
        self.next_tk = self.tokens.nth_token(self.current_index.get() + 1);
        self.nextnext_tk = self.tokens.nth_token(self.current_index.get() + 2);
        self.pos = self.tokens.nth_span(self.current_index.get());
        self.next_pos = self.tokens.nth_span(self.current_index.get() + 1);
        self.nextnext_pos = self.tokens.nth_span(self.current_index.get() + 2);
    }
    pub fn move_next2(&mut self) {
        self.current_index.set(self.current_index.get() + 2);
        self.tk = self.tokens.nth_token(self.current_index.get());
        self.next_tk = self.tokens.nth_token(self.current_index.get() + 1);
        self.nextnext_tk = self.tokens.nth_token(self.current_index.get() + 2);
        self.pos = self.tokens.nth_span(self.current_index.get());
        self.next_pos = self.tokens.nth_span(self.current_index.get() + 1);
        self.nextnext_pos = self.tokens.nth_span(self.current_index.get() + 2);
    }

    // for test compatibility
    pub fn get_current_index(&self) -> usize { self.current_index.get() }

    /// Check current index is keyword, if so, move next and Ok(keyword_strpos),
    /// if not, push unexpect and Err(())
    /// use like `let kw_strpos = sess.expect_keyword(Keyword::In)?;`
    pub fn expect_keyword(&mut self, expect_kw: Keyword) -> Result<Span, ()> {

        let current_index = self.current_index.get();
        self.move_next();
        match (self.tokens.nth_token(current_index), self.tokens.nth_span(current_index)) {
            (&Token::Keyword(ref actual_kw), ref kw_strpos) if actual_kw == &expect_kw => Ok(*kw_strpos),
            (&Token::Keyword(_), _) => self.push_unexpect::<Span>(&format!("{:?}", expect_kw)),
            _ => self.push_unexpect::<Span>(&format!("{:?}", expect_kw)),
        }
    }
    /// Check current index is seperator, if so, move next and Ok(seperator_strpos),
    /// if not, push unexpect and Err(())
    /// use like `let sep_strpos = sess.expect_sep(SeperatorKind::Comma)?;`
    pub fn expect_sep(&mut self, expect_sep: SeperatorKind) -> Result<Span, ()> {

        let current_index = self.current_index.get();
        self.move_next();
        match (self.tokens.nth_token(current_index), self.tokens.nth_span(current_index)) {
            (&Token::Sep(ref actual_sep), ref sep_strpos) if actual_sep == &expect_sep => Ok(*sep_strpos),
            (&Token::Sep(_), _) => self.push_unexpect::<Span>(&format!("{}", expect_sep)),
            _ => self.push_unexpect::<Span>(&format!("{}", expect_sep)),
        }
    }
    /// Check current index is identifier, if so, move next and Ok((owned_ident_name, ident_strpos)),
    /// if not, push unexpect and Err(())
    /// use like `let (ident_name, ident_strpos) = sess.expect_ident()?;`
    pub fn expect_ident(&mut self) -> Result<(SymbolID, Span), ()> {

        let current_index = self.current_index.get();
        self.move_next();
        match (self.tokens.nth_token(current_index), self.tokens.nth_span(current_index)) {
            (&Token::Ident(ref ident), ref ident_strpos) => Ok((*ident, *ident_strpos)),
            _ => self.push_unexpect::<(SymbolID, Span)>("identifier"),
        }
    }
    // TODO: more info unexpect desc string
    /// Check current index is identifier or accept keywords, 
    /// used for some where accept underscore and this
    /// if so, move next and Ok((owned_ident_name, ident_strpos)),
    /// if not, push unexpect and Err(())
    /// use like `let (ident_name, ident_strpos) = sess.expect_ident_or(vec![Keyword::This, Keyword::Underscore])?;`
    pub fn expect_ident_or(&mut self, accept_keywords: Vec<Keyword>) -> Result<(SymbolID, Span), ()> {

        let current_index = self.current_index.get();
        self.move_next();
        match (self.tokens.nth_token(current_index), self.tokens.nth_span(current_index)) {
            (&Token::Ident(ref ident), ref ident_strpos) => Ok((ident.clone(), *ident_strpos)),
            (&Token::Keyword(ref actual_kw), ref kw_strpos) => {
                if accept_keywords.iter().any(|accept_kw| actual_kw == accept_kw) {
                    Ok((self.symbols.intern(format!("{:?}", actual_kw)), *kw_strpos))
                } else {
                    self.push_unexpect::<(SymbolID, Span)>("identifier")
                }
            }
            _ => self.push_unexpect::<(SymbolID, Span)>("identifier"),
        }
    }
    // TODO: more info unexpect desc string
    /// Check current index is identifier or meet the predict
    /// used for some where (typeuse simple) accept more keywords
    /// if so, move next and Ok((owned_ident_name, ident_strpos)),
    /// if not, push unexpect and Err(())
    /// use like `let (ident_name, ident_strpos) = sess.expect_ident_or_if(|kw| kw.is_prim_type())?;`
    pub fn expect_ident_or_if<F>(&mut self, keyword_predict: F) -> Result<(SymbolID, Span), ()> where F: Fn(&Keyword) -> bool {
        
        let current_index = self.current_index.get();
        self.move_next();
        match (self.tokens.nth_token(current_index), self.tokens.nth_span(current_index)) {
            (&Token::Ident(ref ident), ref ident_strpos) => 
                Ok((ident.clone(), *ident_strpos)),
            (&Token::Keyword(ref actual_kw), ref kw_strpos) if keyword_predict(actual_kw) => 
                Ok((self.symbols.intern(format!("{:?}", actual_kw)), *kw_strpos)),
            _ => 
                self.push_unexpect::<(SymbolID, Span)>("identifier"),
        }
    }

    pub fn push_message(&mut self, message: Message) { self.messages.push(message); }
    pub fn push_unexpect<T>(&mut self, expect_desc: &str) -> ParseResult<T> {

        self.messages.push(Message::with_help("Unexpect symbol".to_owned(), 
            vec![(self.tokens.nth_span(self.current_index.get()), format!("Meet {:?}", self.tokens.nth_token(self.current_index.get())))],
            vec![format!("Expect {}", expect_desc)]
        ));

        return Err(());
    }
}

#[cfg(test)] #[test]
fn parse_sess_usage() {

    let tokens = TokenStream::with_test_str("1 2 3 4 5 6 7 8 9 0");
    let mut messages = MessageCollection::new();
    let mut symbols = SymbolCollection::new();
    let mut sess = ParseSession::new(&tokens, &mut messages, &mut symbols);

    match (sess.tk, sess.next_tk) {
        (&Token::Lit(ref _lit1), &Token::Lit(ref _lit2)) => {
            sess.move_next();
            println!("{:?}", sess.pos);
            let _ : ParseResult<i32> = sess.push_unexpect("abc, def");
        }
        _ => (),
    }
}
