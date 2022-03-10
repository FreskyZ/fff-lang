///! lexical: the lexical parser

use crate::source::{Span, SourceChars, FileSystem, EOF};
use crate::diagnostics::{Message, MessageCollection, strings};

#[cfg(test)]
mod tests;
mod token;
mod token_buf;
mod v1lexer;
mod unicode;

mod literal {
    pub mod char;
    pub mod escape;
    pub mod numeric;
    pub mod string;
    pub mod raw_string;
}

use unicode::CharExt;
use literal::numeric::parse_numeric_literal;
use v1lexer::{V1Token, V1Lexer};
use token_buf::{ILexer, BufLexer, ParseSession};
pub use token::{Separator, SeparatorKind, Keyword, KeywordKind};
pub use token::{Numeric, StringLiteralType, Token, TokenFormat};

pub struct TokenStream {
    items: Vec<(Token, Span)>,
    eof_token: (Token, Span),
}
impl TokenStream {
    
    /// main module driver
    pub fn new<F>(chars: SourceChars<F>, messages: &mut MessageCollection) -> TokenStream where F: FileSystem {

        let mut sess = ParseSession::new(messages);
        let mut v2lexer = V2Lexer::new(chars);
        let mut items = Vec::new();
        let eof_span: Span;
        loop {
            match v2lexer.next(&mut sess) {
                (Token::EOF, span) => { eof_span = span; break; }
                (v2, span) => items.push((v2, span)),
            }
        }

        v2lexer.v1.lexer.v0.lexer.0.finish();
        TokenStream { items, eof_token: (Token::EOF, eof_span) }
    }
    pub fn nth_token(&self, idx: usize) -> &Token {
        if idx >= self.items.len() { &self.eof_token.0 } else { &self.items[idx].0 }
    }
    pub fn nth_span(&self, idx: usize) -> Span { 
        if idx >= self.items.len() { self.eof_token.1 } else { self.items[idx].1 }
    }

    // pub fn with_test_str(src: &str) -> TokenStream { TokenStream::with_test_input(src, None).0 }
    // pub fn with_test_input(src: &str, syms: Option<SymbolCollection>) -> (TokenStream, Rc<SourceCode>, MessageCollection, SymbolCollection) {
    //     let mut msgs = MessageCollection::new();
    //     let mut syms = syms.unwrap_or_default();
    //     let source = Rc::new(make_node!(0, src));
    //     let retval = TokenStream::new(source.as_ref(), &mut msgs, &mut syms);
    //     return (retval, source, msgs, syms);
    // }
}

impl Default for Token { fn default() -> Token { Token::EOF } }

fn check_confusable(c: char, span: Span, messages: &mut MessageCollection) -> char {
    match unicode::check_confusable(c) {
        Some((unicode_ch, unicode_name, ascii_ch, ascii_name)) => {
            messages.push(Message::with_help_by_str(strings::UnexpectedNonASCIIChar, vec![
                (span, ""), 
            ], vec![
                &format!("Did you mean `{}`({}) by `{}`({})?", ascii_ch, ascii_name, unicode_ch, unicode_name),
            ]));
            ascii_ch
        }
        None => c,
    }
}

pub struct V2Lexer<'chs, F> {
    pub(super) v1: BufLexer<V1Lexer<'chs, F>, V1Token, F>,
}
impl<'chs, F> ILexer<'chs, F, Token> for V2Lexer<'chs, F> where F: FileSystem {

    fn new(source: SourceChars<'chs, F>) -> V2Lexer<'chs, F> {
        V2Lexer { 
            v1: BufLexer::new(source),
        }
    }

    // input stringliteral or otherchar without comment, output identifier and numeric literal
    fn next(&mut self, sess: &mut ParseSession) -> (Token, Span) {

        #[cfg(feature = "trace_v2_parse")] macro_rules! trace { ($($arg:tt)*) => ({ print!("[V2Next: {}] ", line!()); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_v2_parse"))] macro_rules! trace { ($($arg:tt)*) => () }

        macro_rules! ident_to_v2 { ($ident_value: expr, $ident_pos: expr) => ({
            match Keyword::parse(&$ident_value) { 
                Some(Keyword::True) => Token::Bool(true),
                Some(Keyword::False) => Token::Bool(false),
                Some(other_keyword) => {
                    if other_keyword.kind(KeywordKind::Reserved) {
                        sess.messages.push(Message::new(
                            format!("{}: {:?}", strings::UseReservedKeyword, other_keyword), 
                            vec![($ident_pos, String::new())]
                        ));
                    }
                    Token::Keyword(other_keyword)
                }
                None => Token::Ident(self.v1.lexer.v0.lexer.0.intern(&$ident_value)),
            }
        }) }
        macro_rules! num_lit_to_v2 { ($num_lit_value: expr, $num_lit_span: expr) => ({
            let (num_lit_val, pos) = parse_numeric_literal($num_lit_value, $num_lit_span, sess.messages);
            (Token::Num(num_lit_val.unwrap_or(Numeric::I32(0))), pos)
        }) }

        #[cfg_attr(test, derive(Debug))]
        enum State {
            Nothing,
            InIdent(String, Span),
            InLabel(String, Span),
            InNumLit(String, Span),
        }

        let mut state = State::Nothing;
        loop {
            self.v1.move_next(sess);
            let v15 = match self.v1.current_with_preview2() {
                (&V1Token::StringLiteral(ref value), pos, _2, _3, _4, _5) => {
                    return (Token::Str(*value, StringLiteralType::Normal), pos);
                }
                (&V1Token::RawStringLiteral(ref value), pos, _2, _3, _4, _5) => {
                    return (Token::Str(*value, StringLiteralType::Raw), pos);
                }
                (&V1Token::CharLiteral(ref value), pos, _2, _3, _4, _5) => {
                    return (Token::Char(*value), pos);
                }
                (&V1Token::EOF, eof_pos, _2, _3, _4, _5) => {
                    // because if last token is ident, it will not finish because eof return early here
                    // Update, 17/3/25
                    //     it used be like this: `if eofed { return v2::eof } else { eofed = true, v15 = ' ' }` 
                    //     After add mutliple file support to codemap, here is a bug
                    //     because these codes assume that after EOF and call next again you will still receive a EOF, 
                    //     at that time you can return the true EOF, but now after EOF is other char or normal tokens
                    //     after last EOF is EOFs and after EOFs is EOFs, so EOF is not returned because of this
                    //     So, because not support preview2, preview nextch and return properly in InIdent, not here
                    return (Token::EOF, eof_pos);
                }
                (&V1Token::Other(ch), span, &V1Token::Other(next_ch), next_span, &V1Token::Other(nextnext_ch), nextnext_span) => {
                    let ch = check_confusable(ch, span, sess.messages); // not need check next_ch and nextnext_ch because they will be checked in next loops
                    (ch, span, next_ch, next_span, nextnext_ch, nextnext_span)
                }
                (&V1Token::Other(ch), span, &V1Token::Other(next_ch), next_span, _4, nextnext_span) => {
                    let ch = check_confusable(ch, span, sess.messages);
                    (ch, span, next_ch, next_span, ' ', nextnext_span)
                }
                (&V1Token::Other(ch), span, &V1Token::EOF, eof_span, _4, nextnext_span) => {
                    let ch = check_confusable(ch, span, sess.messages);
                    (ch, span, EOF, eof_span, ' ', nextnext_span)
                } 
                (&V1Token::Other(ch), span, _2, next_span, _4, nextnext_span) => { 
                    let ch = check_confusable(ch, span, sess.messages);
                    (ch, span, ' ', next_span, ' ', nextnext_span)
                }
            };

            match (state, v15) {
                (State::Nothing, (ch, span, EOF, _3, _4, _5)) => {
                    if ch.is_id_start() {
                        let mut value = String::new();
                        value.push(ch);
                        return (ident_to_v2!(value, span), span);
                    } else if ch.is_numeric_start() {
                        let mut value = String::new();
                        value.push(ch);
                        return num_lit_to_v2!(value, span);
                    } else if ch.is_label_start() {
                        return (Token::Label(self.v1.lexer.v0.lexer.0.intern("")), span); // simple '@' is allowed, use @? to represent empty
                    } else {
                        match Separator::parse1(ch) {
                            Some(separator) => return (Token::Sep(separator), span),
                            None => state = State::Nothing,
                        }
                    }
                }
                (State::Nothing, (ch, span, next_ch, next_span, nextnext_ch, nextnext_span)) => {
                    if ch.is_id_start() {
                        let mut value = String::new();
                        value.push(ch);
                        if !next_ch.is_id_continue() {             // TODO future: understand why it is here to make the last case pass
                            return (ident_to_v2!(value, span), span);
                        }
                        state = State::InIdent(value, span);
                    } else if ch.is_numeric_start() {
                        let mut value = String::new();
                        value.push(ch);
                        state = State::InNumLit(value, span);
                    } else if ch.is_label_start() {
                        if !next_ch.is_label_continue() {                // 17/5/8: TODO: same question as before, why this is needed
                            return (Token::Label(self.v1.lexer.v0.lexer.0.intern("")), span);
                        }
                        state = State::InLabel(String::new(), span);
                    } else {
                        match Separator::parse3(ch, next_ch, nextnext_ch) { // the try_from3 will check 3, if not, check 2, if not, check 1
                            Some((separator, 1)) => {
                                trace!("ch is {:?} at {:?}, result is {:?}", ch, span, separator);
                                return (Token::Sep(separator), span);
                            }
                            Some((separator, 2)) => {
                                trace!("ch is {:?} at {:?}, next_ch is {:?}, result is {:?}", ch, span, next_ch, separator);
                                self.v1.prepare_skip1();
                                return (Token::Sep(separator), span + next_span);
                            }
                            Some((separator, 3)) => {
                                trace!("ch is {:?} at {:?}, next_ch is {:?}, nextnext_ch is {:?}, result is {:?}", ch, span, next_ch, nextnext_ch, separator);
                                self.v1.prepare_skip1();
                                self.v1.prepare_skip1();
                                return (Token::Sep(separator), span + nextnext_span);
                            }
                            _ => state = State::Nothing,
                        }
                    }
                } 
                (State::InIdent(mut value, mut ident_span), (ch, span, next_ch, _4, _5, _6)) => {
                    if !ch.is_id_continue() {
                        return (ident_to_v2!(value, ident_span), ident_span);
                    } else if !next_ch.is_id_continue() {
                        value.push(ch); 
                        ident_span = ident_span + span;
                        return (ident_to_v2!(value, ident_span), ident_span);
                    } else {
                        value.push(ch);
                        ident_span = ident_span + span;
                        state = State::InIdent(value, ident_span);
                    }
                }
                (State::InLabel(mut value, mut label_span), (ch, span, next_ch, _4, _5, _6)) => {
                    if !ch.is_label_continue() {
                        return (Token::Label(self.v1.lexer.v0.lexer.0.intern(&value)), label_span);
                    } else if !next_ch.is_label_continue() {
                        value.push(ch);
                        label_span = label_span + span;
                        return (Token::Label(self.v1.lexer.v0.lexer.0.intern(&value)), label_span);
                    } else {
                        value.push(ch);
                        label_span = label_span + span;
                        state = State::InLabel(value, label_span);
                    }
                }
                (State::InNumLit(mut value, mut num_lit_span), (ch, span, next_ch, _4, _5, _6)) => {

                    if (ch == '.' && next_ch == '.')                        // for 1..2
                        || (ch == '.' && next_ch.is_id_start())     // for 1.to_string()
                        || !ch.is_numeric_continue() {                       // normal end
                        self.v1.prepare_dummy1();
                        return num_lit_to_v2!(value, num_lit_span);
                    } else if !next_ch.is_numeric_continue() {
                        value.push(ch);
                        num_lit_span = num_lit_span + span;
                        return num_lit_to_v2!(value, num_lit_span);
                    } else {
                        value.push(ch);
                        num_lit_span = num_lit_span + span;
                        state = State::InNumLit(value, num_lit_span);
                    }
                }
            }
        }
    }
}
