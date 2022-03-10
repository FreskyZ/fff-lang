///! lexical: the lexical parser

use std::cell::Cell;
use crate::source::{Span, SourceChars, FileSystem, EOF, IsId};
use crate::diagnostics::{Message, MessageCollection, strings};

#[cfg(test)]
mod tests;
mod token;
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
use literal::string::{StringLiteralParser, StringLiteralParserResult};
use literal::raw_string::{RawStringLiteralParser, RawStringLiteralParserResult};
use literal::char::{CharLiteralParser, CharLiteralParserResult};
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

struct ParseSession<'a> {
    pub messages: &'a mut MessageCollection,
}
impl<'a> ParseSession<'a> {
    pub fn new(messages: &'a mut MessageCollection) -> Self {
        ParseSession { messages }
    }
}

trait ILexer<'chs, F, TToken> {
    // 17/6/7: it used to be ILexer::new(chars, messages), but the messages is not used currently,
    // I think in some previous versions it has its usage, but I can't think up of one
    // It is removed now, if future needed, add messages back
    // 5 minutes later: the messages is usable! because every ILexer contains a BufLexer
    // BufLexer needs a messages to call 3 times of next in advance
    // 1 hour later: now BufLexer use its skips and dummys mechanism to not call next in new, then sess is not used again
    fn new(source: SourceChars<'chs, F>) -> Self;

    // init
    //     fn next(&mut self, messages: &mut MessageCollection) -> Option<TToken>;
    // <unknown-time>: position is out of token type
    //     fn next(&mut self, messages: &mut MessageCollection) -> Option<(TToken, StringPosition)>;
    // <unknown-time>: token type provide EOF hint by itself
    //     fn next(&mut self, messages: &mut MessageCollection) -> (TToken, StringPosition);
    // 17/6/7: second parameter changes to ParseSession
    fn next(&mut self, sess: &mut ParseSession) -> (TToken, Span);
}

// Buffed lexer
// Example
//
// type BufV0Lexer = BufLexer<V0Lexer, V0Token>;
// let mut bufv0 = ...
// loop {
//    bufv0.move_next();
//    match bufv0.current_with_preview2() {
//        ...
//    }
// }
struct BufLexer<TLexer, TToken, F> {
    lexer: TLexer,
    skips: Cell<i32>,
    dummys: Cell<i32>,
    current: (TToken, Span),
    next: (TToken, Span),
    nextnext: (TToken, Span),
    phantom: std::marker::PhantomData<F>,
}
// skips and dummys designment:
//
// the initial skips = 2, dummys = 0, because,
//     (here id's int value means index of TLexer::next return value, start from 0)
// explicit call to move_next, skips, dummys, current id,   next id, nextnext id,
//            after construct,     2,      0,    default,    default,    default,
//                 first call, i = 0,      0,    default,    default,          0,
//                             i = 1,      0,    default,          0,          1,
//                             i = 2,      0,          0,          1,          2,
//          first call return, i = 0,  -- you can get_current_with_p2 happily!
//                second call,     0,      0,          1,          2,          3,
//                        ...
// when skip is n
//              previous call,     0,      0,         id,     id + 1,     id + 2,
//          prepare_skip1 * n,     n,      0,         id,     id + 1,     id + 2,
//               current call, i = 0,      0,     id + 1,     id + 2,     id + 3,
//                        ...
//                 next calls, i = n,      0, id + n + 1, id + n + 2, id + n + 3,
//                  next call,     0,  -- as you expected, tokens[id + 1 .. id + n] is ignored, n tokens ignored
//                        ...
// when dummys is n
//              previous call,     0,      0,         id,     id + 1,     id + 2,
//          prepare_dummy * n,     0,      n,         id,     id + 1,     id + 2,
//                next call 1,     0,  n - 1,         id,     id + 1,     id + 2,
//                next call 2,     0,  n - 2,         id,     id + 1,     id + 2,
//                        ...
//                next call n,     0,      0,         id,     id + 1,     id + 2,   -- n calls ignored
//            next call n + 1,     0,      0,     id + 1,     id + 2,     id + 3
// review 17-07-22T16:06+8: it's kind of hard to understand but it not so hard and is good enough
#[allow(dead_code)] // prepare skip, dummy, current, current_p1, current_p2 maybe not called
impl<'chs, F, TLexer, TToken> BufLexer<TLexer, TToken, F>
where
    F: FileSystem,
    TToken: Default,
    TLexer: ILexer<'chs, F, TToken>,
{
    pub fn new(source: SourceChars<'chs, F>) -> BufLexer<TLexer, TToken, F> {
        BufLexer {
            lexer: TLexer::new(source),
            current: (TToken::default(), Span::new(0, 0)),
            next: (TToken::default(), Span::new(0, 0)),
            nextnext: (TToken::default(), Span::new(0, 0)),
            skips: Cell::new(2),
            dummys: Cell::new(0),
            phantom: std::marker::PhantomData,
        }
    }

    fn actual_move_next(&mut self, sess: &mut ParseSession) {
        // strange method for
        //     self.current = self.next;  // as self is mutable borrowed, cannot move self.next
        //     self.next = self.nextnext; // same
        unsafe {
            use std::ptr;
            ptr::swap(
                &mut self.current as *mut (TToken, Span),
                &mut self.next as *mut (TToken, Span),
            );
            ptr::swap(
                &mut self.next as *mut (TToken, Span),
                &mut self.nextnext as *mut (TToken, Span),
            );
        }
        self.nextnext = self.lexer.next(sess);
    }
    pub fn move_next(&mut self, sess: &mut ParseSession) {
        if self.dummys.get() != 0 {
            self.dummys.set(self.dummys.get() - 1);
            return;
        }
        for _ in 0..self.skips.get() + 1 {
            self.actual_move_next(sess);
        }
        self.skips.set(0);
    }
    /// this skip will perform at next time of move next
    pub fn prepare_skip1(&self) {
        self.skips.set(self.skips.get() + 1)
    }
    /// this dummy makes next more 1 move attempt to be dummy
    pub fn prepare_dummy1(&self) {
        self.dummys.set(self.dummys.get() + 1);
    }

    pub fn current(&self) -> (&TToken, Span) {
        (&self.current.0, self.current.1)
    }
    pub fn current_with_preview(&self) -> (&TToken, Span, &TToken, Span) {
        (&self.current.0, self.current.1, &self.next.0, self.next.1)
    }
    pub fn current_with_preview2(&self) -> (&TToken, Span, &TToken, Span, &TToken, Span) {
        (
            &self.current.0,
            self.current.1,
            &self.next.0,
            self.next.1,
            &self.nextnext.0,
            self.nextnext.1,
        )
    }

    pub fn current_with_state<TState>(
        &self,
        state: TState,
    ) -> (TState, &TToken, Span, &TToken, Span, &TToken, Span) {
        (
            state,
            &self.current.0,
            self.current.1,
            &self.next.0,
            self.next.1,
            &self.nextnext.0,
            self.nextnext.1,
        )
    }
}

// SourceCodeIter wrapper
struct V0Lexer<'a, F>(SourceChars<'a, F>);
impl<'a, F> ILexer<'a, F, char> for V0Lexer<'a, F> where F: FileSystem {
    fn new(source: SourceChars<'a, F>) -> Self { Self(source) }
    fn next(&mut self, _: &mut ParseSession) -> (char, Span) {
        let ret_val = self.0.next();
        (ret_val.0, ret_val.1.into())
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
enum V1Token {
    EOF,
    StringLiteral(IsId),
    RawStringLiteral(IsId),
    CharLiteral(char),
    Other(char),
}
impl Default for V1Token { fn default() -> V1Token { V1Token::EOF } }

/// lexical/v1, input v0
/// remove line comment
/// report block comment as OtherChar ' '
/// find string literal with only '"' escaped
/// string literal is allowed to cross line, line end is regarded as \n
/// raw string literal supported, `r'C:\\abc'` or `R"C:\\abc"`
struct V1Lexer<'chs, F> {
    v0: BufLexer<V0Lexer<'chs, F>, char, F>,
}
impl<'chs, F> ILexer<'chs, F, V1Token> for V1Lexer<'chs, F> where F: FileSystem {

    fn new(source: SourceChars<'chs, F>) -> V1Lexer<'chs, F> {
        V1Lexer { v0: BufLexer::new(source) }
    }

    fn next(&mut self, sess: &mut ParseSession) -> (V1Token, Span) {
        
        enum State {
            Nothing,
            InStringLiteral { parser: StringLiteralParser },
            InRawStringLiteral { parser: RawStringLiteralParser },
            InLineComment,
            InBlockComment { start_span: Span },
            InCharLiteral { parser: CharLiteralParser },
        }

        let mut state = State::Nothing;
        loop {
            self.v0.move_next(sess);
            match self.v0.current_with_state(state) {
                (State::Nothing, &'/', _1, &'/', _4, _5, _6) => {                       // C1: in nothing, meet //
                    self.v0.prepare_skip1();
                    state = State::InLineComment;                                       
                }
                (State::Nothing, &'/', start_span, &'*', _4, _5, _6) => {             // C2: in nothing, meet /*
                    state = State::InBlockComment{ start_span };                  
                }
                (State::Nothing, &'"', start_span, _3, _4, _5, _6) => {               // C3: in nothing, meet "
                    state = State::InStringLiteral { parser: StringLiteralParser::new(start_span.start) };
                }
                (State::Nothing, &'r', start_span, &'"', _4, _5, _6)
                | (State::Nothing, &'R', start_span, &'"', _4, _5, _6) => {           // C4: in nothing, meet r" or R"
                    self.v0.prepare_skip1();                   
                    state = State::InRawStringLiteral { parser: RawStringLiteralParser::new(start_span.start) };
                }
                (State::Nothing, &'\'', start_span, _3, _4, _5, _6) => {              // C5: in nothing, meet '
                    state = State::InCharLiteral{ parser: CharLiteralParser::new(start_span.start) };
                }
                (State::Nothing, &EOF, eof_pos, _3, _4, _5, _6) => {                // C7: in nothing, meet EOF, return 
                    return (V1Token::EOF, eof_pos);
                }
                (State::Nothing, ch, span, _3, _4, _5, _6) => {                       // C6: in nothing, meet other, return
                    return (V1Token::Other(*ch), span);
                }
                (State::InBlockComment{ start_span }, &'*', _2, &'/', end_pos, _5, _6) => {   // C8: in block, meet */, return
                    self.v0.prepare_skip1();
                    return (V1Token::Other(' '), start_span + end_pos);
                }
                (State::InBlockComment{ start_span }, &EOF, eof_pos, _3, _4, _5, _6) => {  // C10: in block, meet EOF, emit error, return
                    sess.messages.push(Message::new_by_str(strings::UnexpectedEOF, vec![
                        (start_span, strings::BlockCommentStartHere),
                        (eof_pos, strings::EOFHere),
                    ]));
                    return (V1Token::EOF, eof_pos);
                }
                (State::InBlockComment{ start_span }, _1, _2, _3, _4, _5, _6) => {       // C9: in block, continue block
                    state = State::InBlockComment{ start_span };
                }
                (State::InLineComment, &'\n', lf_pos, _3, _4, _5, _6) => {              // C11: in line, meet \n, return
                    return (V1Token::Other('\n'), lf_pos);
                }
                (State::InLineComment, &EOF, eof_pos, _3, _4, _5, _6) => {          // C13: in line, meet EOF, return
                    return (V1Token::EOF, eof_pos);
                }
                (State::InLineComment, _1, _2, _3, _4, _5, _6) => {                     // C12: in line, continue line
                    state = State::InLineComment;
                }

                (State::InStringLiteral{ mut parser }, ch, pos, next_ch, _4, _5, _6) => {   // Cx: anything inside "" is none about this module
                    match parser.input(*ch, pos.start, *next_ch, sess.messages) {
                        StringLiteralParserResult::WantMore => {
                            state = State::InStringLiteral{ parser: parser };
                        },
                        StringLiteralParserResult::WantMoreWithSkip1 => {
                            self.v0.prepare_skip1();
                            state = State::InStringLiteral{ parser: parser };
                        }
                        StringLiteralParserResult::Finished(value, pos) => {
                            if *ch == EOF { // if EOF was consumed by str lit parser, it will not be returned as EOF, which is not designed by feature
                                self.v0.prepare_dummy1();
                            }
                            return (V1Token::StringLiteral(value.map(|v| self.v0.lexer.0.intern(&v)).unwrap_or(IsId::new(1))), pos);
                        }
                    }
                }
                (State::InRawStringLiteral{ mut parser }, ch, span, _3, _4, _5, _6) => {     // Cx, anything inside r"" is none about this module
                    match parser.input(*ch, span.start, sess.messages) {
                        RawStringLiteralParserResult::WantMore => {
                            state = State::InRawStringLiteral{ parser: parser };
                        }
                        RawStringLiteralParserResult::Finished(value, pos) => {
                            if *ch == EOF { // same as str lit parser
                                self.v0.prepare_dummy1();
                            }
                            return (V1Token::RawStringLiteral(value.map(|v| self.v0.lexer.0.intern(&v)).unwrap_or(IsId::new(1))), pos);
                        }
                    }
                }
                (State::InCharLiteral{ mut parser }, ch, span, next_ch, _4, _5, _6) => {     // Cx: anything inside '' is none about this module
                    match parser.input(*ch, span.start, *next_ch, sess.messages) {
                        CharLiteralParserResult::WantMore => {
                            state = State::InCharLiteral{ parser: parser };
                        },
                        CharLiteralParserResult::WantMoreWithSkip1 => {
                            self.v0.prepare_skip1();
                            state = State::InCharLiteral{ parser: parser };
                        }
                        CharLiteralParserResult::Finished(value, pos) => {
                            if *ch == EOF { // same as str lit parser
                                self.v0.prepare_dummy1();
                            }
                            return (V1Token::CharLiteral(value.unwrap_or_default()), pos);
                        }
                    }
                }
            }
        }
    }
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

struct V2Lexer<'chs, F> {
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
