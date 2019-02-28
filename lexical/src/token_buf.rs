///! fff-lang
///!
///! lexical/buf_lexer
// this module is named token_buf because in file explorer
//  - buf_lexer.rs
//  - lib.rs
//  - token_stream.rs
// looks ugly, so rename it to token_buf
use std::cell::Cell;

use codemap::SourceCodeIter;
use codemap::Span;

use codemap::SymbolCollection;
use message::MessageCollection;

pub struct ParseSession<'a, 'b> {
    pub messages: &'a mut MessageCollection,
    pub symbols: &'b mut SymbolCollection,
}
impl<'a, 'b> ParseSession<'a, 'b> {
    pub fn new(messages: &'a mut MessageCollection, symbols: &'b mut SymbolCollection) -> Self {
        ParseSession { messages, symbols }
    }
}

pub trait ILexer<'chs, TToken> {
    // 17/6/7: it used to be ILexer::new(chars, messages), but the messages is not used currently,
    // I think in some previous versions it has its usage, but I can't think up of one
    // It is removed now, if future needed, add messages back
    // 5 minutes later: the messages is usable! because every ILexer contains a BufLexer
    // BufLexer needs a messages to call 3 times of next in advance
    // 1 hour later: now BufLexer use its skips and dummys mechanism to not call next in new, then sess is not used again
    fn new(source: SourceCodeIter<'chs>) -> Self;

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
pub struct BufLexer<TLexer, TToken> {
    lexer: TLexer,
    skips: Cell<i32>,
    dummys: Cell<i32>,
    current: (TToken, Span),
    next: (TToken, Span),
    nextnext: (TToken, Span),
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
impl<'chs, TLexer, TToken> BufLexer<TLexer, TToken>
where
    TToken: Default,
    TLexer: ILexer<'chs, TToken>,
{
    pub fn new(source: SourceCodeIter<'chs>) -> BufLexer<TLexer, TToken> {
        BufLexer {
            lexer: TLexer::new(source),
            current: (TToken::default(), Span::default()),
            next: (TToken::default(), Span::default()),
            nextnext: (TToken::default(), Span::default()),
            skips: Cell::new(2),
            dummys: Cell::new(0),
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

#[cfg(test)]
#[test]
fn buf_lexer_test() {
    use codemap::SourceCode;
    use codemap::SymbolCollection;
    use message::MessageCollection;

    #[derive(Eq, PartialEq, Debug, Default)]
    struct TestToken(usize);
    struct TestLexer(usize);
    impl<'chs> ILexer<'chs, TestToken> for TestLexer {
        fn new(_: SourceCodeIter<'chs>) -> TestLexer {
            TestLexer(0)
        }
        fn next(&mut self, _: &mut ParseSession) -> (TestToken, Span) {
            self.0 += 1;
            (TestToken(self.0), make_span!(self.0, self.0 + 1))
        }
    }
    macro_rules! make_test_token_p2 {
        ($c: expr) => {
            (
                &TestToken($c),
                make_span!($c, $c + 1),
                &TestToken($c + 1),
                make_span!($c + 1, $c + 2),
                &TestToken($c + 2),
                make_span!($c + 2, $c + 3),
            )
        };
    }

    let codemap = SourceCode::with_test_str(0, "");
    let messages = &mut MessageCollection::new();
    let symbols = &mut SymbolCollection::new();
    let sess = &mut ParseSession::new(messages, symbols);
    let mut buflexer = BufLexer::<TestLexer, TestToken>::new(codemap.iter());
    if sess.messages.is_uncontinuable() {
        panic!("messages unexpectedly uncontinuable")
    }

    buflexer.move_next(sess);
    assert_eq!(buflexer.current_with_preview2(), make_test_token_p2!(1));
    buflexer.move_next(sess);
    assert_eq!(buflexer.current_with_preview2(), make_test_token_p2!(2));
    buflexer.prepare_skip1();
    buflexer.move_next(sess);
    assert_eq!(buflexer.current_with_preview2(), make_test_token_p2!(4));
    buflexer.prepare_skip1();
    buflexer.prepare_skip1();
    buflexer.move_next(sess);
    assert_eq!(buflexer.current_with_preview2(), make_test_token_p2!(7));
    buflexer.prepare_dummy1();
    buflexer.move_next(sess);
    assert_eq!(buflexer.current_with_preview2(), make_test_token_p2!(7));
    buflexer.prepare_dummy1();
    buflexer.prepare_dummy1();
    buflexer.move_next(sess);
    buflexer.move_next(sess);
    assert_eq!(buflexer.current_with_preview2(), make_test_token_p2!(7));
}
