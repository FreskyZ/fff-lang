///! fff-lang
// Buffed lexer

use std::cell::Cell;
use codemap::CodeChars;
use codepos::StringPosition;
use message::MessageCollection;

pub trait ILexer<'chs, TToken> {

    fn new(chars: CodeChars<'chs>, messaegs: &mut MessageCollection) -> Self;

    // now position is out of token type
    // now token type provide EOF hint by itself
    fn next(&mut self, messages: &mut MessageCollection) -> (TToken, StringPosition);
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
    current: (TToken, StringPosition),
    next: (TToken, StringPosition),
    nextnext: (TToken, StringPosition),
}

#[allow(dead_code)]
impl<'chs, TLexer, TToken> BufLexer<TLexer, TToken> 
    where TLexer: ILexer<'chs, TToken> {
    
    pub fn new(content_chars: CodeChars<'chs>, messages: &mut MessageCollection) -> BufLexer<TLexer, TToken> {

        let mut lexer = TLexer::new(content_chars, messages);
        let current = lexer.next(messages);
        let next = lexer.next(messages);
        let nextnext = lexer.next(messages);
        BufLexer { 
            lexer: lexer, 
            current: current, next: next, nextnext: nextnext,
            skips: Cell::new(0), 
            dummys: Cell::new(1), 
        }
    }

    fn actual_move_next(&mut self, messages: &mut MessageCollection) {

        // Update: now the feature is based on self.dummys not on self.is_first_time
        // discard first time of next, too make this usage possible
        // loop{ buflexer.move_next(); match buflexer.current() { ... return ... return ... } }
        // if self.is_first_time {   
        //     self.is_first_time = false;
        //     return;
        // }

        // strange method for 
        //     self.current = self.next;  // as self is mutable borrowed, cannot move self.next
        //     self.next = self.nextnext; // same
        unsafe {
            use std::ptr;
            ptr::swap(&mut self.current as *mut (TToken, StringPosition), &mut self.next as *mut (TToken, StringPosition));
            ptr::swap(&mut self.next as *mut (TToken, StringPosition), &mut self.nextnext as *mut (TToken, StringPosition));
        }
        self.nextnext = self.lexer.next(messages);
    }
    pub fn move_next(&mut self, messages: &mut MessageCollection) {
        if self.dummys.get() != 0 {
            self.dummys.set(self.dummys.get() - 1);
            return;
        }
        for _ in 0..self.skips.get() + 1 {
            self.actual_move_next(messages);
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

    pub fn current(&self) -> (&TToken, StringPosition) {
        (&self.current.0, self.current.1)
    }
    pub fn current_with_preview(&self) -> (&TToken, StringPosition, &TToken, StringPosition) {
        (&self.current.0, self.current.1, &self.next.0, self.next.1)
    }
    pub fn current_with_preview2(&self) -> (&TToken, StringPosition, &TToken, StringPosition, &TToken, StringPosition) {
        (&self.current.0, self.current.1, &self.next.0, self.next.1, &self.nextnext.0, self.nextnext.1)
    }

    pub fn current_with_state<TState>(&self, state: TState) -> (TState, &TToken, StringPosition, &TToken, StringPosition, &TToken, StringPosition) {
        (state, &self.current.0, self.current.1, &self.next.0, self.next.1, &self.nextnext.0, self.nextnext.1)
    }
}


#[cfg(test)]
#[test]
fn buf_lexer_test() {
    use codemap::CodeMap;

    #[derive(Eq, PartialEq, Debug)] struct TestToken(u32); // cannot copy and clone and other, eq for test
    struct TestLexer(u32);
    impl<'chs> ILexer<'chs, TestToken> for TestLexer {

        fn new(_: CodeChars<'chs>, _: &mut MessageCollection) -> TestLexer {
            TestLexer(0)
        }
        fn next(&mut self, _: &mut MessageCollection) -> (TestToken, StringPosition) {
            self.0 += 1;
            (TestToken(self.0), make_str_pos!(1, self.0, 1, self.0 + 1))
        }
    } 
    
    let mut buflexer = BufLexer::<TestLexer, TestToken>::new(CodeMap::with_str("").iter(), &mut MessageCollection::new());
    let messages = &mut MessageCollection::new();

    buflexer.move_next(messages);
    assert_eq!(buflexer.current_with_preview2(), 
        (&TestToken(1), make_str_pos!(1, 1, 1, 2), &TestToken(2), make_str_pos!(1, 2, 1, 3), &TestToken(3), make_str_pos!(1, 3, 1, 4)));
    buflexer.move_next(messages);
    assert_eq!(buflexer.current_with_preview2(), 
        (&TestToken(2), make_str_pos!(1, 2, 1, 3), &TestToken(3), make_str_pos!(1, 3, 1, 4), &TestToken(4), make_str_pos!(1, 4, 1, 5)));
    buflexer.prepare_skip1();
    buflexer.move_next(messages);
    assert_eq!(buflexer.current_with_preview2(), 
        (&TestToken(4), make_str_pos!(1, 4, 1, 5), &TestToken(5), make_str_pos!(1, 5, 1, 6), &TestToken(6), make_str_pos!(1, 6, 1, 7)));
    buflexer.prepare_skip1();
    buflexer.prepare_skip1();
    buflexer.move_next(messages);
    assert_eq!(buflexer.current_with_preview2(), 
        (&TestToken(7), make_str_pos!(1, 7, 1, 8), &TestToken(8), make_str_pos!(1, 8, 1, 9), &TestToken(9), make_str_pos!(1, 9, 1, 10)));
    buflexer.prepare_dummy1();
    buflexer.move_next(messages);
    assert_eq!(buflexer.current_with_preview2(), 
        (&TestToken(7), make_str_pos!(1, 7, 1, 8), &TestToken(8), make_str_pos!(1, 8, 1, 9), &TestToken(9), make_str_pos!(1, 9, 1, 10)));
    buflexer.prepare_dummy1();
    buflexer.prepare_dummy1();
    buflexer.move_next(messages);
    buflexer.move_next(messages);
    assert_eq!(buflexer.current_with_preview2(), 
        (&TestToken(7), make_str_pos!(1, 7, 1, 8), &TestToken(8), make_str_pos!(1, 8, 1, 9), &TestToken(9), make_str_pos!(1, 9, 1, 10)));
}