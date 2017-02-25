#![allow(dead_code)] // remove it after new buf lexer finished
// Buffed lexer
//     with TToken: Copy
//     and TLexer: ILexer<TToken>

use std::str::Chars;
use std::cell::Cell;
use codepos::Position;
use codepos::StringPosition;
use message::MessageCollection;

// detail compare with the public interface ILexer
pub trait IDetailLexer<'chs, TToken> {

    fn new(chars: Chars<'chs>, messages: &mut MessageCollection) -> Self;

    fn position(&self) -> Position;

    fn next(&mut self, messages: &mut MessageCollection) -> Option<TToken>;
}

#[cfg(test)]
#[derive(Eq, PartialEq)]
pub struct BufToken<T>
    where T : Clone {
    pub token: T, 
    pub next: Option<T>,
}
#[cfg(not(test))]
pub struct BufToken<T>
    where T : Clone {
    pub token: T, 
    pub next: Option<T>,
}
#[cfg(test)]
use std::fmt;
#[cfg(test)]
impl<T> fmt::Debug for BufToken<T> 
    where T : fmt::Debug + Clone {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(writeln!(f, "{:?}", self.token));
        match self.next {
            Some(ref next) => write!(f, "    Next: {:?}", next),
            None => write!(f, "    Next: None"),
        }
    }
}

// Buffed lexer
// Example
//
// type BufV0Lexer = BufLexer<V0Lexer, V0Token>;
// let mut bufv0 = ...
// loop { 
//    match bufv0.next() {
//        BufToken<V0Token> { token: V0Token { ch, pos }, next: Some(V0Token { ch, pos }) } => {
//            ...
//        }
//        BufToken<V0Token> { token: V0Token { ch, pos }, next: None } => {
//            ...
//        }
//        ...
//    }
// }
pub struct LegacyBufLexer<TLexer, TToken> {
    lexer: TLexer,
    is_first: bool,
    next_to_return: Option<TToken>,
}

impl<'chs, TLexer, TToken> LegacyBufLexer<TLexer, TToken>
    where TLexer: IDetailLexer<'chs, TToken>, TToken: Clone {

    pub fn new(content_chars: Chars<'chs>, messages: &mut MessageCollection) -> LegacyBufLexer<TLexer, TToken> {
        LegacyBufLexer { 
            lexer: TLexer::new(content_chars, messages),
            is_first: true,
            next_to_return: None,
        }
    }

    pub fn inner(&self) -> &TLexer { &self.lexer }
    pub fn inner_mut(&mut self) -> &mut TLexer { &mut self.lexer }

    pub fn next(&mut self, messages: &mut MessageCollection) -> Option<BufToken<TToken>> {

        if self.is_first {
            self.next_to_return = self.lexer.next(messages);
            self.is_first = false;
        }

        let ret_val = match self.next_to_return {
            None => return None,
            Some(ref token) => {
                Some(BufToken {token: token.clone(), next: self.lexer.next(messages)})
            }
        };

        self.next_to_return = match ret_val {
            None => None,
            Some(ref buf_token) => buf_token.next.clone(),
        };
        ret_val
    }

    pub fn skip1(&mut self, messages: &mut MessageCollection) {
        let _ = self.next(messages);
    }
}

pub trait ILexer<'chs, TToken> {

    fn new(chars: Chars<'chs>, messaegs: &mut MessageCollection) -> Self;

    // now position is out of token type
    // now token type provide EOF hint by itself
    fn next(&mut self, messages: &mut MessageCollection) -> (TToken, StringPosition);
}

// TODO: buf lexer that not require token to be Copy
pub struct BufLexer<TLexer, TToken> {
    lexer: TLexer, 
    skips: Cell<i32>,
    is_first_time: bool,
    current: (TToken, StringPosition),
    next: (TToken, StringPosition),
    nextnext: (TToken, StringPosition),
}

impl<'chs, TLexer, TToken> BufLexer<TLexer, TToken> 
    where TLexer: ILexer<'chs, TToken> {
    
    pub fn new(content_chars: Chars<'chs>, messages: &mut MessageCollection) -> BufLexer<TLexer, TToken> {

        let mut lexer = TLexer::new(content_chars, messages);
        let current = lexer.next(messages);
        let next = lexer.next(messages);
        let nextnext = lexer.next(messages);
        BufLexer{ lexer: lexer, current: current, next: next, nextnext: nextnext, skips: Cell::new(0), is_first_time: true }
    }

    fn actual_move_next(&mut self, messages: &mut MessageCollection) {

        // discard first time of next, too make this usage possible
        // loop{ buflexer.move_next(); match buflexer.current() { ... return ... return ... } }
        if self.is_first_time {   
            self.is_first_time = false;
            return;
        }
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
        for _ in 0..self.skips.get() + 1 {
            self.actual_move_next(messages);
        }
        self.skips.set(0);
    }
    /// this skip will perform at next time of move next
    pub fn prepare_skip1(&self) {
        self.skips.set(self.skips.get() + 1)
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

    #[derive(Eq, PartialEq, Debug)] struct TestToken(u32); // cannot copy and clone and other, eq for test
    struct TestLexer(u32);
    impl<'chs> ILexer<'chs, TestToken> for TestLexer {

        fn new(_: Chars<'chs>, _: &mut MessageCollection) -> TestLexer {
            TestLexer(0)
        }
        fn next(&mut self, _: &mut MessageCollection) -> (TestToken, StringPosition) {
            self.0 += 1;
            (TestToken(self.0), make_str_pos!(1, self.0, 1, self.0 + 1))
        }
    } 
    
    let mut buflexer = BufLexer::<TestLexer, TestToken>::new("".chars(), &mut MessageCollection::new());

    buflexer.move_next(&mut MessageCollection::new());
    assert_eq!(buflexer.current_with_preview2(), 
        (&TestToken(1), make_str_pos!(1, 1, 1, 2), &TestToken(2), make_str_pos!(1, 2, 1, 3), &TestToken(3), make_str_pos!(1, 3, 1, 4)));
    buflexer.move_next(&mut MessageCollection::new());
    assert_eq!(buflexer.current_with_preview2(), 
        (&TestToken(2), make_str_pos!(1, 2, 1, 3), &TestToken(3), make_str_pos!(1, 3, 1, 4), &TestToken(4), make_str_pos!(1, 4, 1, 5)));
    buflexer.move_next(&mut MessageCollection::new());
    buflexer.move_next(&mut MessageCollection::new());
    assert_eq!(buflexer.current_with_preview2(), 
        (&TestToken(4), make_str_pos!(1, 4, 1, 5), &TestToken(5), make_str_pos!(1, 5, 1, 6), &TestToken(6), make_str_pos!(1, 6, 1, 7)));
}