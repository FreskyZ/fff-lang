
// Buffed lexer
//     with TToken: Copy
//     and TLexer: ILexer<TToken>

use std::str::Chars;
use message::MessageEmitter;

// detail compare with the public interface ILexer
pub trait IDetailLexer<'chs, TToken> : From<Chars<'chs>> {

    // TODO
    // fn positiona()
    fn next(&mut self, emitter: &mut MessageEmitter) -> Option<TToken>;
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
pub struct BufLexer<TLexer, TToken> {
    lexer: TLexer,
    is_first: bool,
    next_to_return: Option<TToken>,
}

impl<'chs, TLexer, TToken> From<Chars<'chs>> for BufLexer<TLexer, TToken>
    where TLexer: IDetailLexer<'chs, TToken>, TToken: Clone {

    fn from(content_chars: Chars<'chs>) -> BufLexer<TLexer, TToken> {
        BufLexer { 
            lexer: TLexer::from(content_chars),
            is_first: true,
            next_to_return: None,
        }
    }
}

impl<'chs, TLexer, TToken> BufLexer<TLexer, TToken>
    where TLexer: IDetailLexer<'chs, TToken>, TToken: Clone {

    #[cfg(test)]
    pub fn from_test(content: &'chs str) ->  BufLexer<TLexer, TToken> {
        BufLexer::from(content.chars())
    }

    pub fn inner(&self) -> &TLexer { &self.lexer }
    pub fn inner_mut(&mut self) -> &mut TLexer { &mut self.lexer }

    pub fn next(&mut self, emitter: &mut MessageEmitter) -> Option<BufToken<TToken>> {

        if self.is_first {
            self.next_to_return = self.lexer.next(emitter);
            self.is_first = false;
        }

        let ret_val = match self.next_to_return {
            None => return None,
            Some(ref token) => {
                Some(BufToken {token: token.clone(), next: self.lexer.next(emitter)})
            }
        };

        self.next_to_return = match ret_val {
            None => None,
            Some(ref buf_token) => buf_token.next.clone(),
        };
        ret_val
    }

    pub fn skip1(&mut self, emitter: &mut MessageEmitter) {
        let _ = self.next(emitter);
    }
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