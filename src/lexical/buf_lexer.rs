
// Buffed lexer
//     with TToken: Copy
//     and TLexer: ILexer<TToken>

use message::MessageEmitter;

pub trait ILexer<TToken> {

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

impl<TLexer, TToken> From<String> for BufLexer<TLexer, TToken>
    where TLexer: ILexer<TToken> + From<String>, TToken: Clone {

    fn from(content: String) -> BufLexer<TLexer, TToken> {
        BufLexer { 
            lexer: TLexer::from(content),
            is_first: true,
            next_to_return: None,
        }
    }
}

impl<TLexer, TToken> BufLexer<TLexer, TToken>
    where TLexer: ILexer<TToken> + From<String>, TToken: Clone {

    #[cfg(test)]
    pub fn from_test(content: &str) ->  BufLexer<TLexer, TToken> {
        BufLexer::from(content.to_owned())
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