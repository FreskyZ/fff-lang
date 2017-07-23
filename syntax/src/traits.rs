///! fff-lang
///!
///! syntax/traits, for ISyntaxItem for various relevant staffs

use std::fmt::Debug;

use codemap::SymbolCollection;
use message::MessageCollection;
use lexical::TokenStream;

use super::ParseSession;
use super::ParseResult;

// Grammar
pub trait ISyntaxItemGrammar {
    fn is_first_final(sess: &ParseSession) -> bool;
}

// Parse
pub trait ISyntaxItemParse {
    type Target;
    
    fn parse(sess: &mut ParseSession) -> ParseResult<Self::Target>;

    // check is_first_final, if pass, parse, return Ok(Some(T)) or Err(()), else return None
    fn try_parse(sess: &mut ParseSession) -> ParseResult<Option<Self::Target>> where Self: ISyntaxItemGrammar {
        if Self::is_first_final(sess) { Ok(Some(Self::parse(sess)?)) } else { Ok(None) }
    }
}

pub trait WithTestInput {
    type Output: Sized;

    fn with_test_input(input: TestInput) -> (Option<Self::Output>, MessageCollection, SymbolCollection);

    fn with_test_str(src: &str) -> Self::Output {
        Self::with_test_input(TestInput::new(src)).0.unwrap()
    }
}

#[allow(dead_code)] // test members may not be used
pub struct TestInput<'a> {
    pub src_id: usize,
    pub src: &'a str,
    pub syms: Option<SymbolCollection>,
}
#[allow(dead_code)] // test members may not be used
impl<'a> TestInput<'a> {

    pub fn new(src: &'a str) -> Self { TestInput{ src, src_id: 0, syms: None } }
    pub fn set_syms(mut self, syms: SymbolCollection) -> Self { self.syms = Some(syms); self }
    pub fn set_src_id(mut self, id: usize) -> Self { self.src_id = id; self }

    pub fn apply<T, U>(self) -> TestInputResult<U> where T: WithTestInput<Output = U> {
        let result = T::with_test_input(self);
        TestInputResult{ result: result.0, msgs: result.1 }
    }
}

#[allow(dead_code)] // test members may not be used
pub struct TestInputResult<T> {
    result: Option<T>,
    msgs: MessageCollection,
}
#[allow(dead_code)] // test members may not be used
impl<T> TestInputResult<T> {

    pub fn expect_messages(self, expect_msgs: MessageCollection) -> Self where T: Eq + Debug {
        assert_eq!{ self.msgs, expect_msgs }
        self
    }
    pub fn expect_no_message(self) -> Self where T: Eq + Debug {
        assert_eq!{ self.msgs.is_empty(), true }
        self
    }

    pub fn expect_result(self, result: T) -> Self where T: Eq + Debug {
        assert_eq!{ self.result, Some(result) }
        self
    }
    pub fn expect_no_result(self) -> Self where T: Eq + Debug {
        assert_eq!{ self.result, None }
        self
    }

    // drop return value
    pub fn finish(self) { }
}

impl<T, U> WithTestInput for T where T: ISyntaxItemParse<Target = U> {
    type Output = U;

    fn with_test_input(input: TestInput) -> (Option<U>, MessageCollection, SymbolCollection) {
        let (tokens, mut msgs, mut syms) = TokenStream::with_test_input(input.src, input.syms);
        let retval = { 
            let mut parse_sess = ParseSession::new(&tokens, &mut msgs, &mut syms);
            Self::parse(&mut parse_sess).ok()
        };
        (retval, msgs, syms)
    }
}

// pub trait ISyntaxItemWithStr {

//     fn with_test_str(src: &str) 
//         -> <Self as ISyntaxItemParse>::Target 
//         where Self: Sized + ISyntaxItemParse {
//         let full = Self::with_test_input_ret_size_messages(src, SymbolCollection::new());
//         check_messages_continuable!(full.2);
//         return full.0.unwrap();
//     }
//     fn with_test_str_ret_messages(src: &str) 
//         -> (Option<<Self as ISyntaxItemParse>::Target>, MessageCollection) 
//         where Self: Sized + ISyntaxItemParse {
//         let full = Self::with_test_input_ret_size_messages(src, SymbolCollection::new());
//         return (full.0, full.2);
//     }

//     fn with_test_input(src: &str, symbols: SymbolCollection) -> <Self as ISyntaxItemParse>::Target where Self: Sized + ISyntaxItemParse {
//         let full = Self::with_test_input_ret_size_messages(src, symbols);
//         if full.0.is_none() {
//             panic!("with_test_input({}): trying to unwrap None result, messages: {:?}", src, full.2);
//         }
//         return full.0.unwrap();
//     }
//     fn with_test_input_ret_messages(src: &str, symbols: SymbolCollection) 
//         -> (Option<<Self as ISyntaxItemParse>::Target>, MessageCollection) where Self: Sized + ISyntaxItemParse {
//         let full = Self::with_test_input_ret_size_messages(src, symbols);
//         return (full.0, full.2);
//     }
//     fn with_test_input_ret_size_messages(src: &str, mut symbols: SymbolCollection) 
//         -> (Option<<Self as ISyntaxItemParse>::Target>, usize, MessageCollection) where Self: Sized + ISyntaxItemParse {        
//         let (tokens, mut messages, mut symbols) = TokenStream::with_test_input(src, Some(symbols));
//         let ret_val = { // to satisfy liefetime checker
//             let mut sess = ParseSession::new(&tokens, &mut messages, &mut symbols);
//             let retval = match Self::parse(&mut sess) {
//                 Ok(retval) => Some(retval),
//                 Err(_) => None,
//             };
//             let size = sess.get_current_index();
//             (retval, size)
//         };
//         return (ret_val.0, ret_val.1, messages);
//     }
// }
// impl<T> ISyntaxItemWithStr for T where T: ISyntaxItemParse {
// }

const INDENTION_FILLERS: [[&'static str; 16]; 3] = [ [
    "", "1 ", "2 | ", "3 | | ", "4 | | | ", "5 | | | | ", "6 | | | | | ", "7 | | | | | | ", "8 | | | | | | | ", "9 | | | | | | | | ", "10 | | | | | | | | | ",
    "11| | | | | | | | | | ", "12| | | | | | | | | | | ", "13| | | | | | | | | | | | ", "14| | | | | | | | | | | | | ", "15| | | | | | | | | | | | "
], [
    "", "| ", "| | ", "| | | ", "| | | | ", "| | | | | ", "| | | | | | ", "| | | | | | | ", "| | | | | | | | ", "| | | | | | | | | ", "| | | | | | | | | | ",
    "| | | | | | | | | | | ", "| | | | | | | | | | | | ", "| | | | | | | | | | | | | ", "| | | | | | | | | | | | | | ", "| | | | | | | | | | | | | "
], [
    "", "  ", "    ", "      ", "        ", "          ", "            ", "              ", "                ", "                  ", "                    ",
    "                      ", "                        ", "                          ", "                            ", "                          "
]];

pub trait ISyntaxItemFormat {
    fn indent_str(indent: u32) -> &'static str { INDENTION_FILLERS[2][indent as usize] }
    fn format(&self, indent: u32) -> String;
}

#[cfg(test)] #[test]
fn test_input_use() {
    use codemap::Span;
    use message::Message;

    #[derive(Eq, Debug, PartialEq)]
    struct SyntaxTree {
        items: String,
    }
    impl SyntaxTree {
        fn parse(src: &str, messages: &mut MessageCollection, _symbols: &mut SymbolCollection) -> Result<SyntaxTree, ()> {
            if src.as_bytes()[0] == b'1' {
                messages.push(Message::new_by_str("some message", vec![(make_span!(1, 2), "here")]));
            } else if src.as_bytes()[1] == b'2' {
                return Err(());
            }
            Ok(SyntaxTree{ items: src.to_owned() })
        }

        fn new_items(src: &str) -> Self { SyntaxTree{ items: src.to_owned() } }
    }
    impl WithTestInput for SyntaxTree {
        type Output = Self;
        fn with_test_input(input: TestInput) -> (Option<Self>, MessageCollection, SymbolCollection) {
            let mut messages = MessageCollection::new();
            let mut symbols = input.syms.unwrap_or_default();
            (SyntaxTree::parse(input.src, &mut messages, &mut symbols).ok(), messages, symbols)
        }
    }

    TestInput::new("123")
        .set_syms(make_symbols!["a", "b", "c"])
        .set_src_id(42)
        .apply::<SyntaxTree, SyntaxTree>()
        .expect_messages(make_messages![
            Message::new_by_str("some message", vec![(make_span!(1, 2), "here")])
        ])
        .expect_result(SyntaxTree::new_items("123"))
        .finish();

    TestInput::new("324")
        .set_syms(make_symbols!["3", "2", "1"])
        .apply::<SyntaxTree, SyntaxTree>()
        .expect_no_message()
        .expect_no_result()
        .finish();
}
