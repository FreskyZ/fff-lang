///! fff-lang
///!
///! syntax/test_helper

use std::fmt::Debug;
use std::rc::Rc;
use crate::codemap::SourceCode;
use crate::codemap::SymbolCollection;
use crate::diagnostics::MessageCollection;
use crate::lexical::TokenStream;
use super::ParseSession;
use super::ISyntaxParse;

pub trait WithTestInput {
    type Output: Sized;

    fn with_test_input(input: TestInput) -> (Option<Self::Output>, Rc<SourceCode>, MessageCollection, SymbolCollection);

    fn with_test_str(src: &str) -> Self::Output {
        let (maybe_output, _, messages, _) = Self::with_test_input(TestInput::new(src));
        maybe_output.expect(&format!("parse fail: {:?}", messages))
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
        let (result, source, messages, symbols) = T::with_test_input(self);
        TestInputResult{ result, source, messages, symbols }
    }
}

#[allow(dead_code)] // test members may not be used
pub struct TestInputResult<T> {
    result: Option<T>,
    source: Rc<SourceCode>,
    symbols: SymbolCollection,
    messages: MessageCollection,
}
#[allow(dead_code)] // test members may not be used
impl<T> TestInputResult<T> {

    pub fn expect_messages(self, expect_msgs: MessageCollection) -> Self where T: Eq + Debug {
        assert_eq!{ self.messages, expect_msgs }
        self
    }
    pub fn expect_no_message(self) -> Self where T: Eq + Debug {
        assert_eq!{ self.messages.is_empty(), true, "messages are {}", self.messages.format(Some(&self.source)) }
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

    pub fn get_source(&self) -> &SourceCode { 
        self.source.as_ref()
    }
    pub fn get_result(&self) -> Option<&T> {
        self.result.as_ref()
    }
    pub fn get_symbols(&self) -> &SymbolCollection {
        &self.symbols
    }

    // drop return value
    pub fn finish(self) { }
}

impl<T, U> WithTestInput for T where T: ISyntaxParse<Output = U> {
    type Output = U;

    fn with_test_input(input: TestInput) -> (Option<U>, Rc<SourceCode>, MessageCollection, SymbolCollection) {
        let (tokens, source, mut msgs, mut syms) = TokenStream::with_test_input(input.src, input.syms);
        let retval = { 
            let mut parse_sess = ParseSession::new(source.clone(), &tokens, &mut msgs, &mut syms);
            Self::parse(&mut parse_sess).ok()
        };
        (retval, source, msgs, syms)
    }
}

#[cfg(test)] #[test]
fn test_input_use() {
    use crate::codemap::Span;
    use crate::diagnostics::Message;

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
        fn with_test_input(input: TestInput) -> (Option<Self>, Rc<SourceCode>, MessageCollection, SymbolCollection) {
            let mut messages = MessageCollection::new();
            let mut symbols = input.syms.unwrap_or_default();
            (SyntaxTree::parse(input.src, &mut messages, &mut symbols).ok(), Rc::new(SourceCode::with_test_str(0, "1")), messages, symbols)
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
