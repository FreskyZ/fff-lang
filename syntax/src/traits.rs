
// ASTItem interface

use codepos::StringPosition;
use message::MessageCollection;
use lexical::TokenStream;

pub trait ISyntaxItem {

    // Start of start token and end of end token
    fn pos_all(&self) -> StringPosition;

    // some for valid ones, none for invalid and can not recover
    // and consumed symbol length
    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<Self>, usize) where Self: Sized;

    /// Check tokens[index] is acceptable final
    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool;
}

pub trait ISyntaxItemWithStr {

    fn with_test_str_and_index(program: &str, index: usize) -> Self where Self: Sized + ISyntaxItem {
        let tokens = &mut TokenStream::with_test_str(program);
        let messages = &mut MessageCollection::new();
        let ret_val = Self::parse(tokens, messages, index).0.unwrap();
        check_messages_continuable!(messages);
        return ret_val;
    }
    fn with_test_str(program: &str) -> Self where Self: Sized + ISyntaxItem {
        let tokens = &mut TokenStream::with_test_str(program);
        let messages = &mut MessageCollection::new();
        let ret_val = Self::parse(tokens, messages, 0).0.unwrap();
        check_messages_continuable!(messages);
        return ret_val;
    }
    fn with_test_str_ret_size(program: &str) -> (Option<Self>, usize) where Self: Sized + ISyntaxItem {
        let tokens = &mut TokenStream::with_test_str(program);
        let messages = &mut MessageCollection::new();
        let ret_val = Self::parse(tokens, messages, 0);
        check_messages_continuable!(messages);
        return ret_val;
    }
}
impl<T> ISyntaxItemWithStr for T where T: ISyntaxItem {
}

const INDENT_STRS: [&'static str; 16] = [
    "", "  ", "    ", "      ", "        ", "          ", "            ", "              ", "                ", "                  ", "                    ",
    "                      ", "                        ", "                          ", "                            ", "                          "
];
pub trait ISyntaxItemFormat {

    fn indent_str(indent: u32) -> &'static str {
        INDENT_STRS[indent as usize]
    }

    fn format(&self, indent: u32) -> String;
}

// Test infrastructure macro
#[macro_export]
macro_rules! ast_test_case {
    ($program: expr, $len: expr, $expect_pos: expr, $expect: expr) => (
        TestCase::run($program, $len, $expect_pos, $expect, line!(), column!());
    );
    ($program: expr, $len: expr, $expect_pos: expr, $expect: expr, [$($msg: expr)*]) => (
        TestCase::run_e($program, $len, $expect_pos, $expect, vec![$($msg, )*], line!(), column!());
    );    
    ($program: expr, $len: expr, $ty: ty, [$($msg: expr)*]) => (
        TestCase::<$ty>::run_oe($program, $len, vec![$($msg, )*], line!(), column!());
    );
}

#[cfg(test)] use std::fmt;
#[cfg(test)] use std::marker::PhantomData;
#[cfg(test)] use message::LegacyMessage as Message;

// Test infrastructure
#[cfg(test)]
pub struct TestCase<T> {
    phantom: PhantomData<T>,
}

#[cfg(test)]
impl<T> TestCase<T> 
    where T: ISyntaxItem + Eq + PartialEq + fmt::Debug {

    pub fn run(program: &str, expect_len: usize, expect_pos_all: StringPosition, expect_result: T, line: u32, column: u32) {
        perrorln!("Case at {}:{}", line, column);
        if let (Some(actual_result), actual_len) = T::with_test_str_ret_size(program) {
            assert_eq!(actual_result, expect_result, "error result");
            assert_eq!(actual_len, expect_len, "error symbol length");
            assert_eq!(actual_result.pos_all(), expect_pos_all, "error pos all");
        } 
    }

    /// run with check error
    pub fn run_e(program: &str, expect_len: usize, expect_pos_all: StringPosition, expect_result: T, expect_messages: Vec<Message>, line: u32, column: u32) {
        perrorln!("Case at {}:{}", line, column);

        let tokens = &mut TokenStream::with_test_str(program);
        let messages = &mut MessageCollection::new();
        if let (Some(actual_result), actual_len) =T::parse(tokens, messages, 0) {
            assert_eq!(actual_result, expect_result, "error result");
            assert_eq!(actual_len, expect_len, "error symbol length");
            assert_eq!(actual_result.pos_all(), expect_pos_all, "error pos all");
            let mut formated_expect_messages = MessageCollection::new();
            for msg in expect_messages {
                formated_expect_messages.push(msg);
            }
            assert_eq!(messages, &formated_expect_messages, "error messages");
        }
    }

    /// run with only check error
    pub fn run_oe(program: &str, expect_len: usize, expect_messages: Vec<Message>, line: u32, column: u32) {
        perrorln!("Case at {}:{}", line, column);
        
        let tokens = &mut TokenStream::with_test_str(program);
        let messages = &mut MessageCollection::new();
        let (actual_result, actual_len) = T::parse(tokens, messages, 0);
        assert_eq!(actual_len, expect_len, "error symbol length");
        assert_eq!(actual_result, None, "result is not none");

        let mut formated_expect_messages = MessageCollection::new();
        for msg in expect_messages {
            formated_expect_messages.push(msg);
        }
        assert_eq!(messages, &formated_expect_messages, "error messages");
    }
}
