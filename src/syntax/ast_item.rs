
// ASTItem interface
// TODO: add check head symbol

use std::fmt;
use std::marker::PhantomData;

use common::StringPosition;
use message::Message;
use message::MessageEmitter;

use lexical::Lexer;

pub trait IASTItem {

    // Start of start token and end of end token
    fn pos_all(&self) -> StringPosition;

    // some for valid ones, none for invalid and can not recover
    // and consumed symbol length
    fn parse(lexer: &mut Lexer, index: usize) -> (Option<Self>, usize) where Self: Sized;

    /// Check lexer[index] is acceptable final
    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool;
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

// Test infrastructure
#[cfg(test)]
pub struct TestCase<T> {
    phantom: PhantomData<T>,
}

#[cfg(test)]
impl<T> TestCase<T> 
    where T: IASTItem + Eq + PartialEq + fmt::Debug {

    pub fn run(program: &str, expect_len: usize, expect_pos_all: StringPosition, expect_result: T, line: u32, column: u32) {
        let lexer = &mut Lexer::new(program);
        perrorln!("Case at {}:{}", line, column);
        if let (Some(actual_result), actual_len) = T::parse(lexer, 0) {
            assert_eq!(actual_result, expect_result, "error result");
            assert_eq!(actual_len, expect_len, "error symbol length");
            assert_eq!(actual_result.pos_all(), expect_pos_all, "error pos all");
        } else {
            panic!("expr is not some, messages: {:?} at {}:{}", lexer.messages(), line, column)
        }
    }

    /// run with check error
    pub fn run_e(program: &str, expect_len: usize, expect_pos_all: StringPosition, expect_result: T, expect_messages: Vec<Message>, line: u32, column: u32) {
        let lexer = &mut Lexer::new(program);
        perrorln!("Case at {}:{}", line, column);
        if let (Some(actual_result), actual_len) = T::parse(lexer, 0) {
            assert_eq!(actual_result, expect_result, "error result");
            assert_eq!(actual_len, expect_len, "error symbol length");
            assert_eq!(actual_result.pos_all(), expect_pos_all, "error pos all");
            let mut formated_expect_messages = MessageEmitter::new();
            for msg in expect_messages {
                formated_expect_messages.push(msg);
            }
            assert_eq!(lexer.messages(), &formated_expect_messages, "error messages");
        } else {
            panic!("expr is not some, messages: {:?} at {}:{}", lexer.messages(), line, column)
        }
    }

    /// run with only check error
    pub fn run_oe(program: &str, expect_len: usize, expect_messages: Vec<Message>, line: u32, column: u32) {
        let lexer = &mut Lexer::new(program);
        perrorln!("Case at {}:{}", line, column);
        let (actual_result, actual_len) = T::parse(lexer, 0);
        assert_eq!(actual_len, expect_len, "error symbol length");
        assert_eq!(actual_result, None, "result is not none");

        let mut formated_expect_messages = MessageEmitter::new();
        for msg in expect_messages {
            formated_expect_messages.push(msg);
        }
        assert_eq!(lexer.messages(), &formated_expect_messages, "error messages");
    }
}
