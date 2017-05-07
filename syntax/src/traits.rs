///! fff-lang
///!
///! syntax/traits, for ISyntaxItem for various relevant staffs

use message::MessageCollection;
use lexical::TokenStream;
#[cfg(feature = "parse_sess")] use super::ParseSession;
#[cfg(feature = "parse_sess")] use super::ISyntaxItemParseX;

pub trait ISyntaxItemParse {

    // some for valid ones, none for invalid and can not recover
    // and consumed symbol length
    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<Self>, usize) where Self: Sized;
}
// WithStr
#[cfg(not(feature = "parse_sess"))]
pub trait ISyntaxItemWithStr {

    fn with_test_str(program: &str) -> Self where Self: Sized + ISyntaxItemParse {
        let tokens = &mut TokenStream::with_test_str(program);
        let messages = &mut MessageCollection::new();
        let ret_val = Self::parse(tokens, messages, 0).0.unwrap();
        check_messages_continuable!(messages);
        return ret_val;
    }
    fn with_test_str_ret_size(program: &str) -> (Option<Self>, usize) where Self: Sized + ISyntaxItemParse {
        let tokens = &mut TokenStream::with_test_str(program);
        let messages = &mut MessageCollection::new();
        let ret_val = Self::parse(tokens, messages, 0);
        check_messages_continuable!(messages);
        return ret_val;
    }
    fn with_test_str_ret_messages(program: &str) -> (Option<Self>, MessageCollection) where Self: Sized + ISyntaxItemParse {
        let tokens = &mut TokenStream::with_test_str(program);
        let mut messages = MessageCollection::new();
        let ret_val = { // to satisfy liefetime checker
            let ret_val = Self::parse(tokens, &mut messages, 0);
            check_messages_continuable!(&mut messages);
            ret_val
        };
        return (ret_val.0, messages);
    }
    fn with_test_str_ret_size_messages(program: &str) -> (Option<Self>, usize, MessageCollection) where Self: Sized + ISyntaxItemParse {
        let tokens = &mut TokenStream::with_test_str(program);
        let mut messages = MessageCollection::new();
        let ret_val = { // to satisfy liefetime checker
            let ret_val = Self::parse(tokens, &mut messages, 0);
            check_messages_continuable!(&mut messages);
            ret_val
        };
        return (ret_val.0, ret_val.1, messages);
    }
}
#[cfg(feature = "parse_sess")]
pub trait ISyntaxItemWithStr {

    fn with_test_str(program: &str) -> Self where Self: Sized + ISyntaxItemParseX {
        let full = Self::with_test_str_ret_size_messages(program);
        check_messages_continuable!(full.2);
        return full.0.unwrap();
    }
    fn with_test_str_ret_size(program: &str) -> (Option<Self>, usize) where Self: Sized + ISyntaxItemParseX {
        let full = Self::with_test_str_ret_size_messages(program);
        return (full.0, full.1);
    }
    fn with_test_str_ret_messages(program: &str) -> (Option<Self>, MessageCollection) where Self: Sized + ISyntaxItemParseX {
        let full = Self::with_test_str_ret_size_messages(program);
        return (full.0, full.2);
    }
    fn with_test_str_ret_size_messages(program: &str) -> (Option<Self>, usize, MessageCollection) where Self: Sized + ISyntaxItemParseX {
        let tokens = TokenStream::with_test_str(program);
        let mut messages = MessageCollection::new();
        let ret_val = { // to satisfy liefetime checker
            let mut sess = ParseSession::new(&tokens, &mut messages);
            let retval = match Self::parsex(&mut sess) {
                Ok(retval) => Some(retval),
                Err(_) => None,
            };
            let size = sess.get_current_index();
            (retval, size)
        };
        return (ret_val.0, ret_val.1, messages);
    }
}
#[cfg(not(feature = "parse_sess"))]
impl<T> ISyntaxItemWithStr for T where T: ISyntaxItemParse {
}
#[cfg(feature = "parse_sess")]
impl<T> ISyntaxItemWithStr for T where T: ISyntaxItemParseX {
}

pub trait ISyntaxItemGrammar {

    /// Check current state is acceptable final
    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool;
}

// Format
// const INDENT_STRS: [&'static str; 16] = [
//     "", "| ", "| | ", "| | | ", "| | | | ", "| | | | | ", "| | | | | | ", "| | | | | | | ", "| | | | | | | | ", "| | | | | | | | | ", "| | | | | | | | | | ",
//     "| | | | | | | | | | | ", "| | | | | | | | | | | | ", "| | | | | | | | | | | | | ", "| | | | | | | | | | | | | | ", "| | | | | | | | | | | | | "
// ];
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
#[macro_export] macro_rules! impl_debug_for_format {
    ($t: ty) => (
        impl fmt::Debug for $t {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
        }
    )
}

#[macro_export]
macro_rules! push_unexpect {
    ($lexer: expr, $messages: expr, [$($final_tokens: expr, )+], $index: expr, $ret_size: expr) => ({
        use util::format_vector_display;

        let desc = format!("Expect {}", format_vector_display(&vec![$($final_tokens, )+], ", "));
        let actual_token_desc = format!("Meet {:?}", $lexer.nth($index));
        let strpos = $lexer.pos($index);

        $messages.push(Message::with_help("Unexpect symbol".to_owned(), 
            vec![(strpos, actual_token_desc)],
            vec![desc]
        ));

        (None, $ret_size)
    });
    ($lexer: expr, $messages: expr, $final_token: expr, $index: expr, $ret_size: expr) => ({
        push_unexpect!($lexer, $messages, [$final_token, ], $index, $ret_size)
    })
}