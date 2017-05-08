///! fff-lang
///!
///! syntax/traits, for ISyntaxItem for various relevant staffs

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
    
    fn parse(sess: &mut ParseSession) -> ParseResult<Self> where Self: Sized;

    // check is_first_final, if pass, parse, return Ok(Some(T)) or Err(()), else return None
    fn try_parse(sess: &mut ParseSession) -> ParseResult<Option<Self>> where Self: Sized + ISyntaxItemGrammar {
        if Self::is_first_final(sess) { Ok(Some(Self::parse(sess)?)) } else { Ok(None) }
    }
}

// WithStr
pub trait ISyntaxItemWithStr {

    fn with_test_str(program: &str) -> Self where Self: Sized + ISyntaxItemParse {
        let full = Self::with_test_str_ret_size_messages(program);
        check_messages_continuable!(full.2);
        return full.0.unwrap();
    }
    fn with_test_str_ret_size(program: &str) -> (Option<Self>, usize) where Self: Sized + ISyntaxItemParse {
        let full = Self::with_test_str_ret_size_messages(program);
        return (full.0, full.1);
    }
    fn with_test_str_ret_messages(program: &str) -> (Option<Self>, MessageCollection) where Self: Sized + ISyntaxItemParse {
        let full = Self::with_test_str_ret_size_messages(program);
        return (full.0, full.2);
    }
    fn with_test_str_ret_size_messages(program: &str) -> (Option<Self>, usize, MessageCollection) where Self: Sized + ISyntaxItemParse {
        let tokens = TokenStream::with_test_str(program);
        let mut messages = MessageCollection::new();
        let ret_val = { // to satisfy liefetime checker
            let mut sess = ParseSession::new(&tokens, &mut messages);
            let retval = match Self::parse(&mut sess) {
                Ok(retval) => Some(retval),
                Err(_) => None,
            };
            let size = sess.get_current_index();
            (retval, size)
        };
        return (ret_val.0, ret_val.1, messages);
    }
}
impl<T> ISyntaxItemWithStr for T where T: ISyntaxItemParse {
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