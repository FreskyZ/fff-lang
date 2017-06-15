///! fff-lang
///!
///! syntax/traits, for ISyntaxItem for various relevant staffs

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

// WithStr
pub trait ISyntaxItemWithStr {

    fn with_test_str(src: &str) 
        -> <Self as ISyntaxItemParse>::Target 
        where Self: Sized + ISyntaxItemParse {
        let full = Self::with_test_input_ret_size_messages(src, &mut SymbolCollection::new());
        check_messages_continuable!(full.2);
        return full.0.unwrap();
    }
    fn with_test_str_ret_size(src: &str) 
        -> (Option<<Self as ISyntaxItemParse>::Target>, usize) 
        where Self: Sized + ISyntaxItemParse {
        let full = Self::with_test_input_ret_size_messages(src, &mut SymbolCollection::new());
        return (full.0, full.1);
    }
    fn with_test_str_ret_messages(src: &str) 
        -> (Option<<Self as ISyntaxItemParse>::Target>, MessageCollection) 
        where Self: Sized + ISyntaxItemParse {
        let full = Self::with_test_input_ret_size_messages(src, &mut SymbolCollection::new());
        return (full.0, full.2);
    }
    fn with_test_str_ret_size_messages(src: &str) 
        -> (Option<<Self as ISyntaxItemParse>::Target>, usize, MessageCollection) 
        where Self: Sized + ISyntaxItemParse {
        Self::with_test_input_ret_size_messages(src, &mut SymbolCollection::new())
    }

    fn with_test_input(src: &str, symbols: &mut SymbolCollection) -> <Self as ISyntaxItemParse>::Target where Self: Sized + ISyntaxItemParse {
        let full = Self::with_test_input_ret_size_messages(src, symbols);
        check_messages_continuable!(full.2);
        return full.0.unwrap();
    }
    fn with_test_input_ret_size(src: &str, symbols: &mut SymbolCollection) 
        -> (Option<<Self as ISyntaxItemParse>::Target>, usize) 
        where Self: Sized + ISyntaxItemParse {
        let full = Self::with_test_input_ret_size_messages(src, symbols);
        return (full.0, full.1);
    }
    fn with_test_input_ret_messages(src: &str, symbols: &mut SymbolCollection) 
        -> (Option<<Self as ISyntaxItemParse>::Target>, MessageCollection)
        where Self: Sized + ISyntaxItemParse {
        let full = Self::with_test_input_ret_size_messages(src, symbols);
        return (full.0, full.2);
    }
    fn with_test_input_ret_size_messages(src: &str, symbols: &mut SymbolCollection) 
        -> (Option<<Self as ISyntaxItemParse>::Target>, usize, MessageCollection) 
        where Self: Sized + ISyntaxItemParse {
        let tokens = TokenStream::with_test_input(src, symbols);
        let mut messages = MessageCollection::new();
        let ret_val = { // to satisfy liefetime checker
            let mut sess = ParseSession::new(&tokens, &mut messages, symbols);
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