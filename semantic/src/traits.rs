///! fff-lang
///!
///! semantic/traits, common traits and herlpers

use message::MessageCollection;
use syntax::ISyntaxItemWithStr;
use syntax::ISyntaxItemParse;
use super::ResolveSession;
use super::SymbolCollection;

pub trait ISemanticItemFromSyntaxItem<T> {

    fn from_syntax_item(syntax_item: T, sess: &mut ResolveSession) -> Self where T: ISyntaxItemWithStr;
}

pub trait ISemanticItemWithStr<TSyntax> {

    fn with_test_str(program: &str, symbols: SymbolCollection) -> Self 
        where Self: Sized + ISemanticItemFromSyntaxItem<TSyntax>, TSyntax: ISyntaxItemWithStr + ISyntaxItemParse {
        let full = Self::with_test_str_ret_messages(program, symbols);
        check_messages_continuable!(full.1);
        return full.0.unwrap();
    }
    fn with_test_str_ret_messages(program: &str, symbols: SymbolCollection) -> (Option<Self>, MessageCollection)
        where Self: Sized + ISemanticItemFromSyntaxItem<TSyntax>, TSyntax: ISyntaxItemWithStr + ISyntaxItemParse {
        
        let (item, messages) = TSyntax::with_test_str_ret_messages(program);
        let mut sess = ResolveSession::new();
        sess.symbols = symbols;
        return (item.map(|item| Self::from_syntax_item(item, &mut sess)), messages);
    }
}
impl<TSemantic, TSyntax> ISemanticItemWithStr<TSyntax> for TSemantic 
    where TSyntax: ISyntaxItemWithStr + ISyntaxItemParse, TSemantic: ISemanticItemFromSyntaxItem<TSyntax> {
}


const INDENT_STRS: [&'static str; 16] = [
    "", "  ", "    ", "      ", "        ", "          ", "            ", "              ", "                ", "                  ", "                    ",
    "                      ", "                        ", "                          ", "                            ", "                          "
];
/// Same as ISyntaxItemFormat
pub trait ISemanticItemFormat {

    fn indent_str(indent: u32) -> &'static str {
        INDENT_STRS[indent as usize]
    }

    fn format(&self, indent: u32) -> String;
}