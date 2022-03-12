///! lexical::unicode: char extension for unicode, mainly for identifier, include confusable check by the way

#[cfg(test)]
mod tests;
mod security;
mod xid;
pub use security::check_confusable;
use xid::{is_xid_start, is_xid_continue};

pub trait CharExt {

    fn is_id_start(self) -> bool;
    fn is_id_continue(self) -> bool;

    fn is_label_start(self) -> bool;
    fn is_label_continue(self) -> bool;
}

impl CharExt for char {

    fn is_id_start(self) -> bool {
        is_xid_start(self)
    }

    fn is_id_continue(self) -> bool {
        is_xid_continue(self)
    }

    fn is_label_start(self) -> bool {
        self == '@'
    }
    fn is_label_continue(self) -> bool {
        self == '@' || is_xid_continue(self)
    }
}
