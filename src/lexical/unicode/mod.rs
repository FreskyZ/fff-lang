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

    fn is_numeric_start(self) -> bool;
    fn is_numeric_continue(self) -> bool;
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

    // Only digit, '.' start is not supported
    // Update: remove '-' here, 
    //     that is, take several weeks to impl '-' and 'E' feature in num lit, but finally remove the feature in v2
    //     leave the feature in num lit parser for future use of FromStr
    fn is_numeric_start(self) -> bool {
        self.is_digit(10)
    }
    // Only digit or ASCII letters or underscore
    fn is_numeric_continue(self) -> bool {
        self == '_' || self.is_digit(36) || self == '.'
    }
}
