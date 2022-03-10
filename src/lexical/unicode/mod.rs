///! lexical::unicode: char extension for unicode, mainly for identifier, include confusable check by the way

#[cfg(test)]
mod tests;
mod security;
mod xid;
pub use security::check_confusable; 
pub use xid::{is_xid_start, is_xid_continue};
