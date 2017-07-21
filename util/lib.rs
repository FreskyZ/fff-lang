///! fff-lang
///! Some utils

mod date_time;

pub use date_time::DateTime;

use std::fmt;
pub fn format_vector_display<T: fmt::Display>(items: &Vec<T>, sep: &str) -> String {
    let length = items.len();
    let mut buf = String::new();
    for (index, item) in items.iter().enumerate() {
        buf.push_str(&format!("{}", item));
        if index != length - 1 {
            buf.push_str(sep);
        }
    }
    buf
}
pub fn format_vector_debug<T: fmt::Debug>(items: &Vec<T>, sep: &str) -> String {
    let length = items.len();
    let mut buf = String::new();
    for (index, item) in items.iter().enumerate() {
        buf.push_str(&format!("{:?}", item));
        if index != length - 1 {
            buf.push_str(sep);
        }
    }
    buf
}

/// Implement `fmt::Display` by previous implementation of `fmt::Debug`
#[macro_export]
macro_rules! impl_display_by_debug {
    ($t: ty) => (
        impl fmt::Display for $t {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{:?}", self)
            }
        }
    )
}

/// Try convert from
pub trait TryFrom<T>
    where Self: Sized {
    fn try_from(t: T) -> Option<Self>;
}

/// ID type requires From<{integer}>
#[macro_export]
macro_rules! make_id {
    ($id: expr) => (From::from($id))
}