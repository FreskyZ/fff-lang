//! fff-lang
//! Some utils

/// Macro for printing to the standard error.
/// 
/// Equivalent to `print!` macro except that print to stderr
///
/// Standard error unsually is not buffered and displayed immediately, and 
/// default rust test configuration shut down stdout and keeps stderror open 
///
/// # Panics
/// 
/// Panics if writing to io::stderr() fails.
///
/// # Examples
/// ```rust
/// # #[macro_use] extern crate fsz_common;
/// # fn main() {
/// perror!("Hello ");
/// perror!("{}", "World");
/// perror!("!");      // Get `Hello World!` at stderr immediately
/// # }
/// ```
#[macro_export]
macro_rules! perror {
    ($($arg:tt)*) => ({
        use std::io::Write;
        let _ = write!(&mut ::std::io::stderr(), $($arg)* );
    })
}

/// Macros for printing to the standard output, with a newline
///
/// Use the `format!` syntax to write data to the standard error, see 
/// `std::fmt` for more information
///
/// # Panics
/// 
/// Panics if writing to `io::stderr()` fails
///
/// # Examples
/// ```rust
/// # #[macro_use] extern crate fsz_common;
/// # fn main() {
/// perrorln!("Hello world!");
/// perrorln!("format {} arguments", "some");
/// # }
/// ```
#[macro_export]
macro_rules! perrorln {
    ($($arg:tt)*) => ({
        use std::io::Write;
        let _ = writeln!(&mut ::std::io::stderr(), $($arg)* );
    })
}

/// test_only_attr!([derive(1)] [derive(2) Encodable] struct abc{})
#[macro_export]
macro_rules! test_only_attr {
    (
        test: [$($attr_test: meta)*] 
        not_test: [$($attr_build: meta)*] 
        $typedef: item
    ) => (
        #[cfg(test)]
        $(#[$attr_test])*
        $typedef

        #[cfg(not(test))]
        $(#[$attr_build])*
        $typedef
    );

    (
        [$($attr_test: meta)*] 
        ![$($attr_build: meta)*] 
        $typedef: item
    ) => (
        #[cfg(test)]
        $(#[$attr_test])*
        $typedef

        #[cfg(not(test))]
        $(#[$attr_build])*
        $typedef
    )
}

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

#[cfg(test)]
#[macro_export]
macro_rules! test_condition_perrorln {
    ($cond: expr, $format: expr, $($args: expr, )+) => (if $cond { perrorln!($format, $($args, )+); });
    ($cond: expr, $format: expr) => (if $cond { perrorln!($format); });
}
#[cfg(not(test))]
#[macro_export]
macro_rules! test_condition_perrorln {
    ($cond: expr, $format: expr, $($args: expr, )+) => ();
    ($cond: expr, $format: expr) => ();
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
