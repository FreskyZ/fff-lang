
// Some common

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

