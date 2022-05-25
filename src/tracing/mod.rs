#![macro_use]
///! tracing: simple trace! macro and internals
///!
///! # Usage
///! ```text
///! let _event_source = trace!(enter "my pretty parser"); // event source in lexical scope
///! trace!("write something like print! or format! {some_variable} {}", some_other_variable);
///! ```
///!
///! # Setup
///! use environment variables
///!
///! ## `TRACE_OUTPUT`
///! 2 for stdout, 3 for stderr, other is regarded as output file name,
///! e.g. "my-trace-log-{tid}.txt", the {tid} is required and will be replaced by thread id
///! 
///! if not provided, trace is not enabled
///!
///! ## `TRACE_INPUT`
///! comma separated event source names, e.g. TRACE_INPUT="my pretty parser,my another source"
///!
///! if not provided, all trace event source is not enabled

pub use detail::*;

#[macro_export]
macro_rules! trace {
    ($scope:ident $name:literal) => (#[allow(unused_variables)] let $scope = crate::tracing::enter($name, true););
    ($scope:ident $name:literal if $cond:expr) => (#[allow(unused_variables)] let $scope = crate::tracing::enter($name, $cond););
    ($format:literal $($tt:tt)*) => (crate::tracing::write_with(|o, n| write!(o, "[{n}] {}\n", format_args!($format $($tt)*))));
}

#[cfg(debug_assertions)]
mod detail {
    use std::cell::RefCell;
    use std::{env, fs, io, thread};

    const DISABLED: &str = "<disabled>"; // disabled placeholder in source stack

    // the global (thread local) mutable state
    struct Data {
        output: Box<dyn io::Write>,
        accepts: Vec<String>, // whitelist for allowed event sources
        sources: Vec<&'static str>, // current event source stack, top one will be used in write invocations
    }

    // split environment variable, and make some limits
    fn get_input() -> Option<Vec<String>> {
        env::var("TRACE_INPUT").ok().map(|v| v
            .split(',')
            .take(32) // max 32 items
            .map(|s| s.trim())
            .filter(|s| !s.is_empty()) // remove empty item after trim
            .map(|s| s.chars().take(32).collect())
            .collect())
    }

    fn get_output() -> Option<Box<dyn io::Write>> {
        env::var("TRACE_OUTPUT").ok().map(|v| match v.as_str() {
            "2" => Box::new(io::stdout()) as Box<dyn io::Write>, // box<dyn> does not infer well, while this as amazingly works
            "3" => Box::new(io::stderr()),
            other => if !other.contains("{tid}") {
                eprintln!("[tracing] TRACE_OUTPUT provided as file name but missing {{tid}}");
                Box::new(io::stdout()) // any error
            } else {
                let file_name = other.replace("{tid}", &format!("{}", thread::current().id().as_u64()));
                match fs::OpenOptions::new().write(true).create(true).truncate(true).open(&file_name) {
                    Ok(file) => Box::new(file) as Box<dyn io::Write>,
                    Err(e) => {
                        eprintln!("[tracing] TRACE_OUTPUT provided as file name {} failed to open: {}", file_name, e);
                        Box::new(io::stderr())
                    }
                }
            }
        })
    }

    std::thread_local! {
        // none for disabled because of major environment variables not provided
        static DATA: Option<RefCell<Data>> = get_input()
            .and_then(|accepts| get_output()
            .map(|output| RefCell::new(Data{ output, accepts, sources: Vec::new() })))
    }

    // bool: need drop (pop)
    pub struct Guard;

    pub fn enter(source: &'static str, condition: bool) -> Guard {
        DATA.with(|opt| opt.as_ref().map(|cell| {
            let mut data = cell.borrow_mut();
            if condition && data.accepts.iter().any(|a| a == source) {
                data.sources.push(source);
            } else {
                data.sources.push(DISABLED);
            }
        }));
        Guard
    }

    impl Drop for Guard {
        fn drop(&mut self) {
            DATA.with(|opt| opt.as_ref().map(|cell| cell.borrow_mut().sources.pop()));
        }
    }

    pub fn write_with(f: impl FnOnce(&mut dyn io::Write, &'static str) -> io::Result<()>) {
        DATA.with(|opt| opt.as_ref().map(|cell| {
            let mut data = cell.borrow_mut();
            let source = if data.sources.is_empty() {
                eprintln!("[tracing] no event source name specified");
                ""
            } else {
                data.sources.last().unwrap()
            };
            if source != DISABLED {
                let _ = f(data.output.as_mut(), source);
            }
        }));
    }
}

#[cfg(not(debug_assertions))]
mod detail {
    pub fn enter(_: &'static str) {}
    pub fn write_with(_: impl FnOnce(&mut dyn io::Write, &'static str) -> io::Result<()>) {}
}
