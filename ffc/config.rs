///! fff-lang
///!
///! config, argument parser

use std::iter::FromIterator;

#[derive(Eq, PartialEq)]
pub enum ConfigError {
    UnexpectedArgument(String),
}

use std::fmt;
impl fmt::Debug for ConfigError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ConfigError::*;
        match *self {
            UnexpectedArgument(ref arg) => {
                write!(f, "Unexpceted argument `{}`", arg)
            }
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub struct CompileFileConfig {
    pub file_name: String,
}

#[derive(Eq, PartialEq, Debug)]
pub enum Config {
    Help,
    Version,
    CompileFile(CompileFileConfig),
}
impl Config {

    pub fn from_args<T>(mut args: T) -> Result<Config, ConfigError>
      where T : ExactSizeIterator,
              <T as Iterator>::Item: AsRef<str> + fmt::Display,
              Vec<String>: FromIterator<<T as Iterator>::Item> {

        if args.len() == 1 {
            return Ok(Config::Help);
        }

        let _arg0 = args.next();
        if args.len() >= 2 {
            let _arg1 = args.next();
            let arg2 = args.next().unwrap();
            return Err(ConfigError::UnexpectedArgument(arg2.to_string()));
        }

        Ok(Config::CompileFile(CompileFileConfig {
            file_name: args.next().unwrap().to_string()
        }))
    }
}

#[cfg(test)] #[test]
fn config_from_args_test() {
    use std::env::args;

    assert_eq!(Ok(Config::Help), Config::from_args(vec!["smc".to_owned()].into_iter()));
    assert_eq!(Ok(Config::Help), Config::from_args(args()));
    assert_eq!(
        Err(ConfigError::UnexpectedArgument("somearg3".to_owned())), 
        Config::from_args(vec![
            "smc".to_owned(), "balabala".to_owned(), 
            "somearg3".to_owned(), "someargmore".to_owned()].into_iter()));   
    assert_eq!(
        Err(ConfigError::UnexpectedArgument("somearg3".to_owned())), 
        Config::from_args(vec![
            "smc".to_owned(), "balabala".to_owned(), 
            "somearg3".to_owned()].into_iter()));      
    assert_eq!(
        Ok(Config::CompileFile(CompileFileConfig { file_name: "balabala".to_owned() } )), 
        Config::from_args(vec![
            "smc".to_owned(), "balabala".to_owned()].into_iter()));
}