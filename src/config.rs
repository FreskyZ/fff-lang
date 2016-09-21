
// Error
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
impl_display_by_debug!(ConfigError);

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

use std::iter::FromIterator;
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

#[cfg(test)]
mod tests {

    #[test]
    fn config_from_args_test() {
        use super::Config;
        use super::CompileFileConfig;
        use super::ConfigError;
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
}