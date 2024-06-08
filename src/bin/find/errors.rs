use std::error::Error;
use std::fmt::Display;
use std::path::PathBuf;

#[derive(Debug)]
pub enum PredicateFailure {
    StatFailed(PathBuf),
}

impl Display for PredicateFailure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PredicateFailure::StatFailed(name) => {
                writeln!(f, "failed to stat {}", name.display())
            }
        }
    }
}

pub struct UsageError(pub String);

#[derive(Debug)]
pub struct ParseError(pub String);

impl Error for ParseError {}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
