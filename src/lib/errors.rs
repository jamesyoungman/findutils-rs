use std::error::Error;
use std::fmt::Display;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub enum PredicateFailure {
    // We retain the StatFailed error as a string because io::Error
    // does not implement Clone.
    StatFailed(PathBuf, String),
}

impl Display for PredicateFailure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PredicateFailure::StatFailed(name, e) => {
                writeln!(f, "failed to stat {}: {e}", name.display())
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct UsageError(pub String);

#[derive(Debug)]
pub struct ParseError(pub String);

impl Error for ParseError {}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
