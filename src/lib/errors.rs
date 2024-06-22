use std::error::Error;
use std::fmt::Display;
use std::io::{self, stderr, Write};
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub enum PredicateFailure {
    // We retain the StatFailed error as a string because io::Error
    // does not implement Clone.
    StatFailed(PathBuf, String),
    UnrecognisedFileMode(PathBuf, u32),
}

impl Display for PredicateFailure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PredicateFailure::StatFailed(name, e) => {
                writeln!(f, "failed to stat {}: {e}", name.display())
            }
            PredicateFailure::UnrecognisedFileMode(name, mode) => {
                writeln!(
                    f,
                    "failed to recognise mode {mode:#o} of file {}",
                    name.display()
                )
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum UsageError {
    Formatted(String),
    Encoded(Vec<u8>),
}

impl UsageError {
    pub fn report(&self) -> Result<(), io::Error> {
        match self {
            UsageError::Formatted(s) => {
                eprintln!("{s}");
                Ok(())
            }
            UsageError::Encoded(buf) => stderr().write_all(buf),
        }
    }
}

#[derive(Debug)]
pub struct ParseError(pub String);

impl Error for ParseError {}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
