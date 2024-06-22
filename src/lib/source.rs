use std::ffi::{OsStr, OsString};
use std::fmt::Display;

use super::metadata::FoundFile;

use super::errors::PredicateFailure;
use super::execute::VisitOutcome;
use super::options::Options;

mod fts_rs;
mod fts_sys;

#[derive(Debug)]
pub enum Error {
    SetupFailed(String),
    StartPointUnsupported(OsString),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::SetupFailed(msg) => write!(f, "initialisation failed: {msg}"),
            Error::StartPointUnsupported(_) => {
                f.write_str("non-UTF-8 start points are not yet supported")
            }
        }
    }
}

impl std::error::Error for Error {}

pub trait Source {
    fn visit_all<F>(&mut self, visitor: F) -> Result<(), PredicateFailure>
    where
        F: FnMut(&FoundFile) -> Result<VisitOutcome, PredicateFailure>;
}

pub fn new_source(options: &Options, start_points: &[&OsStr]) -> Result<impl Source, Error> {
    //fts_rs::FtsRsSource::new(options, start_points)
    fts_sys::FtsSysSource::new(options, start_points)
}
