use std::cell::OnceCell;
use std::ffi::{OsStr, OsString};
use std::fmt::Display;
use std::fs::Metadata;
use std::path::Path;

use super::errors::PredicateFailure;
use super::execute::VisitOutcome;
use super::options::Options;

mod fts_rs;

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

#[derive(Debug)]
pub enum DirectoryVisitType {
    Preorder,
    Postorder,
    Cycle,
    Unreadable,
}

#[derive(Debug)]
pub struct SymlinkVisitType {
    //broken: bool,
}

#[derive(Debug)]
pub enum VisitType {
    Unknown,
    Directory(DirectoryVisitType),
    Symlink(SymlinkVisitType),
    File,
}

#[derive(Debug)]
pub struct FoundFile<'a> {
    pub visiting: VisitType,
    pub path: &'a Path,
    pub metadata: OnceCell<Result<Metadata, PredicateFailure>>,
    pub depth: i32,
}

impl<'a> FoundFile<'a> {
    pub fn metadata(&self) -> &Result<Metadata, PredicateFailure> {
        self.metadata.get_or_init(|| {
            std::fs::metadata(self.path)
                .map_err(|e| PredicateFailure::StatFailed(self.path.to_path_buf(), e.to_string()))
        })
    }
}

pub trait Source {
    fn visit_all<F>(&mut self, visitor: F) -> Result<(), PredicateFailure>
    where
        F: FnMut(&FoundFile) -> Result<VisitOutcome, PredicateFailure>;
}

pub fn new_source(options: &Options, start_points: &[&OsStr]) -> Result<impl Source, Error> {
    fts_rs::FtsRsSource::new(options, start_points)
}
