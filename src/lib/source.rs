// This file is part of findutils-rs
// Copyright (C) 2024 James Youngman
//
// findutils-rs is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

use std::ffi::{OsStr, OsString};
use std::fmt::Display;

use super::metadata::FoundFile;

use super::errors::PredicateFailure;
use super::execute::VisitOutcome;
use super::options::Options;

#[cfg(feature = "fts_rs")]
mod fts_rs;
#[cfg(feature = "fts_sys")]
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

#[cfg(feature = "fts_rs")]
pub fn new_source(options: &Options, start_points: &[&OsStr]) -> Result<impl Source, Error> {
    fts_rs::FtsRsSource::new(options, start_points)
}

#[cfg(feature = "fts_sys")]
pub fn new_source(options: &Options, start_points: &[&OsStr]) -> Result<impl Source, Error> {
    fts_sys::FtsSysSource::new(options, start_points)
}
