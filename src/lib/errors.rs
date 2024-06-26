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
