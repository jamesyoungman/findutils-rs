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

use std::fs::Metadata;
use std::os::linux::fs::MetadataExt;
use std::path::Path;

use super::super::{errors::PredicateFailure, metadata::TypePredicateFileType};

pub fn file_type_from_metadata(
    md: &Metadata,
    path: &Path,
) -> Result<TypePredicateFileType, PredicateFailure> {
    if md.is_file() {
        return Ok(TypePredicateFileType::RegularFile);
    } else if md.is_dir() {
        return Ok(TypePredicateFileType::Directory);
    } else if md.is_symlink() {
        return Ok(TypePredicateFileType::SymbolicLink {});
    }
    let mode = md.st_mode();
    match super::file_type_from_stat_st_mode(mode) {
        Some(file_type) => Ok(file_type),
        None => Err(PredicateFailure::UnrecognisedFileMode(
            path.to_path_buf(),
            mode,
        )),
    }
}
