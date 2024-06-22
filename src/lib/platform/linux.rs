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
