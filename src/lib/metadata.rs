use std::borrow::Cow;
use std::cell::OnceCell;
use std::ffi::OsStr;
use std::fmt::Display;
use std::fs::{self, Metadata};
use std::path::Path;

use crate::platform;

use super::errors::{ParseError, PredicateFailure};

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
    /// Path from the start point to the file we're considering (including the start point).
    pub reported_path: Cow<'a, Path>,
    /// Path from the current directory to the file we're considering.
    pub access_path: Cow<'a, Path>,
    pub details: OnceCell<Result<FileDetails, PredicateFailure>>,
    pub depth: i32,
}

impl<'a> FoundFile<'a> {
    pub fn details(&self) -> Result<&FileDetails, PredicateFailure> {
        let result = self
            .details
            .get_or_init(|| match fs::metadata(self.access_path()) {
                Ok(md) => FileDetails::from_metadata(&md, self.access_path(), self.reported_path()),
                Err(e) => Err(PredicateFailure::StatFailed(
                    self.access_path.to_path_buf(),
                    e.to_string(),
                )),
            });
        match result {
            Ok(details) => Ok(details),
            Err(e) => Err(e.clone()),
        }
    }

    pub fn access_path(&self) -> &Path {
        self.access_path.as_ref()
    }

    pub fn reported_path(&self) -> &Path {
        self.reported_path.as_ref()
    }
}

#[derive(Debug)]
pub struct FileDetails {
    pub file_type: TypePredicateFileType,
}

impl FileDetails {
    pub fn file_type(&self) -> TypePredicateFileType {
        self.file_type
    }

    pub fn from_metadata(
        md: &Metadata,
        _access_path: &Path,
        reported_path: &Path,
    ) -> Result<FileDetails, PredicateFailure> {
        Ok(FileDetails {
            file_type: platform::file_type_from_metadata(md, reported_path)?,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TypePredicateFileType {
    BlockDevice,
    CharacterDevice,
    Directory,
    NamedPipe,
    RegularFile,
    SymbolicLink,
    Socket,
    Door,
}

impl Display for TypePredicateFileType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            TypePredicateFileType::BlockDevice => "b",
            TypePredicateFileType::CharacterDevice => "c",
            TypePredicateFileType::Directory => "d",
            TypePredicateFileType::NamedPipe => "p",
            TypePredicateFileType::RegularFile => "f",
            TypePredicateFileType::SymbolicLink => "l",
            TypePredicateFileType::Socket => "s",
            TypePredicateFileType::Door => "D",
        })
    }
}

impl TryFrom<&OsStr> for TypePredicateFileType {
    type Error = ParseError;

    fn try_from(value: &OsStr) -> Result<Self, Self::Error> {
        use TypePredicateFileType::*;
        match value.to_str() {
            Some("b") => Ok(BlockDevice),
            Some("c") => Ok(CharacterDevice),
            Some("d") => Ok(Directory),
            Some("p") => Ok(NamedPipe),
            Some("f") => Ok(RegularFile),
            Some("l") => Ok(SymbolicLink),
            Some("s") => Ok(Socket),
            Some("D") => Ok(Door),
            Some(other) => Err(ParseError(format!("invalid file type: -type {other}"))),
            None => Err(ParseError(format!(
                "invalid file type: (because it is not a valid ASCII letter)"
            ))),
        }
    }
}

impl TypePredicateFileType {
    pub fn as_str(&self) -> &'static str {
        match self {
            TypePredicateFileType::BlockDevice => "b",
            TypePredicateFileType::CharacterDevice => "c",
            TypePredicateFileType::Directory => "d",
            TypePredicateFileType::NamedPipe => "p",
            TypePredicateFileType::RegularFile => "f",
            TypePredicateFileType::SymbolicLink => "l",
            TypePredicateFileType::Socket => "s",
            TypePredicateFileType::Door => "D",
        }
    }
}
