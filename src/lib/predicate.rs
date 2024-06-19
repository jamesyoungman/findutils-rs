use std::ffi::{OsStr, OsString};
use std::fmt::Display;

use crate::source::VisitType;

use super::ast::Predicate;
use super::errors::{ParseError, PredicateFailure};
use super::options::{GlobalOptionWithArg, GlobalOptionWithoutArg};
use super::platform;

#[derive(Debug)]
pub struct PrintPredicate {}

impl Predicate for PrintPredicate {
    fn eval(&self, target: &crate::ast::Target) -> Result<bool, PredicateFailure> {
        println!("{}", target.path.display());
        Ok(true)
    }

    fn inhibits_default_print(&self) -> bool {
        true
    }

    fn display_args(&self) -> Vec<String> {
        vec!["-print".to_string()]
    }
}

impl PrintPredicate {
    pub fn new() -> PrintPredicate {
        PrintPredicate {}
    }
}

#[derive(Debug, PartialEq, Eq)]
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
    fn as_str(&self) -> &'static str {
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

#[derive(Debug)]
pub struct TypePredicate(pub TypePredicateFileType);

impl Predicate for TypePredicate {
    fn eval(&self, target: &crate::ast::Target) -> Result<bool, PredicateFailure> {
        match &target.visiting {
            VisitType::Directory(_) => Ok(self.0 == TypePredicateFileType::Directory),
            VisitType::File => Ok(self.0 == TypePredicateFileType::RegularFile),
            VisitType::Symlink(_isbroken) => Ok(self.0 == TypePredicateFileType::SymbolicLink),
            VisitType::Unknown => match target.metadata() {
                Ok(md) => {
                    let ft = md.file_type();
                    match self.0 {
                        TypePredicateFileType::BlockDevice => Ok(platform::is_block_device(&md)),
                        TypePredicateFileType::CharacterDevice => Ok(platform::is_char_device(&md)),
                        TypePredicateFileType::Directory => Ok(ft.is_dir()),
                        TypePredicateFileType::NamedPipe => Ok(platform::is_named_pipe(&md)),
                        TypePredicateFileType::RegularFile => Ok(ft.is_file()),
                        TypePredicateFileType::SymbolicLink => Ok(ft.is_symlink()),
                        TypePredicateFileType::Socket => Ok(platform::is_unix_domain_socket(&md)),
                        TypePredicateFileType::Door => Ok(platform::is_door(&md)),
                    }
                }
                Err(e) => Err(e.clone()),
            },
        }
    }

    fn inhibits_default_print(&self) -> bool {
        false
    }

    fn display_args(&self) -> Vec<String> {
        vec!["-type".to_string(), self.0.as_str().to_string()]
    }
}

impl TypePredicate {
    pub fn new(arg: &OsStr) -> Result<TypePredicate, ParseError> {
        Ok(TypePredicate(TypePredicateFileType::try_from(arg)?))
    }
}

#[derive(Debug)]
pub struct TruePredicate {}

impl Predicate for TruePredicate {
    fn eval(&self, _target: &crate::ast::Target) -> Result<bool, PredicateFailure> {
        Ok(true)
    }

    fn display_args(&self) -> Vec<String> {
        vec!["-true".to_string()]
    }

    fn inhibits_default_print(&self) -> bool {
        false
    }
}

#[derive(Debug)]
pub struct FalsePredicate {}

impl Predicate for FalsePredicate {
    fn eval(&self, _target: &crate::ast::Target) -> Result<bool, PredicateFailure> {
        Ok(false)
    }

    fn display_args(&self) -> Vec<String> {
        vec!["-false".to_string()]
    }

    fn inhibits_default_print(&self) -> bool {
        false
    }
}

#[derive(Debug)]
pub enum GlobalOptionPlaceholder {
    WithoutArg(GlobalOptionWithoutArg),
    WithArg(GlobalOptionWithArg, OsString),
}

impl GlobalOptionPlaceholder {
    pub fn without_arg(opt: GlobalOptionWithoutArg) -> GlobalOptionPlaceholder {
        GlobalOptionPlaceholder::WithoutArg(opt)
    }
    pub fn with_arg(opt: GlobalOptionWithArg, arg: &OsStr) -> GlobalOptionPlaceholder {
        GlobalOptionPlaceholder::WithArg(opt, arg.to_os_string())
    }
}

impl Predicate for GlobalOptionPlaceholder {
    fn eval(&self, _target: &crate::ast::Target) -> Result<bool, PredicateFailure> {
        Ok(true)
    }

    fn display_args(&self) -> Vec<String> {
        match self {
            GlobalOptionPlaceholder::WithoutArg(opt) => vec![format!("{opt}")],
            GlobalOptionPlaceholder::WithArg(opt, arg) => {
                vec![format!("{opt}"), arg.to_string_lossy().to_string()]
            }
        }
    }

    fn inhibits_default_print(&self) -> bool {
        false
    }
}
