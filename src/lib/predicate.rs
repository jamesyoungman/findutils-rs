use std::ffi::{OsStr, OsString};

use super::ast::Predicate;
use super::errors::{ParseError, PredicateFailure};
use super::metadata::TypePredicateFileType;
use super::options::{GlobalOptionWithArg, GlobalOptionWithoutArg};

#[derive(Debug)]
pub struct PrintPredicate {}

impl Predicate for PrintPredicate {
    fn eval(&self, target: &crate::ast::Target) -> Result<bool, PredicateFailure> {
        println!("{}", target.reported_path.display());
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

#[derive(Debug)]
pub struct TypePredicate(pub TypePredicateFileType);

impl Predicate for TypePredicate {
    fn eval(&self, target: &crate::ast::Target) -> Result<bool, PredicateFailure> {
        Ok(self.0 == target.details()?.file_type())
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
