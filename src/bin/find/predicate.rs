use super::ast::Predicate;
use super::errors::{ParseError, PredicateFailure};

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
}

impl PrintPredicate {
    pub fn new() -> PrintPredicate {
        PrintPredicate {}
    }
}

#[derive(Debug)]
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

impl TryFrom<&str> for TypePredicateFileType {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        use TypePredicateFileType::*;
        match value {
            "b" => Ok(BlockDevice),
            "c" => Ok(CharacterDevice),
            "d" => Ok(Directory),
            "p" => Ok(NamedPipe),
            "f" => Ok(RegularFile),
            "l" => Ok(SymbolicLink),
            "s" => Ok(Socket),
            "D" => Ok(Door),
            _ => Err(ParseError(format!("invalid file type: -type {value}"))),
        }
    }
}

#[derive(Debug)]
pub struct TypePredicate(pub TypePredicateFileType);

impl Predicate for TypePredicate {
    fn eval(&self, _target: &crate::ast::Target) -> Result<bool, PredicateFailure> {
        todo!("implement -type {:?}", self.0)
    }

    fn inhibits_default_print(&self) -> bool {
        false
    }
}

impl TypePredicate {
    pub fn new(arg: &str) -> Result<TypePredicate, ParseError> {
        Ok(TypePredicate(TypePredicateFileType::try_from(arg)?))
    }
}
