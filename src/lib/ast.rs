use std::fmt::{Debug, Display, Write};

use downcast_rs::{impl_downcast, Downcast};

use super::errors::PredicateFailure;
use super::source::FoundFile;

pub type Target<'a> = FoundFile<'a>;

pub trait Predicate: Debug + Downcast {
    fn eval(&self, target: &Target) -> Result<bool, PredicateFailure>;
    fn display_args(&self) -> Vec<String>;
    fn inhibits_default_print(&self) -> bool;
}

impl Display for dyn Predicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        for arg in self.display_args() {
            if first {
                first = false;
            } else {
                f.write_char(' ')?;
            }
            write!(f, "{}", arg)?;
        }
        Ok(())
    }
}

impl_downcast!(Predicate);

pub type BoxedPredicate = Box<dyn Predicate + Send + Sync>;

#[derive(PartialEq, Eq, Debug, Copy, Clone, Hash)]
pub enum BinaryOperationKind {
    Comma,
    And,
    Or,
}

impl Display for BinaryOperationKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            BinaryOperationKind::Comma => ",",
            BinaryOperationKind::And => "-a",
            BinaryOperationKind::Or => "-or",
        })
    }
}

#[derive(Debug)]
pub struct BinaryOperation {
    kind: BinaryOperationKind,
    pub children: Vec<Expression>,
}

impl BinaryOperation {
    pub fn new(kind: BinaryOperationKind, children: Vec<Expression>) -> BinaryOperation {
        BinaryOperation { kind, children }
    }

    pub fn kind(&self) -> &BinaryOperationKind {
        &self.kind
    }

    pub fn children(&self) -> impl Iterator<Item = &Expression> {
        self.children.iter()
    }
}

#[derive(Debug)]
pub enum Expression {
    BinaryOp(BinaryOperation),
    Not(Box<Expression>),
    Just(BoxedPredicate),
}

impl Predicate for Expression {
    fn eval(&self, target: &Target) -> Result<bool, PredicateFailure> {
        match self {
            Expression::BinaryOp(op) => op.eval(target),
            Expression::Not(expr) => expr.eval(target).map(|value| !value),
            Expression::Just(pred) => pred.eval(target),
        }
    }

    fn inhibits_default_print(&self) -> bool {
        match self {
            Expression::BinaryOp(op) => op.inhibits_default_print(),
            Expression::Not(expr) => expr.inhibits_default_print(),
            Expression::Just(expr) => expr.inhibits_default_print(),
        }
    }

    fn display_args(&self) -> Vec<String> {
        match self {
            Expression::BinaryOp(op) => {
                let mut result: Vec<String> = vec!["(".to_string()];
                result.extend(op.display_args());
                result.push(")".to_string());
                result
            }
            Expression::Not(expr) => {
                let mut result = vec!["!".to_string()];
                result.extend(expr.display_args());
                result
            }
            Expression::Just(pred) => pred.display_args(),
        }
    }
}
