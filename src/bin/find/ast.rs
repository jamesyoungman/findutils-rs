use std::fmt::Debug;

use downcast_rs::{impl_downcast, Downcast};
use fts::fts::FtsEntry;

use super::errors::PredicateFailure;

pub type Target = FtsEntry;

pub trait Predicate: Debug + Downcast {
    fn eval(&self, target: &Target) -> Result<bool, PredicateFailure>;
    fn inhibits_default_print(&self) -> bool;
}

impl_downcast!(Predicate);

pub type BoxedPredicate = Box<dyn Predicate + Send + Sync>;

#[derive(Debug, Copy, Clone)]
pub enum BinaryOperationKind {
    KeepLast, // like find's comma operator
    And,
    Or,
}

#[derive(Debug)]
pub struct BinaryOperation {
    kind: BinaryOperationKind,
    children: Vec<Expression>,
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
}
