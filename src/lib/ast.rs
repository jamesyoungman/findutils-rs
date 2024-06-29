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

use std::fmt::{Debug, Display, Write};

use downcast_rs::{impl_downcast, Downcast};

use super::effects::EffectSink;
use super::errors::PredicateFailure;
use super::metadata::FoundFile;

pub type Target<'a> = FoundFile<'a>;

pub trait Predicate: Debug + Downcast {
    fn eval(
        &self,
        target: &Target,
        sink: &mut Box<dyn EffectSink>,
    ) -> Result<bool, PredicateFailure>;
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
    fn eval(
        &self,
        target: &Target,
        sink: &mut Box<dyn EffectSink>,
    ) -> Result<bool, PredicateFailure> {
        match self {
            Expression::BinaryOp(op) => op.eval(target, sink),
            Expression::Not(expr) => expr.eval(target, sink).map(|value| !value),
            Expression::Just(pred) => pred.eval(target, sink),
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
