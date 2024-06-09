use fts::fts::{FtsEntry, FtsInfo};
use std::borrow::Cow;

use super::ast::{BinaryOperation, BinaryOperationKind, Expression, Predicate, Target};
use super::errors::PredicateFailure;

impl Predicate for BinaryOperation {
    fn eval(&self, target: &Target) -> Result<bool, PredicateFailure> {
        match self.kind() {
            BinaryOperationKind::KeepLast => self
                .children()
                .try_fold(true, |_, action| action.eval(target)),
            BinaryOperationKind::And => self
                .children()
                .try_fold(true, |acc, action| Ok(acc && action.eval(target)?)),
            BinaryOperationKind::Or => self
                .children()
                .try_fold(true, |acc, action| Ok(acc || action.eval(target)?)),
        }
    }

    fn inhibits_default_print(&self) -> bool {
        self.children()
            .any(|action| action.inhibits_default_print())
    }

    fn display_args<'a>(&self) -> Vec<std::borrow::Cow<'a, str>> {
        let result: Vec<Cow<'_, str>> =
            Vec::with_capacity(self.children.len().checked_mul(2).unwrap_or(1));
        let operator: &'static str = match self.kind() {
            BinaryOperationKind::KeepLast => ",",
            BinaryOperationKind::And => "-a",
            BinaryOperationKind::Or => "-o",
        };
        self.children.iter().fold(result, |mut acc, child| {
            if !acc.is_empty() {
                acc.push(Cow::from(operator));
            }
            acc.extend(child.display_args());
            acc
        })
    }
}

pub fn visit(prog: &Expression, entry: &FtsEntry) -> Result<(), PredicateFailure> {
    match entry.info {
        FtsInfo::IsDirPost => {
            // TODO: we will probably need to use this case to
            // complete any pending executions for -execdir.
            Ok(())
        }
        FtsInfo::IsNoStat => Err(PredicateFailure::StatFailed(entry.path.to_path_buf())),
        _ => prog.eval(entry).map(|_| ()),
    }
}
