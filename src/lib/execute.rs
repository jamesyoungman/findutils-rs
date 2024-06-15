use fts::fts::{FtsEntry, FtsInfo, FtsSetOption};
use std::borrow::Cow;

use super::ast::{BinaryOperation, BinaryOperationKind, Expression, Predicate, Target};
use super::errors::PredicateFailure;
use super::options::Options;

impl Predicate for BinaryOperation {
    fn eval(&self, target: &Target) -> Result<bool, PredicateFailure> {
        match self.kind() {
            BinaryOperationKind::Comma => self
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
            BinaryOperationKind::Comma => ",",
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

pub fn visit(
    prog: &Expression,
    entry: &FtsEntry,
    options: &Options,
) -> Result<Option<FtsSetOption>, PredicateFailure> {
    if entry.level < options.mindepth() {
        // Don't set the skip option, because we still want to visit
        // its descendants.
        return Ok(None);
    }
    match entry.info {
        FtsInfo::IsDirPost if !options.depth_first() => {
            // TODO: we will probably need to use this case to
            // complete any pending executions for -execdir.
            return Ok(None);
        }
        FtsInfo::IsDir if options.depth_first() => {
            return Ok(None);
        }
        FtsInfo::IsNoStat => {
            return Err(PredicateFailure::StatFailed(entry.path.to_path_buf()));
        }
        _ => {
            prog.eval(entry)?;
        }
    }
    if let Some(limit) = options.maxdepth() {
        if entry.level >= limit {
            // Skip the children of this entry.
            Ok(Some(FtsSetOption::Skip))
        } else {
            Ok(None)
        }
    } else {
        Ok(None)
    }
}
