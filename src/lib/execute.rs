use std::path::PathBuf;

use super::ast::{BinaryOperation, BinaryOperationKind, Expression, Predicate, Target};
use super::errors::PredicateFailure;
use super::metadata::{DirectoryVisitType, FoundFile, VisitType};
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

    fn display_args(&self) -> Vec<String> {
        let result: Vec<String> =
            Vec::with_capacity(self.children.len().checked_mul(2).unwrap_or(1));
        let operator: &'static str = match self.kind() {
            BinaryOperationKind::Comma => ",",
            BinaryOperationKind::And => "-a",
            BinaryOperationKind::Or => "-o",
        };
        self.children.iter().fold(result, |mut acc, child| {
            if !acc.is_empty() {
                acc.push(operator.to_string());
            }
            acc.extend(child.display_args());
            acc
        })
    }
}

#[derive(Debug)]
pub enum VisitOutcome {
    Continue,
    Cycle(PathBuf),
    PruneChildren,
}

pub fn visit(
    prog: &Expression,
    entry: &FoundFile,
    options: &Options,
) -> Result<VisitOutcome, PredicateFailure> {
    if entry.depth < options.mindepth() {
        // Don't set the skip option, because we still want to visit
        // its descendants.
        return Ok(VisitOutcome::Continue);
    }
    match &entry.visiting {
        VisitType::Directory(directory_visit_type) => match directory_visit_type {
            DirectoryVisitType::Cycle => {
                return Ok(VisitOutcome::Cycle(entry.reported_path.to_path_buf()));
            }
            DirectoryVisitType::Postorder if !options.depth_first() => {
                // TODO: we will probably need to use this case to
                // complete any pending executions for -execdir.
                return Ok(VisitOutcome::Continue);
            }
            DirectoryVisitType::Preorder if options.depth_first() => {
                return Ok(VisitOutcome::Continue);
            }
            DirectoryVisitType::Preorder | DirectoryVisitType::Postorder => {
                prog.eval(entry)?;
            }
            DirectoryVisitType::Unreadable => {
                if options.depth_first() {
                    // I would be surprised if fts implementations generally
                    // visit an unreadable directory in postorder.
                    return Ok(VisitOutcome::Continue);
                } else {
                    eprintln!(
                        "error: cannot visit children of {} because it is unreadable",
                        entry.reported_path.display()
                    );
                    return Ok(VisitOutcome::Continue);
                }
            }
        },
        _ => {
            prog.eval(entry)?;
        }
    }
    if let Some(limit) = options.maxdepth() {
        if entry.depth >= limit {
            // Skip the children of this entry.
            return Ok(VisitOutcome::PruneChildren);
        }
    }
    Ok(VisitOutcome::Continue)
}
