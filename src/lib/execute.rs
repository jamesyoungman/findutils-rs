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

use std::path::PathBuf;

use super::ast::{BinaryOperation, BinaryOperationKind, Expression, Predicate, Target};
use super::effects::EffectSink;
use super::errors::PredicateFailure;
use super::metadata::{DirectoryVisitType, FoundFile, VisitType};
use super::options::Options;

impl Predicate for BinaryOperation {
    fn eval(
        &self,
        target: &Target,
        sink: &mut Box<dyn EffectSink>,
    ) -> Result<bool, PredicateFailure> {
        match self.kind() {
            BinaryOperationKind::Comma => self
                .children()
                .try_fold(true, |_, action| action.eval(target, sink)),
            BinaryOperationKind::And => self
                .children()
                .try_fold(true, |acc, action| Ok(acc && action.eval(target, sink)?)),
            BinaryOperationKind::Or => self
                .children()
                .try_fold(true, |acc, action| Ok(acc || action.eval(target, sink)?)),
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
    sink: &mut Box<dyn EffectSink>,
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
                prog.eval(entry, sink)?;
            }
            DirectoryVisitType::Unreadable => {
                if options.depth_first() {
                    // I would be surprised if fts implementations generally
                    // visit an unreadable directory in postorder.
                    return Ok(VisitOutcome::Continue);
                } else {
                    sink.emit_encoded_errors(
                        1,
                        &[
                            b"error: cannot visit children of ",
                            entry.reported_path.as_os_str().as_encoded_bytes(),
                            b" because it is unreadable",
                        ],
                    );
                    return Ok(VisitOutcome::Continue);
                }
            }
        },
        _ => {
            prog.eval(entry, sink)?;
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
