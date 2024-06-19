use std::cell::OnceCell;
use std::ffi::{OsStr, OsString};
use std::fs;
use std::path::Path;

use fts::fts::fts_option::Flags;
use fts::fts::{Fts, FtsEntry, FtsError, FtsInfo, FtsSetOption};

use super::super::errors::PredicateFailure;
use super::super::execute::VisitOutcome;
use super::super::options::Options;
use super::super::NameResolutionMode;
use super::{DirectoryVisitType, Error, FoundFile, Source, SymlinkVisitType, VisitType};

pub struct FtsRsSource {
    fts: Fts,
}

impl FtsRsSource {
    pub fn new(options: &Options, start_points: &[&OsStr]) -> Result<FtsRsSource, Error> {
        let mut paths: Vec<String> = Vec::with_capacity(start_points.len());
        for start in start_points {
            match start.to_str() {
                None => {
                    let what: OsString = start.to_os_string();
                    return Err(Error::StartPointUnsupported(what));
                }
                Some(s) => {
                    paths.push(s.to_string());
                }
            }
        }

        let mut ftsflags = Flags::empty();
        // TODO: parsing the predicates will tell us if we should
        // set Flags::XDEV.  But see the (somewhat) recent
        // comments from the Austin group about the distinction
        // between -mount and -xdev.
        ftsflags.insert(match options.name_resolution() {
            NameResolutionMode::P => Flags::PHYSICAL,
            NameResolutionMode::L => Flags::LOGICAL,
            NameResolutionMode::H => Flags::COMFOLLOW,
        });

        let fts: Fts = match Fts::new(paths, ftsflags, None) {
            Ok(searcher) => searcher,
            Err(FtsError::PathWithNull) => unreachable!("NUL character in starting point"),
            Err(FtsError::SetFail) => {
                unreachable!("fts_set apparently failed but was not called")
            }
        };
        Ok(FtsRsSource { fts })
    }
}

fn convert_visit_type(ftsentry: &FtsEntry) -> VisitType {
    use DirectoryVisitType::*;
    use VisitType::*;
    match ftsentry.info {
        fts::fts::FtsInfo::IsDir => Directory(Preorder),
        fts::fts::FtsInfo::IsDirCyclic => Directory(Cycle),
        fts::fts::FtsInfo::IsDefault => Unknown,
        fts::fts::FtsInfo::IsDontRead => Directory(Unreadable),
        fts::fts::FtsInfo::IsDot => {
            // The option selection should prevent this.
            unreachable!("should not visit '.'");
        }
        fts::fts::FtsInfo::IsDirPost => Directory(Postorder),
        fts::fts::FtsInfo::IsFile => File,
        fts::fts::FtsInfo::IsSymlink => Symlink(SymlinkVisitType { /* broken: false */ }),
        fts::fts::FtsInfo::IsSymlinkNone => Symlink(SymlinkVisitType { /* broken: true */ }),
        fts::fts::FtsInfo::IsErr
        | fts::fts::FtsInfo::IsNoStat
        | fts::fts::FtsInfo::IsNoStatOk
        | fts::fts::FtsInfo::IsUnknown => Unknown,
    }
}
fn convert_metadata(ftsentry: &FtsEntry) -> Option<Result<fs::Metadata, PredicateFailure>> {
    if matches!(ftsentry.info, FtsInfo::IsNoStatOk) {
        None
    } else {
        // TODO: can we avoid the clone here?
        ftsentry.stat.as_ref().map(|md| Ok(md.clone()))
    }
}

fn convert_ftsentry(ftsentry: &FtsEntry) -> FoundFile {
    let visiting = convert_visit_type(&ftsentry);
    let path: &Path = &ftsentry.path;
    FoundFile {
        visiting,
        path,
        metadata: if let Some(md) = convert_metadata(&ftsentry) {
            let cell = OnceCell::new();
            cell.set(md).expect("cell should have been empty");
            cell
        } else {
            OnceCell::new()
        },
        depth: ftsentry.level,
    }
}

impl Source for FtsRsSource {
    fn visit_all<F>(&mut self, mut visitor: F) -> Result<(), PredicateFailure>
    where
        F: FnMut(&super::FoundFile) -> Result<VisitOutcome, PredicateFailure>,
    {
        while let Some(ftsentry) = self.fts.read() {
            let found = convert_ftsentry(&ftsentry);
            match visitor(&found) {
                Ok(VisitOutcome::Continue) => (),
                Ok(VisitOutcome::Cycle(path)) => {
                    eprintln!(
                        "error: {} is part of a self-referential cycle and will not be searched",
                        path.display()
                    );
                }
                Ok(VisitOutcome::PruneChildren) => {
                    if let Err(e) = self.fts.set(&ftsentry, FtsSetOption::Skip) {
                        panic!("fts.set unexpectedly failed: {e:?}");
                    }
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }
        Ok(())
    }
}
