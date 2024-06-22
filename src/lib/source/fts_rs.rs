use std::borrow::Cow;
use std::cell::OnceCell;
use std::ffi::{OsStr, OsString};

use fts::fts::fts_option::Flags;
use fts::fts::{Fts, FtsEntry, FtsError, FtsInfo, FtsSetOption};

use crate::metadata::FileDetails;

use super::super::{
    errors::PredicateFailure,
    execute::VisitOutcome,
    metadata::{DirectoryVisitType, FoundFile, SymlinkVisitType, VisitType},
    options::Options,
    source::{Error, Source},
    NameResolutionMode,
};

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

fn convert_metadata(ftsentry: &FtsEntry) -> Option<Result<FileDetails, PredicateFailure>> {
    if matches!(ftsentry.info, FtsInfo::IsNoStatOk) {
        None
    } else {
        match ftsentry.stat.as_ref() {
            Some(md) => Some(FileDetails::from_metadata(
                md,
                &ftsentry.path,
                &ftsentry.path,
            )),
            None => None,
        }
    }
}

fn convert_ftsentry(ftsentry: &FtsEntry) -> FoundFile {
    let visiting = convert_visit_type(&ftsentry);
    FoundFile {
        visiting,
        reported_path: Cow::from(&ftsentry.path),
        // Gnulib's fts implementation has a fts_accpath member which is
        // the path name through which the file we're examining is
        // accessible when FTS_NOCHDIR is set.  But this is not part of
        // the BSD API and it is not exposed via fts-rs.
        access_path: Cow::from(&ftsentry.path),
        details: if let Some(details) = convert_metadata(&ftsentry) {
            let cell = OnceCell::new();
            cell.set(details).expect("cell should have been empty");
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
        F: FnMut(&FoundFile) -> Result<VisitOutcome, PredicateFailure>,
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
