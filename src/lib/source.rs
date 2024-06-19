use std::cell::OnceCell;
use std::ffi::{OsStr, OsString};
use std::fmt::Display;
use std::fs::Metadata;
use std::path::Path;

use super::errors::PredicateFailure;
use super::execute::VisitOutcome;
use super::options::Options;

#[derive(Debug)]
pub enum Error {
    SetupFailed(String),
    StartPointUnsupported(OsString),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::SetupFailed(msg) => write!(f, "initialisation failed: {msg}"),
            Error::StartPointUnsupported(_) => {
                f.write_str("non-UTF-8 start points are not yet supported")
            }
        }
    }
}

impl std::error::Error for Error {}

#[derive(Debug)]
pub enum DirectoryVisitType {
    Preorder,
    Postorder,
    Cycle,
    Unreadable,
}

#[derive(Debug)]
pub struct SymlinkVisitType {
    //broken: bool,
}

#[derive(Debug)]
pub enum VisitType {
    Unknown,
    Directory(DirectoryVisitType),
    Symlink(SymlinkVisitType),
    File,
}

#[derive(Debug)]
pub struct FoundFile<'a> {
    pub visiting: VisitType,
    pub path: &'a Path,
    pub metadata: OnceCell<Result<Metadata, PredicateFailure>>,
    pub depth: i32,
}

impl<'a> FoundFile<'a> {
    pub fn metadata(&self) -> &Result<Metadata, PredicateFailure> {
        self.metadata.get_or_init(|| {
            std::fs::metadata(self.path)
                .map_err(|e| PredicateFailure::StatFailed(self.path.to_path_buf(), e.to_string()))
        })
    }
}

pub trait Source {
    fn visit_all<F>(&mut self, visitor: F) -> Result<(), PredicateFailure>
    where
        F: FnMut(&FoundFile) -> Result<VisitOutcome, PredicateFailure>;
}

mod fts_rs {
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
                        eprintln!("error: {} is part of a self-referential cycle and will not be searched",
				  path.display());
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
}

pub fn new_source(options: &Options, start_points: &[&OsStr]) -> Result<impl Source, Error> {
    fts_rs::FtsRsSource::new(options, start_points)
}
