use std::borrow::Cow;
use std::cell::OnceCell;
use std::ffi::{c_char, c_int, CStr, CString, OsStr};
use std::fs;
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::ptr;
use std::slice;

use fts_sys::{
    fts_close, fts_open, fts_read, fts_set, FTS, FTSENT, FTS_COMFOLLOW, FTS_D, FTS_DC, FTS_DNR,
    FTS_DOT, FTS_DP, FTS_F, FTS_LOGICAL, FTS_NSOK, FTS_PHYSICAL, FTS_SKIP, FTS_SL, FTS_SLNONE,
};

use crate::metadata::TypePredicateFileType;

use super::super::{
    errors::PredicateFailure,
    execute::VisitOutcome,
    metadata::{DirectoryVisitType, FileDetails, FoundFile, SymlinkVisitType, VisitType},
    options::Options,
    platform,
    source::{Error, Source},
    NameResolutionMode,
};

pub struct FtsSysSource {
    paths: Vec<Pin<Box<CString>>>,
    inner: *mut FTS,
}

impl FtsSysSource {
    pub fn new(options: &Options, start_points: &[&OsStr]) -> Result<FtsSysSource, Error> {
        if start_points.is_empty() {
            return Err(Error::SetupFailed("start_points is empty".to_string()));
        }
        let fts_options: c_int = match options.name_resolution() {
            NameResolutionMode::P => FTS_PHYSICAL,
            NameResolutionMode::L => FTS_LOGICAL,
            NameResolutionMode::H => FTS_COMFOLLOW,
        };
        let mut paths: Vec<Pin<Box<CString>>> = Vec::with_capacity(start_points.len());
        for start in start_points.iter() {
            match CString::new(start.as_bytes()) {
                Ok(cs) => {
                    paths.push(Box::pin(cs));
                }
                Err(_) => {
                    // This start point is unsupported since it
                    // contains a NUL byte which isn't at the end.
                    return Err(Error::StartPointUnsupported(start.to_os_string()));
                }
            }
        }
        let mut start_points: Vec<*const c_char> = Vec::with_capacity(paths.len() + 1);
        start_points.extend(paths.iter().map(|path| path.as_ptr().cast()));
        start_points.push(ptr::null());
        let fts = unsafe { fts_open(start_points.as_ptr().cast(), fts_options, None) };
        if fts.is_null() {
            // On error, fts_open returns NULL and sets errno.
            let os_error = std::io::Error::last_os_error();
            return Err(Error::SetupFailed(os_error.to_string()));
        }
        Ok(FtsSysSource { paths, inner: fts })
    }
}

impl Drop for FtsSysSource {
    fn drop(self: &mut FtsSysSource) {
        unsafe {
            fts_close(self.inner);
            self.inner = ptr::null_mut();
        }
    }
}

#[derive(Debug)]
struct FtsentConversionFailed {}

fn ftsent_visiting(ftsent: &FTSENT) -> VisitType {
    use fts_sys::*;
    match ftsent.fts_info as i32 {
        FTS_D => VisitType::Directory(DirectoryVisitType::Preorder),
        FTS_DC => VisitType::Directory(DirectoryVisitType::Cycle),
        FTS_DEFAULT => VisitType::Unknown,
        FTS_DNR => VisitType::Directory(DirectoryVisitType::Unreadable),
        FTS_DOT => unreachable!("we did not call fts_open with FTS_SEEDOT"),
        FTS_DP => VisitType::Directory(DirectoryVisitType::Postorder),
        FTS_ERR => VisitType::Unknown,
        FTS_F => VisitType::File,
        FTS_NS => VisitType::Unknown,
        FTS_NSOK => VisitType::Unknown,
        FTS_SL => VisitType::Symlink(SymlinkVisitType{/* broken: false */}),
        FTS_SLNONE => VisitType::Symlink(SymlinkVisitType{/* broken: true. */}),
        _ => VisitType::Unknown,
    }
}

fn ftsent_details(ftsent: &FTSENT) -> Option<FileDetails> {
    const FLAG_TO_TYPE_MAP: &[(i32, TypePredicateFileType)] = &[
        (FTS_D, TypePredicateFileType::Directory),
        (FTS_DC, TypePredicateFileType::Directory),
        // FTS_DEFAULT is deliberately missing
        (FTS_DNR, TypePredicateFileType::Directory),
        (FTS_DOT, TypePredicateFileType::Directory),
        (FTS_DP, TypePredicateFileType::Directory),
        // FTS_ERR is deliberately missing
        (FTS_F, TypePredicateFileType::RegularFile),
        // FTS_NS is deliberately missing
        // FTS_NSOK is deliberately missing, see above.
        (FTS_SL, TypePredicateFileType::SymbolicLink),
        (FTS_SLNONE, TypePredicateFileType::SymbolicLink),
    ];
    FLAG_TO_TYPE_MAP
        .iter()
        .find_map(|(val, file_type)| {
            if &i32::from(ftsent.fts_info) == val {
                Some(*file_type)
            } else {
                None
            }
        })
        .map(|file_type| FileDetails { file_type })
}

fn convert_ftsent<'a>(ftsent: &FTSENT) -> Result<FoundFile<'a>, FtsentConversionFailed> {
    let reported_path = ftsent_reported_path(ftsent);
    let access_path = ftsent_access_path(ftsent);
    let mut details_result: Option<Result<FileDetails, PredicateFailure>> =
        match ftsent_details(ftsent) {
            Some(details) => {
                // We were able to collect the details from the output of
                // fts() at no additional cost.
                Some(Ok(details))
            }
            None => {
                if i32::from(ftsent.fts_info) == FTS_NSOK {
                    // We're deliberately deferring stat and similar, for
                    // efficiency.
                    None
                } else {
                    // We beliece the file details are needed (as
                    // fts_open() has not been called with FTS_NOSTAT).
                    //
                    // So now we pay the performance cost of touching the
                    // file system.
                    match fs::metadata(&access_path) {
                        Ok(md) => {
                            let file_type_result: Result<TypePredicateFileType, PredicateFailure> =
                                platform::file_type_from_metadata(&md, &reported_path);
                            Some(file_type_result.map(|file_type| FileDetails { file_type }))
                        }
                        Err(e) => Some(Err(PredicateFailure::StatFailed(
                            reported_path.to_path_buf(),
                            e.to_string(),
                        ))),
                    }
                }
            }
        };
    Ok(FoundFile {
        visiting: ftsent_visiting(ftsent),
        reported_path: Cow::from(reported_path),
        access_path: Cow::from(access_path),
        details: match details_result.take() {
            Some(d) => OnceCell::from(d),
            None => OnceCell::new(),
        },
        depth: ftsent.fts_level.into(),
    })
}

fn make_path(s: *const u8, len: usize) -> PathBuf {
    let slice = unsafe { slice::from_raw_parts(s, len) };
    Path::new(OsStr::from_bytes(slice)).to_path_buf()
}

fn ftsent_reported_path(ftsent: &FTSENT) -> PathBuf {
    let len = ftsent.fts_pathlen as usize;
    let iptr: *const i8 = ftsent.fts_path;
    let uptr: *const u8 = iptr as *const u8;
    make_path(uptr, len)
}

fn ftsent_access_path(ftsent: &FTSENT) -> PathBuf {
    let iptr: *const i8 = ftsent.fts_accpath;
    let cs = unsafe { CStr::from_ptr(iptr) };
    platform::cstr_to_pathbuf(cs)
}

impl Source for FtsSysSource {
    fn visit_all<F>(&mut self, mut visitor: F) -> Result<(), crate::errors::PredicateFailure>
    where
        F: FnMut(&FoundFile) -> Result<crate::VisitOutcome, crate::errors::PredicateFailure>,
    {
        loop {
            let entry = unsafe { fts_read(self.inner) };
            if entry.is_null() {
                break;
            }
            if let Some(ftsent) = unsafe { entry.as_ref() } {
                match convert_ftsent(ftsent) {
                    Ok(found) => match visitor(&found) {
                        Ok(VisitOutcome::Continue) => (),
                        Ok(VisitOutcome::Cycle(path)) => {
                            eprintln!(
				"error: {} is part of a self-referential cycle and will not be searched",
				path.display()
			    );
                        }
                        Ok(VisitOutcome::PruneChildren) => unsafe {
                            fts_set(self.inner, entry, FTS_SKIP);
                        },
                        Err(e) => {
                            return Err(e);
                        }
                    },
                    Err(_) => {
                        todo!("handle convert_ftsent failure or make it no-fail");
                    }
                }
            }
        }
        Ok(())
    }
}
