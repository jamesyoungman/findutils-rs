use std::os::unix::prelude::OsStrExt;

use std::ffi::{CStr, OsStr};
use std::path::PathBuf;

pub fn cstr_to_pathbuf(cs: &CStr) -> PathBuf {
    let os: &OsStr = OsStrExt::from_bytes(cs.to_bytes());
    PathBuf::from(os.to_owned())
}
