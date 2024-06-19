use std::fs::Metadata;

use libc::{S_IFBLK, S_IFCHR, S_IFIFO, S_IFSOCK};
use std::os::linux::fs::MetadataExt;

pub fn is_char_device(md: &Metadata) -> bool {
    md.st_mode() & S_IFCHR != 0
}

pub fn is_block_device(md: &Metadata) -> bool {
    md.st_mode() & S_IFBLK != 0
}

pub fn is_named_pipe(md: &Metadata) -> bool {
    md.st_mode() & S_IFIFO != 0
}

pub fn is_unix_domain_socket(md: &Metadata) -> bool {
    md.st_mode() & S_IFSOCK != 0
}

pub fn is_door(_md: &Metadata) -> bool {
    false // doors do not exist on Linux.
}
