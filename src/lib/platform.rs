use libc::{S_IFBLK, S_IFCHR, S_IFDIR, S_IFIFO, S_IFLNK, S_IFREG, S_IFSOCK};

use super::metadata::TypePredicateFileType;

// TODO: make this conditional
mod linux;
pub use linux::*;
mod unix;
pub use unix::*;

fn file_type_from_stat_st_mode(mut mode: u32) -> Option<TypePredicateFileType> {
    mode = mode & (!0o777);
    for (mask, filetype) in [
        (S_IFBLK, TypePredicateFileType::BlockDevice),
        (S_IFCHR, TypePredicateFileType::CharacterDevice),
        (S_IFDIR, TypePredicateFileType::Directory),
        (S_IFIFO, TypePredicateFileType::NamedPipe),
        (S_IFREG, TypePredicateFileType::RegularFile),
        (S_IFLNK, TypePredicateFileType::SymbolicLink),
        (S_IFSOCK, TypePredicateFileType::Socket),
    ] {
        if mode | mask != 0 {
            return Some(filetype);
        }
    }
    None
}
