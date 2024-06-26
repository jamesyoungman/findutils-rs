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

use libc::{S_IFBLK, S_IFCHR, S_IFDIR, S_IFIFO, S_IFLNK, S_IFREG, S_IFSOCK};

use super::metadata::TypePredicateFileType;

// TODO: make this conditional
mod linux;
pub use linux::*;
mod unix;
pub use unix::*;

fn file_type_from_stat_st_mode(mut mode: u32) -> Option<TypePredicateFileType> {
    mode &= !0o777;
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
