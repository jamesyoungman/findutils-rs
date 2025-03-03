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

use std::os::unix::prelude::OsStrExt;

use std::ffi::{CStr, OsStr};
use std::path::PathBuf;

pub fn cstr_to_pathbuf(cs: &CStr) -> PathBuf {
    let os: &OsStr = OsStrExt::from_bytes(cs.to_bytes());
    PathBuf::from(os.to_owned())
}
