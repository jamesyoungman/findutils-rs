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

// We need POSIX compliance for option processing (just for the
// initial options, -H, -P, -L).  This is hard to achieve with common
// option-processing crates, for which this kind of compliance doesn't
// seem much to be a priority.
//
//
// getopt
// ======
// We currently use getopt.  Unfortunately it doesn't support OsStr
// arguments, so this is a compliance problem.
//
// argh
// ====
// Same for argh as getopt.
//
// getopts
// =======
//
// We don't use getopts because it cannot return OsStr
// free arguments.
//
// pico-args
// =========
//
// We don't use pico-args because it doesn't seem to have a simple way
// to arrange for mutually-overriding options (such as -H, -L, -P).
//
// clap
// ====
//
// We might be able to use Clap (I looked at version 4) but it's not
// obvious that this can be used to parse options where there is no
// long-option form (e.g. -H).
//
// xflags
// ======
//
// The xflags crate will probably be unsuitable as is uses Fuschia
// convenstion (i.e. that -HP is a long option and not equivalent to
// -H -P).
//
// bpaf
// ====
//
// bpaf is nearly suitable but doesn't handle positional arguments in
// the way that we would like.  Without .strict(), options are allowed
// to exist on the right of start points (e.g. "/tmp -H" and "-H /tmp"
// have the same parse result).  With strict, positional arguments are
// not recognised on the left of "--".

use std::error::Error;
use std::ffi::OsStr;
use std::fmt::Display;
use std::io::Write as WriteRaw;
use std::str::FromStr;

use getopt::{ErrorKind, Opt};

use super::UsageError;

#[derive(Debug)]
pub struct InvalidOptionError(String);

impl Display for InvalidOptionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Error for InvalidOptionError {}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum DebugOption {
    Tree,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Default)]
pub struct DebugOptions {
    tree: bool,
}

impl DebugOptions {
    pub fn set(&mut self, opt: &str) -> Result<(), ()> {
        match opt {
            "tree" => {
                self.tree = true;
                Ok(())
            }
            _ => Err(()),
        }
    }

    pub fn get(&self, opt: &DebugOption) -> bool {
        match opt {
            DebugOption::Tree => self.tree,
        }
    }
}

#[derive(Default, Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum NameResolutionMode {
    #[default]
    P,
    L,
    H,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct Options {
    name_resolution: NameResolutionMode,
    depth_first: bool,
    debug_options: DebugOptions,
    maxdepth: Option<i32>,
    mindepth: i32,
}

fn parse_os_str<T: FromStr>(s: &OsStr, what: &str) -> Result<T, InvalidOptionError> {
    match s.to_str() {
        Some(s) => match s.parse() {
            Ok(n) => Ok(n),
            Err(_) => Err(InvalidOptionError(format!("{s} is not a valid {what}"))),
        },
        None => Err(InvalidOptionError(format!(
            "argument {} is not valid utf-8 and so cannot be used as an option argument here",
            s.to_string_lossy()
        ))),
    }
}

impl Options {
    pub fn apply(&mut self, opt: GlobalOptionWithoutArg) {
        match opt {
            GlobalOptionWithoutArg::DepthFirst => {
                self.depth_first = true;
            }
        }
    }

    pub fn apply_with_arg(
        &mut self,
        opt: GlobalOptionWithArg,
        arg: &OsStr,
    ) -> Result<(), InvalidOptionError> {
        match opt {
            GlobalOptionWithArg::MaxDepth => match parse_os_str(arg, "depth") {
                Ok(n) if n < 0 => Err(InvalidOptionError(
                    "maximum depth must not be negative".to_string(),
                )),
                Ok(n) => {
                    self.maxdepth = Some(n);
                    Ok(())
                }
                Err(e) => Err(InvalidOptionError(format!(
                    "invalid maximum depth {}: {e}",
                    arg.to_string_lossy(),
                ))),
            },
            GlobalOptionWithArg::MinDepth => match parse_os_str(arg, "depth") {
                Ok(n) if n < 0 => Err(InvalidOptionError(
                    "minimum depth must not be negative".to_string(),
                )),
                Ok(n) => {
                    self.mindepth = n;
                    Ok(())
                }
                Err(e) => Err(InvalidOptionError(format!(
                    "invalid minimum depth {}: {e}",
                    arg.to_string_lossy(),
                ))),
            },
        }
    }

    pub fn name_resolution(&self) -> NameResolutionMode {
        self.name_resolution
    }

    pub fn set_name_resolution(mut self, val: NameResolutionMode) -> Options {
        self.name_resolution = val;
        self
    }

    pub fn depth_first(&self) -> bool {
        self.depth_first
    }

    pub fn set_debug_option(mut self, name: &str) -> Result<Options, ()> {
        match self.debug_options.set(name) {
            Err(_) => Err(()),
            Ok(_) => Ok(self),
        }
    }

    pub fn get_debug_option(&self, opt: &DebugOption) -> bool {
        self.debug_options.get(opt)
    }

    pub fn mindepth(&self) -> i32 {
        self.mindepth
    }

    pub fn maxdepth(&self) -> Option<i32> {
        self.maxdepth
    }
}

pub fn parse_options<'a>(os_args: &'a [&OsStr]) -> Result<(Options, &'a [&'a OsStr]), UsageError> {
    // TODO: we need to support starting points which aren't valid UTF-8.
    //
    // To do that we will probably need to access the FFI interface
    // directly, and change the type of the `args` argument and the
    // return value, too.
    let parser_args = {
        let mut parser_args: Vec<String> = Vec::with_capacity(os_args.len());
        for (i, os_arg) in os_args.iter().enumerate() {
            match os_arg.to_str() {
                Some(s) => {
                    parser_args.push(s.to_string());
                }
                None => {
                    let mut msg: Vec<u8> = Vec::new();
                    let _ = write!(&mut msg, "Command-line argument {i} (");
                    msg.extend(os_arg.as_encoded_bytes());
                    let _ = write!(
			&mut msg,
			") is not valid UTF-8 and processing such arguments is not yet implemented."
                    );
                    return Err(UsageError::Encoded(msg));
                }
            }
        }
        parser_args
    };

    let mut opts = getopt::Parser::new(&parser_args, "HLPD:"); // E not yet implemented.
    let mut options = Options::default();
    // Process the leading options.
    loop {
        match opts.next() {
            Some(Ok(opt)) => match opt {
                Opt('P', None) => {
                    options = options.set_name_resolution(NameResolutionMode::P);
                }
                Opt('L', None) => {
                    options = options.set_name_resolution(NameResolutionMode::L);
                }
                Opt('H', None) => {
                    options = options.set_name_resolution(NameResolutionMode::H);
                }
                Opt('D', debug_opt) => match debug_opt {
                    Some(name) => match options.set_debug_option(name.as_str()) {
                        Ok(new_options) => {
                            options = new_options;
                        }
                        Err(_) => {
                            return Err(UsageError::Formatted(format!(
                                "unknown debug option '{name}'"
                            )));
                        }
                    },
                    None => {
                        return Err(UsageError::Formatted(
                            "the -D option should be followed by an argument".to_string(),
                        ));
                    }
                },
                _ => unreachable!(),
            },
            None => break,
            Some(Err(e)) => {
                if e.kind() == ErrorKind::UnknownOption {
                    // We come here when we have a command line like
                    // "find -L -print" because 'p' is not a known
                    // option letter.
                    //
                    // We benefit I suppose from the fact that none of
                    // the valid option letters is also the first
                    // character of a predicate.
                    break;
                } else {
                    return Err(UsageError::Formatted(e.to_string()));
                }
            }
        }
    }
    Ok((options, &os_args[opts.index()..]))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_physical_name_resolution_is_the_default() {
        // This is essential for conformance to the POSIX standard.
        assert_eq!(NameResolutionMode::default(), NameResolutionMode::P);
    }

    fn s(p: &str) -> &OsStr {
        OsStr::new(p)
    }

    fn parse_opt_vec<'a>(
        os_args: &'a [&'a OsStr],
    ) -> Result<(Options, &'a [&'a OsStr]), UsageError> {
        assert!(os_args.len() > 0);
        dbg!(os_args);
        dbg!(parse_options(&os_args))
    }

    #[test]
    fn parse_otions_no_options() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::P),
                [].as_slice(),
            )),
            parse_opt_vec(&[OsStr::new("find")])
        );
    }

    #[test]
    fn parse_otions_p() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::P),
                [].as_slice()
            )),
            parse_opt_vec(&[OsStr::new("find"), OsStr::new("-P")])
        );
    }

    #[test]
    fn parse_otions_hp() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::P),
                [].as_slice()
            )),
            parse_opt_vec(&[OsStr::new("-HP")])
        );
    }

    #[test]
    fn parse_otions_lp() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::P),
                [].as_slice()
            )),
            parse_opt_vec(&[OsStr::new("find"), OsStr::new("-LP")])
        );
    }

    #[test]
    fn parse_otions_l() {
        let expected = Ok((
            Options::default().set_name_resolution(NameResolutionMode::L),
            [].as_slice(),
        ));
        assert_eq!(
            expected,
            parse_opt_vec(&[OsStr::new("find"), OsStr::new("-L")])
        );
        assert_eq!(
            expected,
            parse_opt_vec(&[OsStr::new("find"), OsStr::new("-LL")])
        );
        assert_eq!(
            expected,
            parse_opt_vec(&[OsStr::new("find"), OsStr::new("-HL")])
        );
        assert_eq!(
            expected,
            parse_opt_vec(&[OsStr::new("find"), OsStr::new("-PL")])
        );
        assert_eq!(
            expected,
            parse_opt_vec(&[OsStr::new("find"), OsStr::new("-P"), OsStr::new("-L")])
        );
    }

    #[test]
    fn parse_otions_h() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::H),
                [].as_slice()
            )),
            parse_opt_vec(&[OsStr::new("find"), OsStr::new("-H")])
        );
    }

    #[test]
    fn parse_otions_double_dash() {
        let expected = Ok((
            Options::default().set_name_resolution(NameResolutionMode::H),
            [].as_slice(),
        ));
        assert_eq!(
            expected,
            parse_opt_vec(&[OsStr::new("find"), OsStr::new("-H")])
        );
        assert_eq!(
            expected,
            parse_opt_vec(&[OsStr::new("find"), OsStr::new("-H"), OsStr::new("--")])
        );
    }

    #[test]
    fn parse_otions_non_options() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::P),
                [OsStr::new("foo/")].as_slice(),
            )),
            parse_opt_vec([s("find"), s("foo/")].as_slice())
        );

        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::P),
                [OsStr::new("foo/")].as_slice(),
            )),
            parse_opt_vec([OsStr::new("find"), OsStr::new("--"), OsStr::new("foo/")].as_slice())
        );
    }

    #[test]
    fn parse_otions_with_start_and_pred() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::L),
                [OsStr::new("foo/"), OsStr::new("-print")].as_slice(),
            )),
            parse_opt_vec(&[
                OsStr::new("find"),
                OsStr::new("-L"),
                OsStr::new("foo/"),
                OsStr::new("-print")
            ])
        );
    }

    #[test]
    fn parse_otions_with_doubledash_pred_but_no_start() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::L),
                [OsStr::new("-print")].as_slice(),
            )),
            parse_opt_vec(&[OsStr::new("find"), s("-L"), s("--"), s("-print")])
        );
    }

    #[test]
    fn parse_otions_with_pred_but_no_start() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::L),
                [OsStr::new("-print")].as_slice(),
            )),
            parse_opt_vec(&[s("find"), s("-L"), s("-print")])
        );
    }

    #[test]
    fn parse_h_after_positional_option() {
        // This is the example from https://github.com/pacak/bpaf/discussions/364#discussioncomment-9859291
        let expected_options = Options::default()
            .set_name_resolution(NameResolutionMode::L)
            .set_debug_option("tree")
            .expect("should be a valid option");
        assert_eq!(
            Ok((
                expected_options,
                vec![s("/tmp"), s("/var/tmp"), s("-H"),].as_slice(),
            )),
            parse_opt_vec(&[
                s("find"),
                s("-H"),
                s("-H"),
                s("-LPH"),
                s("-D"),
                s("tree"),
                s("-PL"),
                s("/tmp"),
                s("/var/tmp"),
                s("-H"),
            ])
        );
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum GlobalOptionWithoutArg {
    DepthFirst,
}

impl Display for GlobalOptionWithoutArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            GlobalOptionWithoutArg::DepthFirst => "-depth",
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum GlobalOptionWithArg {
    MaxDepth,
    MinDepth,
}

impl Display for GlobalOptionWithArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            GlobalOptionWithArg::MaxDepth => "-maxdepth",
            GlobalOptionWithArg::MinDepth => "-mindepth",
        })
    }
}
