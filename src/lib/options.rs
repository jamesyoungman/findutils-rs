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

use std::collections::BTreeSet;
use std::error::Error;
use std::ffi::{OsStr, OsString};
use std::fmt::Display;
use std::str::FromStr;

use super::UsageError;
use bpaf::*;

#[derive(Debug)]
pub struct InvalidOptionError(String);

impl Display for InvalidOptionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Error for InvalidOptionError {}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum DebugOption {
    Tree,
}

fn str_to_debug_options(s: String) -> Result<DebugOptions, BadDebugOption> {
    s.split(',')
        .try_fold(DebugOptions::default(), |mut options, opt| {
            options.set(DebugOption::try_from(opt)?);
            Ok(options)
        })
}

#[test]
fn test_valid_debug_options() {
    let _ =
        str_to_debug_options(VALID_DEBUG_OPTIONS.to_string()).expect("all options should be valid");
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct BadDebugOption(String);

impl Display for BadDebugOption {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "'{}' is not a valid debug option; valid options are: {VALID_DEBUG_OPTIONS}",
            self.0
        )
    }
}

impl Error for BadDebugOption {}

const VALID_DEBUG_OPTIONS: &str = "tree";

impl TryFrom<&str> for DebugOption {
    type Error = BadDebugOption;
    fn try_from(s: &str) -> Result<DebugOption, BadDebugOption> {
        match s {
            "tree" => Ok(DebugOption::Tree),
            other => Err(BadDebugOption(other.to_string())),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Default)]
pub struct DebugOptions {
    values: BTreeSet<DebugOption>,
}

impl DebugOptions {
    pub fn set(&mut self, opt: DebugOption) {
        self.values.insert(opt);
    }

    pub fn get(&self, opt: &DebugOption) -> bool {
        self.values.contains(opt)
    }
}

impl FromIterator<DebugOptions> for DebugOptions {
    fn from_iter<T: IntoIterator<Item = DebugOptions>>(iter: T) -> Self {
        iter.into_iter()
            .fold(DebugOptions::default(), |mut acc, mut more| {
                acc.values.append(&mut more.values);
                acc
            })
    }
}

#[derive(Default, Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum NameResolutionMode {
    #[default]
    P,
    L,
    H,
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct BadNameResolutionMode(pub String);

impl Display for BadNameResolutionMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}' is not a valid name resoltuion mode", self.0)
    }
}

impl FromStr for NameResolutionMode {
    type Err = BadNameResolutionMode;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "physical" => Ok(NameResolutionMode::P),
            "logical" => Ok(NameResolutionMode::L),
            "hybrid" => Ok(NameResolutionMode::H),
            _ => Err(BadNameResolutionMode(s.to_string())),
        }
    }
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

    pub fn set_debug_option(mut self, opt: DebugOption) -> Options {
        self.debug_options.set(opt);
        self
    }

    pub fn set_debug_options(mut self, mut opts: DebugOptions) -> Options {
        self.debug_options.values.append(&mut opts.values);
        self
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

fn make_option_parser() -> OptionParser<(NameResolutionMode, DebugOptions, Vec<OsString>)> {
    let h = short('H')
        .help("dereference symbolic links on the command line")
        .req_flag(NameResolutionMode::H);
    let l = short('L')
        .help("dereference all symbolic links")
        .req_flag(NameResolutionMode::L);
    let p = short('P')
        .help("do not dereference symbolic links")
        .req_flag(NameResolutionMode::P);
    let debug = short('D')
        .help("selects debugging output; comma-separated liist of values")
        .argument::<String>("CATEGORIES")
        .parse(str_to_debug_options)
        .collect::<DebugOptions>();
    let traversal_options = construct!([h, l, p]).last().fallback(NameResolutionMode::P);
    let tail = positional::<OsString>("start point or predicate").many();
    let option_parser = construct!(traversal_options, debug, tail)
        .to_options()
        .with_usage(|u| {
            let mut doc = Doc::default();
            doc.emphasis("Usage: ");
            doc.literal("find");
            doc.text(" ");
            doc.doc(&u);
            doc
        });
    option_parser
}

pub fn parse_options(mut args: &[&OsStr]) -> Result<(Options, Vec<OsString>), UsageError> {
    assert!(!args.is_empty());
    args = &args[1..];

    let option_parser = make_option_parser();
    match dbg!(option_parser.run_inner(args)) {
        Ok((traversal, debug_options, tail)) => {
            let options = Options::default()
                .set_name_resolution(traversal)
                .set_debug_options(debug_options);
            Ok((options, tail))
        }
        Err(ParseFailure::Stdout(doc, _ok)) => Err(UsageError::Formatted(format!("{doc}"))),
        Err(ParseFailure::Stderr(doc)) => Err(UsageError::Formatted(format!("{doc}"))),
        Err(ParseFailure::Completion(_)) => todo!("handle options completion"),
    }
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

    fn parse_opt_vec<'a>(os_args: &'a [&'a OsStr]) -> Result<(Options, Vec<OsString>), UsageError> {
        assert!(os_args.len() > 0);
        dbg!(os_args);
        dbg!(parse_options(&os_args))
    }

    #[test]
    fn parse_options_no_options() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::P),
                vec![],
            )),
            parse_opt_vec(&[OsStr::new("find")])
        );
    }

    #[test]
    fn parse_options_p() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::P),
                vec![]
            )),
            parse_opt_vec(&[OsStr::new("find"), OsStr::new("-P")])
        );
    }

    #[test]
    fn parse_options_hp() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::P),
                vec![]
            )),
            parse_opt_vec(&[OsStr::new("-HP")])
        );
    }

    #[test]
    fn parse_options_ph() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::H),
                vec![]
            )),
            parse_opt_vec(&[OsStr::new("-PH")])
        );
    }

    #[test]
    fn parse_options_lp() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::P),
                vec![]
            )),
            parse_opt_vec(&[OsStr::new("find"), OsStr::new("-LP")])
        );
    }

    #[test]
    fn parse_options_l() {
        let expected = Ok((
            Options::default().set_name_resolution(NameResolutionMode::L),
            vec![],
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
    fn parse_options_h() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::H),
                vec![]
            )),
            parse_opt_vec(&[OsStr::new("find"), OsStr::new("-H")])
        );
    }

    #[test]
    fn parse_options_double_dash() {
        let expected = Ok((
            Options::default().set_name_resolution(NameResolutionMode::H),
            vec![],
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
    fn parse_options_non_options() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::P),
                vec![OsString::from("foo/")]
            )),
            parse_opt_vec([s("find"), s("foo/")].as_slice())
        );

        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::P),
                vec![OsString::from("foo/")],
            )),
            parse_opt_vec([OsStr::new("find"), OsStr::new("--"), OsStr::new("foo/")].as_slice())
        );
    }

    #[test]
    fn parse_options_with_start_and_pred() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::L),
                vec![OsString::from("foo/"), OsString::from("-print")],
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
    fn parse_options_with_doubledash_pred_but_no_start() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::L),
                vec![OsString::from("-print")],
            )),
            parse_opt_vec(&[OsStr::new("find"), s("-L"), s("--"), s("-print")])
        );
    }

    #[test]
    fn parse_options_with_pred_but_no_start() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::L),
                vec![OsString::from("-print")],
            )),
            parse_opt_vec(&[s("find"), s("-L"), s("-print")])
        );
    }

    #[test]
    fn parse_h_after_positional_option() {
        // This is the example from https://github.com/pacak/bpaf/discussions/364#discussioncomment-9859291
        let expected_options = Options::default()
            .set_name_resolution(NameResolutionMode::L)
            .set_debug_option(DebugOption::Tree);
        assert_eq!(
            Ok((
                expected_options,
                vec![
                    OsString::from("/tmp"),
                    OsString::from("/var/tmp"),
                    OsString::from("-H"),
                ],
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
