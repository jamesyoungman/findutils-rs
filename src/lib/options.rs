use std::error::Error;
use std::fmt::{Display, Write};

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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum NameResolutionMode {
    P,
    L,
    H,
}

impl Default for NameResolutionMode {
    fn default() -> Self {
        NameResolutionMode::P
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
        arg: &str,
    ) -> Result<(), InvalidOptionError> {
        match opt {
            GlobalOptionWithArg::MaxDepth => match arg.parse() {
                Ok(n) if n < 0 => Err(InvalidOptionError(
                    "maximum depth must not be negative".to_string(),
                )),
                Ok(n) => {
                    self.maxdepth = Some(n);
                    Ok(())
                }
                Err(e) => Err(InvalidOptionError(format!(
                    "invalid maximum depth {arg}: {e}"
                ))),
            },
            GlobalOptionWithArg::MinDepth => match arg.parse() {
                Ok(n) if n < 0 => Err(InvalidOptionError(
                    "minimum depth must not be negative".to_string(),
                )),
                Ok(n) => {
                    self.mindepth = n;
                    Ok(())
                }
                Err(e) => Err(InvalidOptionError(format!(
                    "invalid minimum depth {arg}: {e}"
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

pub fn parse_options<'a>(args: &'a [&'a str]) -> Result<(Options, &'a [&'a str]), UsageError> {
    // TODO: we need to support starting points which aren't valid UTF-8.
    //
    // To do that we will probably need to access the FFI interface
    // directly, and change the type of the `args` argument and the
    // return value, too.
    let args_for_opt_parser: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    let mut opts = getopt::Parser::new(&args_for_opt_parser, "HLPD:"); // E not yet implemented.
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
                            return Err(UsageError(format!("unknown debug option '{name}'")));
                        }
                    },
                    None => {
                        return Err(UsageError(format!(
                            "the -D option should be followed by an argument"
                        )));
                    }
                },
                _ => unreachable!(),
            },
            None => break,
            Some(Err(e)) => {
                let mut msg = String::new();
                write!(msg, "{}", e).expect("writes to string always succeeds");
                let kind = e.kind();
                if kind == ErrorKind::UnknownOption {
                    // We come here when we have a command line like
                    // "find -L -print" because 'p' is not a known
                    // option letter.
                    //
                    // We benefit I suppose from the fact that none of
                    // the valid option letters is also the first
                    // character of a predicate.
                    break;
                } else {
                    return Err(UsageError(msg));
                }
            }
        }
    }
    Ok((options, &args[opts.index()..]))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_opt_vec<'a>(args: &'a [&'a str]) -> Result<(Options, &'a [&'a str]), UsageError> {
        assert!(args.len() > 0);
        dbg!(args);
        dbg!(parse_options(&args))
    }

    #[test]
    fn parse_otions_no_options() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::P),
                [].as_slice(),
            )),
            parse_opt_vec(&["find"])
        );
    }

    #[test]
    fn parse_otions_p() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::P),
                [].as_slice()
            )),
            parse_opt_vec(&["find", "-P"])
        );
    }

    #[test]
    fn parse_otions_hp() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::P),
                [].as_slice()
            )),
            parse_opt_vec(&["-HP"])
        );
    }

    #[test]
    fn parse_otions_lp() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::P),
                [].as_slice()
            )),
            parse_opt_vec(&["find", "-LP"])
        );
    }

    #[test]
    fn parse_otions_l() {
        let expected = Ok((
            Options::default().set_name_resolution(NameResolutionMode::L),
            [].as_slice(),
        ));
        assert_eq!(expected, parse_opt_vec(&["find", "-L"]));
        assert_eq!(expected, parse_opt_vec(&["find", "-LL"]));
        assert_eq!(expected, parse_opt_vec(&["find", "-HL"]));
        assert_eq!(expected, parse_opt_vec(&["find", "-PL"]));
        assert_eq!(expected, parse_opt_vec(&["find", "-P", "-L"]));
    }

    #[test]
    fn parse_otions_h() {
        assert_eq!(
            Ok((
                Options::default().set_name_resolution(NameResolutionMode::H),
                [].as_slice()
            )),
            parse_opt_vec(&["find", "-H"])
        );
    }

    #[test]
    fn parse_otions_double_dash() {
        let expected = Ok((
            Options::default().set_name_resolution(NameResolutionMode::H),
            [].as_slice(),
        ));
        assert_eq!(expected, parse_opt_vec(&["find", "-H"]));
        assert_eq!(expected, parse_opt_vec(&["find", "-H", "--"]));
    }

    #[test]
    fn parse_otions_non_options() {
        let expected = Ok((
            Options::default().set_name_resolution(NameResolutionMode::P),
            ["foo/"].as_slice(),
        ));
        assert_eq!(expected, parse_opt_vec(["find", "foo/"].as_slice()));
        assert_eq!(expected, parse_opt_vec(&["find", "--", "foo/"]));
    }

    #[test]
    fn parse_otions_with_start_and_pred() {
        let expected = Ok((
            Options::default().set_name_resolution(NameResolutionMode::L),
            ["foo/", "-print"].as_slice(),
        ));
        assert_eq!(expected, parse_opt_vec(&["find", "-L", "foo/", "-print"]));
    }

    #[test]
    fn parse_otions_with_doubledash_pred_but_no_start() {
        let expected = Ok((
            Options::default().set_name_resolution(NameResolutionMode::L),
            ["-print"].as_slice(),
        ));
        assert_eq!(expected, parse_opt_vec(&["find", "-L", "--", "-print"]));
    }

    #[test]
    fn parse_otions_with_pred_but_no_start() {
        let expected = Ok((
            Options::default().set_name_resolution(NameResolutionMode::L),
            ["-print"].as_slice(),
        ));
        assert_eq!(expected, parse_opt_vec(&["find", "-L", "-print"]));
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
