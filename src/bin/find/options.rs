use std::fmt::Write;

use getopt::{ErrorKind, Opt};

use findlib::UsageError;

#[derive(Debug, PartialEq, Eq)]
pub enum TraversalMode {
    P,
    L,
    H,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Options {
    pub traversal: TraversalMode,
}

pub fn parse_options<'a>(args: &'a [&'a str]) -> Result<(Options, &'a [&'a str]), UsageError> {
    // TODO: we need to support starting points which aren't valid UTF-8.
    //
    // To do that we will probably need to access the FFI interface
    // directly, and change the type of the `args` argument and the
    // return value, too.
    let args_for_opt_parser: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    let mut opts = getopt::Parser::new(&args_for_opt_parser, "HLP"); // E not yet implemented.
    let mut traversal = TraversalMode::P;
    let mut need_unshift = false;
    // Process the leading options.
    loop {
        match opts.next() {
            Some(Ok(opt)) => match opt {
                Opt('P', None) => traversal = TraversalMode::P,
                Opt('L', None) => traversal = TraversalMode::L,
                Opt('H', None) => traversal = TraversalMode::H,
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
                    need_unshift = true;
                } else {
                    return Err(UsageError(msg));
                }
            }
        }
    }
    let limit = opts.index() - if need_unshift { 1 } else { 0 };
    let opts = Options { traversal };
    Ok((opts, &args[limit..]))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_opt_vec<'a>(args: &'a [&'a str]) -> Result<(Options, &'a [&'a str]), UsageError> {
        assert!(args.len() > 0);
        parse_options(&args)
    }

    #[test]
    fn parse_otions_no_options() {
        assert_eq!(
            Ok((
                Options {
                    traversal: TraversalMode::P,
                },
                [].as_slice(),
            )),
            parse_opt_vec(&["find"])
        );
    }

    #[test]
    fn parse_otions_p() {
        assert_eq!(
            Ok((
                Options {
                    traversal: TraversalMode::P,
                },
                [].as_slice()
            )),
            parse_opt_vec(&["find", "-P"])
        );
    }

    #[test]
    fn parse_otions_hp() {
        assert_eq!(
            Ok((
                Options {
                    traversal: TraversalMode::P,
                },
                [].as_slice()
            )),
            parse_opt_vec(&["-HP"])
        );
    }

    #[test]
    fn parse_otions_lp() {
        assert_eq!(
            Ok((
                Options {
                    traversal: TraversalMode::P,
                },
                [].as_slice()
            )),
            parse_opt_vec(&["find", "-LP"])
        );
    }

    #[test]
    fn parse_otions_l() {
        let expected = Ok((
            Options {
                traversal: TraversalMode::L,
            },
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
                Options {
                    traversal: TraversalMode::H,
                },
                [].as_slice()
            )),
            parse_opt_vec(&["find", "-H"])
        );
    }

    #[test]
    fn parse_otions_double_dash() {
        let expected = Ok((
            Options {
                traversal: TraversalMode::H,
            },
            [].as_slice(),
        ));
        assert_eq!(expected, parse_opt_vec(&["find", "-H"]));
        assert_eq!(expected, parse_opt_vec(&["find", "-H", "--"]));
    }

    #[test]
    fn parse_otions_non_options() {
        let expected = Ok((
            Options {
                traversal: TraversalMode::P,
            },
            ["foo/"].as_slice(),
        ));
        assert_eq!(expected, parse_opt_vec(["find", "foo/"].as_slice()));
        assert_eq!(expected, parse_opt_vec(&["find", "--", "foo/"]));
    }

    #[test]
    fn parse_otions_with_start_and_pred() {
        let expected = Ok((
            Options {
                traversal: TraversalMode::L,
            },
            ["foo/", "-print"].as_slice(),
        ));
        assert_eq!(expected, parse_opt_vec(&["find", "-L", "foo/", "-print"]));
    }

    #[test]
    fn parse_otions_with_doubledash_pred_but_no_start() {
        let expected = Ok((
            Options {
                traversal: TraversalMode::L,
            },
            ["-print"].as_slice(),
        ));
        assert_eq!(expected, parse_opt_vec(&["find", "-L", "--", "-print"]));
    }

    #[test]
    fn parse_otions_with_pred_but_no_start() {
        let expected = Ok((
            Options {
                traversal: TraversalMode::L,
            },
            ["-print"].as_slice(),
        ));
        assert_eq!(expected, parse_opt_vec(&["find", "-L", "-print"]));
    }
}
