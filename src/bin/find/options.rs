use std::collections::VecDeque;
use std::fmt::Write;

use getopt::{ErrorKind, Opt};

use super::errors::UsageError;

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

pub fn parse_options(mut args: VecDeque<&str>) -> Result<(Options, VecDeque<&str>), UsageError> {
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
    args.drain(..limit);
    let opts = Options { traversal };
    Ok((opts, args))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_opt_vec(mut args: Vec<&str>) -> Result<(Options, Vec<&str>), UsageError> {
        args.insert(0, "find"); // argv[0]
        let dq: VecDeque<&str> = args.iter().copied().collect();
        match parse_options(dq) {
            Err(e) => Err(e),
            Ok((opts, args)) => {
                let v: Vec<&str> = args.iter().copied().collect();
                Ok((opts, v))
            }
        }
    }

    #[test]
    fn parse_otions_no_options() {
        assert_eq!(
            Ok((
                Options {
                    traversal: TraversalMode::P,
                },
                vec![]
            )),
            parse_opt_vec(Vec::new())
        );
    }

    #[test]
    fn parse_otions_p() {
        assert_eq!(
            Ok((
                Options {
                    traversal: TraversalMode::P,
                },
                vec![]
            )),
            parse_opt_vec(vec!["-P"])
        );
    }

    #[test]
    fn parse_otions_hp() {
        assert_eq!(
            Ok((
                Options {
                    traversal: TraversalMode::P,
                },
                vec![]
            )),
            parse_opt_vec(vec!["-HP"])
        );
    }

    #[test]
    fn parse_otions_lp() {
        assert_eq!(
            Ok((
                Options {
                    traversal: TraversalMode::P,
                },
                vec![]
            )),
            parse_opt_vec(vec!["-LP"])
        );
    }

    #[test]
    fn parse_otions_l() {
        let expected = Ok((
            Options {
                traversal: TraversalMode::L,
            },
            vec![],
        ));
        assert_eq!(expected, parse_opt_vec(vec!["-L"]));
        assert_eq!(expected, parse_opt_vec(vec!["-LL"]));
        assert_eq!(expected, parse_opt_vec(vec!["-HL"]));
        assert_eq!(expected, parse_opt_vec(vec!["-PL"]));
        assert_eq!(expected, parse_opt_vec(vec!["-P", "-L"]));
    }

    #[test]
    fn parse_otions_h() {
        assert_eq!(
            Ok((
                Options {
                    traversal: TraversalMode::H,
                },
                vec![]
            )),
            parse_opt_vec(vec!["-H"])
        );
    }

    #[test]
    fn parse_otions_double_dash() {
        let expected = Ok((
            Options {
                traversal: TraversalMode::H,
            },
            vec![],
        ));
        assert_eq!(expected, parse_opt_vec(vec!["-H"]));
        assert_eq!(expected, parse_opt_vec(vec!["-H", "--"]));
    }

    #[test]
    fn parse_otions_non_options() {
        let expected = Ok((
            Options {
                traversal: TraversalMode::P,
            },
            vec!["foo/"],
        ));
        assert_eq!(expected, parse_opt_vec(vec!["foo/"]));
        assert_eq!(expected, parse_opt_vec(vec!["--", "foo/"]));
    }

    #[test]
    fn parse_otions_with_start_and_pred() {
        let expected = Ok((
            Options {
                traversal: TraversalMode::L,
            },
            vec!["foo/", "-print"],
        ));
        assert_eq!(expected, parse_opt_vec(vec!["-L", "foo/", "-print"]));
    }

    #[test]
    fn parse_otions_with_doubledash_pred_but_no_start() {
        let expected = Ok((
            Options {
                traversal: TraversalMode::L,
            },
            vec!["-print"],
        ));
        assert_eq!(expected, parse_opt_vec(vec!["-L", "--", "-print"]));
    }

    #[test]
    fn parse_otions_with_pred_but_no_start() {
        let expected = Ok((
            Options {
                traversal: TraversalMode::L,
            },
            vec!["-print"],
        ));
        assert_eq!(expected, parse_opt_vec(vec!["-L", "-print"]));
    }
}
