use std::collections::VecDeque;
use std::fmt::Write;

use getopt::Opt;

use super::errors::UsageError;

#[derive(Debug)]
pub enum TraversalMode {
    P,
    L,
    H,
}

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
                return Err(UsageError(msg));
            }
        }
    }
    args.drain(..opts.index());
    let opts = Options { traversal };
    Ok((opts, args))
}
