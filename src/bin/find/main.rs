use std::fmt::Write;

use fts::fts::fts_option::Flags;
use fts::fts::{Fts, FtsEntry, FtsError, FtsInfo};
use getopt::Opt;

struct UsageError(String);

enum TraversalMode {
    P,
    L,
    H,
}

struct Options {
    traversal: TraversalMode,
}

fn parse_args(args: Vec<String>) -> Result<(Options, Vec<String>), UsageError> {
    // TODO: we need to support starting points which aren't valid UTF-8.
    //
    // To do that we will probably need to access the FFI interface
    // directly, and change the type of the `args` argument and the
    // return value, too.
    let mut opts = getopt::Parser::new(&args, "HLP"); // E not yet implemented.
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
    let tail: Vec<String> = args.into_iter().skip(opts.index()).collect();
    let opts = Options { traversal };
    Ok((opts, tail))
}

fn visit(entry: &FtsEntry) {
    match entry.info {
        FtsInfo::IsDirPost => {
            // TODO: we will probably need to use this case to
            // complete any pending executions for -execdir.
        }
        _ => {
            println!("{}", entry.path.display());
        }
    }
}

fn run(args: Vec<String>) -> i32 {
    match parse_args(args) {
        Ok((options, mut args)) => {
            let mut ftsflags = Flags::empty();
            // TODO: parsing the predicates will tell us if we should
            // set Flags::XDEV.  But see the (somewhat) recent
            // comments from the Austin group about the distinction
            // between -mount and -xdev.
            ftsflags.insert(match options.traversal {
                TraversalMode::P => Flags::PHYSICAL,
                TraversalMode::L => Flags::LOGICAL,
                TraversalMode::H => Flags::COMFOLLOW,
            });
            if args.is_empty() {
                args.push(".".to_string());
            }
            let mut fts = match Fts::new(
                args, ftsflags, None, // unordered
            ) {
                Ok(searcher) => searcher,
                Err(FtsError::PathWithNull) => unreachable!("NUL character in starting point"),
                Err(FtsError::SetFail) => {
                    unreachable!("fts_set apparently failed but was not called")
                }
            };
            while let Some(entry) = fts.read() {
                visit(&entry);
            }
            0
        }
        Err(UsageError(msg)) => {
            eprintln!("usage error: {}", msg);
            1
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    std::process::exit(run(args));
}
