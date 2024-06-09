use fts::fts::fts_option::Flags;
use fts::fts::{Fts, FtsError};
use std::collections::VecDeque;

mod options;

use findlib::{parser, visit, UsageError};
use options::{parse_options, TraversalMode};

fn run(args: Vec<String>) -> i32 {
    let parser_args: VecDeque<&str> = args.iter().map(|s| s.as_str()).collect();
    match parse_options(parser_args) {
        Ok((options, remaining_args)) => {
            let (start_points, program) = match parser::parse_program(remaining_args) {
                Ok((start, program)) => (start, program),
                Err(e) => {
                    eprintln!("parse error: {}", e);
                    return 1;
                }
            };

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
            let start_points: Vec<String> =
                start_points.into_iter().map(|s| s.to_string()).collect();
            let mut fts = match Fts::new(
                start_points,
                ftsflags,
                None, // unordered
            ) {
                Ok(searcher) => searcher,
                Err(FtsError::PathWithNull) => unreachable!("NUL character in starting point"),
                Err(FtsError::SetFail) => {
                    unreachable!("fts_set apparently failed but was not called")
                }
            };
            let mut result = 0;
            while let Some(entry) = fts.read() {
                if let Err(e) = visit(&program, &entry) {
                    eprintln!("error: {e}");
                    result = 1;
                }
            }
            result
        }
        Err(UsageError(msg)) => {
            eprintln!("usage error: {msg}");
            1
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    std::process::exit(run(args));
}
