use std::ffi::{OsStr, OsString};

use findlib::{new_source, parse_options, parse_program, visit, Source, VisitOutcome};

fn run(args: Vec<OsString>) -> i32 {
    let dot = OsStr::new(".");
    let default_start_points = [dot; 1];

    let args: Vec<&OsStr> = args.iter().map(|s| s.as_os_str()).collect();
    match parse_options(&args) {
        Ok((mut options, remaining_args)) => {
            let (start_points, program) = match parse_program(remaining_args, &mut options) {
                Ok((start, program)) => (
                    if start.is_empty() {
                        &default_start_points
                    } else {
                        start
                    },
                    program,
                ),
                Err(e) => {
                    eprintln!("parse error: {}", e);
                    return 1;
                }
            };
            let mut source = match new_source(&options, start_points) {
                Err(e) => {
                    eprintln!("failed to initialise file system searcher: {e}");
                    return 1;
                }
                Ok(source) => source,
            };

            let mut result = 0;
            if let Err(e) = source.visit_all(|entry| match visit(&program, entry, &options) {
                Err(e) => {
                    eprintln!("error: {e}");
                    result = 1;
                    Ok(VisitOutcome::Continue) // we don't want to stop.
                }
                Ok(outcome) => Ok(outcome),
            }) {
                eprintln!("error: {e}");
                result = 1;
            }
            result
        }
        Err(e) => {
            let _ = e.report();
            1
        }
    }
}

fn main() {
    let args: Vec<OsString> = std::env::args_os().collect();
    std::process::exit(run(args));
}
