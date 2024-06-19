use std::ffi::{OsStr, OsString};
use std::io::{stderr, Write};

use findlib::{new_source, parse_options, parse_program, visit, Source, UsageError, VisitOutcome};

fn run(args: Vec<OsString>) -> i32 {
    let args: Vec<&OsStr> = args.iter().map(|s| s.as_os_str()).collect();
    let mut parser_args: Vec<&str> = Vec::with_capacity(args.len());
    for (i, arg) in args.iter().enumerate() {
        match arg.to_str() {
            Some(s) => {
                parser_args.push(s);
            }
            None => {
                let mut msg: Vec<u8> = Vec::new();
                let _ = write!(&mut msg, "Command-line argument {i} (");
                msg.extend(arg.as_encoded_bytes());
                let _ = write!(
                    &mut msg,
                    ") is not valid UTF-8 and processing such arguments is not yet implemented."
                );
                let _ = stderr().write_all(&msg);
                return 1;
            }
        }
    }

    match parse_options(&args, &parser_args) {
        Ok((mut options, remaining_args)) => {
            let (start_points, program) = match parse_program(remaining_args, &mut options) {
                Ok((start, program)) => (start, program),
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
        Err(UsageError(msg)) => {
            eprintln!("usage error: {msg}");
            1
        }
    }
}

fn main() {
    let args: Vec<OsString> = std::env::args_os().collect();
    std::process::exit(run(args));
}
