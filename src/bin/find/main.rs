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

use findlib::{
    new_source, parse_options, parse_program, visit, EffectSink, Expression, Options, Source,
    VisitOutcome,
};
use std::cmp::max;
use std::ffi::{OsStr, OsString};
use std::io::{self, Write};
use std::io::{BufWriter, StderrLock, StdoutLock};

#[derive(Debug)]
struct LockedOutputStream<W: Write> {
    name: String,
    error: bool,
    handle: BufWriter<W>,
}

impl<W: Write> Write for LockedOutputStream<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let res = self.handle.write(buf);
        self.diagnose(res)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        let res = self.handle.flush();
        self.diagnose(res)
    }
}

impl<W: Write> LockedOutputStream<W> {
    fn stdout() -> LockedOutputStream<StdoutLock<'static>> {
        LockedOutputStream {
            name: "standard output".to_string(),
            error: false,
            handle: BufWriter::new(io::stdout().lock()),
        }
    }

    fn stderr() -> LockedOutputStream<StderrLock<'static>> {
        LockedOutputStream {
            name: "standard error".to_string(),
            error: false,
            handle: BufWriter::new(io::stderr().lock()),
        }
    }

    fn diagnose<T>(&mut self, r: std::io::Result<T>) -> std::io::Result<T> {
        match &r {
            Err(e) => {
                if !self.error {
                    eprintln!("{}: {e}", self.name);
                    self.error = true;
                }
            }
            Ok(_) => (),
        }
        r
    }
}

#[derive(Debug)]
struct Effects<'a> {
    returncode: i32,
    stdout: LockedOutputStream<StdoutLock<'a>>,
    stderr: LockedOutputStream<StderrLock<'a>>,
}

impl<'a> EffectSink for Effects<'a> {
    fn emit_encoded_error(&mut self, returncode: i32, bytes: &[u8]) {
        self.emit_encoded_errors(returncode, &[bytes]);
    }

    fn emit_encoded_errors(&mut self, returncode: i32, byte_sequences: &[&[u8]]) {
        self.returncode = max(returncode, self.returncode);
        let mut needs_newline = false;
        for bytes in byte_sequences {
            let _ = self.stderr.write_all(bytes);
            if !bytes.is_empty() && bytes.last() != Some(&10) {
                needs_newline = true;
            }
        }
        if needs_newline {
            let _ = self.stderr.write_all(b"\n");
        }
    }

    fn emit_encoded_output(&mut self, bytes: &[u8]) {
        // If we automatically added a new line here we woudl get the
        // wrong output for command lines like `find . -printf x`.
        let _ = self.stdout.write_all(bytes);
    }

    fn flush(&mut self) {
        let _ = self.stdout.flush();
        let _ = self.stderr.flush();
    }

    fn deferred_exit_code(&self) -> i32 {
        max(
            if self.stdout.error || self.stderr.error {
                1
            } else {
                0
            },
            self.returncode,
        )
    }
}

impl<'a> Effects<'a> {
    fn new(
        stdout: LockedOutputStream<StdoutLock<'a>>,
        stderr: LockedOutputStream<StderrLock<'a>>,
    ) -> Effects<'a> {
        Effects {
            returncode: 0,
            stdout,
            stderr,
        }
    }
}

fn examine_filesystem(
    options: &Options,
    start_points: &[&OsStr],
    program: &Expression,
    sink: &mut Box<dyn EffectSink>,
) {
    let mut source = match new_source(&options, start_points) {
        Err(e) => {
            sink.emit_encoded_error(
                1,
                format!("failed to initialise file system searcher: {e}").as_bytes(),
            );
            return;
        }
        Ok(source) => source,
    };

    if let Err(e) = source.visit_all(|entry| match visit(&program, entry, &options, sink) {
        Err(e) => {
            sink.emit_encoded_error(1, format!("error: {e}").as_bytes());
            Ok(VisitOutcome::Continue) // we don't want to stop.
        }
        Ok(outcome) => Ok(outcome),
    }) {
        sink.emit_encoded_error(1, format!("error: {e}").as_bytes());
    }
}

fn run(args: Vec<OsString>) -> i32 {
    let dot = OsStr::new(".");
    let default_start_points = [dot; 1];

    let eff = Effects::new(
        LockedOutputStream::<StdoutLock>::stdout(),
        LockedOutputStream::<StderrLock>::stderr(),
    );
    let mut effects: Box<dyn EffectSink> = Box::new(eff) as Box<dyn EffectSink>;
    let sink: &mut Box<dyn EffectSink> = &mut effects;

    let args: Vec<&OsStr> = args.iter().map(|s| s.as_os_str()).collect();
    match parse_options(&args) {
        Ok((mut options, remaining_args)) => match parse_program(remaining_args, &mut options) {
            Ok((start, program)) => {
                let start_points = if start.is_empty() {
                    &default_start_points
                } else {
                    start
                };
                examine_filesystem(&options, start_points, &program, sink);
            }
            Err(e) => {
                sink.emit_encoded_error(1, format!("parse error: {}", e).as_bytes());
            }
        },
        Err(e) => {
            sink.emit_encoded_error(1, (&e).into());
        }
    };
    sink.flush();
    sink.deferred_exit_code()
}

fn main() {
    let args: Vec<OsString> = std::env::args_os().collect();
    std::process::exit(run(args));
}
