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

use std::collections::VecDeque;

use criterion::{criterion_group, criterion_main, Criterion};

use findlib::parser;

fn build_paren_input(n_parens: usize) -> Vec<&'static str> {
    let mut input: Vec<&str> = Vec::with_capacity(n_parens * 2 + 1);
    for _i in 0..n_parens {
        input.push("(");
    }
    input.push("-print");
    for _i in 0..n_parens {
        input.push(")");
    }
    input
}

// Previous algorithm:
//
//   parens 0                time:   [62.474 ns 62.669 ns 63.144 ns]
//                           change: [-2.1305% -0.5851% +0.8854%] (p = 0.52 > 0.05)
//                           No change in performance detected.
//
//   parens 10               time:   [1.3612 µs 1.3634 µs 1.3655 µs]
//                           change: [-1.3835% -0.6346% +0.0185%] (p = 0.11 > 0.05)
//                           No change in performance detected.
//
//   parens 100              time:   [77.329 µs 77.494 µs 77.834 µs]
//                           change: [-0.4435% +0.0787% +0.6481%] (p = 0.80 > 0.05)
//                           No change in performance detected.
//
//   parens 1000             time:   [7.0437 ms 7.0672 ms 7.0915 ms]
//                           change: [-54.530% -54.307% -54.071%] (p = 0.00 < 0.05)
//                           Performance has improved.
//
//   Benchmarking parens 10000: Warming up for 3.0000 s
//   Warning: Unable to complete 10 samples in 5.0s. You may wish to increase target time to 15.5s.
//   parens 10000            time:   [1.4998 s 1.5037 s 1.5079 s]
//                           change: [-3.0709% -2.6398% -2.2016%] (p = 0.00 < 0.05)
//                           Performance has improved.
//
// New algorithm:
//
//   parens 0                time:   [197.35 ns 197.92 ns 198.47 ns]
//                           change: [+211.04% +214.39% +216.96%] (p = 0.00 < 0.05)
//                           Performance has regressed.
//
//   parens 10               time:   [1.9553 µs 1.9603 µs 1.9714 µs]
//                           change: [+43.472% +44.784% +46.481%] (p = 0.00 < 0.05)
//                           Performance has regressed.
//
//   parens 100              time:   [16.963 µs 16.991 µs 17.053 µs]
//                           change: [-78.248% -78.125% -78.012%] (p = 0.00 < 0.05)
//                           Performance has improved.
//
//   parens 1000             time:   [167.15 µs 170.29 µs 172.34 µs]
//                           change: [-97.630% -97.608% -97.585%] (p = 0.00 < 0.05)
//                           Performance has improved.
//
//   parens 10000            time:   [2.0903 ms 2.1049 ms 2.1217 ms]
//                           change: [-99.860% -99.859% -99.857%] (p = 0.00 < 0.05)
//                           Performance has improved.
//
fn many_parens(c: &mut Criterion) {
    c.bench_function("parens 0", |b| {
        let input0 = build_paren_input(0);
        b.iter(|| {
            let _ = parser::parse_program(&input0);
        })
    });
    c.bench_function("parens 10", |b| {
        let input10 = build_paren_input(10);
        b.iter(|| {
            let _ = parser::parse_program(&input10);
        })
    });
    c.bench_function("parens 100", |b| {
        let input100 = build_paren_input(100);
        b.iter(|| {
            let _ = parser::parse_program(&input100);
        })
    });
    c.bench_function("parens 1000", |b| {
        let input1000 = build_paren_input(1000);
        b.iter(|| {
            let _ = parser::parse_program(&input1000);
        })
    });
    c.bench_function("parens 10000", |b| {
        let input10k = build_paren_input(10_000);
        b.iter(|| {
            let _ = parser::parse_program(&input10k);
        })
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(10);
    targets = many_parens
}
criterion_main!(benches);
