use std::collections::VecDeque;

use criterion::{criterion_group, criterion_main, Criterion};

use findlib::parser;

fn build_paren_input(n_parens: usize) -> Vec<&'static str> {
    let mut input: Vec<&str> = Vec::with_capacity(n_parens * 2 + 1);
    for _i in 0..n_parens {
        input.push_back("(");
    }
    input.push_back("-print");
    for _i in 0..n_parens {
        input.push_back(")");
    }
    input
}

// Our current (I suppose quadratic) algorithm for extracting
// parenthesised expressions is unsurprisingly slow flr large numbers
// of parentheses.  Here is a benchmark result:
//
// parens 0                time:   [62.758 ns 62.952 ns 63.363 ns]
//                         change: [-17.647% -13.590% -9.7491%] (p = 0.00 < 0.05)
//                         Performance has improved.
//
// parens 10               time:   [1.3519 µs 1.3584 µs 1.3681 µs]
//                         change: [-1.7614% -1.0598% -0.3665%] (p = 0.06 > 0.05)
//                         No change in performance detected.
//
// parens 100              time:   [77.259 µs 77.529 µs 78.053 µs]
//                         change: [-1.8876% -0.9837% -0.0573%] (p = 0.27 > 0.05)
//                         No change in performance detected.
//
// parens 1000             time:   [15.281 ms 15.382 ms 15.451 ms]
//                         change: [+115.26% +116.66% +118.08%] (p = 0.00 < 0.05)
//                         Performance has regressed.
//
// Benchmarking parens 10000: Warming up for 3.0000 s
// Warning: Unable to complete 10 samples in 5.0s. You may wish to increase target time to 15.3s.
// parens 10000            time:   [1.5207 s 1.5241 s 1.5276 s]
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
