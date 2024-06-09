use criterion::{criterion_group, criterion_main, Criterion};

fn noop(c: &mut Criterion) {
    c.bench_function("noop", |b| b.iter(|| ()));
}

criterion_group!(benches, noop);
criterion_main!(benches);
