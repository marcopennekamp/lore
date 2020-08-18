extern crate criterion;
extern crate lore;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use lore::types::*;

pub fn criterion_benchmark(c: &mut Criterion) {
    let sum2 = Type::Sum { parts: vec![Type::String, Type::Int, Type::Boolean] };
    let sum3 = Type::Sum { parts: vec![Type::String, Type::Int, Type::Boolean] };

    c.bench_function("test are_equal", |b| b.iter(|| are_equal(black_box(&sum2), black_box(&sum3))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
