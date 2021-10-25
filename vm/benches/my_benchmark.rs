extern crate criterion;
extern crate lore;

use std::time::Duration;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use lore::evaluator::evaluate;
use lore::instruction::{Instruction, Operation};
use lore::tpe::*;

pub fn criterion_benchmark(c: &mut Criterion) {
    let sum2: Type = Type::Sum { parts: vec![Type::String, Type::Int, Type::Boolean] };
    let sum3: Type = Type::Sum { parts: vec![Type::String, Type::Int, Type::Boolean] };

    let tuple1: Type = Type::Tuple {
        elements: vec![
            Type::Sum { parts: vec![Type::String, Type::Int, Type::Boolean] },
            Type::Intersection { parts: vec![Type::String, Type::Int, Type::Boolean] },
            Type::List { element: Box::new(Type::Map { key: Box::new(Type::String), value: Box::new(Type::Int) }) }
        ]
    };
    let tuple2: Type = Type::Tuple {
        elements: vec![
            Type::Sum { parts: vec![Type::String, Type::Int, Type::Boolean] },
            Type::Intersection { parts: vec![Type::String, Type::Int, Type::Boolean] },
            Type::List { element: Box::new(Type::Map { key: Box::new(Type::String), value: Box::new(Type::Int) }) }
        ]
    };

    let instructions = vec![
        Instruction { operation: Operation::IntPush, arguments: [1, 0] },
        Instruction { operation: Operation::IntPush, arguments: [2, 0] },
        Instruction { operation: Operation::IntAdd, arguments: [0, 0] },
        Instruction { operation: Operation::IntPush, arguments: [3, 0] },
        Instruction { operation: Operation::IntPush, arguments: [4, 0] },
        Instruction { operation: Operation::IntAdd, arguments: [0, 0] },
        Instruction { operation: Operation::IntAdd, arguments: [0, 0] },
        Instruction { operation: Operation::IntPush, arguments: [0xffff, 0] },
        Instruction { operation: Operation::IntAdd, arguments: [0, 0] },
        Instruction { operation: Operation::Return, arguments: [0, 0] },
    ];

    let instructions2 = vec![
        Instruction { operation: Operation::Return, arguments: [0, 0] },
    ];

    c.bench_function("1 + 2 + 3 + 4 - 1", |b| b.iter(|| evaluate(black_box(&instructions), black_box(0))));

    /* c.bench_function("are_equal (string, string)", |b| b.iter(|| are_equal(black_box(&Type::String), black_box(&Type::String))));
    c.bench_function("are_equal (string, int)", |b| b.iter(|| are_equal(black_box(&Type::String), black_box(&Type::Int))));

    c.bench_function("are_equal sums (referential)", |b| b.iter(|| are_equal(black_box(&sum2), black_box(&sum2))));
    c.bench_function("are_equal sums (structural)", |b| b.iter(|| are_equal(black_box(&sum2), black_box(&sum3))));
    c.bench_function("are_equal sums (structural, no black box)", |b| b.iter(|| are_equal(&sum2, &sum3)));

    c.bench_function("are_equal tuples (referential)", |b| b.iter(|| are_equal(black_box(&tuple1), black_box(&tuple1))));
    c.bench_function("are_equal tuples (structural)", |b| b.iter(|| are_equal(black_box(&tuple1), black_box(&tuple2)))); */


}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
