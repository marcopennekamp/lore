import times, os

import evaluator
import values
from utils import when_release

from examples/fib import example_function, example_arguments, example_runs

# TODO (vm): Move this to its own module.
template benchmark(benchmark_name: string, runs: int, code: untyped) =
  block:
    let t0 = epochTime()
    var i: int = 0
    while i < runs:
      code
      i += 1
    let elapsed = epochTime() - t0
    let elapsed_ns = elapsed * 1_000_000_000
    let per_run = uint(elapsed_ns / runs.float)
    echo benchmark_name, ": ", per_run, "ns/op"

let value = evaluator.evaluate(example_function, example_arguments)
echo cast[IntValue](value)[]

when_release:
  benchmark("VM run", example_runs):
    discard evaluator.evaluate(example_function, example_arguments)

#[
# TODO (vm): Move this to `types.nim` with `when main:`.
let sum1 = types.sum(@[types.string, types.int, types.boolean])
let sum2 = types.sum(@[types.string, types.int, types.boolean])
let sum3 = types.sum(@[types.real, types.boolean])

echo types.are_equal(sum1, sum1)
benchmark("sum1 == sum1", 100_000_000):
  discard types.are_equal(sum1, sum1)

echo types.are_equal(sum1, sum2)
benchmark("sum1 == sum2", 100_000_000):
  discard types.are_equal(sum1, sum2)

echo types.are_equal(sum1, sum2)
benchmark("sum1 == sum3", 100_000_000):
  discard types.are_equal(sum1, sum3)

let tuple1 = types.`tuple`(@[
  types.sum(@[types.string, types.int, types.boolean]),
  types.intersection(@[types.string, types.int, types.boolean]),
  types.list(types.map(types.string, types.int)),
])

let tuple2 = types.`tuple`(@[
  types.sum(@[types.string, types.int, types.boolean]),
  types.intersection(@[types.string, types.int, types.boolean]),
  types.list(types.map(types.string, types.int)),
])

echo types.are_equal(tuple1, tuple2)
benchmark("tuple1 == tuple2", 100_000_000):
  discard types.are_equal(tuple1, tuple2)
]#
