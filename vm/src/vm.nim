import times, os, strutils

from bytecode import Operation, Instruction, new_instruction
import evaluator
import types
import values

# TODO (vm): Move this to its own module.
template benchmark(benchmark_name: string, runs: uint, code: untyped) =
  block:
    let t0 = epochTime()
    var i = 0
    while i < runs:
      code
      i += 1
    let elapsed = epochTime() - t0
    let elapsed_ns = elapsed * 1_000_000_000
    let per_run = uint(elapsed_ns / runs)
    echo benchmark_name, ": ", per_run, "ns/op"

let code = @[
  new_instruction(Operation.IntBoxPush, 1, 0), # This simulates loading an Int argument.
  new_instruction(Operation.IntUnbox, 0, 0),
  new_instruction(Operation.IntPush, 2, 0),
  new_instruction(Operation.IntAdd, 0, 0),
  new_instruction(Operation.IntPush, 3, 0),
  new_instruction(Operation.IntPush, 4, 0),
  new_instruction(Operation.IntAdd, 0, 0),
  new_instruction(Operation.IntAdd, 0, 0),
  new_instruction(Operation.IntPush, 0xffff, 0), # 0xffff is -1 as an int16.
  new_instruction(Operation.IntAdd, 0, 0),
  new_instruction(Operation.IntBox, 0, 0),
  new_instruction(Operation.Return, 0, 0),
]

let value = evaluator.evaluate(code)
echo cast[IntValue](value)[]

benchmark("VM run", 100_000_000):
  discard evaluator.evaluate(code)

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
