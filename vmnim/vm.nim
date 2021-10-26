import times, os, strutils

from bytecode import Operation, Instruction, new_instruction
import evaluator
import values

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
    echo "CPU Time [", benchmark_name, "] ", per_run, "ns/op"

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

benchmark("VM run", 50_000_000):
  discard evaluator.evaluate(code)
