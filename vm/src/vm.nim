import sugar
import tables

from examples/common import Example
import evaluator
import values
from utils import when_release, when_debug, benchmark

from functions import Function
from poems import Poem
import universes

# TODO (vm): Should be a util, perhaps.
proc with_frame_mem(f: (pointer) -> void) =
  # Note that alloc0 allocates memory specifically on the Boehm GC heap when the Boehm GC is selected.
  let frame_mem: pointer = alloc0(sizeof(uint64) * 250_000)
  f(frame_mem)
  dealloc(frame_mem)

# TODO (vm): This should also be a util as it will be needed by `run_all.nim` as well as this main VM file.
proc run(target: Function, frame_mem: pointer) =
  let res = evaluator.evaluate(target, frame_mem)
  if values.is_reference(res):
    if res.reference != nil:
      echo "Result: Some reference..."
    else:
      echo "Result: nil"
  elif values.is_int(res):
    echo "Result: ", values.untag_int(res)
  elif values.is_boolean(res):
    echo "Result: ", values.untag_boolean(res)
  else:
    echo "Result: unknown"

proc bench(name: string, target: Function, runs: int, frame_mem: pointer) =
  benchmark(name, runs):
    discard evaluator.evaluate(target, frame_mem)
  run(target, frame_mem)

# TODO (vm): The file's name should be a CLI argument.
let poem = poems.read("target/nine.poem")
let universe = universes.resolve(@[poem])
let target = universe.multi_functions["nine"].functions[0]

when_debug:
  with_frame_mem(proc (frame_mem: pointer) = run(target, frame_mem))

when_release:
  with_frame_mem(proc (frame_mem: pointer) = bench("nine", target, 50_000_000, frame_mem))

# TODO (vm): Move these benchmarks and runs to separate `benchmark_all.nim` and `run_all.nim` files.

#[
import examples/empty, examples/nine, examples/add_five, examples/fib
from fib_native import test_fib

proc with_frame_mem(f: (pointer) -> void) =
  # Note that alloc0 allocates memory specifically on the Boehm GC heap when the Boehm GC is selected.
  let frame_mem: pointer = alloc0(sizeof(uint64) * 250_000)
  f(frame_mem)
  dealloc(frame_mem)

proc run_example(example: Example, runs: int) =
  with_frame_mem(proc (frame_mem: pointer) =
    var i = 0
    while i < runs:
      let res = evaluator.evaluate(example.function, frame_mem)
      if values.is_reference(res):
        if res.reference != nil:
          echo "Result: Some reference..."
        else:
          echo "Result: nil"
      elif values.is_int(res):
        echo "Result: ", values.untag_int(res)
      elif values.is_boolean(res):
        echo "Result: ", values.untag_boolean(res)
      else:
        echo "Result: unknown"
      i += 1
  )

proc bench_example(example: Example) =
  with_frame_mem(proc (frame_mem: pointer) =
    benchmark(example.name, example.runs):
      discard evaluator.evaluate(example.function, frame_mem)
  )
  run_example(example, 1)

let examples = [
  empty.example,
  nine.example,
  add_five.example,
  fib.example,
]

when_release:
  for example in examples:
    bench_example(example)
  benchmark("fib native", 5_000_000):
    discard test_fib()

when_debug:
  for example in examples:
    run_example(example, 1)
]#
