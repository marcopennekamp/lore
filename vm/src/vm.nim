import sugar

from examples/common import Example
import evaluator
import values
from utils import when_release, when_debug, benchmark

import examples/empty, examples/nine, examples/add_five, examples/fib, examples/fib_primitive
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
      if (res != nil):
        echo "Result: ", cast[IntValue](res)[]
      else:
        echo "Result: nil"
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
  fib_primitive.example,
]

when_release:
  for example in examples:
    bench_example(example)
  benchmark("fib native", 5_000_000):
    discard test_fib()

when_debug:
  for example in examples:
    run_example(example, 1)
