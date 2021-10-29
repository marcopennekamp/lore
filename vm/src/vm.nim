from examples/common import Example
import evaluator
import values
from utils import when_release, when_debug, benchmark

import examples/nine, examples/add_five, examples/fib

proc run_example(example: Example) =
  echo "Result: ", cast[IntValue](evaluator.evaluate(example.function, example.arguments))[]

proc bench_example(example: Example) =
  benchmark(example.name, example.runs):
    discard evaluator.evaluate(example.function, example.arguments)
  run_example(example)

let examples = [
  nine.example,
  add_five.example,
  fib.example,
]

when_release:
  for example in examples:
    bench_example(example)

when_debug:
  for example in examples:
    bench_example(example)
