from examples/common import Example
import evaluator
import values
from utils import when_release, benchmark

proc bench_example(example: Example) =
  benchmark(example.name, example.runs):
    discard evaluator.evaluate(example.function, example.arguments)
  echo "Result: ", cast[IntValue](evaluator.evaluate(example.function, example.arguments))[]

when_release:
  import examples/nine, examples/add_five, examples/fib
  bench_example(nine.example)
  bench_example(add_five.example)
  bench_example(fib.example)
