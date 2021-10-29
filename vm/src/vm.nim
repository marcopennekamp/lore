from examples/fib import example_function, example_arguments, example_runs
import evaluator
import types
import values
from utils import when_release, benchmark

let value = evaluator.evaluate(example_function, example_arguments)
echo cast[IntValue](value)[]

when_release:
  benchmark("VM run", example_runs):
    discard evaluator.evaluate(example_function, example_arguments)
