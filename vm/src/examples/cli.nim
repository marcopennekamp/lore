import os, std/strformat, sugar, tables

from "../definitions" import Function, FunctionInstance, set_active_universe, is_monomorphic
from "../evaluator" import nil
from "../poems" import Poem
from "../universes" import nil
from "../utils" import with_frame_mem, benchmark
from "../vm" import nil

########################################################################################################################
# Examples definitions.                                                                                                #
########################################################################################################################

from empty import nil
from nine import nil
from add_five import nil
from fib import nil
from hello_name import nil
from add_tuples import nil
from lambdas import nil
from multi_function_values import nil
from list_appends import nil
from list_concat import nil
from list_get import nil
from list_print_each import nil
from shape_constant import nil
from shape_positions import nil
from stats import nil
from schema_options import nil
from tau import nil

type Example* = ref object
  name*: string
  poem*: Poem

  ## The main function target that is run or benchmarked. This must be a single function.
  main*: string

  ## The number of times the example will be run when benchmarking. A value of 0 disables benchmarks for this example.
  runs*: int

let examples = @[
  Example(name: "empty", poem: empty.poem, main: "empty", runs: 100_000_000),
  Example(name: "nine", poem: nine.poem, main: "nine", runs: 100_000_000),
  Example(name: "add_five", poem: add_five.poem, main: "test", runs: 100_000_000),
  Example(name: "fib", poem: fib.poem, main: "test", runs: 1_000_000),
  Example(name: "hello_name", poem: hello_name.poem, main: "test", runs: 5_000_000),
  Example(name: "add_tuples", poem: add_tuples.poem, main: "test", runs: 25_000_000),
  Example(name: "lambdas", poem: lambdas.poem, main: "test", runs: 100_000_000),
  Example(name: "multi_function_values", poem: multi_function_values.poem, main: "test", runs: 5_000_000),
  Example(name: "list_appends", poem: list_appends.poem, main: "test", runs: 25_000_000),
  Example(name: "list_concat", poem: list_concat.poem, main: "test", runs: 5_000_000),
  Example(name: "list_get", poem: list_get.poem, main: "test", runs: 25_000_000),
  # `list_print_each` shouldn't be benchmarked because it prints to stdout.
  Example(name: "list_print_each", poem: list_print_each.poem, main: "test", runs: 0),
  Example(name: "shape_constant", poem: shape_constant.poem, main: "test", runs: 25_000_000),
  Example(name: "shape_positions", poem: shape_positions.poem, main: "test", runs: 1_000_000),
  Example(name: "stats", poem: stats.poem, main: "test", runs: 5_000_000),
  Example(name: "schema_options", poem: schema_options.poem, main: "test", runs: 1_000_000),
  Example(name: "tau", poem: tau.poem, main: "test", runs: 25_000_000),
]

proc prepare_example(example: Example): ptr FunctionInstance =
  let poem = poems.read_poem(fmt"target/{example.name}.poem")
  let universe = universes.resolve(@[poem])
  set_active_universe(universe)
  let function = universe.multi_functions[example.main].functions[0]
  if not function.is_monomorphic:
    quit(fmt"The example {example.name} must have a monomorphic entry function.")
  addr function.monomorphic_instance

########################################################################################################################
# Example writing.                                                                                                     #
########################################################################################################################

proc write_all() =
  for example in examples:
    poems.write_poem(fmt"target/{example.name}.poem", example.poem)

########################################################################################################################
# Running all examples.                                                                                                #
########################################################################################################################

proc run_all() =
  write_all()
  for example in examples:
    let target = prepare_example(example)
    with_frame_mem((frame_mem: pointer) => vm.run_and_print(target, frame_mem))

########################################################################################################################
# Example benchmarks.                                                                                                  #
########################################################################################################################

proc bench_all() =
  write_all()
  for example in examples:
    let target = prepare_example(example)

    # This bypasses the problem that `example` cannot be captured in the lambda below because it is a `lent Example`.
    let name = example.name
    let runs = example.runs

    if runs > 0:
      with_frame_mem(proc (frame_mem: pointer) =
        benchmark(name, runs):
          discard evaluator.evaluate(target, frame_mem)
        vm.run_and_print(target, frame_mem)
      )

########################################################################################################################
# CLI commands.                                                                                                        #
########################################################################################################################

# This needs to be at the bottom of the file so that `examples` is properly initialized first.
let help = "Please run `cli.nim` with the commands `run`, `bench`, or `write`."
if param_count() >= 1:
  let command = param_str(1)
  case command
  of "run": run_all()
  of "bench": bench_all()
  of "write": write_all()
  else: echo help
else: echo help
