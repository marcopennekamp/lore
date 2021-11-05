import os, std/strformat, sugar, tables

from "../evaluator" import nil
from "../functions" import Function
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

type Example* = ref object
  name*: string
  poem*: Poem

  ## The main function target that is run or benchmarked. This must be a single function.
  main*: string

  ## The number of times the example will be run when benchmarking.
  runs*: int

let examples = @[
  Example(name: "empty", poem: empty.poem, main: "empty", runs: 100_000_000),
  Example(name: "nine", poem: nine.poem, main: "nine", runs: 100_000_000),
  Example(name: "add_five", poem: add_five.poem, main: "test", runs: 100_000_000),
  Example(name: "fib", poem: fib.poem, main: "test", runs: 1_000_000),
]

proc prepare_example(example: Example): Function =
  let poem = poems.read(fmt"target/{example.name}.poem")
  let universe = universes.resolve(@[poem])
  let target = universe.multi_functions[example.main].functions[0]
  target

########################################################################################################################
# Example writing.                                                                                                     #
########################################################################################################################

proc write_all() =
  for example in examples:
    poems.write(fmt"target/{example.name}.poem", example.poem)

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
