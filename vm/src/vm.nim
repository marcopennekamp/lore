import os

import definitions
from evaluator import nil
from poems import nil
from specs import run_tests
from types import `$`
from universes import nil
from utils import with_frame_mem, benchmark
from values import `$`, get_type

proc run_and_print*(function_instance: ptr FunctionInstance, frame_mem: pointer) =
  let res = evaluator.evaluate(function_instance, frame_mem)
  echo "Result: ", res, " :: ", res.get_type

########################################################################################################################
# CLI.                                                                                                                 #
########################################################################################################################

const runs = 1
const help =
  """
Please run `vm.nim` with one of the following commands:

- `run`: Execute a poem file. `run` expects a `.poem` file as the first and the entry function's name as the second argument. (Usage: `vm run binary.poem test`)
- `test`: Execute test specs contained in a poem file. `test` expects a `.poem` file as the first argument. (Usage: `vm test binary.poem`)
  """

proc load_universe(poem_file_path: string): Universe =
  let poem = poems.read_poem(param_str(2))
  let universe = universes.resolve(@[poem])
  set_active_universe(universe)
  universe

proc require_min_cli_arg_count(expected: int) =
  if param_count() < expected:
    echo help
    quit()

proc run_vm() =
  require_min_cli_arg_count(1)
  let command = param_str(1)
  if command == "run":
    require_min_cli_arg_count(3)
    let universe = load_universe(param_str(2))
    let target = universe.get_multi_function(param_str(3)).get_single_monomorphic_function_instance
    with_frame_mem(proc (frame_mem: pointer) =
      when runs == 1:
        run_and_print(target, frame_mem)
      else:
        benchmark("Execution time", runs):
          discard evaluator.evaluate(target, frame_mem)
    )
  elif command == "test":
    require_min_cli_arg_count(2)
    let universe = load_universe(param_str(2))
    universe.run_tests()
  else:
    echo help

when is_main_module:
  run_vm()
