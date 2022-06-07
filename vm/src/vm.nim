import os

import definitions
from evaluator import nil
import imseqs
from poems import nil
from specs import ModuleNameFilter, run_tests, run_benchmarks
from types import `$`
from universes import nil
from utils import with_frame_mem
from values import `$`, get_type

proc run_and_print*(function_instance: ptr FunctionInstance, frame_mem: pointer) =
  let res = evaluator.evaluate(function_instance, frame_mem)
  echo "Result: ", res, " :: ", res.get_type

########################################################################################################################
# CLI.                                                                                                                 #
########################################################################################################################

const help =
  """
Please run `vm.nim` with one of the following commands:

- `run`: Execute a poem file. `run` expects a `.poem` file as the first and the entry function's name as the second argument. (Usage: `vm run binary.poem test`)
- specs:
  - `test`: Execute test specs contained in a poem file. `test` expects a `.poem` file as the first argument. Additional arguments are interpreted as module names (see below). (Usage: `vm test binary.poem`)
  - `bench`: Execute benchmark specs contained in a poem file. `bench` expects a `.poem` file as the first argument. Additional arguments are interpreted as module names (see below). (Usage: `vm bench binary.poem`)
  - Any additional arguments to `test` and `bench` are interpreted as module names. If no module names are specified, all specs will be run. If at least one module name is specified, only specs whose module is one of the specified modules (or a sub-module) will be run. (Usage: `vm test binary.poem lessons language.dispatch`)
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
      run_and_print(target, frame_mem)
    )
  elif command == "test" or command == "bench":
    require_min_cli_arg_count(2)
    let universe = load_universe(param_str(2))
    let module_names = command_line_params()[2 ..< param_count()]
    let module_name_filter = ModuleNameFilter(new_immutable_seq[string](module_names))
    if command == "test":
      universe.run_tests(module_name_filter)
    else:
      universe.run_benchmarks(module_name_filter)
  else:
    echo help

when is_main_module:
  run_vm()
