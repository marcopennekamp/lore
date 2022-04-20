from definitions import FunctionInstance, set_active_universe
from evaluator import nil
from types import `$`
from values import `$`, get_type

proc run_and_print*(function_instance: ptr FunctionInstance, frame_mem: pointer) =
  let res = evaluator.evaluate(function_instance, frame_mem)
  echo "Result: ", res, " :: ", get_type(res)

when is_main_module:
  import os, tables

  from poems import nil
  from universes import nil
  from utils import with_frame_mem, benchmark

  let help = "Please run `vm.nim` with a `.poem` file as the first and the entry function's name as the second argument." &
    " The entry function should be a single function."
  if param_count() >= 2:
    let poem = poems.read_poem(param_str(1))
    let universe = universes.resolve(@[poem])
    set_active_universe(universe)
    let target = addr universe.multi_functions[param_str(2)].functions[0].monomorphic_instance
    with_frame_mem(proc (frame_mem: pointer) =
      benchmark("Execution time", 1):
        run_and_print(target, frame_mem)
    )
  else:
    echo help
