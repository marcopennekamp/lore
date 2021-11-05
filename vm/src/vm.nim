import evaluator
from functions import Function
import values

proc run_and_print*(target: Function, frame_mem: pointer) =
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

when is_main_module:
  import os, sugar, tables

  import poems
  import universes
  from utils import with_frame_mem

  let help = "Please run `vm.nim` with a `.poem` file as the first and the entry function's name as the second argument." &
    " The entry function should be a single function."
  if param_count() >= 2:
    let poem = poems.read(param_str(1))
    let universe = universes.resolve(@[poem])
    let target = universe.multi_functions[param_str(2)].functions[0]
    with_frame_mem((frame_mem: pointer) => run_and_print(target, frame_mem))
  else:
    echo help
