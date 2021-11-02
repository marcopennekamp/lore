from common import Example
from evaluator import init_frame_stats
from functions import MultiFunction, Function, Constants, new_constants
from instructions import Operation, Instruction, new_instruction

let constants = new_constants()

let nine_0 = Function(
  register_count: 2,
  code: @[
    new_instruction(Operation.IntBoxConst, 0, 1),          # This simulates an Int argument.
    new_instruction(Operation.IntUnbox, 1, 0),
    new_instruction(Operation.IntAddConst, 1, 1, 2),
    new_instruction(Operation.IntAddConst, 1, 1, 3),
    new_instruction(Operation.IntAddConst, 1, 1, 4),
    new_instruction(Operation.IntAddConst, 1, 1, 0xffff),  # 0xffff is -1 as an int16.
    new_instruction(Operation.IntBox, 0, 1),
    new_instruction(Operation.Return0),
  ],
  constants: constants,
)
init_frame_stats(nine_0)

let nine = MultiFunction(
  name: "nine",
  functions: @[nine_0],
)
nine_0.multi_function = nine

let example* = Example(
  name: "nine",
  function: nine_0,
  arguments: @[],
  runs: 50_000_000,
)
