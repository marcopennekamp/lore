from common import Example
from evaluator import init_frame_stats
from functions import MultiFunction, Function, Constants, new_constants
from instructions import Operation, Instruction, new_instruction

let constants = new_constants()

let empty_0 = Function(
  register_count: 1,
  code: @[
    new_instruction(Operation.Return, 0),
  ],
  constants: constants,
)
init_frame_stats(empty_0)

let empty = MultiFunction(
  name: "empty",
  functions: @[empty_0],
)
empty_0.multi_function = empty

let example* = Example(
  name: "empty",
  function: empty_0,
  arguments: @[],
  runs: 50_000_000,
)
