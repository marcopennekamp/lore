from common import Example
from evaluator import init_frame_stats
from functions import MultiFunction, Function, Constants, new_constants
from instructions import Operation, Instruction, new_instruction

let constants = new_constants()

let add_five_0 = Function(
  register_count: 1,
  code: @[
    new_instruction(Operation.IntAddConst, 0, 0, 5),
    new_instruction(Operation.Return0),
  ],
  constants: constants,
)
init_frame_stats(add_five_0)

let add_five = MultiFunction(
  name: "add_five",
  functions: @[add_five_0],
)
add_five_0.multi_function = add_five

let test_0 = Function(
  register_count: 1,
  code: @[
    new_instruction(Operation.IntConst, 0, 7),
    new_instruction(Operation.Dispatch1, 0, 0, 0),
    new_instruction(Operation.Return0),
  ],
  constants: constants,
)
init_frame_stats(test_0)

let test = MultiFunction(
  name: "test",
  functions: @[test_0],
)
test_0.multi_function = test

constants.multi_functions = @[add_five]

let example* = Example(
  name: "add five",
  function: test_0,
  arguments: @[],
  runs: 50_000_000,
)
