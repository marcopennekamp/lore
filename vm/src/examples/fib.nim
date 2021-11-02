from common import Example
from evaluator import init_frame_stats
from functions import MultiFunction, Function, Constants, new_constants
from instructions import Operation, Instruction, new_instruction

let constants = new_constants()

let fib_0 = Function(
  register_count: 3,
  code: @[
    new_instruction(Operation.IntBoxGtConst, 1, 0, 1),
    new_instruction(Operation.JumpIfFalse, 7, 1),

    new_instruction(Operation.IntBoxSubConst, 1, 0, 1),
    new_instruction(Operation.Dispatch1, 1, 0, 1),
    new_instruction(Operation.IntBoxSubConst, 2, 0, 2),
    new_instruction(Operation.Dispatch1, 2, 0, 2),
    new_instruction(Operation.IntBoxAdd, 0, 1, 2),

    new_instruction(Operation.Return0),                  # 7
  ],
  constants: constants,
)
init_frame_stats(fib_0)

let fib = MultiFunction(
  name: "fib",
  functions: @[fib_0],
)
fib_0.multi_function = fib

let test_0 = Function(
  register_count: 1,
  code: @[
    new_instruction(Operation.IntBoxConst, 0, 10),
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

constants.multi_functions = @[fib]

let example* = Example(
  name: "fib",
  function: test_0,
  arguments: @[],
  runs: 500_000,
)
