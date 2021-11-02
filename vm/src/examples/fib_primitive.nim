from common import Example
from evaluator import init_frame_stats
from functions import MultiFunction, Function, Constants, new_constants
from instructions import Operation, Instruction, new_instruction

let constants = new_constants()

## In contrast to the `fib` example, we forego any boxing here and just work with Ints. This optimization will have to
## be made by the Lore compiler, which can only do this if e.g. `fib`'s first argument is ALWAYS an Int, and not a type
## like `Real | Int`.
let fib_primitive_0 = Function(
  register_count: 3,
  code: @[
    new_instruction(Operation.IntGtConst, 1, 0, 1),
    new_instruction(Operation.JumpIfFalse, 7, 1),

    new_instruction(Operation.IntSubConst, 1, 0, 1),
    new_instruction(Operation.Dispatch1, 1, 0, 1),
    new_instruction(Operation.IntSubConst, 2, 0, 2),
    new_instruction(Operation.Dispatch1, 2, 0, 2),
    new_instruction(Operation.IntAdd, 0, 1, 2),

    new_instruction(Operation.Return0),                  # 7
  ],
  constants: constants,
)
init_frame_stats(fib_primitive_0)

let fib_primitive = MultiFunction(
  name: "fib_primitive",
  functions: @[fib_primitive_0],
)
fib_primitive_0.multi_function = fib_primitive

let test_0 = Function(
  register_count: 1,
  code: @[
    new_instruction(Operation.IntConst, 0, 10),
    new_instruction(Operation.Dispatch1, 0, 0, 0),
    new_instruction(Operation.IntBox, 0, 0),
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

constants.multi_functions = @[fib_primitive]

let example* = Example(
  name: "fib primitive",
  function: test_0,
  arguments: @[],
  runs: 500_000,
)
