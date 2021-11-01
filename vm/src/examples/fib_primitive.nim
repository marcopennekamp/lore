from bytecode import Operation, Instruction, Function, Constants, new_instruction
from common import Example
from evaluator import init_frame_stats

## In contrast to the `fib` example, we forego any boxing here and just work with Ints. This optimization will have to
## be made by the Lore compiler, which can only do this if e.g. `fib`'s first argument is ALWAYS an Int, and not a type
## like `Real | Int`.
let fib_primitive = Function(
  name: "fib_primitive",
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
  constants: nil,
)
init_frame_stats(fib_primitive)

let constants = Constants(
  functions: @[fib_primitive],
)

fib_primitive.constants = constants

let test = Function(
  name: "test",
  register_count: 1,
  code: @[
    new_instruction(Operation.IntConst, 0, 10),
    new_instruction(Operation.Dispatch1, 0, 0, 0),
    new_instruction(Operation.IntBox, 0, 0),
    new_instruction(Operation.Return0),
  ],
  constants: constants,
)
init_frame_stats(test)

let example* = Example(
  name: "fib primitive",
  function: test,
  arguments: @[],
  runs: 500_000,
)
