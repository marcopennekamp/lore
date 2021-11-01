from bytecode import Operation, Instruction, Function, Constants, new_instruction
from common import Example
from evaluator import init_frame_stats

let fib = Function(
  name: "fib",
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
  constants: nil,
)
init_frame_stats(fib)

let constants = Constants(
  functions: @[fib],
)

fib.constants = constants

let test = Function(
  name: "test",
  register_count: 1,
  code: @[
    new_instruction(Operation.IntBoxConst, 0, 10),
    new_instruction(Operation.Dispatch1, 0, 0, 0),
    new_instruction(Operation.Return0),
  ],
  constants: constants,
)
init_frame_stats(test)

let example* = Example(
  name: "fib",
  function: test,
  arguments: @[],
  runs: 500_000,
)
