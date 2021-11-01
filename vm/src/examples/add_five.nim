from bytecode import Operation, Instruction, Function, Constants, new_instruction
from common import Example
from evaluator import init_frame_stats

let add_five = Function(
  name: "add_five",
  locals_size: 0,
  code: @[
    new_instruction(Operation.IntUnbox, 0, 0),
    new_instruction(Operation.IntPush, 5, 0),
    new_instruction(Operation.IntAdd, 0, 0),
    new_instruction(Operation.IntBox, 0, 0),
    new_instruction(Operation.Return, 0, 0),
  ],
  constants: nil,
)
init_frame_stats(add_five)

let constants = Constants(
  functions: @[add_five],
)

add_five.constants = constants

let test = Function(
  name: "test",
  locals_size: 0,
  code: @[
    new_instruction(Operation.IntBoxPush, 7, 0),
    new_instruction(Operation.Dispatch, 1, 0),
    new_instruction(Operation.Return, 0, 0),
  ],
  constants: constants,
)
init_frame_stats(test)

let example* = Example(
  name: "add five",
  function: test,
  arguments: @[],
  runs: 50_000_000,
)
