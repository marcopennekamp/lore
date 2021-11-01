from bytecode import Operation, Instruction, Function, Constants, new_instruction
from common import Example
from evaluator import init_frame_stats

let add_five = Function(
  name: "add_five",
  register_count: 1,
  code: @[
    new_instruction(Operation.IntBoxAddConst, 0, 0, 5),
    new_instruction(Operation.Return0),
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
  register_count: 1,
  code: @[
    new_instruction(Operation.IntBoxConst, 0, 1),
    new_instruction(Operation.Dispatch1, 0, 0, 0),
    new_instruction(Operation.Return0),
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
