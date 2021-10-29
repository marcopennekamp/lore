from bytecode import Operation, Instruction, Function, Constants, new_instruction
from common import Example

let add_five = Function(
  name: "add_five",
  arguments_count: 1,
  locals_count: 0,
  code: @[
    new_instruction(Operation.ArgumentLoad, 0, 0),
    new_instruction(Operation.IntUnbox, 0, 0),
    new_instruction(Operation.IntPush, 5, 0),
    new_instruction(Operation.IntAdd, 0, 0),
    new_instruction(Operation.IntBox, 0, 0),
    new_instruction(Operation.Return, 0, 0),
  ],
  constants: nil,
)

let constants = Constants(
  functions: @[add_five],
)

add_five.constants = constants

let test = Function(
  name: "test",
  arguments_count: 0,
  locals_count: 0,
  code: @[
    new_instruction(Operation.IntBoxPush, 7, 0),
    new_instruction(Operation.Dispatch, 1, 0),
    new_instruction(Operation.Return, 0, 0),
  ],
  constants: constants,
)

let example* = Example(
  name: "add five",
  function: test,
  arguments: @[],
  runs: 50_000_000,
)
