from bytecode import Operation, Instruction, Function, Constants, new_instruction
from common import Example

let constants = Constants(functions: @[])

let empty = Function(
  name: "empty",
  stack_size: 0,
  locals_size: 0,
  ref_stack_size: 0,
  ref_locals_size: 0,
  code: @[
    new_instruction(Operation.Return, 0, 0),
  ],
  constants: constants,
)

let example* = Example(
  name: "empty",
  function: empty,
  arguments: @[],
  runs: 50_000_000,
)
