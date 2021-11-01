from bytecode import Operation, Instruction, Function, Constants, new_instruction
from common import Example
from evaluator import init_frame_stats

let constants = Constants(functions: @[])

let empty = Function(
  name: "empty",
  locals_size: 0,
  code: @[
    new_instruction(Operation.Return, 0, 0),
  ],
  constants: constants,
)
init_frame_stats(empty)

let example* = Example(
  name: "empty",
  function: empty,
  arguments: @[],
  runs: 50_000_000,
)
