from bytecode import Operation, Instruction, Function, Constants, new_instruction
from common import Example
from evaluator import init_frame_stats

let constants = Constants(functions: @[])

let nine = Function(
  name: "nine",
  register_count: 2,
  code: @[
    new_instruction(Operation.IntBoxConst, 0, 1),          # This simulates an Int argument.
    new_instruction(Operation.IntUnbox, 1, 0),
    new_instruction(Operation.IntAddConst, 1, 1, 2),
    new_instruction(Operation.IntAddConst, 1, 1, 3),
    new_instruction(Operation.IntAddConst, 1, 1, 4),
    new_instruction(Operation.IntAddConst, 1, 1, 0xffff),  # 0xffff is -1 as an int16.
    new_instruction(Operation.IntBox, 0, 1),
    new_instruction(Operation.Return0),
  ],
  constants: constants,
)
init_frame_stats(nine)

let example* = Example(
  name: "nine",
  function: nine,
  arguments: @[],
  runs: 50_000_000,
)
