from bytecode import Operation, Instruction, Function, Constants, new_instruction
from common import Example
from evaluator import init_frame_stats

let constants = Constants(functions: @[])

let nine = Function(
  name: "nine",
  stack_size: 3,
  locals_size: 0,
  code: @[
    new_instruction(Operation.IntBoxPush, 1, 0), # This simulates loading an Int argument.
    new_instruction(Operation.IntUnbox, 0, 0),
    new_instruction(Operation.IntPush, 2, 0),
    new_instruction(Operation.IntAdd, 0, 0),
    new_instruction(Operation.IntPush, 3, 0),
    new_instruction(Operation.IntPush, 4, 0),
    new_instruction(Operation.IntAdd, 0, 0),
    new_instruction(Operation.IntAdd, 0, 0),
    new_instruction(Operation.IntPush, 0xffff, 0), # 0xffff is -1 as an int16.
    new_instruction(Operation.IntAdd, 0, 0),
    new_instruction(Operation.IntBox, 0, 0),
    new_instruction(Operation.Return, 0, 0),
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
