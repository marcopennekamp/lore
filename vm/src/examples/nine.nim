from bytecode import Operation, Instruction, Function, Constants, new_instruction
from values import Value

let constants = Constants(functions: @[])

let example_function* = Function(
  name: "nine",
  arguments_count: 0,
  locals_count: 0,
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

let example_arguments*: seq[Value] = @[]

let example_runs* = 50_000_000
