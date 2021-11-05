from "../instructions" import Operation, Instruction, new_instruction
from "../poems" import Poem, PoemConstants, PoemFunction

let fib = PoemFunction(
  name: "fib",
  input_type: poems.tuple_type([poems.int_type]),
  output_type: poems.int_type,
  register_count: 3,
  instructions: @[
    new_instruction(Operation.IntGtConst, 1, 0, 1),
    new_instruction(Operation.JumpIfFalse, 7, 1),

    new_instruction(Operation.IntSubConst, 1, 0, 1),
    new_instruction(Operation.Dispatch1, 1, 0, 1),
    new_instruction(Operation.IntSubConst, 2, 0, 2),
    new_instruction(Operation.Dispatch1, 2, 0, 2),
    new_instruction(Operation.IntAdd, 0, 1, 2),

    new_instruction(Operation.Return0),               # 7
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: poems.int_type,
  register_count: 1,
  instructions: @[
    new_instruction(Operation.IntConst, 0, 10),
    new_instruction(Operation.Dispatch1, 0, 0, 0),
    new_instruction(Operation.Return0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(multi_functions: @["fib"]),
  functions: @[fib, test],
)
