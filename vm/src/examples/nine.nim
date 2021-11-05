from "../instructions" import Operation, Instruction, new_instruction
from "../poems" import Poem, PoemConstants, PoemFunction

let nine = PoemFunction(
  name: "nine",
  input_type: poems.unit_type,
  output_type: poems.int_type,
  register_count: 1,
  instructions: @[
    new_instruction(Operation.IntConst, 0, 1),
    new_instruction(Operation.IntAddConst, 0, 0, 2),
    new_instruction(Operation.IntAddConst, 0, 0, 3),
    new_instruction(Operation.IntAddConst, 0, 0, 4),
    new_instruction(Operation.IntAddConst, 0, 0, 0xffff),  # 0xffff is -1 as an int16.
    new_instruction(Operation.Return0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(multi_functions: @[]),
  functions: @[nine],
)
