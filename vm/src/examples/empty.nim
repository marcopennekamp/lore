from "../instructions" import Operation, Instruction, new_instruction
from "../poems" import Poem, PoemConstants, PoemFunction

let empty = PoemFunction(
  name: "empty",
  input_type: poems.unit_type,
  output_type: poems.unit_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    new_instruction(Operation.Return, 0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(multi_functions: @[]),
  functions: @[empty],
)
