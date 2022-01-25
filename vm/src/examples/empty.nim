from "../instructions" import Operation, Instruction, new_instruction
from "../poems" import Poem, PoemConstants, PoemFunction

let empty = PoemFunction(
  name: "empty",
  input_type: poems.unit_type,
  output_type: poems.unit_type,
  is_abstract: false,
  register_count: 0,
  instructions: @[
    new_instruction(Operation.ReturnUnit),
  ],
)

let poem* = Poem(
  constants: PoemConstants(),
  functions: @[empty],
)
