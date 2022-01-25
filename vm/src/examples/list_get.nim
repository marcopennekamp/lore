from "../instructions" import Operation, Instruction, new_instruction
from "../poems" import Poem, PoemConstants, PoemFunction

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    new_instruction(Operation.Const, 0, 0),
    new_instruction(Operation.IntConst, 1, 1),
    new_instruction(Operation.Intrinsic2, 0, 0, 0, 1),  # lore.lists.get
    new_instruction(Operation.Return0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    values: @[
      poems.list_value(
        @[poems.int_value(1), poems.int_value(2), poems.int_value(3)],
        poems.list_type(poems.int_type),
      ),
    ],
    intrinsics: @["lore.lists.get"],
  ),
  functions: @[test],
)
