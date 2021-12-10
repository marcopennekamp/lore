from "../instructions" import Operation, Instruction, new_instruction
from "../poems" import Poem, PoemConstants, PoemFunction

let result_type = poems.list_type(
  poems.sum_type(@[poems.int_type, poems.string_type]),
)

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: result_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    new_instruction(Operation.Const, 0, 0),
    new_instruction(Operation.Const, 1, 1),
    new_instruction(Operation.ListAppend, 0, 0, 1, 0),
    new_instruction(Operation.IntConst, 1, 5),
    new_instruction(Operation.ListAppend, 0, 0, 1, 0),
    new_instruction(Operation.Return0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    types: @[
      result_type,
    ],
    values: @[
      poems.list_value(
        @[poems.int_value(1)],
        poems.list_type(poems.int_type),
      ),
      poems.string_value("foo"),
    ],
  ),
  functions: @[test],
)
