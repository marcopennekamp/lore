from "../instructions" import Operation, Instruction, new_instruction
from "../poems" import Poem, PoemConstants, PoemFunction

let int_or_string = poems.sum_type([poems.int_type, poems.string_type])
let lambda_input = poems.tuple_type([int_or_string])

let test_lambda0 = PoemFunction(
  name: "test/lambda0",
  input_type: lambda_input,
  output_type: poems.unit_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    new_instruction(Operation.Intrinsic1, 0, 1, 0),
    new_instruction(Operation.Return0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: poems.unit_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    new_instruction(Operation.Const, 0, 0),
    new_instruction(Operation.Const, 1, 1),
    new_instruction(Operation.IntrinsicFa2, 0, 0, 0, 1),
    new_instruction(Operation.Return0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    values: @[
      poems.list_value(
        @[poems.int_value(-5), poems.string_value("I'm a string!"), poems.int_value(7)],
        poems.list_type(int_or_string),
      ),
      poems.lambda_function_value("test/lambda0", poems.function_type(lambda_input, poems.unit_type)),
    ],
    intrinsics: @["lore.lists.each", "lore.io.println"],
  ),
  functions: @[test_lambda0, test],
)
