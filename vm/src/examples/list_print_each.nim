from "../poems" import Poem, PoemConstants, PoemFunction, PoemOperation

let int_or_string = poems.sum_type([poems.int_type, poems.string_type])
let lambda_input = poems.tuple_type([int_or_string])

let test_lambda0 = PoemFunction(
  name: "test/lambda0",
  input_type: lambda_input,
  output_type: poems.unit_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poems.inst_intrinsic_void(1, 0),        # lore.io.println
    poems.inst_tuple(0),
    poems.inst_return(0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: poems.unit_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poems.inst(PoemOperation.Const, 0, 0),
    poems.inst(PoemOperation.Const, 1, 1),
    poems.inst_intrinsic_void(0, 0, 1),     # lore.lists.each
    poems.inst_tuple(0),
    poems.inst_return(0),
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
