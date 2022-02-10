from "../poems" import Poem, PoemConstants, PoemFunction, PoemOperation

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
    poems.inst(PoemOperation.IntConst, 0, 1),
    poems.inst_list(0, 0, 0),
    poems.inst(PoemOperation.Const, 1, 0),
    poems.inst_list_append(0, 0, 1, 1),
    poems.inst(PoemOperation.IntConst, 1, 5),
    poems.inst_list_append(0, 0, 1, 1),
    poems.inst(PoemOperation.Return0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    types: @[
      poems.list_type(poems.int_type),
      result_type,
    ],
    values: @[
      poems.string_value("foo"),
    ],
  ),
  functions: @[test],
)
