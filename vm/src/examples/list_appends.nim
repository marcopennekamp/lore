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
    poems.inst(PoemOperation.Const, 0, 0),
    poems.inst(PoemOperation.Const, 1, 1),
    poems.inst(PoemOperation.ListAppend, 0, 0, 1, 0),
    poems.inst(PoemOperation.IntConst, 1, 5),
    poems.inst(PoemOperation.ListAppend, 0, 0, 1, 0),
    poems.inst(PoemOperation.Return0),
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
