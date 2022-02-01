from "../poems" import Poem, PoemConstants, PoemFunction, PoemOperation

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poems.inst(PoemOperation.Const, 0, 0),
    poems.inst(PoemOperation.IntConst, 1, 1),
    poems.inst_intrinsic(0, 0, 0, 1),           # lore.lists.get
    poems.inst(PoemOperation.Return0),
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
