from "../poems" import Poem, PoemConstants, PoemFunction, PoemOperation

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 3,
  instructions: @[
    poems.inst(PoemOperation.IntConst, 0, 1),
    poems.inst(PoemOperation.IntConst, 1, 2),
    poems.inst(PoemOperation.IntConst, 2, 3),
    poems.inst_list(0, 0, 0, 1, 2),
    poems.inst(PoemOperation.IntConst, 1, 1),
    poems.inst_intrinsic(0, 0, 0, 1),           # lore.lists.get
    poems.inst(PoemOperation.Return0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    types: @[
      poems.list_type(poems.int_type),
    ],
    intrinsics: @["lore.lists.get"],
  ),
  functions: @[test],
)
