from "../poems" import Poem, PoemConstants, PoemFunction, PoemOperation

let nine = PoemFunction(
  name: "nine",
  input_type: poems.unit_type,
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poems.inst(PoemOperation.IntConst, 0, 1),
    poems.inst(PoemOperation.IntConst, 1, 2),
    poems.inst(PoemOperation.IntAdd, 0, 0, 1),
    poems.inst(PoemOperation.IntConst, 1, 3),
    poems.inst(PoemOperation.IntAdd, 0, 0, 1),
    poems.inst(PoemOperation.IntConst, 1, 4),
    poems.inst(PoemOperation.IntAdd, 0, 0, 1),
    poems.inst(PoemOperation.IntConst, 1, 0xffff), # 0xffff is -1 as an int16.
    poems.inst(PoemOperation.IntAdd, 0, 0, 1),
    poems.inst_return(0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(),
  functions: @[nine],
)
