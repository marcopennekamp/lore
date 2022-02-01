from "../poems" import Poem, PoemConstants, PoemFunction, PoemOperation

let empty = PoemFunction(
  name: "empty",
  input_type: poems.unit_type,
  output_type: poems.unit_type,
  is_abstract: false,
  register_count: 0,
  instructions: @[
    poems.inst(PoemOperation.ReturnUnit),
  ],
)

let poem* = Poem(
  constants: PoemConstants(),
  functions: @[empty],
)
