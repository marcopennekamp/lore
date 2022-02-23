from "../poems" import Poem, PoemConstants, PoemFunction, PoemOperation

let empty = PoemFunction(
  name: "empty",
  input_type: poems.unit_type,
  output_type: poems.unit_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poems.inst_tuple(0),
    poems.inst_return(0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(),
  functions: @[empty],
)
