from "../poems" import Poem, PoemConstants, PoemFunction, PoemOperation

let add_five = PoemFunction(
  name: "add_five",
  input_type: poems.tuple_type([poems.int_type]),
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poems.inst(PoemOperation.IntConst, 1, 5),
    poems.inst(PoemOperation.IntAdd, 0, 0, 1),
    poems.inst_return(0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poems.inst(PoemOperation.IntConst, 0, 7),
    poems.inst_dispatch(0, 0, 0),
    poems.inst_return(0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(multi_functions: @["add_five"]),
  functions: @[add_five, test],
)
