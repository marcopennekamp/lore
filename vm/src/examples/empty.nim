import "../poems"

let empty = PoemFunction(
  name: "empty",
  input_type: poem_unit_type,
  output_type: poem_unit_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poem_inst_tuple(0),
    poem_inst_return(0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(),
  functions: @[empty],
)
