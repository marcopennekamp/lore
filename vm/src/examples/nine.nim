import "../poems"

let nine = PoemFunction(
  name: "nine",
  input_type: poem_unit_type,
  output_type: poem_int_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poem_inst_int_const(0, 1),
    poem_inst_int_const(1, 2),
    poem_inst(PoemOperation.IntAdd, 0, 0, 1),
    poem_inst_int_const(1, 3),
    poem_inst(PoemOperation.IntAdd, 0, 0, 1),
    poem_inst_int_const(1, 4),
    poem_inst(PoemOperation.IntAdd, 0, 0, 1),
    poem_inst_int_const(1, -1),
    poem_inst(PoemOperation.IntAdd, 0, 0, 1),
    poem_inst_return(0),
  ],
)

let poem* = Poem(
  constants: poem_constants(),
  functions: @[nine],
)
