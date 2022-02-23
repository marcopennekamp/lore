import "../poems"

let test = PoemFunction(
  name: "test",
  input_type: poem_unit_type,
  output_type: poem_int_type,
  is_abstract: false,
  register_count: 3,
  instructions: @[
    poem_inst(PoemOperation.IntConst, 0, 1),
    poem_inst(PoemOperation.IntConst, 1, 2),
    poem_inst(PoemOperation.IntConst, 2, 3),
    poem_inst_list(0, 0, 0, 1, 2),
    poem_inst(PoemOperation.IntConst, 1, 1),
    poem_inst(PoemOperation.ListGet, 0, 0, 1),
    poem_inst_return(0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    types: @[
      poem_list_type(poem_int_type),
    ],
  ),
  functions: @[test],
)
