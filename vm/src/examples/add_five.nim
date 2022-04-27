import "../poems"

let add_five = PoemFunction(
  name: "add_five",
  input_type: poem_tuple_type([poem_int_type]),
  output_type: poem_int_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poem_inst_int_const(1, 5),
    poem_inst(PoemOperation.IntAdd, 0, 0, 1),
    poem_inst_return(0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poem_unit_type,
  output_type: poem_int_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poem_inst_int_const(0, 7),
    poem_inst_dispatch(0, 0, 0),
    poem_inst_return(0),
  ],
)

let poem* = Poem(
  constants: poem_constants(poem_const_multi_function("add_five")),
  functions: @[add_five, test],
)
