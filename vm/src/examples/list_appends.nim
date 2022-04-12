import "../poems"

let result_type = poem_list_type(
  poem_sum_type(@[poem_int_type, poem_string_type]),
)

let test = PoemFunction(
  name: "test",
  input_type: poem_unit_type,
  output_type: result_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poem_inst_int_const(0, 1),
    poem_inst_list(0, 0, 0),
    poem_inst(PoemOperation.Const, 1, 0),
    poem_inst_list_append(0, 0, 1, 1),
    poem_inst_int_const(1, 5),
    poem_inst_list_append(0, 0, 1, 1),
    poem_inst_return(0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    types: @[
      poem_list_type(poem_int_type),
      result_type,
    ],
    values: @[
      poem_string_value("foo"),
    ],
  ),
  functions: @[test],
)
