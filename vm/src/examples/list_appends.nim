import "../poems"

let result_type = poem_list_type(
  poem_sum_type(@[poem_int_type, poem_string_type]),
)

let test = PoemFunction(
  name: "test",
  input_type: poem_unit_type,
  output_type: result_type,
  is_abstract: false,
  constants: poem_constants(
    poem_const_type(poem_list_type(poem_int_type)),
    poem_const_type(result_type),
    poem_const_value(poem_string_value("foo")),
  ),
  register_count: 2,
  instructions: @[
    poem_inst_int_const(0, 1),
    poem_inst_list(0, 0, 0),
    poem_inst(PoemOperation.Const, 1, 2),
    poem_inst_list_append(0, 0, 1, 1),
    poem_inst_int_const(1, 5),
    poem_inst_list_append(0, 0, 1, 1),
    poem_inst_return(0),
  ],
)

let poem* = Poem(
  functions: @[test],
)
