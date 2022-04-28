import "../poems"

let int_or_string = poem_sum_type([poem_int_type, poem_string_type])
let lambda_input = poem_tuple_type([int_or_string])

let test_lambda0 = PoemFunction(
  name: "test/lambda0",
  input_type: lambda_input,
  output_type: poem_unit_type,
  is_abstract: false,
  constants: poem_constants(
    poem_const_intrinsic("lore.io.println"),
  ),
  register_count: 1,
  instructions: @[
    poem_inst_intrinsic(0, 0, 0),  # lore.io.println
    poem_inst_return(0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poem_unit_type,
  output_type: poem_unit_type,
  is_abstract: false,
  constants: poem_constants(
    poem_const_value(
      poem_list_value(
        @[poem_int_value(-5), poem_string_value("I'm a string!"), poem_int_value(7)],
        poem_list_type(int_or_string),
      ),
    ),
    poem_const_value(
      poem_single_function_value("test/lambda0", @[], poem_function_type(lambda_input, poem_unit_type)),
    ),
    poem_const_intrinsic("lore.list.each"),
  ),
  register_count: 2,
  instructions: @[
    poem_inst(PoemOperation.Const, 0, 0),
    poem_inst(PoemOperation.Const, 1, 1),
    poem_inst_intrinsic(0, 2, 0, 1),       # lore.lists.each
    poem_inst_return(0),
  ],
)

let poem* = Poem(
  functions: @[test_lambda0, test],
)
