import "../poems"

let int_or_string = poem_sum_type([poem_int_type, poem_string_type])
let lambda_input = poem_tuple_type([int_or_string])

let test_lambda0 = PoemFunction(
  name: "test/lambda0",
  input_type: lambda_input,
  output_type: poem_unit_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poem_inst_intrinsic_void(1, 0),        # lore.io.println
    poem_inst_tuple(0),
    poem_inst_return(0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poem_unit_type,
  output_type: poem_unit_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poem_inst(PoemOperation.Const, 0, 0),
    poem_inst(PoemOperation.Const, 1, 1),
    poem_inst_intrinsic_void(0, 0, 1),     # lore.lists.each
    poem_inst_tuple(0),
    poem_inst_return(0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    values: @[
      poem_list_value(
        @[poem_int_value(-5), poem_string_value("I'm a string!"), poem_int_value(7)],
        poem_list_type(int_or_string),
      ),
      poem_single_function_value("test/lambda0", @[], poem_function_type(lambda_input, poem_unit_type)),
    ],
    intrinsics: @["lore.lists.each", "lore.io.println"],
  ),
  functions: @[test_lambda0, test],
)
