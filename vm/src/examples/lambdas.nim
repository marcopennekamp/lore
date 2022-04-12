import "../poems"

let increment = PoemFunction(
  name: "increment",
  input_type: poem_tuple_type([poem_int_type]),
  output_type: poem_int_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poem_inst_int_const(1, 1),
    poem_inst(PoemOperation.IntAdd, 0, 0, 1),
    poem_inst_return(0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poem_unit_type,
  output_type: poem_int_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poem_inst_int_const(0, 41),
    poem_inst(PoemOperation.Const, 1, 0),
    poem_inst_function_call(0, 1, 0),
    poem_inst_return(0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    values: @[
      poem_single_function_value(
        "increment",
        @[],
        poem_function_type(
          poem_tuple_type([poem_int_type]),
          poem_int_type,
        ),
      ),
    ],
  ),
  functions: @[increment, test],
)
