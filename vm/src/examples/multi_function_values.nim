import "../poems"

let handle_input = poem_tuple_type([poem_sum_type([poem_int_type, poem_real_type])])
let handle_output = poem_sum_type([poem_int_type, poem_real_type])

let handle0 = PoemFunction(
  name: "handle",
  input_type: handle_input,
  output_type: handle_output,
  is_abstract: true,
)

let handle1 = PoemFunction(
  name: "handle",
  input_type: poem_tuple_type([poem_int_type]),
  output_type: poem_int_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poem_inst(PoemOperation.IntConst, 1, 5),
    poem_inst(PoemOperation.IntAdd, 0, 0, 1),
    poem_inst_return(0),
  ],
)

let handle2 = PoemFunction(
  name: "handle",
  input_type: poem_tuple_type([poem_real_type]),
  output_type: poem_real_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poem_inst(PoemOperation.Const, 1, 1),
    poem_inst(PoemOperation.RealAdd, 0, 0, 1),
    poem_inst_return(0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poem_unit_type,
  output_type: poem_tuple_type([handle_output, handle_output]),
  is_abstract: false,
  register_count: 3,
  instructions: @[
    poem_inst(PoemOperation.Const, 0, 2),
    poem_inst(PoemOperation.IntConst, 1, 14),
    poem_inst_function_call(1, 0, 1),
    poem_inst(PoemOperation.Const, 2, 0),
    poem_inst_function_call(2, 0, 2),
    poem_inst_tuple(0, 1, 2),
    poem_inst_return(0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    values: @[
      poem_real_value(3.3),
      poem_real_value(2.5),
      poem_multi_function_value("handle", poem_function_type(handle_input, handle_output)),
    ],
  ),
  functions: @[handle0, handle1, handle2, test],
)
