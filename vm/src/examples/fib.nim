import "../poems"

let fib = PoemFunction(
  name: "fib",
  input_type: poem_tuple_type([poem_int_type]),
  output_type: poem_int_type,
  is_abstract: false,
  register_count: 4,
  instructions: @[
    poem_inst_int_const(3, 1),
    poem_inst(PoemOperation.IntLt, 1, 3, 0),
    poem_inst(PoemOperation.JumpIfFalse, 10, 1),

    poem_inst_int_const(3, 1),
    poem_inst(PoemOperation.IntSub, 1, 0, 3),
    poem_inst_dispatch(1, 0, 1),
    poem_inst_int_const(3, 2),
    poem_inst(PoemOperation.IntSub, 2, 0, 3),
    poem_inst_dispatch(2, 0, 2),
    poem_inst(PoemOperation.IntAdd, 0, 1, 2),

    poem_inst_return(0),                            # 10
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poem_unit_type,
  output_type: poem_int_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poem_inst_int_const(0, 10),
    poem_inst_dispatch(0, 0, 0),
    poem_inst_return(0),
  ],
)

let poem* = Poem(
  constants: poem_constants(
    poem_const_multi_function("fib"),
  ),
  functions: @[fib, test],
)
