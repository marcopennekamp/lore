import "../poems"

let int_tuple2 = poem_tuple_type([poem_int_type, poem_int_type])

let add_tuples = PoemFunction(
  name: "add_tuples",
  input_type: poem_tuple_type([
    int_tuple2,
    int_tuple2,
  ]),
  output_type: int_tuple2,
  is_abstract: false,
  register_count: 4,
  instructions: @[
    poem_inst(PoemOperation.TupleGet, 2, 0, 1),
    poem_inst(PoemOperation.TupleGet, 3, 1, 1),
    poem_inst(PoemOperation.TupleGet, 0, 0, 0),
    poem_inst(PoemOperation.TupleGet, 1, 1, 0),
    poem_inst(PoemOperation.IntAdd, 0, 0, 1),
    poem_inst(PoemOperation.IntAdd, 1, 2, 3),
    poem_inst_tuple(0, 0, 1),
    poem_inst_return(0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poem_unit_type,
  output_type: int_tuple2,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poem_inst(PoemOperation.Const, 0, 0),
    poem_inst(PoemOperation.Const, 1, 1),
    poem_inst_dispatch(0, 2, 0, 1),
    poem_inst_return(0),
  ],
)

let poem* = Poem(
  constants: poem_constants(
    poem_const_value(
      poem_tuple_value(
        @[poem_int_value(1), poem_int_value(2)],
        int_tuple2,
      ),
    ),
    poem_const_value(
      poem_tuple_value(
        @[poem_int_value(5), poem_int_value(7)],
        int_tuple2,
      ),
    ),
    poem_const_multi_function("add_tuples"),
  ),
  functions: @[add_tuples, test],
)
