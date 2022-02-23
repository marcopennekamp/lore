from "../poems" import Poem, PoemConstants, PoemFunction, PoemOperation

let int_tuple2 = poems.tuple_type([poems.int_type, poems.int_type])

let add_tuples = PoemFunction(
  name: "add_tuples",
  input_type: poems.tuple_type([
    int_tuple2,
    int_tuple2,
  ]),
  output_type: int_tuple2,
  is_abstract: false,
  register_count: 4,
  instructions: @[
    poems.inst(PoemOperation.TupleGet, 2, 0, 1),
    poems.inst(PoemOperation.TupleGet, 3, 1, 1),
    poems.inst(PoemOperation.TupleGet, 0, 0, 0),
    poems.inst(PoemOperation.TupleGet, 1, 1, 0),
    poems.inst(PoemOperation.IntAdd, 0, 0, 1),
    poems.inst(PoemOperation.IntAdd, 1, 2, 3),
    poems.inst_tuple(0, 0, 1),
    poems.inst_return(0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: int_tuple2,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poems.inst(PoemOperation.Const, 0, 0),
    poems.inst(PoemOperation.Const, 1, 1),
    poems.inst_dispatch(0, 0, 0, 1),
    poems.inst_return(0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    values: @[
      poems.tuple_value(
        @[poems.int_value(1), poems.int_value(2)],
        int_tuple2,
      ),
      poems.tuple_value(
        @[poems.int_value(5), poems.int_value(7)],
        int_tuple2,
      ),
    ],
    multi_functions: @["add_tuples"],
  ),
  functions: @[add_tuples, test],
)
