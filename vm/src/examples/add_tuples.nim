from "../instructions" import Operation, Instruction, new_instruction
from "../poems" import Poem, PoemConstants, PoemFunction

let int_tuple2 = poems.tuple_type([poems.int_type, poems.int_type])

let add_tuples = PoemFunction(
  name: "add_tuples",
  input_type: poems.tuple_type([
    int_tuple2,
    int_tuple2,
  ]),
  output_type: int_tuple2,
  register_count: 4,
  instructions: @[
    new_instruction(Operation.TupleGet, 2, 0, 1),
    new_instruction(Operation.TupleGet, 3, 1, 1),
    new_instruction(Operation.TupleGet, 0, 0, 0),
    new_instruction(Operation.TupleGet, 1, 1, 0),
    new_instruction(Operation.IntAdd, 0, 0, 1),
    new_instruction(Operation.IntAdd, 1, 2, 3),
    new_instruction(Operation.Tuple2, 0, 0, 1),
    new_instruction(Operation.Return0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: int_tuple2,
  register_count: 2,
  instructions: @[
    new_instruction(Operation.Const, 0, 0),
    new_instruction(Operation.Const, 1, 1),
    new_instruction(Operation.Dispatch2, 0, 0, 0, 1),
    new_instruction(Operation.Return0),
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
