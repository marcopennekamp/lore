from "../instructions" import Operation, Instruction, new_instruction
from "../poems" import Poem, PoemConstants, PoemFunction

let handle_input = poems.tuple_type([poems.sum_type([poems.int_type, poems.real_type])])
let handle_output = poems.sum_type([poems.int_type, poems.real_type])

let handle0 = PoemFunction(
  name: "handle",
  input_type: handle_input,
  output_type: handle_output,
  is_abstract: true,
)

let handle1 = PoemFunction(
  name: "handle",
  input_type: poems.tuple_type([poems.int_type]),
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    new_instruction(Operation.IntAddConst, 0, 0, 5),
    new_instruction(Operation.Return0),
  ],
)

let handle2 = PoemFunction(
  name: "handle",
  input_type: poems.tuple_type([poems.real_type]),
  output_type: poems.real_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    new_instruction(Operation.Const, 1, 1),
    new_instruction(Operation.RealAdd, 0, 0, 1),
    new_instruction(Operation.Return0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: poems.tuple_type([handle_output, handle_output]),
  is_abstract: false,
  register_count: 3,
  instructions: @[
    new_instruction(Operation.Const, 0, 2),
    new_instruction(Operation.IntConst, 1, 14),
    new_instruction(Operation.FunctionCall1, 1, 0, 1),
    new_instruction(Operation.Const, 2, 0),
    new_instruction(Operation.FunctionCall1, 2, 0, 2),
    new_instruction(Operation.Tuple2, 0, 1, 2),
    new_instruction(Operation.Return0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    values: @[
      poems.real_value(3.3),
      poems.real_value(2.5),
      poems.multi_function_value("handle", poems.function_type(handle_input, handle_output)),
    ],
  ),
  functions: @[handle0, handle1, handle2, test],
)
