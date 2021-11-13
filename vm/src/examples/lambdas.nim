from "../instructions" import Operation, Instruction, new_instruction
from "../poems" import Poem, PoemConstants, PoemFunction

let increment = PoemFunction(
  name: "increment",
  input_type: poems.tuple_type([poems.int_type]),
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    new_instruction(Operation.IntAddConst, 0, 0, 1),
    new_instruction(Operation.Return0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    new_instruction(Operation.IntConst, 0, 41),
    new_instruction(Operation.Const, 1, 0),
    new_instruction(Operation.FunctionCall1, 0, 1, 0),
    new_instruction(Operation.Return0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    values: @[
      poems.lambda_function_value(
        "increment",
        poems.function_type(
          poems.tuple_type([poems.int_type]),
          poems.int_type,
        ),
      ),
    ],
    multi_functions: @["increment"],
  ),
  functions: @[increment, test],
)
