from "../poems" import Poem, PoemConstants, PoemFunction, PoemOperation

let increment = PoemFunction(
  name: "increment",
  input_type: poems.tuple_type([poems.int_type]),
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poems.inst(PoemOperation.IntConst, 1, 1),
    poems.inst(PoemOperation.IntAdd, 0, 0, 1),
    poems.inst(PoemOperation.Return0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poems.inst(PoemOperation.IntConst, 0, 41),
    poems.inst(PoemOperation.Const, 1, 0),
    poems.inst_function_call(0, 1, 0),
    poems.inst(PoemOperation.Return0),
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
  ),
  functions: @[increment, test],
)
