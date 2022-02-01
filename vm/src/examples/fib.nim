from "../poems" import Poem, PoemConstants, PoemFunction, PoemOperation

let fib = PoemFunction(
  name: "fib",
  input_type: poems.tuple_type([poems.int_type]),
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 3,
  instructions: @[
    poems.inst(PoemOperation.IntGtConst, 1, 0, 1),
    poems.inst(PoemOperation.JumpIfFalse, 7, 1),

    poems.inst(PoemOperation.IntSubConst, 1, 0, 1),
    poems.inst_dispatch(1, 0, 1),
    poems.inst(PoemOperation.IntSubConst, 2, 0, 2),
    poems.inst_dispatch(2, 0, 2),
    poems.inst(PoemOperation.IntAdd, 0, 1, 2),

    poems.inst(PoemOperation.Return0),               # 7
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poems.inst(PoemOperation.IntConst, 0, 10),
    poems.inst_dispatch(0, 0, 0),
    poems.inst(PoemOperation.Return0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(multi_functions: @["fib"]),
  functions: @[fib, test],
)
