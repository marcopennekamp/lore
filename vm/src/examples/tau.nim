from "../instructions" import Operation, Instruction, new_instruction
import "../poems"

let pi = PoemEagerGlobalVariable(
  name: "pi",
  value: poems.real_value(3.14),
)

let tau = PoemLazyGlobalVariable(
  name: "tau",
  initializer_name: "tau/initialize",
)

let tau_initialize = PoemFunction(
  name: "tau/initialize",
  input_type: poems.unit_type,
  output_type: poems.real_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    new_instruction(Operation.GlobalGetEager, 0, 0),
    new_instruction(Operation.RealAdd, 0, 0, 0),
    new_instruction(Operation.Return0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: poems.tuple_type([poems.real_type, poems.real_type]),
  is_abstract: false,
  register_count: 2,
  instructions: @[
    new_instruction(Operation.GlobalGetEager, 0, 0),
    new_instruction(Operation.GlobalGetLazy, 1, 1),
    new_instruction(Operation.Tuple2, 0, 0, 1),
    new_instruction(Operation.Return0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    global_variables: @["pi", "tau"],
  ),
  global_variables: @[pi, tau],
  functions: @[tau_initialize, test],
)
