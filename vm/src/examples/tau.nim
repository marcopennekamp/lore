from "../poems" import Poem, PoemConstants, PoemEagerGlobalVariable, PoemLazyGlobalVariable, PoemFunction,
                       PoemOperation

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
    poems.inst_global_get(0, 0),
    poems.inst(PoemOperation.RealAdd, 0, 0, 0),
    poems.inst_return(0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: poems.tuple_type([poems.real_type, poems.real_type]),
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poems.inst_global_get(0, 0),
    poems.inst_global_get(1, 1),
    poems.inst_tuple(0, 0, 1),
    poems.inst_return(0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    global_variables: @["pi", "tau"],
  ),
  global_variables: @[pi, tau],
  functions: @[tau_initialize, test],
)
