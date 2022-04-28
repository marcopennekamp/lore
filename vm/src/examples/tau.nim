import "../poems"

let pi = PoemEagerGlobalVariable(
  name: "pi",
  value: poem_real_value(3.14),
)

let tau = PoemLazyGlobalVariable(
  name: "tau",
  initializer_name: "tau/initialize",
)

let tau_initialize = PoemFunction(
  name: "tau/initialize",
  input_type: poem_unit_type,
  output_type: poem_real_type,
  is_abstract: false,
  constants: poem_constants(
    poem_const_global_variable("pi"),
  ),
  register_count: 1,
  instructions: @[
    poem_inst_global_get(0, 0),
    poem_inst(PoemOperation.RealAdd, 0, 0, 0),
    poem_inst_return(0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poem_unit_type,
  output_type: poem_tuple_type([poem_real_type, poem_real_type]),
  is_abstract: false,
  constants: poem_constants(
    poem_const_global_variable("pi"),
    poem_const_global_variable("tau"),
  ),
  register_count: 2,
  instructions: @[
    poem_inst_global_get(0, 0),
    poem_inst_global_get(1, 1),
    poem_inst_tuple(0, 0, 1),
    poem_inst_return(0),
  ],
)

let poem* = Poem(
  global_variables: @[pi, tau],
  functions: @[tau_initialize, test],
)
