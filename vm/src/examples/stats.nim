import "../poems"

let attack_type = poem_symbol_type("attack")
let defense_type = poem_symbol_type("defense")
let speed_type = poem_symbol_type("speed")

let stat_0 = PoemFunction(
  name: "stat",
  input_type: poem_tuple_type([
    poem_sum_type([attack_type, defense_type, speed_type])
  ]),
  output_type: poem_int_type,
  is_abstract: true,
)

let stat_1 = PoemFunction(
  name: "stat",
  input_type: poem_tuple_type([attack_type]),
  output_type: poem_int_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poem_inst_int_const(0, 11),
    poem_inst_return(0),
  ],
)

let stat_2 = PoemFunction(
  name: "stat",
  input_type: poem_tuple_type([defense_type]),
  output_type: poem_int_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poem_inst_int_const(0, 7),
    poem_inst_return(0),
  ],
)

let stat_3 = PoemFunction(
  name: "stat",
  input_type: poem_tuple_type([speed_type]),
  output_type: poem_int_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poem_inst_int_const(0, 3),
    poem_inst_return(0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poem_unit_type,
  output_type: poem_int_type,
  is_abstract: false,
  register_count: 3,
  instructions: @[
    poem_inst(PoemOperation.Const, 0, 0),
    poem_inst(PoemOperation.Const, 1, 1),
    poem_inst(PoemOperation.Const, 2, 2),
    poem_inst_dispatch(0, 3, 0),
    poem_inst_dispatch(1, 3, 1),
    poem_inst_dispatch(2, 3, 2),
    poem_inst(PoemOperation.IntAdd, 0, 0, 1),
    poem_inst(PoemOperation.IntAdd, 0, 0, 2),
    poem_inst_return(0),
  ],
)

let poem* = Poem(
  constants: poem_constants(
    poem_const_value(poem_symbol_value("attack")),
    poem_const_value(poem_symbol_value("defense")),
    poem_const_value(poem_symbol_value("speed")),
    poem_const_multi_function("stat"),
  ),
  functions: @[stat_0, stat_1, stat_2, stat_3, test],
)
