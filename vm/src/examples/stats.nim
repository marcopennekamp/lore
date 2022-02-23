from "../poems" import Poem, PoemConstants, PoemFunction, PoemOperation

let attack_type = poems.symbol_type("attack")
let defense_type = poems.symbol_type("defense")
let speed_type = poems.symbol_type("speed")

let stat_0 = PoemFunction(
  name: "stat",
  input_type: poems.tuple_type([
    poems.sum_type([attack_type, defense_type, speed_type])
  ]),
  output_type: poems.int_type,
  is_abstract: true,
)

let stat_1 = PoemFunction(
  name: "stat",
  input_type: poems.tuple_type([attack_type]),
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poems.inst(PoemOperation.IntConst, 0, 11),
    poems.inst_return(0),
  ],
)

let stat_2 = PoemFunction(
  name: "stat",
  input_type: poems.tuple_type([defense_type]),
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poems.inst(PoemOperation.IntConst, 0, 7),
    poems.inst_return(0),
  ],
)

let stat_3 = PoemFunction(
  name: "stat",
  input_type: poems.tuple_type([speed_type]),
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poems.inst(PoemOperation.IntConst, 0, 3),
    poems.inst_return(0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 3,
  instructions: @[
    poems.inst(PoemOperation.Const, 0, 0),
    poems.inst(PoemOperation.Const, 1, 1),
    poems.inst(PoemOperation.Const, 2, 2),
    poems.inst_dispatch(0, 0, 0),
    poems.inst_dispatch(1, 0, 1),
    poems.inst_dispatch(2, 0, 2),
    poems.inst(PoemOperation.IntAdd, 0, 0, 1),
    poems.inst(PoemOperation.IntAdd, 0, 0, 2),
    poems.inst_return(0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    values: @[
      poems.symbol_value("attack"),
      poems.symbol_value("defense"),
      poems.symbol_value("speed"),
    ],
    multi_functions: @["stat"],
  ),
  functions: @[stat_0, stat_1, stat_2, stat_3, test],
)
