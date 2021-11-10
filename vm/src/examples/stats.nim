from "../instructions" import Operation, Instruction, new_instruction
from "../poems" import Poem, PoemConstants, PoemFunction

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
    new_instruction(Operation.IntConst, 0, 11),
    new_instruction(Operation.Return0),
  ],
)

let stat_2 = PoemFunction(
  name: "stat",
  input_type: poems.tuple_type([defense_type]),
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    new_instruction(Operation.IntConst, 0, 7),
    new_instruction(Operation.Return0),
  ],
)

let stat_3 = PoemFunction(
  name: "stat",
  input_type: poems.tuple_type([speed_type]),
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    new_instruction(Operation.IntConst, 0, 3),
    new_instruction(Operation.Return0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 3,
  instructions: @[
    new_instruction(Operation.Const, 0, 0),
    new_instruction(Operation.Const, 1, 1),
    new_instruction(Operation.Const, 2, 2),
    new_instruction(Operation.Dispatch1, 0, 0, 0),
    new_instruction(Operation.Dispatch1, 1, 0, 1),
    new_instruction(Operation.Dispatch1, 2, 0, 2),
    new_instruction(Operation.IntAdd, 0, 0, 1),
    new_instruction(Operation.IntAdd, 0, 0, 2),
    new_instruction(Operation.Return0),
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
