from "../instructions" import Operation, Instruction, new_instruction
from "../poems" import Poem, PoemConstants, PoemFunction

# List concatenation will be implemented by an intrinsic, but this test's objective is to verify type variable handling
# in type creation.
let concat = PoemFunction(
  name: "concat",
  type_parameters: @[
    poems.type_parameter("A"),
    poems.type_parameter("B"),
  ],
  input_type: poems.tuple_type([poems.list_type(poems.type_variable(0)), poems.list_type(poems.type_variable(1))]),
  output_type: poems.list_type(poems.sum_type([poems.type_variable(0), poems.type_variable(1)])),
  is_abstract: false,
  register_count: 6,
  instructions: @[
    # input: list1 in register 0, list2 at register 1
    new_instruction(Operation.IntConst, 2, 0),              # i = 0
    new_instruction(Operation.Intrinsic1, 3, 0, 1),         # list2_len = lore.lists.length(list2)

    new_instruction(Operation.IntLt, 4, 2, 3),              # continue? = i < list2_len
    new_instruction(Operation.JumpIfFalse, 8, 4),           # if !continue?: jump to end
    new_instruction(Operation.Intrinsic2, 5, 1, 1, 2),      # element = lore.lists.get(list2, i)
    new_instruction(Operation.ListAppendPoly, 0, 0, 5, 0),  # list1 = list1 :+ element
    new_instruction(Operation.IntAddConst, 2, 2, 1),        # i += 1
    new_instruction(Operation.Jump, 2),                     # jump to loop start

    new_instruction(Operation.Return0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: poems.list_type(poems.sum_type([poems.int_type, poems.string_type])),
  is_abstract: false,
  register_count: 2,
  instructions: @[
    # input: list1 in register 0, list2 in register 1
    new_instruction(Operation.Const, 0, 0),
    new_instruction(Operation.Const, 1, 1),
    new_instruction(Operation.Dispatch2, 0, 0, 0, 1),   # list1 = concat(list1, list2)
    new_instruction(Operation.Return0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    types: @[
      poems.list_type(
        poems.sum_type(@[poems.type_variable(0), poems.type_variable(1)]),
      ),
    ],
    values: @[
      poems.list_value(
        @[poems.int_value(1)],
        poems.list_type(poems.int_type),
      ),
      poems.list_value(
        @[poems.string_value("hello"), poems.string_value("world")],
        poems.list_type(poems.string_type),
      ),
    ],
    intrinsics: @["lore.lists.length", "lore.lists.get"],
    multi_functions: @["concat"],
  ),
  functions: @[concat, test],
)
