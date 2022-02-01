from "../poems" import Poem, PoemConstants, PoemFunction, PoemOperation

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
    poems.inst(PoemOperation.IntConst, 2, 0),              # i = 0
    poems.inst_intrinsic(3, 0, 1),                         # list2_len = lore.lists.length(list2)

    poems.inst(PoemOperation.IntLt, 4, 2, 3),              # continue? = i < list2_len
    poems.inst(PoemOperation.JumpIfFalse, 8, 4),           # if !continue?: jump to end
    poems.inst_intrinsic(5, 1, 1, 2),                      # element = lore.lists.get(list2, i)
    poems.inst(PoemOperation.ListAppendPoly, 0, 0, 5, 0),  # list1 = list1 :+ element
    poems.inst(PoemOperation.IntAddConst, 2, 2, 1),        # i += 1
    poems.inst(PoemOperation.Jump, 2),                     # jump to loop start

    poems.inst(PoemOperation.Return0),
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
    poems.inst(PoemOperation.Const, 0, 0),
    poems.inst(PoemOperation.Const, 1, 1),
    poems.inst_dispatch(0, 0, 0, 1),        # list1 = concat(list1, list2)
    poems.inst(PoemOperation.Return0),
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
