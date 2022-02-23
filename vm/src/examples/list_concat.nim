import "../poems"

# List concatenation will be implemented by an intrinsic, but this test's objective is to verify type variable handling
# in type creation.
let concat = PoemFunction(
  name: "concat",
  type_parameters: @[
    poem_type_parameter("A"),
    poem_type_parameter("B"),
  ],
  input_type: poem_tuple_type([poem_list_type(poem_type_variable(0)), poem_list_type(poem_type_variable(1))]),
  output_type: poem_list_type(poem_sum_type([poem_type_variable(0), poem_type_variable(1)])),
  is_abstract: false,
  register_count: 6,
  instructions: @[
    # input: list1 in reg0, list2 in reg1
    poem_inst(PoemOperation.IntConst, 2, 0),              # i = 0
    poem_inst(PoemOperation.ListLength, 3, 1),            # list2_len = list2.length

    poem_inst(PoemOperation.IntLt, 4, 2, 3),              # continue? = i < list2_len
    poem_inst(PoemOperation.JumpIfFalse, 9, 4),           # if !continue?: jump to end
    poem_inst(PoemOperation.ListGet, 5, 1, 2),            # element = list2.get(i)
    poem_inst_list_append(0, 0, 5, 0),                    # list1 = list1 :+ element
    poem_inst(PoemOperation.IntConst, 5, 1),
    poem_inst(PoemOperation.IntAdd, 2, 2, 5),             # i += 1
    poem_inst(PoemOperation.Jump, 2),                     # jump to loop start

    poem_inst_return(0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poem_unit_type,
  output_type: poem_list_type(poem_sum_type([poem_int_type, poem_string_type])),
  is_abstract: false,
  register_count: 2,
  instructions: @[
    # input: list1 in register 0, list2 in register 1
    poem_inst(PoemOperation.Const, 0, 0),
    poem_inst(PoemOperation.Const, 1, 1),
    poem_inst_dispatch(0, 0, 0, 1),        # list1 = concat(list1, list2)
    poem_inst_return(0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    types: @[
      poem_list_type(
        poem_sum_type(@[poem_type_variable(0), poem_type_variable(1)]),
      ),
    ],
    values: @[
      poem_list_value(
        @[poem_int_value(1)],
        poem_list_type(poem_int_type),
      ),
      poem_list_value(
        @[poem_string_value("hello"), poem_string_value("world")],
        poem_list_type(poem_string_type),
      ),
    ],
    multi_functions: @["concat"],
  ),
  functions: @[concat, test],
)
