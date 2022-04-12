import "../poems"

# TODO (vm): For now, this implements a single-element version of "hello name", so that we don't have to implement the
#            `lore.Enum.map` function just yet.

let hello_0 = PoemFunction(
  name: "hello",
  input_type: poem_tuple_type([poem_string_type]),
  output_type: poem_string_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poem_inst(PoemOperation.Const, 1, 0),
    poem_inst(PoemOperation.StringConcat, 0, 1, 0),
    poem_inst(PoemOperation.Const, 1, 1),
    poem_inst(PoemOperation.StringConcat, 0, 0, 1),
    poem_inst_return(0),
  ],
)

let hello_1 = PoemFunction(
  name: "hello",
  input_type: poem_tuple_type([poem_int_type]),
  output_type: poem_string_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poem_inst(PoemOperation.StringOf, 0, 0),
    poem_inst(PoemOperation.Const, 1, 2),
    poem_inst(PoemOperation.StringConcat, 0, 1, 0),
    poem_inst(PoemOperation.Const, 1, 1),
    poem_inst(PoemOperation.StringConcat, 0, 0, 1),
    poem_inst_return(0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poem_unit_type,
  output_type: poem_string_type,
  is_abstract: false,
  register_count: 3,
  instructions: @[
    # Call `hello` with 'world' and with 42, then finally concat the two strings with a separating space.
    poem_inst(PoemOperation.Const, 0, 3),
    poem_inst_dispatch(0, 0, 0),

    poem_inst_int_const(1, 42),
    poem_inst_dispatch(1, 0, 1),

    poem_inst(PoemOperation.Const, 2, 4),
    poem_inst(PoemOperation.StringConcat, 0, 0, 2),
    poem_inst(PoemOperation.StringConcat, 0, 0, 1),
    poem_inst_return(0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    multi_functions: @["hello"],
    values: @[
      poem_string_value("Hello, "),
      poem_string_value("."),
      poem_string_value("Hello, anonymous #"),
      poem_string_value("world"),
      poem_string_value(" "),
    ],
  ),
  functions: @[hello_0, hello_1, test],
)
