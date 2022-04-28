import "../poems"

let hello_0 = PoemFunction(
  name: "hello",
  input_type: poem_tuple_type([poem_string_type]),
  output_type: poem_string_type,
  is_abstract: false,
  constants: poem_constants(
    poem_const_value(poem_string_value("Hello, ")),
    poem_const_value(poem_string_value(".")),
  ),
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
  constants: poem_constants(
    poem_const_value(poem_string_value("Hello, anonymous #")),
    poem_const_value(poem_string_value(".")),
  ),
  register_count: 2,
  instructions: @[
    poem_inst(PoemOperation.StringOf, 0, 0),
    poem_inst(PoemOperation.Const, 1, 0),
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
  constants: poem_constants(
    poem_const_value(poem_string_value("world")),
    poem_const_value(poem_string_value(" ")),
    poem_const_multi_function("hello"),
  ),
  register_count: 3,
  instructions: @[
    # Call `hello` with 'world' and with 42, then finally concat the two strings with a separating space.
    poem_inst(PoemOperation.Const, 0, 0),
    poem_inst_dispatch(0, 2, 0),

    poem_inst_int_const(1, 42),
    poem_inst_dispatch(1, 2, 1),

    poem_inst(PoemOperation.Const, 2, 1),
    poem_inst(PoemOperation.StringConcat, 0, 0, 2),
    poem_inst(PoemOperation.StringConcat, 0, 0, 1),
    poem_inst_return(0),
  ],
)

let poem* = Poem(
  functions: @[hello_0, hello_1, test],
)
