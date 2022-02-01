from "../poems" import Poem, PoemConstants, PoemFunction, PoemOperation

# TODO (vm): For now, this implements a single-element version of "hello name", so that we don't have to implement the
#            `lore.Enum.map` function just yet.

let hello_0 = PoemFunction(
  name: "hello",
  input_type: poems.tuple_type([poems.string_type]),
  output_type: poems.string_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poems.inst(PoemOperation.StringConcatConstl, 0, 0, 0),
    poems.inst(PoemOperation.StringConcatConst, 0, 0, 1),
    poems.inst(PoemOperation.Return0),
  ],
)

let hello_1 = PoemFunction(
  name: "hello",
  input_type: poems.tuple_type([poems.int_type]),
  output_type: poems.string_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poems.inst(PoemOperation.StringOf, 0, 0),
    poems.inst(PoemOperation.StringConcatConstl, 0, 2, 0),
    poems.inst(PoemOperation.StringConcatConst, 0, 0, 1),
    poems.inst(PoemOperation.Return0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: poems.string_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    # Call `hello` with 'world' and with 42, then finally concat the two strings with a separating space.
    poems.inst(PoemOperation.Const, 0, 3),
    poems.inst_dispatch(0, 0, 0),

    poems.inst(PoemOperation.IntConst, 1, 42),
    poems.inst_dispatch(1, 0, 1),

    poems.inst(PoemOperation.StringConcatConst, 0, 0, 4),
    poems.inst(PoemOperation.StringConcat, 0, 0, 1),
    poems.inst(PoemOperation.Return0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    multi_functions: @["hello"],
    values: @[
      poems.string_value("Hello, "),
      poems.string_value("."),
      poems.string_value("Hello, anonymous #"),
      poems.string_value("world"),
      poems.string_value(" "),
    ],
  ),
  functions: @[hello_0, hello_1, test],
)
