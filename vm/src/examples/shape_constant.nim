from "../poems" import Poem, PoemConstants, PoemMetaShape, PoemFunction, PoemOperation

# In this example, a shape value is loaded as a constant, and the sum of its `foo` int property and the length of the
# `zoo` string property is returned.

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 3,
  instructions: @[
    poems.inst(PoemOperation.Const, 1, 0),                # Get the constant shape value.
    poems.inst_shape_property_get(2, 1, 0),               # Get property zoo from the shape value.
    poems.inst_intrinsic(0, 0, 2),                        # Put zoo's length (lore.strings.length) into register 0.
    poems.inst_shape_property_get(2, 1, 1),               # Get property foo from the shape value.
    poems.inst(PoemOperation.IntAdd, 0, 0, 2),            # Finally add foo to register 0.
    poems.inst(PoemOperation.Return0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    values: @[
      poems.shape_value(
        @["bar", "foo", "zoo"],
        @[poems.real_type, poems.int_type, poems.string_type],
        @[poems.real_value(2.7), poems.int_value(7), poems.string_value("smith")],
      ),
    ],
    names: @["zoo", "foo"],
    intrinsics: @["lore.strings.length"],
  ),
  functions: @[test],
)
