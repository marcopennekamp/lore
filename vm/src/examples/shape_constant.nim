import "../poems"

# In this example, a shape value is loaded as a constant, and the sum of its `foo` int property and the length of the
# `zoo` string property is returned.

let test = PoemFunction(
  name: "test",
  input_type: poem_unit_type,
  output_type: poem_int_type,
  is_abstract: false,
  register_count: 3,
  instructions: @[
    poem_inst(PoemOperation.Const, 1, 0),                # Get the constant shape value.
    poem_inst_shape_property_get(2, 1, 0),               # Get property zoo from the shape value.
    poem_inst_intrinsic(0, 0, 2),                        # Put zoo's length (lore.strings.length) into register 0.
    poem_inst_shape_property_get(2, 1, 1),               # Get property foo from the shape value.
    poem_inst(PoemOperation.IntAdd, 0, 0, 2),            # Finally add foo to register 0.
    poem_inst_return(0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    values: @[
      poem_shape_value(
        @["bar", "foo", "zoo"],
        @[poem_real_type, poem_int_type, poem_string_type],
        @[poem_real_value(2.7), poem_int_value(7), poem_string_value("smith")],
      ),
    ],
    names: @["zoo", "foo"],
    intrinsics: @["lore.string.length"],
  ),
  functions: @[test],
)
