import "../poems"

# In this example, we are adding positions of different dimensions that are represented by shapes.

let meta_position_1D = PoemMetaShape(property_names: @["x"])
let meta_position_2D = PoemMetaShape(property_names: @["x", "y"])
let meta_position_3D = PoemMetaShape(property_names: @["x", "y", "z"])

let position_1D = poem_shape_type(@["x"], @[poem_real_type])
let position_2D = poem_shape_type(@["x", "y"], @[poem_real_type, poem_real_type])
let position_3D = poem_shape_type(@["x", "y", "z"], @[poem_real_type, poem_real_type, poem_real_type])

let add1 = PoemFunction(
  name: "add",
  input_type: poem_tuple_type([position_1D, position_1D]),
  output_type: position_1D,
  is_abstract: false,
  register_count: 4,
  instructions: @[
    # Add x coordinates.
    poem_inst_shape_property_get(2, 0, 7),
    poem_inst_shape_property_get(3, 1, 7),
    poem_inst(PoemOperation.RealAdd, 0, 2, 3),

    # Create and return the new shape.
    poem_inst_shape(0, 11, 0),
    poem_inst_return(0),
  ],
)

let add2 = PoemFunction(
  name: "add",
  input_type: poem_tuple_type([position_2D, position_2D]),
  output_type: position_2D,
  is_abstract: false,
  register_count: 5,
  instructions: @[
    # Add x coordinates.
    poem_inst_shape_property_get(2, 0, 7),
    poem_inst_shape_property_get(3, 1, 7),
    poem_inst(PoemOperation.RealAdd, 4, 2, 3),

    # Add y coordinates.
    poem_inst_shape_property_get(2, 0, 8),
    poem_inst_shape_property_get(3, 1, 8),
    poem_inst(PoemOperation.RealAdd, 0, 2, 3),

    # Create and return the new shape.
    poem_inst_shape(0, 12, 4, 0),
    poem_inst_return(0),
  ],
)

let add3 = PoemFunction(
  name: "add",
  input_type: poem_tuple_type([position_3D, position_3D]),
  output_type: position_3D,
  is_abstract: false,
  register_count: 7,
  instructions: @[
    # Add x coordinates.
    poem_inst_shape_property_get(2, 0, 7),
    poem_inst_shape_property_get(3, 1, 7),
    poem_inst(PoemOperation.RealAdd, 4, 2, 3),

    # Add y coordinates.
    poem_inst_shape_property_get(2, 0, 8),
    poem_inst_shape_property_get(3, 1, 8),
    poem_inst(PoemOperation.RealAdd, 5, 2, 3),

    # Add z coordinates.
    poem_inst_shape_property_get(2, 0, 9),
    poem_inst_shape_property_get(3, 1, 9),
    poem_inst(PoemOperation.RealAdd, 6, 2, 3),

    # Create and return the new shape.
    poem_inst_shape(0, 13, 4, 5, 6),
    poem_inst_return(0),
  ],
)

proc add_positions(index_1: uint16, index_2: uint16): seq[PoemInstruction] = @[
  poem_inst(PoemOperation.Const, 1, index_1),
  poem_inst(PoemOperation.Const, 2, index_2),
  poem_inst_dispatch(1, 10, 1, 2),
  poem_inst(PoemOperation.ListAppendUntyped, 0, 0, 1),
]

let test = PoemFunction(
  name: "test",
  input_type: poem_unit_type,
  output_type: poem_list_type(position_1D),
  is_abstract: false,
  register_count: 3,
  instructions:
    # Prepare the result list.
    @[poem_inst(PoemOperation.Const, 0, 0)] &

    # Add all position combinations to the list.
    add_positions(1, 2) &  # Add p1_1 and p1_2.
    add_positions(1, 4) &  # Add p1_1 and p2_2.
    add_positions(1, 6) &  # Add p1_1 and p3_2.
    add_positions(3, 2) &  # Add p2_1 and p1_2.
    add_positions(3, 4) &  # Add p2_1 and p2_2.
    add_positions(3, 6) &  # Add p2_1 and p3_2.
    add_positions(5, 2) &  # Add p3_1 and p1_2.
    add_positions(5, 4) &  # Add p3_1 and p2_2.
    add_positions(5, 6) &  # Add p3_1 and p3_2.

    # Return the resulting list.
    @[poem_inst_return(0)],
)

let poem* = Poem(
  constants: poem_constants(
    # All shapes added to this list will have the supertype position_1D. Hence we can pretype the list and use
    # `ListAppendUntyped`.
    poem_const_value(
      poem_list_value(
        @[],
        poem_list_type(position_1D),
      ),
    ),

    # p1_1
    poem_const_value(
      poem_shape_value_cast_type(
        position_1D,
        @[poem_real_value(1.5)],
      ),
    ),

    # p1_2
    poem_const_value(
      poem_shape_value_cast_type(
        position_1D,
        @[poem_real_value(4.5)],
      ),
    ),

    # p2_1
    poem_const_value(
      poem_shape_value_cast_type(
        position_2D,
        @[poem_real_value(0.5), poem_real_value(2.1)],
      ),
    ),

    # p2_2
    poem_const_value(
      poem_shape_value_cast_type(
        position_2D,
        @[poem_real_value(-2.5), poem_real_value(-2.8)],
      ),
    ),

    # p3_1
    poem_const_value(
      poem_shape_value_cast_type(
        position_3D,
        @[poem_real_value(2.5), poem_real_value(-0.7), poem_real_value(-0.3)],
      ),
    ),

    # p3_2
    poem_const_value(
      poem_shape_value_cast_type(
        position_3D,
        @[poem_real_value(3.5), poem_real_value(1.4), poem_real_value(0.6)],
      ),
    ),

    # ID: 7..9
    poem_const_name("x"),
    poem_const_name("y"),
    poem_const_name("z"),

    # ID: 10
    poem_const_multi_function("add"),

    # ID: 11..13
    poem_const_meta_shape(meta_position_1D),
    poem_const_meta_shape(meta_position_2D),
    poem_const_meta_shape(meta_position_3D),
  ),
  functions: @[test, add1, add2, add3],
)
