from "../instructions" import Operation, Instruction, new_instruction
from "../poems" import Poem, PoemConstants, PoemMetaShape, PoemType, PoemShapeType, PoemFunction

# In this example, we are adding positions of different dimensions that are represented by shapes.

let meta_position_1D = PoemMetaShape(property_names: @["x"])
let meta_position_2D = PoemMetaShape(property_names: @["x", "y"])
let meta_position_3D = PoemMetaShape(property_names: @["x", "y", "z"])

let position_1D = poems.shape_type(@["x"], @[poems.real_type])
let position_2D = poems.shape_type(@["x", "y"], @[poems.real_type, poems.real_type])
let position_3D = poems.shape_type(@["x", "y", "z"], @[poems.real_type, poems.real_type, poems.real_type])

let add1 = PoemFunction(
  name: "add",
  input_type: poems.tuple_type([position_1D, position_1D]),
  output_type: position_1D,
  is_abstract: false,
  register_count: 4,
  instructions: @[
    # Add x coordinates.
    new_instruction(Operation.ShapeGetProperty, 2, 0, 0),
    new_instruction(Operation.ShapeGetProperty, 3, 1, 0),
    new_instruction(Operation.RealAdd, 0, 2, 3),

    # Create and return the new shape.
    new_instruction(Operation.Shape1, 0, 0, 0),
    new_instruction(Operation.Return0),
  ],
)

let add2 = PoemFunction(
  name: "add",
  input_type: poems.tuple_type([position_2D, position_2D]),
  output_type: position_2D,
  is_abstract: false,
  register_count: 5,
  instructions: @[
    # Add x coordinates.
    new_instruction(Operation.ShapeGetProperty, 2, 0, 0),
    new_instruction(Operation.ShapeGetProperty, 3, 1, 0),
    new_instruction(Operation.RealAdd, 4, 2, 3),

    # Add y coordinates.
    new_instruction(Operation.ShapeGetProperty, 2, 0, 1),
    new_instruction(Operation.ShapeGetProperty, 3, 1, 1),
    new_instruction(Operation.RealAdd, 0, 2, 3),

    # Create and return the new shape.
    new_instruction(Operation.Shape2, 0, 1, 4, 0),
    new_instruction(Operation.Return0),
  ],
)

let add3 = PoemFunction(
  name: "add",
  input_type: poems.tuple_type([position_3D, position_3D]),
  output_type: position_3D,
  is_abstract: false,
  register_count: 7,
  instructions: @[
    # Add x coordinates.
    new_instruction(Operation.ShapeGetProperty, 2, 0, 0),
    new_instruction(Operation.ShapeGetProperty, 3, 1, 0),
    new_instruction(Operation.RealAdd, 4, 2, 3),

    # Add y coordinates.
    new_instruction(Operation.ShapeGetProperty, 2, 0, 1),
    new_instruction(Operation.ShapeGetProperty, 3, 1, 1),
    new_instruction(Operation.RealAdd, 5, 2, 3),

    # Add z coordinates.
    new_instruction(Operation.ShapeGetProperty, 2, 0, 2),
    new_instruction(Operation.ShapeGetProperty, 3, 1, 2),
    new_instruction(Operation.RealAdd, 6, 2, 3),

    # Create and return the new shape. Keep in mind that for the `Shape` operation, we need to have the property values
    # in contiguous registers.
    new_instruction(Operation.Shape, 0, 2, 4, 6),
    new_instruction(Operation.Return0),
  ],
)

proc add_positions(index_1: uint16, index_2: uint16): seq[Instruction] = @[
  new_instruction(Operation.Const, 1, index_1),
  new_instruction(Operation.Const, 2, index_2),
  new_instruction(Operation.Dispatch2, 1, 0, 1, 2),
  new_instruction(Operation.ListAppendUntyped, 0, 0, 1),
]

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: poems.list_type(position_1D),
  is_abstract: false,
  register_count: 3,
  instructions:
    # Prepare the result list.
    @[new_instruction(Operation.Const, 0, 0)] &

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
    @[new_instruction(Operation.Return0)],
)

let poem* = Poem(
  constants: PoemConstants(
    values: @[
      # All shapes added to this list will have the supertype position_1D. Hence we can pretype the list and use
      # ListAppendUntyped.
      poems.list_value(
        @[],
        poems.list_type(position_1D),
      ),

      # p1_1
      poems.shape_value_cast_type(
        position_1D,
        @[poems.real_value(1.5)],
      ),

      # p1_2
      poems.shape_value_cast_type(
        position_1D,
        @[poems.real_value(4.5)],
      ),

      # p2_1
      poems.shape_value_cast_type(
        position_2D,
        @[poems.real_value(0.5), poems.real_value(2.1)],
      ),

      # p2_2
      poems.shape_value_cast_type(
        position_2D,
        @[poems.real_value(-2.5), poems.real_value(-2.8)],
      ),

      # p3_1
      poems.shape_value_cast_type(
        position_3D,
        @[poems.real_value(2.5), poems.real_value(-0.7), poems.real_value(-0.3)],
      ),

      # p3_2
      poems.shape_value_cast_type(
        position_3D,
        @[poems.real_value(3.5), poems.real_value(1.4), poems.real_value(0.6)],
      ),
    ],
    names: @["x", "y", "z"],
    multi_functions: @["add"],
    meta_shapes: @[meta_position_1D, meta_position_2D, meta_position_3D],
  ),
  functions: @[test, add1, add2, add3],
)
