import "../poems"
from "../types" import Kind, Variance

# In this example, we declare option types and instantiate them with various sample data.

proc new_option_type(value_type: PoemType): PoemType =
  poem_named_type("lore.option.Option", @[value_type])

proc new_option2_type(value_type: PoemType): PoemType =
  new_option_type(new_option_type(value_type))

proc new_some_type(value_type: PoemType): PoemType =
  poem_named_type_concrete("lore.option.Some", @[value_type])

proc new_some_value(value_type: PoemType, value: PoemValue): PoemValue =
  poem_struct_value("lore.option.Some", @[value_type], @[value])

let none_type: PoemType = poem_named_type_concrete("lore.option.None")

let Option: PoemSchema = PoemTraitSchema(
  kind: Kind.Trait,
  name: "lore.option.Option",
  type_parameters: @[
    poem_type_parameter("A", Variance.Covariant),
  ],
  supertraits: @[],
  inherited_shape_type: poem_empty_shape_type,
)

let Some: PoemSchema = PoemStructSchema(
  kind: Kind.Struct,
  name: "lore.option.Some",
  type_parameters: @[
    poem_type_parameter("A", Variance.Covariant),
  ],
  supertraits: @[poem_named_type_concrete("lore.option.Option", @[poem_type_variable(0)])],
  properties: @[poem_struct_property("value", poem_type_variable(0), false)],
)

let None: PoemSchema = PoemStructSchema(
  kind: Kind.Struct,
  name: "lore.option.None",
  supertraits: @[poem_named_type_concrete("lore.option.Option", @[poem_nothing_type])],
)

let ExampleResult: PoemSchema = PoemStructSchema(
  kind: Kind.Struct,
  name: "ExampleResult",
  properties: @[
    poem_struct_property("number", poem_int_type, false),
    poem_struct_property("text", poem_string_type, false),
  ],
)

let ExampleResult_construct = PoemFunction(
  name: "ExampleResult$new",
  input_type: poem_tuple_type([poem_int_type, poem_string_type]),
  output_type: poem_named_type("ExampleResult"),
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poem_inst_struct(0, 0, [0'u16, 1]),
    poem_inst_return(0),
  ],
)

let get0 = PoemFunction(
  name: "lore.option.get!",
  type_parameters: @[poem_type_parameter("A")],
  input_type: poem_tuple_type([new_option_type(poem_type_variable(0))]),
  output_type: poem_type_variable(0),
  is_abstract: true,
)

let get1 = PoemFunction(
  name: "lore.option.get!",
  type_parameters: @[poem_type_parameter("A")],
  input_type: poem_tuple_type([new_some_type(poem_type_variable(0))]),
  output_type: poem_type_variable(0),
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poem_inst_struct_property_get(0, 0, 1, 0),
    poem_inst_return(0),
  ],
)

let get2 = PoemFunction(
  name: "lore.option.get!",
  input_type: poem_tuple_type([none_type]),
  output_type: poem_nothing_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poem_inst_intrinsic_void(0),  # lore.core.panic
  ],
)

let flatten0 = PoemFunction(
  name: "lore.option.flatten",
  type_parameters: @[poem_type_parameter("A")],
  input_type: poem_tuple_type([new_option2_type(poem_type_variable(0))]),
  output_type: new_option_type(poem_type_variable(0)),
  is_abstract: true,
)

let flatten1 = PoemFunction(
  name: "lore.option.flatten",
  type_parameters: @[poem_type_parameter("A")],
  input_type: poem_tuple_type([new_some_type(new_option_type(poem_type_variable(0)))]),
  output_type: new_option_type(poem_type_variable(0)),
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poem_inst_struct_property_get(0, 0, 1, 0),
    poem_inst_return(0),
  ],
)

let flatten2 = PoemFunction(
  name: "lore.option.flatten",
  input_type: poem_tuple_type([none_type]),
  output_type: none_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poem_inst_return(0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poem_unit_type,
  output_type: poem_named_type("ExampleResult"),
  is_abstract: false,
  register_count: 3,
  instructions: @[
    # Assure that None is flattened to None. The VM panics if it isn't.
    poem_inst(PoemOperation.Const, 0, 0),
    poem_inst_dispatch(0, 1, 0),                 # r0 = flatten(None)
    poem_inst(PoemOperation.Const, 1, 0),
    poem_inst(PoemOperation.StructEq, 0, 0, 1),  # r0 == None
    poem_inst(PoemOperation.JumpIfTrue, 6, 0),
    poem_inst_intrinsic_void(0),                 # panic()

    # Get `12` from the first Some constant.
    poem_inst(PoemOperation.Const, 0, 1),        # r0 = Some(12)
    poem_inst_dispatch(1, 0, 0),                 # r1 = get!(r0)

    # Get `"42"` from the second nested Some constant.
    poem_inst(PoemOperation.Const, 0, 2),        # r0 = Some(Some("42"))
    poem_inst_dispatch(0, 1, 0),                 # r0 = flatten(r0)
    poem_inst_dispatch(2, 0, 0),                 # r2 = get!(r0)

    # Build an ExampleResult from `12` and `"42"`.
    poem_inst_dispatch(0, 2, 1, 2),              # r0 = ExampleResult$new(r1, r2)
    poem_inst_return(0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    types: @[
      poem_named_type("ExampleResult"),
    ],
    values: @[
      poem_struct_value("lore.option.None", @[], @[]),
      new_some_value(poem_int_type, poem_int_value(12)),
      new_some_value(
        new_some_type(poem_string_type),
        new_some_value(poem_string_type, poem_string_value("42")),
      ),
    ],
    names: @["value"],
    intrinsics: @["lore.core.panic"],
    schemas: @["ExampleResult", "lore.option.Some"],
    multi_functions: @["lore.option.get!", "lore.option.flatten", "ExampleResult$new"],
  ),
  schemas: @[Option, Some, None, ExampleResult],
  functions: @[ExampleResult_construct, get0, get1, get2, flatten0, flatten1, flatten2, test],
)
