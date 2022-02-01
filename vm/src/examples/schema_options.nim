from "../poems" import Poem, PoemConstants, PoemSchema, PoemTraitSchema, PoemStructSchema, PoemStructProperty,
                       PoemMetaShape, PoemFunction, PoemOperation, PoemTypeParameter, PoemType, PoemNamedType,
                       PoemValue
from "../types" import Kind, Variance

# In this example, we declare option types and instantiate them with various sample data.

proc new_option_type(value_type: PoemType): PoemType =
  poems.named_type("lore.option.Option", @[value_type])

proc new_option2_type(value_type: PoemType): PoemType =
  new_option_type(new_option_type(value_type))

proc new_some_type(value_type: PoemType): PoemType =
  poems.named_type_concrete("lore.option.Some", @[value_type])

proc new_some_value(value_type: PoemType, value: PoemValue): PoemValue =
  poems.struct_value("lore.option.Some", @[value_type], @[value])

let none_type: PoemType = poems.named_type_concrete("lore.option.None")

let Option: PoemSchema = PoemTraitSchema(
  kind: Kind.Trait,
  name: "lore.option.Option",
  type_parameters: @[
    poems.type_parameter("A", Variance.Covariant),
  ],
  supertraits: @[],
  inherited_shape_type: poems.empty_shape_type,
)

let Some: PoemSchema = PoemStructSchema(
  kind: Kind.Struct,
  name: "lore.option.Some",
  type_parameters: @[
    poems.type_parameter("A", Variance.Covariant),
  ],
  supertraits: @[poems.named_type_concrete("lore.option.Option", @[poems.type_variable(0)])],
  properties: @[poems.struct_property("value", poems.type_variable(0), false)],
)

let None: PoemSchema = PoemStructSchema(
  kind: Kind.Struct,
  name: "lore.option.None",
  supertraits: @[poems.named_type_concrete("lore.option.Option", @[poems.nothing_type])],
)

let ExampleResult: PoemSchema = PoemStructSchema(
  kind: Kind.Struct,
  name: "ExampleResult",
  properties: @[
    poems.struct_property("number", poems.int_type, false),
    poems.struct_property("text", poems.string_type, false),
  ],
)

let ExampleResult_construct = PoemFunction(
  name: "ExampleResult$new",
  input_type: poems.tuple_type([poems.int_type, poems.string_type]),
  output_type: poems.named_type("ExampleResult"),
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poems.inst_struct(0, 0, [], [0'u16, 1]),
    poems.inst(PoemOperation.Return0),
  ],
)

let get0 = PoemFunction(
  name: "lore.option.get!",
  type_parameters: @[poems.type_parameter("A")],
  input_type: poems.tuple_type([new_option_type(poems.type_variable(0))]),
  output_type: poems.type_variable(0),
  is_abstract: true,
)

let get1 = PoemFunction(
  name: "lore.option.get!",
  type_parameters: @[poems.type_parameter("A")],
  input_type: poems.tuple_type([new_some_type(poems.type_variable(0))]),
  output_type: poems.type_variable(0),
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poems.inst(PoemOperation.StructGetProperty, 0, 0, 0),
    poems.inst(PoemOperation.Return0),
  ],
)

let get2 = PoemFunction(
  name: "lore.option.get!",
  input_type: poems.tuple_type([none_type]),
  output_type: poems.nothing_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poems.inst_intrinsic_void(0),  # lore.core.panic
  ],
)

let flatten0 = PoemFunction(
  name: "lore.option.flatten",
  type_parameters: @[poems.type_parameter("A")],
  input_type: poems.tuple_type([new_option2_type(poems.type_variable(0))]),
  output_type: new_option_type(poems.type_variable(0)),
  is_abstract: true,
)

let flatten1 = PoemFunction(
  name: "lore.option.flatten",
  type_parameters: @[poems.type_parameter("A")],
  input_type: poems.tuple_type([new_some_type(new_option_type(poems.type_variable(0)))]),
  output_type: new_option_type(poems.type_variable(0)),
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poems.inst(PoemOperation.StructGetProperty, 0, 0, 0),
    poems.inst(PoemOperation.Return0),
  ],
)

let flatten2 = PoemFunction(
  name: "lore.option.flatten",
  input_type: poems.tuple_type([none_type]),
  output_type: none_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    poems.inst(PoemOperation.Return0),
  ],
)

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: poems.named_type("ExampleResult"),
  is_abstract: false,
  register_count: 3,
  instructions: @[
    # Assure that None is flattened to None. The VM panics if it isn't.
    poems.inst(PoemOperation.Const, 0, 0),
    poems.inst_dispatch(0, 1, 0),                 # r0 = flatten(None)
    poems.inst(PoemOperation.Const, 1, 0),
    poems.inst(PoemOperation.StructEq, 0, 0, 1),  # r0 == None
    poems.inst(PoemOperation.JumpIfTrue, 6, 0),
    poems.inst_intrinsic_void(0),                 # panic()

    # Get `12` from the first Some constant.
    poems.inst(PoemOperation.Const, 0, 1),        # r0 = Some(12)
    poems.inst_dispatch(1, 0, 0),                 # r1 = get!(r0)

    # Get `"42"` from the second nested Some constant.
    poems.inst(PoemOperation.Const, 0, 2),        # r0 = Some(Some("42"))
    poems.inst_dispatch(0, 1, 0),                 # r0 = flatten(r0)
    poems.inst_dispatch(2, 0, 0),                 # r2 = get!(r0)

    # Build an ExampleResult from `12` and `"42"`.
    poems.inst_dispatch(0, 2, 1, 2),              # r0 = ExampleResult$new(r1, r2)
    poems.inst(PoemOperation.Return0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(
    values: @[
      poems.struct_value("lore.option.None", @[], @[]),
      new_some_value(poems.int_type, poems.int_value(12)),
      new_some_value(
        new_some_type(poems.string_type),
        new_some_value(poems.string_type, poems.string_value("42")),
      ),
    ],
    intrinsics: @["lore.core.panic"],
    schemas: @["ExampleResult"],
    multi_functions: @["lore.option.get!", "lore.option.flatten", "ExampleResult$new"],
  ),
  schemas: @[Option, Some, None, ExampleResult],
  functions: @[ExampleResult_construct, get0, get1, get2, flatten0, flatten1, flatten2, test],
)
