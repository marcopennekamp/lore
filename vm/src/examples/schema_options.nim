from "../instructions" import Operation, Instruction, new_instruction
from "../poems" import Poem, PoemConstants, PoemSchema, PoemTraitSchema, PoemStructSchema, PoemStructProperty,
                       PoemMetaShape, PoemFunction, PoemTypeParameter
from "../types" import Kind, Variance

# In this example, we declare option types and instantiate them with various sample data.

let Option: PoemSchema = PoemTraitSchema(
  kind: Kind.Trait,
  name: "lore.option.Option",
  type_parameters: @[
    PoemTypeParameter(name: "A", lower_bound: poems.nothing_type, upper_bound: poems.any_type, variance: Variance.Covariant),
  ],
  supertraits: @[],
  inherited_shape_type: poems.empty_shape_type,
)

let Some: PoemSchema = PoemStructSchema(
  kind: Kind.Struct,
  name: "lore.option.Some",
  type_parameters: @[
    # TODO (vm/schemas): This should be an open type parameter.
    PoemTypeParameter(name: "A", lower_bound: poems.nothing_type, upper_bound: poems.any_type, variance: Variance.Covariant),
  ],
  supertraits: @[poems.named_type_concrete("lore.option.Option", @[poems.type_variable(0)])],
  properties: @[poems.struct_property("value", poems.type_variable(0), false)],
)

let test = PoemFunction(
  name: "test",
  input_type: poems.unit_type,
  output_type: poems.int_type,
  is_abstract: false,
  register_count: 1,
  instructions: @[
    new_instruction(Operation.IntConst, 0, 42),
    new_instruction(Operation.Return0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(),
  schemas: @[Option, Some],
  functions: @[test],
)
