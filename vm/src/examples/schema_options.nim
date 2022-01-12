from "../instructions" import Operation, Instruction, new_instruction
from "../poems" import Poem, PoemConstants, PoemSchema, PoemTraitSchema, PoemMetaShape, PoemFunction, PoemTypeParameter
from "../types" import Kind, Variance

# In this example, we declare option types and instantiate them with various sample data.

let Option: PoemSchema = PoemTraitSchema(
  kind: Kind.Trait,
  name: "lore.Option",
  type_parameters: @[
    PoemTypeParameter(name: "A", lower_bound: poems.nothing_type, upper_bound: poems.any_type, variance: Variance.Covariant),
  ],
  supertraits: @[],
  inherited_shape_type: poems.empty_shape_type,
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
  schemas: @[Option],
  functions: @[test],
)
