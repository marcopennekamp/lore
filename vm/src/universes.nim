import std/algorithm
import std/sequtils
import std/strformat
import std/sugar
import std/tables

import definitions
from dispatch import find_dispatch_target, build_dispatch_hierarchy
import imseqs
import instructions
import poems
import property_index
from pyramid import nil
import schema_order
import types
import values

proc resolve_schemas(universe: Universe, poems: seq[Poem])

proc resolve(universe: Universe, poem_global_variable: PoemGlobalVariable): GlobalVariable
proc resolve(universe: Universe, poem_function: PoemFunction)
proc resolve(universe: Universe, poem_function_instance: PoemFunctionInstance, type_variables: ImSeq[TypeVariable]): ptr FunctionInstance
proc resolve(universe: Universe, poem_constants: PoemConstants, type_variables: ImSeq[TypeVariable]): Constants
proc resolve(universe: Universe, poem_instructions: seq[PoemInstruction], constants: Constants): seq[Instruction]
proc resolve(universe: Universe, poem_spec: PoemSpec): Spec
proc resolve(universe: Universe, poem_type_parameters: seq[PoemTypeParameter]): ImSeq[TypeVariable]
proc resolve(universe: Universe, poem_type: PoemType, type_variables: ImSeq[TypeVariable]): Type
proc resolve(universe: Universe, poem_value: PoemValue, type_variables: ImSeq[TypeVariable]): TaggedValue

template resolve_many(universe, sequence): untyped = sequence.map(o => universe.resolve(o))
template resolve_many(universe, sequence, argument): untyped = sequence.map(o => universe.resolve(o, argument))

# Post-construction initialization
proc finalize(mf: MultiFunction)
proc attach_constants(poem_function: PoemFunction, universe: Universe)
proc attach_instructions(poem_function: PoemFunction, universe: Universe)

# Feature initialization
proc initialize_introspection(universe: Universe)

########################################################################################################################
# Top-level resolution.                                                                                                #
########################################################################################################################

proc resolve*(poems: seq[Poem]): Universe =
  ## Resolves a Universe from the given set of Poem definitions.
  ##
  ## Regarding the order of resolution:
  ##  - Schemas have to be resolved first, as types in global variables and functions may refer to any schema. Because
  ##    schemas can reference each other, we have to resolve them in a specific order based on their dependencies. In
  ##    addition, struct schema properties must be resolved in a second step because they may reference any type.
  ##  - Functions are resolved at this point, building an understanding of the program's multi-functions. However,
  ##    constants and instructions are resolved later due to dependencies on other multi-functions and global
  ##    variables.
  ##  - Global variables are resolved after functions, because lazy global variables point to a Function initializer.
  ##  - Specs are resolved after functions, because specs point to a Function executable.
  ##  - Constants tables and instructions for all functions are resolved after schemas, multi-functions, and global
  ##    variables due to their dependency on these entities. Instructions depend on the constants table and are thus
  ##    resolved immediately after.
  var universe = Universe(
    intrinsics: new_table[string, Intrinsic](),
    schemas: new_table[string, Schema](),
    global_variables: new_table[string, GlobalVariable](),
    multi_functions: new_table[string, MultiFunction](),
    specs: @[],
  )

  # Step 1: Register intrinsics first, as they have no dependencies.
  for intrinsic in pyramid.intrinsics:
    universe.intrinsics[intrinsic.name] = intrinsic

  # Step 2: Resolve schemas.
  universe.resolve_schemas(poems)

  # Step 3: Resolve functions.
  for poem in poems:
    for poem_function in poem.functions:
      universe.resolve(poem_function)

  # Step 4: Resolve global variables and specs. Global variables and specs are independent of each other and can be
  # resolved in one go.
  for poem in poems:
    for poem_global_variable in poem.global_variables:
      universe.global_variables[poem_global_variable.name] = universe.resolve(poem_global_variable)
    for poem_spec in poem.specs:
      universe.specs.add(universe.resolve(poem_spec))

  # Step 5: Attach constants and instructions to each function.
  for poem in poems:
    for poem_function in poem.functions:
      attach_constants(poem_function, universe)
      attach_instructions(poem_function, universe)

  # Step 6: Finalize each multi-function.
  for mf in universe.multi_functions.values:
    finalize(mf)

  # Step 7: Sort specs by module name. This is important for test and benchmark reporting.
  universe.specs.sort do (a: Spec, b: Spec) -> int:
    cmp(a.module_name, b.module_name)

  universe.initialize_introspection()
  universe

########################################################################################################################
# Schema resolution.                                                                                                   #
########################################################################################################################

proc get_schema_dependencies(poem_schema: PoemSchema): seq[string]
proc resolve_schema(universe: Universe, poem_schema: PoemSchema): Schema
proc resolve_struct_properties(universe: Universe, poem_properties: seq[PoemStructProperty]): ImSeq[StructSchemaProperty]
proc resolve_struct_property_declaration_order(universe: Universe, poem_properties: seq[PoemStructProperty]): ImSeq[string]
proc resolve_schema_attachments(universe: Universe, poem_schemas: TableRef[string, PoemSchema])

proc resolve_schemas(universe: Universe, poems: seq[Poem]) =
  ## Resolves all schemas in their resolution order.
  var poem_schemas = new_table[string, PoemSchema]()
  for poem in poems:
    for poem_schema in poem.schemas:
      if poem_schema.name in poem_schemas:
        quit(fmt"The poem schema {poem_schema.name} is declared twice. All schema names must be unique!")
      poem_schemas[poem_schema.name] = poem_schema

  var dependency_graph = new_schema_dependency_graph()
  for poem_schema in poem_schemas.values():
    # A schema that has no dependencies and which isn't the dependency of another schema must be added to the graph
    # manually to become part of the resolution order. We accomplish this by adding a dependency on "<root>".
    let dependencies = get_schema_dependencies(poem_schema)
    if dependencies.len > 0:
      for dependency in dependencies:
        dependency_graph.add_dependency(poem_schema.name, dependency)
    else:
      dependency_graph.add_dependency(poem_schema.name, "<root>")

  # "<root>" may have been added as a dependency to the graph. It must be the first element in the resolution order by
  # nature of its definition, but it must not necessarily exist.
  var resolution_order = dependency_graph.sort_topological()
  if resolution_order.len > 0 and resolution_order[0] == "<root>":
    resolution_order = resolution_order[1 ..< resolution_order.len]

  for name in resolution_order:
    let schema = universe.resolve_schema(poem_schemas[name])
    universe.schemas[name] = schema

  # Now that all schemas have been registered, we can resolve inherited shape types and properties.
  universe.resolve_schema_attachments(poem_schemas)

proc get_schema_dependencies(poem_schema: PoemSchema): seq[string] =
  ## Collects the names of all schemas that `poem_schema` depends on. Inherited shape types are resolved in a second
  ## step and thus don't count as dependencies.
  var dependencies = new_seq[string]()

  for parameter in poem_schema.type_parameters:
    poems.collect_type_name_dependencies(parameter, dependencies)

  for supertrait in poem_schema.supertraits:
    poems.collect_type_name_dependencies(supertrait, dependencies)

  dependencies

proc resolve_schema(universe: Universe, poem_schema: PoemSchema): Schema =
  let type_parameters = universe.resolve(poem_schema.type_parameters)

  # The poem reader ensures that the supertraits are all named types, but we have to additionally rule out that some of
  # these named types might be structs.
  let supertypes = new_immutable_seq(universe.resolve_many(poem_schema.supertraits, type_parameters))
  for supertype in supertypes:
    if supertype.kind != Kind.Trait:
      quit(fmt"The schema {poem_schema.name} has a supertrait which isn't a trait.")
  let supertraits = cast[ImSeq[TraitType]](supertypes)

  if poem_schema.kind == Kind.Trait:
    new_trait_schema(poem_schema.name, type_parameters, supertraits)
  else:
    let poem_schema = cast[PoemStructSchema](poem_schema)
    let properties = universe.resolve_struct_properties(poem_schema.properties)
    let declaration_order = universe.resolve_struct_property_declaration_order(poem_schema.properties)
    new_struct_schema(poem_schema.name, type_parameters, supertraits, properties, declaration_order)

proc resolve_struct_properties(universe: Universe, poem_properties: seq[PoemStructProperty]): ImSeq[StructSchemaProperty] =
  ## Resolves struct properties from the given list of poem struct properties. Property types won't be added, as they
  ## are resolved in a second step.
  var properties = new_immutable_seq[StructSchemaProperty](poem_properties.len)
  var open_index = 0'u16

  for i in 0 ..< poem_properties.len:
    let poem_property = poem_properties[i]
    var property = StructSchemaProperty(
      name: poem_property.name,
      tpe: nil,
      is_open: poem_property.is_open,
    )

    if property.is_open:
      property.open_index = open_index
      open_index += 1
    properties[i] = property

  properties

proc resolve_struct_property_declaration_order(universe: Universe, poem_properties: seq[PoemStructProperty]): ImSeq[string] =
  var declaration_order = new_immutable_seq[string](poem_properties.len)
  for poem_property in poem_properties:
    declaration_order[poem_property.declaration_index] = poem_property.name
  declaration_order

proc resolve_schema_attachments(universe: Universe, poem_schemas: TableRef[string, PoemSchema]) =
  ## Resolves inherited shape types and property types in a second schema resolution phase.
  for name, schema in universe.schemas:
    if schema.kind == Kind.Trait:
      let schema = cast[TraitSchema](schema)
      let poem_schema = cast[PoemTraitSchema](poem_schemas[name])
      # The poem reader ensures that this is a shape type, so we can just cast!
      let shape_type = cast[ShapeType](universe.resolve(poem_schema.inherited_shape_type, schema.type_parameters))
      schema.attach_inherited_shape_type(shape_type)
    elif schema.kind == Kind.Struct:
      let schema = cast[StructSchema](schema)
      let poem_schema = cast[PoemStructSchema](poem_schemas[name])
      for poem_property in poem_schema.properties:
        schema.attach_property_type(poem_property.name, universe.resolve(poem_property.tpe, schema.type_parameters))

########################################################################################################################
# Global variable resolution.                                                                                          #
########################################################################################################################

method resolve(poem_global_variable: PoemGlobalVariable, universe: Universe): GlobalVariable {.base, locks: "unknown".} =
  quit(fmt"Please implement `resolve` for all PoemGlobalVariables.")

proc resolve(universe: Universe, poem_global_variable: PoemGlobalVariable): GlobalVariable =
  let name = poem_global_variable.name
  if name in universe.global_variables:
    quit(fmt"The global variable `{name}` is declared twice.")
  poem_global_variable.resolve(universe)

method resolve(poem_global_variable: PoemEagerGlobalVariable, universe: Universe): GlobalVariable {.locks: "unknown".} =
  let value = universe.resolve(poem_global_variable.value, empty_immutable_seq[TypeVariable]())
  new_eager_global(poem_global_variable.name, value)

method resolve(poem_global_variable: PoemLazyGlobalVariable, universe: Universe): GlobalVariable {.locks: "unknown".} =
  let name = poem_global_variable.name
  let initializer = universe.get_multi_function(poem_global_variable.initializer_name).get_single_monomorphic_function_instance
  new_lazy_global(name, initializer[])

########################################################################################################################
# Function resolution.                                                                                                 #
########################################################################################################################

proc get_or_register_multi_function(universe: Universe, name: string): MultiFunction

proc resolve(universe: Universe, poem_function: PoemFunction) =
  ## Resolves a PoemFunction. Does not create the constants table, which must be resolved in a separate step.
  let multi_function = universe.get_or_register_multi_function(poem_function.name)
  let type_parameters = universe.resolve(poem_function.type_parameters)

  let input_type_raw = universe.resolve(poem_function.input_type, type_parameters)
  if input_type_raw.kind != Kind.Tuple:
    quit(fmt"Functions must always have a tuple as their input type. Function name: {poem_function.name}.")
  let input_type = cast[TupleType](input_type_raw)
  let output_type = universe.resolve(poem_function.output_type, type_parameters)

  # Constants and instructions are resolved in a second step.
  let function = Function(
    multi_function: multi_function,
    type_parameters: type_parameters,
    input_type: input_type,
    output_type: output_type,
    is_abstract: poem_function.is_abstract,
    register_count: poem_function.register_count,
  )

  if function.is_monomorphic:
    function.monomorphic_instance = FunctionInstance(function: function, type_arguments: empty_immutable_seq[Type]())

  init_frame_stats(function)

  poem_function.resolved_function = function
  multi_function.functions.add(function)

proc get_or_register_multi_function(universe: Universe, name: string): MultiFunction =
  ## Ensures that the universe contains a multi-function with the given full name, and returns it.
  if not (name in universe.multi_functions):
    universe.multi_functions[name] = MultiFunction(
      name: name,
      functions: @[],
    )
  universe.multi_functions[name]

proc finalize(mf: MultiFunction) =
  ## Ensures that all functions of the multi-function are unique (none are equally specific) and builds the dispatch
  ## hierarchy.
  if not mf.are_functions_unique:
    quit(fmt"The multi-function `{mf.name}` has two or more functions that are equally specific. Your bytecode is incorrect or you might have loaded bytecode from two conflicting compiler sources.")
  mf.attach_dispatch_hierarchy(mf.build_dispatch_hierarchy())

########################################################################################################################
# Constants.                                                                                                           #
########################################################################################################################

proc attach_constants(poem_function: PoemFunction, universe: Universe) =
  let function = poem_function.resolved_function
  function.constants =
    if not poem_function.is_abstract:
      universe.resolve(poem_function.constants, function.type_parameters)
    else:
      Constants(empty_immutable_seq[ConstantsEntry]())

proc resolve(universe: Universe, poem_constants: PoemConstants, type_variables: ImSeq[TypeVariable]): Constants =
  var entries = new_immutable_seq[ConstantsEntry](poem_constants.entries.len)
  for i in 0 ..< poem_constants.entries.len:
    let poem_entry = poem_constants.entries[i]
    entries[i] =
      case poem_entry.variant
      of PoemConstantsEntryVariant.Type:
        cast[ConstantsEntry](universe.resolve(poem_entry.tpe, type_variables))
      of PoemConstantsEntryVariant.Value:
        cast[ConstantsEntry](universe.resolve(poem_entry.value, type_variables))
      of PoemConstantsEntryVariant.Name:
        cast[ConstantsEntry](ConstantsEntryName(name: poem_entry.name))
      of PoemConstantsEntryVariant.Intrinsic:
        cast[ConstantsEntry](universe.get_intrinsic(poem_entry.name))
      of PoemConstantsEntryVariant.Schema:
        cast[ConstantsEntry](universe.get_schema(poem_entry.name))
      of PoemConstantsEntryVariant.GlobalVariable:
        cast[ConstantsEntry](universe.get_global_variable(poem_entry.name))
      of PoemConstantsEntryVariant.MultiFunction:
        cast[ConstantsEntry](universe.get_multi_function(poem_entry.name))
      of PoemConstantsEntryVariant.FunctionInstance:
        cast[ConstantsEntry](universe.resolve(poem_entry.function_instance, type_variables))
      of PoemConstantsEntryVariant.MetaShape:
        cast[ConstantsEntry](get_meta_shape(poem_entry.meta_shape.property_names))
  Constants(entries)

########################################################################################################################
# Instruction resolution.                                                                                              #
########################################################################################################################

proc attach_instructions(poem_function: PoemFunction, universe: Universe) =
  if not poem_function.is_abstract:
    let function = poem_function.resolved_function
    function.instructions = universe.resolve(poem_function.instructions, function.constants)

type
  InstructionResolutionContext = ref object
    universe: Universe
    constants: Constants
    instructions: seq[Instruction]
    location_mapping: seq[int]
      ## Maps each original PoemInstruction location to the first corresponding Instruction location, which might
      ## differ once multiple Instructions are generated for a single PoemInstruction. Each index of the sequence
      ## represents the original PoemInstruction location number, while sequence values are Instruction location
      ## numbers. This is used to update the target location of jump instructions.

proc add_instructions(context: InstructionResolutionContext, instructions: open_array[Instruction]) =
  ## Adds the given instructions to the context. This function assumes that the given instructions were spawned from a
  ## single PoemInstruction and updates `location_mapping` accordingly.
  context.location_mapping.add(context.instructions.len)
  for instruction in instructions:
    context.instructions.add(instruction)

proc generate_xary_application(
  operation_opl: Operation,
  operation_x: open_array[Operation],
  prefix: open_array[uint16],
  arguments: open_array[uint16],
): seq[Instruction]

proc generate_opl_or_direct(
  operation_opl: Operation,
  operation_direct: Operation,
  prefix: open_array[uint16],
  arguments: open_array[uint16],
): seq[Instruction]

proc generate_opl_or_direct2(
  operation_opl: Operation,
  operation_direct: Operation,
  prefix: open_array[uint16],
  arguments1: open_array[uint16],
  arguments2: open_array[uint16],
): seq[Instruction]

proc generate_opl_pushes(arguments: open_array[uint16]): seq[Instruction]

proc simple_poem_operation_to_operation(poem_operation: PoemOperation): Operation

method resolve_instruction(poem_instruction: PoemInstruction, context: InstructionResolutionContext): seq[Instruction] {.base, locks: "unknown".} =
  quit("Please implement `resolve_instruction` for all poem instructions.")

proc resolve(universe: Universe, poem_instructions: seq[PoemInstruction], constants: Constants): seq[Instruction] =
  let context = InstructionResolutionContext(
    constants: constants,
    instructions: @[],
    location_mapping: @[],
  )
  for poem_instruction in poem_instructions:
    let instructions = poem_instruction.resolve_instruction(context)
    context.add_instructions(instructions)

  # Update jump locations in a second iteration, after the location mapping is built during the first iteration.
  for instruction in context.instructions.mitems:
    if instruction.operation.is_jump_operation:
      let target_index = int(instruction.arg(0))
      instruction.set_arg(0, uint16(context.location_mapping[target_index]))

  context.instructions

method resolve_instruction(poem_instruction: PoemSimpleInstruction, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  let operation = simple_poem_operation_to_operation(poem_instruction.operation)
  @[new_instruction(operation, poem_instruction.arguments)]

method resolve_instruction(poem_instruction: PoemInstructionIntConst, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  if low(int16) <= poem_instruction.value and poem_instruction.value <= high(int16):
    @[new_instruction(Operation.IntConst, poem_instruction.target_reg, cast[uint16](int16(poem_instruction.value)))]
  else:
    @[new_instruction64(
      Operation.IntConst64,
      poem_instruction.target_reg,
      cast[uint64](poem_instruction.value),
    )]

method resolve_instruction(poem_instruction: PoemInstructionTuple, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  generate_xary_application(
    Operation.Tuple,
    [Operation.Tuple0, Operation.Tuple1, Operation.Tuple2],
    [poem_instruction.target_reg],
    poem_instruction.element_regs,
  )

method resolve_instruction(poem_instruction: PoemInstructionFunctionCall, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  generate_xary_application(
    Operation.FunctionCall,
    [Operation.FunctionCall0, Operation.FunctionCall1, Operation.FunctionCall2],
    [poem_instruction.target_reg, poem_instruction.function_reg],
    poem_instruction.argument_regs,
  )

method resolve_instruction(poem_instruction: PoemInstructionFunctionSingle, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  generate_xary_application(
    Operation.FunctionSingle,
    [],
    [poem_instruction.target_reg, poem_instruction.mf],
    poem_instruction.type_argument_regs,
  )

method resolve_instruction(poem_instruction: PoemInstructionFunctionLambda, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  if poem_instruction.captured_regs.len > operand_list_limit:
    quit(fmt"The `FunctionLambda` operation cannot yet handle more than {operand_list_limit} captured registers.")

  let tpe = context.constants.const_type(poem_instruction.tpe)
  let prefix = [poem_instruction.target_reg, poem_instruction.mf, poem_instruction.tpe]
  if tpe.is_monomorphic:
    generate_xary_application(Operation.FunctionLambda, [Operation.FunctionLambda0], prefix, poem_instruction.captured_regs)
  else:
    generate_xary_application(Operation.FunctionLambdaPoly, [Operation.FunctionLambdaPoly0], prefix, poem_instruction.captured_regs)

method resolve_instruction(poem_instruction: PoemInstructionList, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  if poem_instruction.element_regs.len > operand_list_limit:
    quit(fmt"The `List` operation cannot yet handle more than {operand_list_limit} elements.")

  let tpe = context.constants.const_type(poem_instruction.tpe)
  let prefix = [poem_instruction.target_reg, poem_instruction.tpe]
  if tpe.is_monomorphic:
    generate_xary_application(Operation.List, [Operation.List0, Operation.List1], prefix, poem_instruction.element_regs)
  else:
    generate_xary_application(Operation.ListPoly, [Operation.ListPoly0, Operation.ListPoly1], prefix, poem_instruction.element_regs)

method resolve_instruction(poem_instruction: PoemInstructionListAppend, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  let tpe = context.constants.const_type(poem_instruction.tpe)
  let operation = if tpe.is_monomorphic: Operation.ListAppend else: Operation.ListAppendPoly
  @[new_instruction(operation, poem_instruction.target_reg, poem_instruction.list_reg, poem_instruction.element_reg, poem_instruction.tpe)]

method resolve_instruction(poem_instruction: PoemInstructionShape, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  generate_xary_application(
    Operation.Shape,
    [Operation.Shape0, Operation.Shape1, Operation.Shape2],
    [poem_instruction.target_reg, poem_instruction.meta_shape],
    poem_instruction.property_value_regs,
  )

method resolve_instruction(poem_instruction: PoemInstructionStruct, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  generate_opl_or_direct(
    Operation.Struct,
    Operation.StructDirect,
    [poem_instruction.target_reg, poem_instruction.tpe],
    poem_instruction.value_argument_regs,
  )

method resolve_instruction(poem_instruction: PoemInstructionStructPoly, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  generate_opl_or_direct2(
    Operation.StructPoly,
    Operation.StructPolyDirect,
    [poem_instruction.target_reg, poem_instruction.schema],
    poem_instruction.type_argument_regs,
    poem_instruction.value_argument_regs,
  )

proc get_struct_property_offset(instance_schema: uint16, name: uint16, context: InstructionResolutionContext): uint16

method resolve_instruction(poem_instruction: PoemInstructionPropertyGet, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  let (operation, property_id) =
    case poem_instruction.instance_kind
    of InstanceKind.Any: (Operation.PropertyGetNamed, poem_instruction.name)
    of InstanceKind.Shape: (Operation.ShapePropertyGetNamed, poem_instruction.name)
    of InstanceKind.Trait: (Operation.StructPropertyGetNamed, poem_instruction.name)
    of InstanceKind.Struct:
      let offset = get_struct_property_offset(poem_instruction.instance_schema, poem_instruction.name, context)
      (Operation.StructPropertyGet, offset)
  @[new_instruction(operation, poem_instruction.target_reg, poem_instruction.instance_reg, property_id)]

method resolve_instruction(poem_instruction: PoemInstructionPropertySet, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  let (operation, property_id) =
    case poem_instruction.instance_kind
    of InstanceKind.Trait: (Operation.StructPropertySetNamed, poem_instruction.name)
    of InstanceKind.Struct:
      let offset = get_struct_property_offset(poem_instruction.instance_schema, poem_instruction.name, context)
      (Operation.StructPropertySet, offset)
    else: quit(fmt"Instance kind {poem_instruction.instance_kind} is not allowed for `PropertySet`.")
  @[new_instruction(operation, poem_instruction.instance_reg, property_id, poem_instruction.value_reg)]

proc get_struct_property_offset(instance_schema: uint16, name: uint16, context: InstructionResolutionContext): uint16 =
  let constants = context.constants
  let schema = cast[StructSchema](constants.const_schema(instance_schema))
  let name = constants.const_name(name)
  let offset = schema.property_index.find_offset_if_exists(name)
  if offset < 0:
    quit(fmt"Cannot access property `{name}` of schema {schema.name}: the property doesn't exist.")
  cast[uint16](offset)

method resolve_instruction(poem_instruction: PoemInstructionIntrinsic, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  let intrinsic = context.constants.const_intrinsic(poem_instruction.intrinsic)
  let argument_count = poem_instruction.argument_regs.len
  if intrinsic.arity != argument_count:
    quit(fmt"The intrinsic `{intrinsic.name}` with arity {intrinsic.arity} cannot be called with {argument_count} arguments.")

  generate_opl_or_direct(
    Operation.Intrinsic,
    Operation.IntrinsicDirect,
    [poem_instruction.target_reg, poem_instruction.intrinsic],
    poem_instruction.argument_regs,
  )

method resolve_instruction(poem_instruction: PoemInstructionGlobalGet, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  let global = context.constants.const_global_variable(poem_instruction.global)
  let operation = if global.is_initialized: Operation.GlobalGetEager else: Operation.GlobalGetLazy
  @[new_instruction(operation, poem_instruction.target_reg, poem_instruction.global)]

method resolve_instruction(poem_instruction: PoemInstructionDispatch, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  generate_xary_application(
    Operation.Dispatch,
    [Operation.Dispatch0, Operation.Dispatch1, Operation.Dispatch2],
    [poem_instruction.target_reg, poem_instruction.mf],
    poem_instruction.argument_regs,
  )

proc check_call_function_arity(function: Function, argument_count: int) =
  ## Ensures that the given function has the required arity. If not, the VM quits with an error. This catches bytecode
  ## where a function is called with the wrong arity, which is hard to catch once it reaches the evaluator.
  if function.arity != argument_count:
    quit(fmt"The function `{function.name}` with arity {function.arity} cannot be called with {argument_count} arguments.")

method resolve_instruction(poem_instruction: PoemInstructionCall, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  let function_instance = context.constants.const_function_instance(poem_instruction.fin)
  check_call_function_arity(function_instance.function, poem_instruction.value_argument_regs.len)
  generate_opl_or_direct(
    Operation.Call,
    Operation.CallDirect,
    [poem_instruction.target_reg, poem_instruction.fin],
    poem_instruction.value_argument_regs,
  )

method resolve_instruction(poem_instruction: PoemInstructionCallPoly, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  let mf = context.constants.const_multi_function(poem_instruction.mf)
  check_call_function_arity(mf.get_single_function, poem_instruction.value_argument_regs.len)
  generate_opl_or_direct2(
    Operation.CallPoly,
    Operation.CallPolyDirect,
    [poem_instruction.target_reg, poem_instruction.mf],
    poem_instruction.type_argument_regs,
    poem_instruction.value_argument_regs,
  )

method resolve_instruction(poem_instruction: PoemInstructionReturn, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  if poem_instruction.value_reg == 0:
    @[new_instruction(Operation.Return0)]
  else:
    @[new_instruction(Operation.Return, poem_instruction.value_reg)]

method resolve_instruction(poem_instruction: PoemInstructionTypeConst, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  let tpe = context.constants.const_type(poem_instruction.tpe)
  let operation = if tpe.is_monomorphic: Operation.TypeConst else: Operation.TypeConstPoly
  @[new_instruction(operation, poem_instruction.target_reg, poem_instruction.tpe)]

proc generate_xary_application(
  operation_opl: Operation,
  operation_x: open_array[Operation],
  prefix: open_array[uint16],
  arguments: open_array[uint16],
): seq[Instruction] =
  ## Generates an application of a xary operation that takes a list of register arguments.
  ##
  ## `operation_opl`: This operation will be applied with `OplPushX`.
  ## `operation_x`: These operations will be applied without using the operand list. At index `x`, the operation has
  ##                arity `x`.
  ## `prefix`: Any instruction arguments before the actual xary arguments.
  ##
  ## You may set `operation_opl` to `Operation.Invalid` to disable operand lists for this xary application. The VM will
  ## quit when it cannot generate a fixed-arity instruction.
  let arity = arguments.len
  let maximum_direct_arguments = maximum_instruction_arguments - prefix.len
  if arity <= maximum_direct_arguments and arity in 0 .. operation_x.high:
    @[new_instruction(operation_x[arity], @prefix & @arguments)]
  else:
    if operation_opl == Operation.Invalid:
      quit("Operand lists are not supported for the given operation. Fixed-arity operations: {operand_x}.")
    let pushes = generate_opl_pushes(arguments)
    let instruction = new_instruction(operation_opl, @prefix & @[uint16(arguments.len)])
    pushes & @[instruction]

proc generate_opl_or_direct(
  operation_opl: Operation,
  operation_direct: Operation,
  prefix: open_array[uint16],
  arguments: open_array[uint16],
): seq[Instruction] =
  ## Generates a direct instruction if the operand count is small enough, or an operand list-based instruction
  ## otherwise.
  # The `-1` takes the argument count into account.
  let maximum_direct_arguments = maximum_instruction_arguments - prefix.len - 1
  if arguments.len <= maximum_direct_arguments:
    @[new_instruction(operation_direct, @prefix & @[uint16(arguments.len)] & @arguments)]
  else:
    let pushes = generate_opl_pushes(@arguments)
    pushes & @[
      new_instruction(operation_opl, @prefix & @[uint16(arguments.len)]),
    ]

proc generate_opl_or_direct2(
  operation_opl: Operation,
  operation_direct: Operation,
  prefix: open_array[uint16],
  arguments1: open_array[uint16],
  arguments2: open_array[uint16],
): seq[Instruction] =
  ## Generates a direct instruction if the operand count is small enough, or an operand list-based instruction
  ## otherwise. This generation function supports two distinct argument lists. When generating a direct instruction, a
  ## composite count is used to represent both argument lists' lengths in a single instruction argument.
  let operand_count = arguments1.len + arguments2.len

  # The `-1` takes the composite count argument into account.
  let maximum_direct_arguments = maximum_instruction_arguments - prefix.len - 1

  if operand_count <= maximum_direct_arguments:
    let composite_count = uint16(((arguments1.len shl 8) and 0xFF00) or (arguments2.len and 0xFF))
    @[new_instruction(operation_direct, @prefix & @[composite_count] & @arguments1 & @arguments2)]
  else:
    let pushes = generate_opl_pushes(@arguments1 & @arguments2)
    pushes & @[
      new_instruction(operation_opl, @prefix & @[uint16(arguments1.len), uint16(arguments2.len)]),
    ]

proc generate_opl_pushes(arguments: open_array[uint16]): seq[Instruction] =
  ## Slices `arguments` into a number of `OplPushX` instructions.
  let max_push = 6
  var instructions = new_seq[Instruction]()
  var remaining = arguments.len

  while remaining >= max_push:
    let base_index = arguments.len - remaining
    let arguments_slice = arguments[base_index ..< base_index + max_push]
    instructions.add(new_instruction(Operation.OplPush6, uint16(base_index), arguments_slice))
    remaining -= max_push

  if remaining == 0:
    return instructions

  block:
    let base_index = arguments.len - remaining
    let arguments_slice = arguments[base_index ..< base_index + remaining]
    let operation = case remaining
      of 1: Operation.OplPush1
      of 2: Operation.OplPush2
      of 3: Operation.OplPush3
      of 4: Operation.OplPush4
      of 5: Operation.OplPush5
      else: quit("The remaining number of operands to push with OplPushX must be 5 or less.")
    instructions.add(new_instruction(operation, uint16(base_index), arguments_slice))

  instructions

proc simple_poem_operation_to_operation(poem_operation: PoemOperation): Operation =
  ## This is only defined for simple poem operations.
  case poem_operation
  of PoemOperation.Assign: Operation.Assign

  of PoemOperation.Const: Operation.Const

  of PoemOperation.IntNeg: Operation.IntNeg
  of PoemOperation.IntAdd: Operation.IntAdd
  of PoemOperation.IntSub: Operation.IntSub
  of PoemOperation.IntMul: Operation.IntMul
  of PoemOperation.IntDiv: Operation.IntDiv
  of PoemOperation.IntEq: Operation.IntEq
  of PoemOperation.IntLt: Operation.IntLt
  of PoemOperation.IntLte: Operation.IntLte
  of PoemOperation.IntToReal: Operation.IntToReal

  of PoemOperation.RealNeg: Operation.RealNeg
  of PoemOperation.RealAdd: Operation.RealAdd
  of PoemOperation.RealSub: Operation.RealSub
  of PoemOperation.RealMul: Operation.RealMul
  of PoemOperation.RealDiv: Operation.RealDiv
  of PoemOperation.RealEq: Operation.RealEq
  of PoemOperation.RealLt: Operation.RealLt
  of PoemOperation.RealLte: Operation.RealLte

  of PoemOperation.BooleanConst: Operation.BooleanConst
  of PoemOperation.BooleanNot: Operation.BooleanNot
  of PoemOperation.BooleanOr: Operation.BooleanOr
  of PoemOperation.BooleanAnd: Operation.BooleanAnd
  of PoemOperation.BooleanEq: Operation.BooleanEq

  of PoemOperation.StringOf: Operation.StringOf
  of PoemOperation.StringConcat: Operation.StringConcat
  of PoemOperation.StringEq: Operation.StringEq
  of PoemOperation.StringLt: Operation.StringLt
  of PoemOperation.StringLte: Operation.StringLte

  of PoemOperation.SymbolEq: Operation.SymbolEq

  of PoemOperation.TupleGet: Operation.TupleGet

  of PoemOperation.LambdaLocal: Operation.LambdaLocal

  of PoemOperation.ListAppendUntyped: Operation.ListAppendUntyped
  of PoemOperation.ListLength: Operation.ListLength
  of PoemOperation.ListGet: Operation.ListGet

  of PoemOperation.StructEq: Operation.StructEq

  of PoemOperation.Jump: Operation.Jump
  of PoemOperation.JumpIfFalse: Operation.JumpIfFalse
  of PoemOperation.JumpIfTrue: Operation.JumpIfTrue

  of PoemOperation.GlobalSet: Operation.GlobalSet

  of PoemOperation.TypeArg: Operation.TypeArg
  of PoemOperation.TypeOf: Operation.TypeOf
  of PoemOperation.TypePathIndex: Operation.TypePathIndex
  of PoemOperation.TypePathProperty: Operation.TypePathProperty
  of PoemOperation.TypePathTypeArgument: Operation.TypePathTypeArgument

  of PoemOperation.IntConst, PoemOperation.Tuple, PoemOperation.FunctionCall, PoemOperation.FunctionSingle,
     PoemOperation.FunctionLambda, PoemOperation.List, PoemOperation.ListAppend, PoemOperation.Shape,
     PoemOperation.Struct, PoemOperation.StructPoly, PoemOperation.PropertyGet, PoemOperation.PropertySet,
     PoemOperation.Intrinsic, PoemOperation.GlobalGet, PoemOperation.Dispatch, PoemOperation.Call,
     PoemOperation.CallPoly, PoemOperation.Return, PoemOperation.TypeConst:
    quit(fmt"The poem operation {poem_operation} is not simple!")

########################################################################################################################
# Function instance resolution.                                                                                        #
########################################################################################################################

proc resolve(universe: Universe, poem_function_instance: PoemFunctionInstance, type_variables: ImSeq[TypeVariable]): ptr FunctionInstance =
  let mf = universe.get_multi_function(poem_function_instance.name)
  let type_arguments = new_immutable_seq(universe.resolve_many(poem_function_instance.type_arguments, type_variables))

  # As the type arguments are static, their bound conformity has already been checked at compile time. However, as
  # universe resolution isn't performance-critical, the added redundancy of a bounds check doesn't hurt.
  mf.instantiate_single_function(type_arguments)

########################################################################################################################
# Spec resolution.                                                                                                     #
########################################################################################################################

proc resolve(universe: Universe, poem_spec: PoemSpec): Spec =
  let executable = universe.get_multi_function(poem_spec.executable_name).get_single_monomorphic_function_instance
  Spec(
    module_name: poem_spec.module_name,
    description: poem_spec.description,
    is_test: poem_spec.is_test,
    is_benchmark: poem_spec.is_benchmark,
    executable: executable[],
  )

########################################################################################################################
# Type parameter resolution.                                                                                           #
########################################################################################################################

proc resolve(universe: Universe, poem_type_parameters: seq[PoemTypeParameter]): ImSeq[TypeVariable] =
  ## Resolves the given poem type parameters to a list of type variables. The type variables may already be used in the
  ## bounds of preceding type variables, and should be used in any other types to ensure referential equality of type
  ## variables.
  var type_variables: ImSeq[TypeVariable] = new_immutable_seq[TypeVariable](poem_type_parameters.len)
  for index in 0 ..< poem_type_parameters.len:
    let poem_type_parameter = poem_type_parameters[index]
    let lower_bound = universe.resolve(poem_type_parameter.lower_bound, type_variables)
    let upper_bound = universe.resolve(poem_type_parameter.upper_bound, type_variables)

    type_variables[index] = TypeVariable(
      kind: Kind.TypeVariable,
      index: uint8(index),
      name: poem_type_parameter.name,
      lower_bound: lower_bound,
      upper_bound: upper_bound,
      variance: poem_type_parameter.variance,
    )
  type_variables

########################################################################################################################
# Type resolution.                                                                                                     #
########################################################################################################################

type TypeResolutionContext = object
  universe: Universe
  type_variables: ImSeq[TypeVariable]
    ## All type variables that the type to resolve may reference. Using `type_variables` for type variable resolution
    ## ensures that all occurences of a type variable are resolved to the same reference, which allows checking type
    ## variable equality with referential equality.

method resolve(poem_type: PoemType, context: TypeResolutionContext): Type {.base, locks: "unknown".} =
  quit("Please implement `resolve` for all PoemTypes.")

proc resolve(universe: Universe, poem_type: PoemType, type_variables: ImSeq[TypeVariable]): Type =
  ## Resolves the given poem type, taking type variables from `type_variables`.
  poem_type.resolve(TypeResolutionContext(universe: universe, type_variables: type_variables))

template resolve_many_types(poem_types, context): untyped = poem_types.map(pt => pt.resolve(context))

method resolve(poem_type: PoemTypeVariable, context: TypeResolutionContext): Type {.locks: "unknown".} =
  if int(poem_type.index) >= context.type_variables.len:
    quit(fmt"Cannot get type variable with index {poem_type.index}: out of bounds.")
  context.type_variables[poem_type.index]

method resolve(poem_type: PoemBasicType, context: TypeResolutionContext): Type {.locks: "unknown".} =
  poem_type.tpe

method resolve(poem_type: PoemSymbolType, context: TypeResolutionContext): Type {.locks: "unknown".} =
  new_symbol_type(poem_type.name)

method resolve(poem_type: PoemXaryType, context: TypeResolutionContext): Type =
  if poem_type.kind == Kind.Sum:
    new_sum_type(poem_type.types.resolve_many_types(context))
  elif poem_type.kind == Kind.Intersection:
    new_intersection_type(poem_type.types.resolve_many_types(context))
  elif poem_type.kind == Kind.Tuple:
    if poem_type.types.len == 0:
      unit_type
    else:
      new_tuple_type(poem_type.types.resolve_many_types(context))
  elif poem_type.kind == Kind.Function:
    let resolved_types = poem_type.types.resolve_many_types(context)
    let input = resolved_types[0]
    if (input.kind != Kind.Tuple):
      quit("Function type inputs must be tuple types.")
    new_function_type(cast[TupleType](input), resolved_types[1])
  elif poem_type.kind == Kind.List:
    new_list_type(poem_type.types[0].resolve(context))
  else:
    quit(fmt"Unsupported kind {poem_type.kind}.")

method resolve(poem_type: PoemShapeType, context: TypeResolutionContext): Type {.locks: "unknown".} =
  let meta_shape = get_meta_shape(poem_type.property_names)
  let property_types = poem_type.property_types.resolve_many_types(context)
  new_shape_type(meta_shape, property_types)

method resolve(poem_type: PoemNamedType, context: TypeResolutionContext): Type {.locks: "unknown".} =
  let schema = context.universe.get_schema(poem_type.name)
  let type_arguments = new_immutable_seq(poem_type.type_arguments.resolve_many_types(context))
  force_instantiate_schema(schema, type_arguments)

########################################################################################################################
# Value resolution.                                                                                                    #
########################################################################################################################

type ValueResolutionContext = object
  universe: Universe
  type_variables: ImSeq[TypeVariable]

method resolve(poem_value: PoemValue, context: ValueResolutionContext): TaggedValue {.base, locks: "unknown".} =
  quit("Please implement `resolve` for all PoemValues.")

proc resolve(universe: Universe, poem_value: PoemValue, type_variables: ImSeq[TypeVariable]): TaggedValue =
  poem_value.resolve(ValueResolutionContext(universe: universe, type_variables: type_variables))

proc resolve_value_type[T: Type](poem_type: PoemType, expected_kind: Kind, context: ValueResolutionContext): T {.inline.} =
  let tpe = context.universe.resolve(poem_type, context.type_variables)
  if tpe.kind != expected_kind:
    quit(fmt"A {expected_kind} value must have a {expected_kind} type, but the type has kind {tpe.kind}.")
  cast[T](tpe)

method resolve(poem_value: PoemIntValue, context: ValueResolutionContext): TaggedValue {.locks: "unknown".} =
  tag_int(poem_value.int)

method resolve(poem_value: PoemRealValue, context: ValueResolutionContext): TaggedValue {.locks: "unknown".} =
  new_real_value_tagged(poem_value.real)

method resolve(poem_value: PoemBooleanValue, context: ValueResolutionContext): TaggedValue {.locks: "unknown".} =
  tag_boolean(poem_value.boolean)

method resolve(poem_value: PoemStringValue, context: ValueResolutionContext): TaggedValue {.locks: "unknown".} =
  new_string_value_tagged(poem_value.string)

method resolve(poem_value: PoemSymbolValue, context: ValueResolutionContext): TaggedValue {.locks: "unknown".} =
  new_symbol_value_tagged(poem_value.name)

method resolve(poem_value: PoemTupleValue, context: ValueResolutionContext): TaggedValue =
  let tpe = resolve_value_type[TupleType](poem_value.tpe, Kind.Tuple, context)
  let elements = context.universe.resolve_many(poem_value.elements, context.type_variables)
  new_tuple_value_tagged(new_immutable_seq(elements), tpe)

method resolve(poem_value: PoemFunctionValue, context: ValueResolutionContext): TaggedValue {.locks: "unknown".} =
  quit("Please implement `resolve` for all PoemFunctionValues.")

method resolve(poem_value: PoemMultiFunctionValue, context: ValueResolutionContext): TaggedValue {.locks: "unknown".} =
  let tpe = resolve_value_type[FunctionType](poem_value.tpe, Kind.Function, context)
  let mf = context.universe.multi_functions[poem_value.name]
  tag_reference(new_multi_function_value(cast[pointer](mf), tpe))

method resolve(poem_value: PoemSingleFunctionValue, context: ValueResolutionContext): TaggedValue {.locks: "unknown".} =
  let tpe = resolve_value_type[FunctionType](poem_value.tpe, Kind.Function, context)
  let mf = context.universe.multi_functions[poem_value.name]
  let function = mf.get_single_function
  let type_arguments = new_immutable_seq(context.universe.resolve_many(poem_value.type_arguments, context.type_variables))
  let instance = function.instantiate(type_arguments)
  tag_reference(new_single_function_value(cast[pointer](instance), tpe))

method resolve(poem_value: PoemFixedFunctionValue, context: ValueResolutionContext): TaggedValue {.locks: "unknown".} =
  let tpe = resolve_value_type[FunctionType](poem_value.tpe, Kind.Function, context)
  let mf = context.universe.multi_functions[poem_value.name]
  let input_type = context.universe.resolve(poem_value.input_type, context.type_variables)
  if input_type.kind != Kind.Tuple:
    quit(fmt"A single function value's input type be a tuple type.")
  let input_types = cast[TupleType](input_type).elements

  var target = new_function_instance()
  find_dispatch_target(mf, input_types.to_open_array, target[])

  tag_reference(new_single_function_value(target, tpe))

method resolve(poem_value: PoemListValue, context: ValueResolutionContext): TaggedValue {.locks: "unknown".} =
  let tpe = resolve_value_type[ListType](poem_value.tpe, Kind.List, context)
  let elements = context.universe.resolve_many(poem_value.elements, context.type_variables)
  new_list_value_tagged(new_immutable_seq(elements), tpe)

method resolve(poem_value: PoemShapeValue, context: ValueResolutionContext): TaggedValue {.locks: "unknown".} =
  let tpe = resolve_value_type[ShapeType](poem_value.tpe, Kind.Shape, context)
  let property_values = context.universe.resolve_many(poem_value.property_values, context.type_variables)
  new_shape_value_tagged(tpe.meta, property_values)

method resolve(poem_value: PoemStructValue, context: ValueResolutionContext): TaggedValue {.locks: "unknown".} =
  if poem_value.tpe.name notin context.universe.schemas:
    quit(fmt"The schema {poem_value.tpe.name} for a struct value doesn't exist.")

  let schema = context.universe.schemas[poem_value.tpe.name]
  let type_arguments = new_immutable_seq(context.universe.resolve_many(poem_value.tpe.type_arguments, context.type_variables))
  let property_values = context.universe.resolve_many(poem_value.property_values, context.type_variables)
  new_struct_value_tagged(schema, type_arguments, property_values)

########################################################################################################################
# Introspection.                                                                                                       #
########################################################################################################################

proc initialize_introspection(universe: Universe) =
  ## Initializes a backing struct type for the `lore.core.Type` trait. If `Type` doesn't exist, no backing struct type
  ## is generated, which is usually the case when executing examples.
  if universe.schemas.has_key(introspection_type_trait_name):
    let supertrait_schema = universe.schemas[introspection_type_trait_name]
    if supertrait_schema.kind != Kind.Trait:
      quit("`lore.core.Type` must be a trait.")

    let supertrait = cast[TraitSchema](supertrait_schema).get_representative
    let schema = new_struct_schema(
      "lore.core.Type/impl",
      empty_immutable_seq[TypeVariable](),
      new_immutable_seq[TraitType]([supertrait]),
      # The struct schema has no properties, because the actual value is represented by a IntrospectionTypeValue, which
      # contains no openly available properties, but actually hides the Type as a `boxed_type` field.
      empty_immutable_seq[StructSchemaProperty](),
      empty_immutable_seq[string](),
    )
    universe.introspection_type_struct_schema = schema
