import std/sequtils
import std/strformat
import std/tables
import sugar

import definitions
import imseqs
import instructions
import poems
import property_index
from pyramid import nil
import schema_order
import types
import values

type
  Universe* = ref object
    ## The Universe object provides access to all top-level entities of the current Lore program.
    intrinsics*: TableRef[string, Intrinsic]
    schemas*: TableRef[string, Schema]
    global_variables*: TableRef[string, GlobalVariable]
    multi_functions*: TableRef[string, MultiFunction]

proc resolve_schemas(universe: Universe, poems: seq[Poem])

proc resolve(universe: Universe, poem_global_variable: PoemGlobalVariable)
proc resolve(universe: Universe, poem_constants: PoemConstants): Constants
proc resolve(universe: Universe, poem_function: PoemFunction)
proc resolve(universe: Universe, poem_function_instance: PoemFunctionInstance): ptr FunctionInstance
proc resolve(universe: Universe, poem_type_parameter: PoemTypeParameter): TypeParameter
proc resolve(universe: Universe, poem_type: PoemType): Type
proc resolve(universe: Universe, poem_value: PoemValue): TaggedValue

template resolve_many(universe, sequence): untyped = sequence.map(o => universe.resolve(o))

proc resolve_instructions(poem_function: PoemFunction, universe: Universe)

proc attach_type_parameters(tpe: Type, type_parameters: ImSeq[TypeParameter])
proc attach_type_parameters(types: ImSeq[Type], type_parameters: ImSeq[TypeParameter])

########################################################################################################################
# Top-level resolution.                                                                                                #
########################################################################################################################

proc attach_constants(universe: Universe, poem: Poem)

proc resolve*(poems: seq[Poem]): Universe =
  ## Resolves a Universe from the given set of Poem definitions.
  ##
  ## Schemas have to be resolved first, as types in global variables and functions may refer to any schema. Because
  ## schemas can reference each other, we have to resolve them in a specific order based on their dependencies. In
  ## addition, struct schema properties must be resolved in a second step because they may reference any type, and
  ## after functions due to default values.
  ##
  ## Multi-functions have to be resolved before constants tables so that constants tables for each Poem can point to
  ## multi-functions and fixed functions. Constants tables for all functions are then resolved in a separate step.
  ## Global variables are resolved after functions, because lazy global variables point to a Function initializer.
  var universe = Universe(
    intrinsics: new_table[string, Intrinsic](),
    schemas: new_table[string, Schema](),
    global_variables: new_table[string, GlobalVariable](),
    multi_functions: new_table[string, MultiFunction](),
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

  # Step 4: Resolve global variables.
  for poem in poems:
    for poem_global_variable in poem.global_variables:
      universe.resolve(poem_global_variable)

  # Step 5: Resolve constants and attach them to all functions in each Poem.
  for poem in poems:
    universe.attach_constants(poem)

  universe

proc attach_constants(universe: Universe, poem: Poem) =
  let constants = universe.resolve(poem.constants)
  for poem_function in poem.functions:
    poem_function.resolved_function.constants = constants
    poem_function.resolve_instructions(universe)

proc resolve(universe: Universe, poem_constants: PoemConstants): Constants =
  let constants = new_constants()

  for poem_type in poem_constants.types:
    constants.types.add(universe.resolve(poem_type))

  for poem_value in poem_constants.values:
    constants.values.add(universe.resolve(poem_value))

  constants.names = poem_constants.names

  for name in poem_constants.intrinsics:
    if name notin universe.intrinsics:
      quit(fmt"The intrinsic `{name}` doesn't exist.")
    constants.intrinsics.add(universe.intrinsics[name])

  for name in poem_constants.schemas:
    if name notin universe.schemas:
      quit(fmt"The schema `{name}` doesn't exist.")
    constants.schemas.add(universe.schemas[name])

  for name in poem_constants.global_variables:
    if name notin universe.global_variables:
      quit(fmt"The global variable `{name}` doesn't exist.")
    constants.global_variables.add(universe.global_variables[name])

  for name in poem_constants.multi_functions:
    if name notin universe.multi_functions:
      quit(fmt"The multi-function `{name}` doesn't exist.")
    constants.multi_functions.add(universe.multi_functions[name])

  for poem_function_instance in poem_constants.function_instances:
    constants.function_instances.add(universe.resolve(poem_function_instance))

  for poem_meta_shape in poem_constants.meta_shapes:
    constants.meta_shapes.add(get_meta_shape(poem_meta_shape.property_names))

  constants

########################################################################################################################
# Schema resolution.                                                                                                   #
########################################################################################################################

proc get_schema_dependencies(poem_schema: PoemSchema): seq[string]
proc resolve_schema(universe: Universe, poem_schema: PoemSchema): Schema
proc resolve_struct_properties(universe: Universe, poem_properties: seq[PoemStructProperty]): ImSeq[StructSchemaProperty]
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
  let type_parameters = new_immutable_seq(universe.resolve_many(poem_schema.type_parameters))

  # The poem reader ensures that the supertraits are all named types, but we have to additionally rule out that some of
  # these named types might be structs.
  let supertypes = new_immutable_seq(universe.resolve_many(poem_schema.supertraits))
  for supertype in supertypes:
    if supertype.kind != Kind.Trait:
      quit(fmt"The schema {poem_schema.name} has a supertrait which isn't a trait.")
  let supertraits = cast[ImSeq[TraitType]](supertypes)

  if poem_schema.kind == Kind.Trait:
    new_trait_schema(poem_schema.name, type_parameters, supertraits)
  else:
    let poem_schema = cast[PoemStructSchema](poem_schema)
    let properties = universe.resolve_struct_properties(poem_schema.properties)
    new_struct_schema(poem_schema.name, type_parameters, supertraits, properties)

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

proc resolve_schema_attachments(universe: Universe, poem_schemas: TableRef[string, PoemSchema]) =
  for name, schema in universe.schemas:
    if schema.kind == Kind.Trait:
      let schema = cast[TraitSchema](schema)
      let poem_schema = cast[PoemTraitSchema](poem_schemas[name])
      # The poem reader ensures that this is a shape type, so we can just cast!
      schema.attach_inherited_shape_type(cast[ShapeType](universe.resolve(poem_schema.inherited_shape_type)))
    elif schema.kind == Kind.Struct:
      let schema = cast[StructSchema](schema)
      let poem_schema = cast[PoemStructSchema](poem_schemas[name])
      for poem_property in poem_schema.properties:
        schema.attach_property_type(poem_property.name, universe.resolve(poem_property.tpe))

########################################################################################################################
# Global variable resolution.                                                                                          #
########################################################################################################################

method resolve(poem_global_variable: PoemGlobalVariable, universe: Universe): GlobalVariable {.base, locks: "unknown".} =
  quit(fmt"Please implement `resolve` for all PoemGlobalVariables.")

proc resolve(universe: Universe, poem_global_variable: PoemGlobalVariable) =
  let name = poem_global_variable.name
  if name in universe.global_variables:
    quit(fmt"The global variable `{name}` is declared twice.")
  universe.global_variables[name] = poem_global_variable.resolve(universe)

method resolve(poem_global_variable: PoemEagerGlobalVariable, universe: Universe): GlobalVariable {.locks: "unknown".} =
  let value = universe.resolve(poem_global_variable.value)
  new_eager_global(poem_global_variable.name, value)

method resolve(poem_global_variable: PoemLazyGlobalVariable, universe: Universe): GlobalVariable {.locks: "unknown".} =
  let name = poem_global_variable.name
  let initializer_name = poem_global_variable.initializer_name
  if not (initializer_name in universe.multi_functions):
    quit(fmt"The global variable `{name}` requires an initializer function `{initializer_name}`, but it does not exist.")

  let mf = universe.multi_functions[initializer_name]
  if mf.functions.len > 1:
    quit(fmt"The global variable `{name}` requires an initializer function `{initializer_name}`, but the associated multi-function has multiple function definitions.")

  let initializer = mf.functions[0]
  if not initializer.is_monomorphic:
    quit(fmt"The global variable `{name}` has a polymorphic initializer function `{initializer_name}`. Initializer functions must be monomorphic.")

  new_lazy_global(name, initializer.monomorphic_instance)

########################################################################################################################
# Function resolution.                                                                                                 #
########################################################################################################################

proc get_or_register_multi_function(universe: Universe, name: string): MultiFunction

proc resolve(universe: Universe, poem_function: PoemFunction) =
  ## Resolves a PoemFunction. Does not create the constants table, which must be resolved in a separate step.
  let multi_function = universe.get_or_register_multi_function(poem_function.name)
  let type_parameters = new_immutable_seq(universe.resolve_many(poem_function.type_parameters))

  let input_type_raw = universe.resolve(poem_function.input_type)
  if input_type_raw.kind != Kind.Tuple:
    quit(fmt"Functions must always have a tuple as their input type. Function name: {poem_function.name}.")
  let input_type = cast[TupleType](input_type_raw)
  let output_type = universe.resolve(poem_function.output_type)

  # We have to attach the type parameters to each type variable contained in the input and output types.
  attach_type_parameters(input_type, type_parameters)
  attach_type_parameters(output_type, type_parameters)

  # Instructions are resolved when the constants table is attached.
  let function = Function(
    multi_function: multi_function,
    type_parameters: type_parameters,
    input_type: input_type,
    output_type: output_type,
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

########################################################################################################################
# Instruction resolution.                                                                                              #
########################################################################################################################

type
  InstructionResolutionContext = ref object
    universe: Universe
    poem_function: PoemFunction
    pc_offset: int
      ## The program counter offset grows when a PoemInstruction generates more than one Instruction. The target
      ## locations of all following jump instructions must be incremented by this offset.

proc get_function(context: InstructionResolutionContext): Function = context.poem_function.resolved_function
proc get_constants(context: InstructionResolutionContext): Constants = context.get_function.constants

proc add_instructions(context: InstructionResolutionContext, instructions: open_array[Instruction]) =
  ## Adds the given instructions to the resolved function. This function assumes that the given instructions were
  ## spawned from a single PoemInstruction and increments the `pc_offset` accordingly.
  for instruction in instructions:
    context.get_function.instructions.add(instruction)
  context.pc_offset += instructions.len - 1

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

proc resolve_instructions(poem_function: PoemFunction, universe: Universe) =
  let context = InstructionResolutionContext(universe: universe, poem_function: poem_function, pc_offset: 0)
  for poem_instruction in poem_function.instructions:
    let instructions = poem_instruction.resolve_instruction(context)
    context.add_instructions(instructions)

method resolve_instruction(poem_instruction: PoemSimpleInstruction, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  let operation = simple_poem_operation_to_operation(poem_instruction.operation)
  var instruction = new_instruction(operation, poem_instruction.arguments)

  # We have to add `pc_offset` to the `pc` of all jump instructions.
  if operation.is_jump_operation:
    instruction.set_arg(0, uint16(int(instruction.arg(0)) + context.pc_offset))

  @[instruction]

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

method resolve_instruction(poem_instruction: PoemInstructionLambda, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  if poem_instruction.captured_regs.len > operand_list_limit:
    quit(fmt"The `Lambda` operation cannot yet handle more than {operand_list_limit} captured registers.")

  let tpe = context.get_constants.types[poem_instruction.tpe]
  let prefix = [poem_instruction.target_reg, poem_instruction.mf, poem_instruction.tpe]
  if tpe.is_monomorphic:
    generate_xary_application(Operation.Lambda, [Operation.Lambda0], prefix, poem_instruction.captured_regs)
  else:
    generate_xary_application(Operation.LambdaPoly, [Operation.LambdaPoly0], prefix, poem_instruction.captured_regs)

method resolve_instruction(poem_instruction: PoemInstructionList, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  if poem_instruction.element_regs.len > operand_list_limit:
    quit(fmt"The `List` operation cannot yet handle more than {operand_list_limit} elements.")

  let tpe = context.get_constants.types[poem_instruction.tpe]
  let prefix = [poem_instruction.target_reg, poem_instruction.tpe]
  if tpe.is_monomorphic:
    generate_xary_application(Operation.List, [Operation.List0, Operation.List1], prefix, poem_instruction.element_regs)
  else:
    generate_xary_application(Operation.ListPoly, [Operation.ListPoly0, Operation.ListPoly1], prefix, poem_instruction.element_regs)

method resolve_instruction(poem_instruction: PoemInstructionListAppend, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  let tpe = context.get_constants.types[poem_instruction.tpe]
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
  let constants = context.get_constants
  let schema = cast[StructSchema](constants.schemas[instance_schema])
  let name = constants.names[name]
  let offset = schema.property_index.find_offset_if_exists(name)
  if offset < 0:
    quit(fmt"Cannot access property `{name}` of schema {schema.name}: the property doesn't exist.")
  cast[uint16](offset)

method resolve_instruction(poem_instruction: PoemInstructionIntrinsic, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  let intrinsic = context.get_constants.intrinsics[poem_instruction.intrinsic]
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
  let global = context.get_constants.global_variables[poem_instruction.global]
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
  let function_instance = context.get_constants.function_instances[poem_instruction.fin]
  check_call_function_arity(function_instance.function, poem_instruction.value_argument_regs.len)
  generate_opl_or_direct(
    Operation.Call,
    Operation.CallDirect,
    [poem_instruction.target_reg, poem_instruction.fin],
    poem_instruction.value_argument_regs,
  )

method resolve_instruction(poem_instruction: PoemInstructionCallPoly, context: InstructionResolutionContext): seq[Instruction] {.locks: "unknown".} =
  let mf = context.get_constants.multi_functions[poem_instruction.mf]
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
  let tpe = context.get_constants.types[poem_instruction.tpe]
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

  of PoemOperation.IntConst: Operation.IntConst
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

  of PoemOperation.StringOf: Operation.StringOf
  of PoemOperation.StringConcat: Operation.StringConcat
  of PoemOperation.StringEq: Operation.StringEq
  of PoemOperation.StringLt: Operation.StringLt
  of PoemOperation.StringLte: Operation.StringLte

  of PoemOperation.TupleGet: Operation.TupleGet

  of PoemOperation.LambdaLocal: Operation.LambdaLocal

  of PoemOperation.ListAppendUntyped: Operation.ListAppendUntyped
  of PoemOperation.ListLength: Operation.ListLength
  of PoemOperation.ListGet: Operation.ListGet

  of PoemOperation.SymbolEq: Operation.SymbolEq

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

  of PoemOperation.Tuple, PoemOperation.FunctionCall, PoemOperation.FunctionSingle, PoemOperation.Lambda,
     PoemOperation.List, PoemOperation.ListAppend, PoemOperation.Shape, PoemOperation.Struct, PoemOperation.StructPoly,
     PoemOperation.PropertyGet, PoemOperation.PropertySet, PoemOperation.Intrinsic, PoemOperation.GlobalGet,
     PoemOperation.Dispatch, PoemOperation.Call, PoemOperation.CallPoly, PoemOperation.Return, PoemOperation.TypeConst:
    quit(fmt"The poem operation {poem_operation} is not simple!")

########################################################################################################################
# Function instance resolution.                                                                                        #
########################################################################################################################

proc resolve(universe: Universe, poem_function_instance: PoemFunctionInstance): ptr FunctionInstance =
  if poem_function_instance.name notin universe.multi_functions:
    quit(fmt"The multi-function `{poem_function_instance.name}` doesn't exist.")

  let mf = universe.multi_functions[poem_function_instance.name]
  let type_arguments = new_immutable_seq(universe.resolve_many(poem_function_instance.type_arguments))

  # As the type arguments are static, their bound conformity has already been checked at compile time. However, as
  # universe resolution isn't performance-critical, the added redundancy of a bounds check doesn't hurt.
  mf.instantiate_single_function(type_arguments)

########################################################################################################################
# Type parameter resolution.                                                                                           #
########################################################################################################################

proc resolve(universe: Universe, poem_type_parameter: PoemTypeParameter): TypeParameter =
  let lower_bound = universe.resolve(poem_type_parameter.lower_bound)
  let upper_bound = universe.resolve(poem_type_parameter.upper_bound)

  TypeParameter(
    name: poem_type_parameter.name,
    lower_bound: lower_bound,
    upper_bound: upper_bound,
    variance: poem_type_parameter.variance,
  )

########################################################################################################################
# Type resolution.                                                                                                     #
########################################################################################################################

method resolve(poem_type: PoemType, universe: Universe): Type {.base, locks: "unknown".} =
  quit("Please implement `resolve` for all PoemTypes.")

proc resolve(universe: Universe, poem_type: PoemType): Type =
  poem_type.resolve(universe)

method resolve(poem_type: PoemTypeVariable, universe: Universe): Type {.locks: "unknown".} = new_type_variable(poem_type.index)

method resolve(poem_type: PoemBasicType, universe: Universe): Type {.locks: "unknown".} = poem_type.tpe

method resolve(poem_type: PoemXaryType, universe: Universe): Type =
  if poem_type.kind == Kind.Sum:
    new_sum_type(universe.resolve_many(poem_type.types))
  elif poem_type.kind == Kind.Intersection:
    new_intersection_type(universe.resolve_many(poem_type.types))
  elif poem_type.kind == Kind.Tuple:
    if poem_type.types.len == 0:
      unit_type
    else:
      new_tuple_type(universe.resolve_many(poem_type.types))
  elif poem_type.kind == Kind.Function:
    let resolved_types = universe.resolve_many(poem_type.types)
    let input = resolved_types[0]
    if (input.kind != Kind.Tuple):
      quit("Function type inputs must be tuple types.")
    new_function_type(cast[TupleType](input), resolved_types[1])
  elif poem_type.kind == Kind.List:
    new_list_type(universe.resolve(poem_type.types[0]))
  else:
    quit(fmt"Unsupported kind {poem_type.kind}.")

method resolve(poem_type: PoemShapeType, universe: Universe): Type {.locks: "unknown".} =
  let meta_shape = get_meta_shape(poem_type.property_names)
  let property_types = universe.resolve_many(poem_type.property_types)
  new_shape_type(meta_shape, property_types)

method resolve(poem_type: PoemSymbolType, universe: Universe): Type {.locks: "unknown".} = new_symbol_type(poem_type.name)

method resolve(poem_type: PoemNamedType, universe: Universe): Type {.locks: "unknown".} =
  if poem_type.name notin universe.schemas:
    quit(fmt"The schema for a named type {poem_type.name} hasn't been resolved yet or doesn't exist.")

  let schema = universe.schemas[poem_type.name]
  let type_arguments = new_immutable_seq(universe.resolve_many(poem_type.type_arguments))
  force_instantiate_schema(schema, type_arguments)

########################################################################################################################
# Value resolution.                                                                                                    #
########################################################################################################################

method resolve(poem_value: PoemValue, universe: Universe): TaggedValue {.base, locks: "unknown".} =
  quit("Please implement `resolve` for all PoemValues.")

proc resolve(universe: Universe, poem_value: PoemValue): TaggedValue =
  poem_value.resolve(universe)

method resolve(poem_value: PoemIntValue, universe: Universe): TaggedValue {.locks: "unknown".} = tag_int(poem_value.int)
method resolve(poem_value: PoemRealValue, universe: Universe): TaggedValue {.locks: "unknown".} = new_real_value_tagged(poem_value.real)
method resolve(poem_value: PoemBooleanValue, universe: Universe): TaggedValue {.locks: "unknown".} = tag_boolean(poem_value.boolean)
method resolve(poem_value: PoemStringValue, universe: Universe): TaggedValue {.locks: "unknown".} = new_string_value_tagged(poem_value.string)

method resolve(poem_value: PoemTupleValue, universe: Universe): TaggedValue =
  let tpe = universe.resolve(poem_value.tpe)
  assert(tpe.kind == Kind.Tuple)
  let elements = universe.resolve_many(poem_value.elements)
  new_tuple_value_tagged(new_immutable_seq(elements), tpe)

method resolve(poem_value: PoemFunctionValue, universe: Universe): TaggedValue {.locks: "unknown".} =
  quit("Please implement `resolve` for all PoemFunctionValues.")

method resolve(poem_value: PoemMultiFunctionValue, universe: Universe): TaggedValue {.locks: "unknown".} =
  let tpe = universe.resolve(poem_value.tpe)
  assert(tpe.kind == Kind.Function)
  let mf = universe.multi_functions[poem_value.name]
  tag_reference(new_multi_function_value(cast[pointer](mf), tpe))

method resolve(poem_value: PoemSingleFunctionValue, universe: Universe): TaggedValue {.locks: "unknown".} =
  let tpe = universe.resolve(poem_value.tpe)
  assert(tpe.kind == Kind.Function)

  let mf = universe.multi_functions[poem_value.name]
  if not mf.is_single_function:
    quit(fmt"A multi-function backing a lambda must be a single function. Multi-function name: {mf.name}.")

  let function = mf.functions[0]
  let type_arguments = new_immutable_seq(universe.resolve_many(poem_value.type_arguments))
  let instance = function.instantiate(type_arguments)
  tag_reference(new_single_function_value(cast[pointer](instance), LambdaContext(nil), tpe))

method resolve(poem_value: PoemFixedFunctionValue, universe: Universe): TaggedValue {.locks: "unknown".} =
  quit("Fixed function value resolution is not yet implemented.")

method resolve(poem_value: PoemListValue, universe: Universe): TaggedValue {.locks: "unknown".} =
  let tpe = universe.resolve(poem_value.tpe)
  assert(tpe.kind == Kind.List)
  let elements = universe.resolve_many(poem_value.elements)
  new_list_value_tagged(new_immutable_seq(elements), tpe)

method resolve(poem_value: PoemShapeValue, universe: Universe): TaggedValue {.locks: "unknown".} =
  let tpe = universe.resolve(poem_value.tpe)
  assert(tpe.kind == Kind.Shape)
  let shape_type = cast[ShapeType](tpe)
  let property_values = universe.resolve_many(poem_value.property_values)
  new_shape_value_tagged(shape_type.meta, property_values)

method resolve(poem_value: PoemSymbolValue, universe: Universe): TaggedValue {.locks: "unknown".} = new_symbol_value_tagged(poem_value.name)

method resolve(poem_value: PoemStructValue, universe: Universe): TaggedValue {.locks: "unknown".} =
  if poem_value.tpe.name notin universe.schemas:
    quit(fmt"The schema {poem_value.tpe.name} for a struct value doesn't exist.")

  let schema = universe.schemas[poem_value.tpe.name]
  let type_arguments = new_immutable_seq(universe.resolve_many(poem_value.tpe.type_arguments))
  let property_values = universe.resolve_many(poem_value.property_values)
  new_struct_value_tagged(schema, type_arguments, property_values)

########################################################################################################################
# Helpers.                                                                                                             #
########################################################################################################################

proc attach_type_parameters(tpe: Type, type_parameters: ImSeq[TypeParameter]) =
  case tpe.kind
  of Kind.TypeVariable:
    let tv = cast[TypeVariable](tpe)
    tv.parameter = type_parameters[tv.index]

  of Kind.Sum: attach_type_parameters(cast[SumType](tpe).parts, type_parameters)
  of Kind.Intersection: attach_type_parameters(cast[IntersectionType](tpe).parts, type_parameters)
  of Kind.Tuple: attach_type_parameters(cast[TupleType](tpe).elements, type_parameters)

  of Kind.Function:
    let tpe = cast[FunctionType](tpe)
    attach_type_parameters(tpe.input, type_parameters)
    attach_type_parameters(tpe.output, type_parameters)

  of Kind.List:
    attach_type_parameters(cast[ListType](tpe).element, type_parameters)

  of Kind.Map:
    let tpe = cast[MapType](tpe)
    attach_type_parameters(tpe.key, type_parameters)
    attach_type_parameters(tpe.value, type_parameters)

  of Kind.Trait, Kind.Struct:
    let tpe: DeclaredType = cast[DeclaredType](tpe)
    attach_type_parameters(tpe.type_arguments, type_parameters)
    attach_type_parameters(cast[ImSeq[Type]](tpe.supertraits), type_parameters)

  else: discard

proc attach_type_parameters(types: ImSeq[Type], type_parameters: ImSeq[TypeParameter]) =
  for tpe in types:
    attach_type_parameters(tpe, type_parameters)
