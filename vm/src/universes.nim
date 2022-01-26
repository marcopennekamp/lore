import std/sequtils
import std/strformat
import std/tables
import sugar

import definitions
import imseqs
import poems
from pyramid import nil
import schema_order
from types import Kind, Type, TypeParameter, TypeVariable, SumType, IntersectionType, TupleType, FunctionType,
                  ListType, MapType, ShapeType, Schema,  DeclaredType, TraitSchema, TraitType, StructSchema,
                  StructSchemaProperty, attach_inherited_shape_type, attach_property_type
from values import TaggedValue

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
proc resolve(universe: Universe, poem_type_parameter: PoemTypeParameter): TypeParameter
proc resolve(universe: Universe, poem_type: PoemType): Type
proc resolve(universe: Universe, poem_value: PoemValue): TaggedValue

template resolve_many(universe, sequence): untyped = sequence.map(o => universe.resolve(o))

proc attach_constants(universe: Universe, poem: Poem)

########################################################################################################################
# Top-level resolution.                                                                                                #
########################################################################################################################

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

  # TODO (vm/schemas): Do we really have to resolve properties after functions?

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

proc resolve(universe: Universe, poem_constants: PoemConstants): Constants =
  let constants = new_constants()

  for poem_type in poem_constants.types:
    constants.types.add(universe.resolve(poem_type))

  for poem_value in poem_constants.values:
    constants.values.add(universe.resolve(poem_value))

  constants.names = poem_constants.names

  # TODO (vm): We should probably check whether the name exists and output an error. Same goes for global variables,
  #            schemas, and multi-functions.
  for name in poem_constants.intrinsics:
    constants.intrinsics.add(universe.intrinsics[name])

  for name in poem_constants.schemas:
    constants.schemas.add(universe.schemas[name])

  for name in poem_constants.global_variables:
    constants.global_variables.add(universe.global_variables[name])

  for name in poem_constants.multi_functions:
    constants.multi_functions.add(universe.multi_functions[name])

  for poem_meta_shape in poem_constants.meta_shapes:
    constants.meta_shapes.add(types.get_meta_shape(poem_meta_shape.property_names))

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
    types.new_trait_schema(poem_schema.name, type_parameters, supertraits)
  else:
    let poem_schema = cast[PoemStructSchema](poem_schema)
    let properties = universe.resolve_struct_properties(poem_schema.properties)
    types.new_struct_schema(poem_schema.name, type_parameters, supertraits, properties)

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
proc attach_type_parameters(tpe: Type, type_parameters: ImSeq[TypeParameter])
proc attach_type_parameters(types: ImSeq[Type], type_parameters: ImSeq[TypeParameter])

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

  let function = Function(
    multi_function: multi_function,
    type_parameters: type_parameters,
    input_type: input_type,
    output_type: output_type,
    register_count: poem_function.register_count,
    instructions: poem_function.instructions,
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

  else: discard

proc attach_type_parameters(types: ImSeq[Type], type_parameters: ImSeq[TypeParameter]) =
  for tpe in types:
    attach_type_parameters(tpe, type_parameters)

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

method resolve(poem_type: PoemTypeVariable, universe: Universe): Type {.locks: "unknown".} = types.variable(poem_type.index)

method resolve(poem_type: PoemBasicType, universe: Universe): Type {.locks: "unknown".} = poem_type.tpe

method resolve(poem_type: PoemXaryType, universe: Universe): Type =
  if poem_type.kind == Kind.Sum:
    types.sum(universe.resolve_many(poem_type.types))
  elif poem_type.kind == Kind.Tuple:
    if poem_type.types.len == 0:
      types.unit_type
    else:
      types.tpl(universe.resolve_many(poem_type.types))
  elif poem_type.kind == Kind.Function:
    let resolved_types = universe.resolve_many(poem_type.types)
    let input = resolved_types[0]
    if (input.kind != Kind.Tuple):
      quit("Function type inputs must be tuple types.")
    types.function(cast[TupleType](input), resolved_types[1])
  elif poem_type.kind == Kind.List:
    types.list(universe.resolve(poem_type.types[0]))
  else:
    quit(fmt"Unsupported kind {poem_type.kind}.")

method resolve(poem_type: PoemShapeType, universe: Universe): Type {.locks: "unknown".} =
  let meta_shape = types.get_meta_shape(poem_type.property_names)
  let property_types = universe.resolve_many(poem_type.property_types)
  types.new_shape_type(meta_shape, property_types)

method resolve(poem_type: PoemSymbolType, universe: Universe): Type {.locks: "unknown".} = types.symbol(poem_type.name)

method resolve(poem_type: PoemNamedType, universe: Universe): Type {.locks: "unknown".} =
  if poem_type.name notin universe.schemas:
    quit(fmt"The schema for a named type {poem_type.name} hasn't been resolved yet or doesn't exist.")

  let schema = universe.schemas[poem_type.name]
  let type_arguments = new_immutable_seq(universe.resolve_many(poem_type.type_arguments))
  types.force_instantiate_schema(schema, type_arguments)

########################################################################################################################
# Value resolution.                                                                                                    #
########################################################################################################################

method resolve(poem_value: PoemValue, universe: Universe): TaggedValue {.base, locks: "unknown".} =
  quit("Please implement `resolve` for all PoemValues.")

proc resolve(universe: Universe, poem_value: PoemValue): TaggedValue =
  poem_value.resolve(universe)

method resolve(poem_value: PoemIntValue, universe: Universe): TaggedValue {.locks: "unknown".} = values.tag_int(poem_value.int)
method resolve(poem_value: PoemRealValue, universe: Universe): TaggedValue {.locks: "unknown".} = values.new_real_tagged(poem_value.real)
method resolve(poem_value: PoemBooleanValue, universe: Universe): TaggedValue {.locks: "unknown".} = values.tag_boolean(poem_value.boolean)
method resolve(poem_value: PoemStringValue, universe: Universe): TaggedValue {.locks: "unknown".} = values.new_string_tagged(poem_value.string)

method resolve(poem_value: PoemTupleValue, universe: Universe): TaggedValue =
  let tpe = universe.resolve(poem_value.tpe)
  assert(tpe.kind == Kind.Tuple)
  let elements = universe.resolve_many(poem_value.elements)
  values.new_tuple_tagged(new_immutable_seq(elements), tpe)

method resolve(poem_value: PoemFunctionValue, universe: Universe): TaggedValue {.locks: "unknown".} =
  quit("Please implement `resolve` for all PoemFunctionValues.")

method resolve(poem_value: PoemFixedFunctionValue, universe: Universe): TaggedValue {.locks: "unknown".} =
  quit("Fixed function value resolution is not yet implemented.")

method resolve(poem_value: PoemLambdaFunctionValue, universe: Universe): TaggedValue {.locks: "unknown".} =
  let tpe = universe.resolve(poem_value.tpe)
  assert(tpe.kind == Kind.Function)

  let mf = universe.multi_functions[poem_value.name]
  if mf.functions.len > 1:
    quit(fmt"Cannot create a lambda value from multi-function `{mf.name}`, because it has more than one function.")

  let function = mf.functions[0]
  if not function.is_monomorphic:
    quit(fmt"Cannot create a lambda value from function `{mf.name}`, because the function is polymorphic.")

  values.new_function_tagged(true, cast[pointer](addr function.monomorphic_instance), tpe)

method resolve(poem_value: PoemMultiFunctionValue, universe: Universe): TaggedValue {.locks: "unknown".} =
  let tpe = universe.resolve(poem_value.tpe)
  assert(tpe.kind == Kind.Function)
  let mf = universe.multi_functions[poem_value.name]
  values.new_function_tagged(false, cast[pointer](mf), tpe)

method resolve(poem_value: PoemListValue, universe: Universe): TaggedValue {.locks: "unknown".} =
  let tpe = universe.resolve(poem_value.tpe)
  assert(tpe.kind == Kind.List)
  let elements = universe.resolve_many(poem_value.elements)
  values.new_list_tagged(new_immutable_seq(elements), tpe)

method resolve(poem_value: PoemShapeValue, universe: Universe): TaggedValue {.locks: "unknown".} =
  let tpe = universe.resolve(poem_value.tpe)
  assert(tpe.kind == Kind.Shape)
  let shape_type = cast[ShapeType](tpe)
  let property_values = universe.resolve_many(poem_value.property_values)
  values.new_shape_value_tagged(shape_type.meta, property_values)

method resolve(poem_value: PoemSymbolValue, universe: Universe): TaggedValue {.locks: "unknown".} = values.new_symbol_tagged(poem_value.name)

method resolve(poem_value: PoemStructValue, universe: Universe): TaggedValue {.locks: "unknown".} =
  if poem_value.tpe.name notin universe.schemas:
    quit(fmt"The schema {poem_value.tpe.name} for a struct value doesn't exist.")

  let schema = universe.schemas[poem_value.tpe.name]
  let type_arguments = new_immutable_seq(universe.resolve_many(poem_value.tpe.type_arguments))
  let property_values = universe.resolve_many(poem_value.property_values)
  values.new_struct_value_tagged(schema, type_arguments, property_values)
