import std/sequtils
import std/strformat
import std/tables
import sugar

import definitions
import imseqs
import poems
from pyramid import nil
from types import Kind, Type, TypeVariable, SumType, IntersectionType, TupleType, FunctionType, ListType, MapType, TypeParameter
from values import TaggedValue

type
  Universe* = ref object
    ## The Universe object provides access to all top-level entities of the current Lore program.
    intrinsics*: Table[string, Intrinsic]
    global_variables*: Table[string, GlobalVariable]
    multi_functions*: Table[string, MultiFunction]

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
  ## Multi-functions have to be resolved right away such that constants tables for each Poem can point to
  ## multi-functions and also fixed functions. Constants tables for all functions are then resolved in a separate step.
  ## Global variables are resolved after functions, because lazy global variables point to a Function initializer.
  var universe = Universe()

  # Step 1: Register intrinsics first, as they have no dependencies.
  for intrinsic in pyramid.intrinsics:
    universe.intrinsics[intrinsic.name] = intrinsic

  # Step 2: Resolve functions.
  for poem in poems:
    for poem_function in poem.functions:
      universe.resolve(poem_function)

  # Step 3: Resolve global variables.
  for poem in poems:
    for poem_global_variable in poem.global_variables:
      universe.resolve(poem_global_variable)

  # Step 4: Resolve constants and attach them to all functions in each Poem.
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

  # TODO (vm): We should probably check whether the name exists and output an error. Same goes for global variables and
  #            multi-functions.
  for name in poem_constants.intrinsics:
    constants.intrinsics.add(universe.intrinsics[name])

  for name in poem_constants.global_variables:
    constants.global_variables.add(universe.global_variables[name])

  for name in poem_constants.multi_functions:
    constants.multi_functions.add(universe.multi_functions[name])

  for poem_meta_shape in poem_constants.meta_shapes:
    constants.meta_shapes.add(types.get_meta_shape(poem_meta_shape.property_names))

  constants

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
  quit(fmt"Cannot resolve named types yet. Name: {poem_type.name}.")

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

method resolve(poem_value: PoemSymbolValue, universe: Universe): TaggedValue {.locks: "unknown".} = values.new_symbol_tagged(poem_value.name)
