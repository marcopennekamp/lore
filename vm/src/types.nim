from std/algorithm import sort
import std/macros
import std/sets
from std/sequtils import any_it, deduplicate
import std/strformat
import tables

import imseqs
import property_index
import stackseqs
from utils import call_if_any_exists

type
  Kind* {.pure.} = enum
    TypeVariable
    Any
    Nothing
    Int
    Real
    Boolean
    String
    Sum
    Intersection
    Tuple
    Function
    List
    Map
    Shape
    Symbol
    Trait
    Struct

  Type* {.inheritable, pure.} = ref object
    ## Types are considered immutable and should never be mutated once they're attached to a value. Types must also be
    ## acyclic, resembling a tree structure.
    ## The pragmas `inheritable` and `pure` omit the `m_type` pointer from each type instance. This type tag is usually
    ## used to discriminate between types at run time. However, `kind` already sufficiently distinguishes type
    ## instances. Types which themselves contain other types should be marked `acyclic`.
    kind*: Kind

  TypeParameter* = ref object
    name*: string
    lower_bound*: Type
    upper_bound*: Type
    variance*: Variance

  Variance* {.pure.} = enum
    Covariant
    Contravariant
    Invariant

  TypeVariable* {.pure, acyclic.} = ref object of Type
    ## A type variable represents the application of a type argument at the index. For example, if we have a function
    ## with two type parameters A and B, the type variable with index 1 would refer to the second type argument, whose
    ## parameter is B.
    index*: uint8

    parameter*: TypeParameter
      ## `parameter` is only defined for type variables contained in a function's input or output type. For all other
      ## kinds of type variables, `parameter` is `nil`. It is specifically used by subtyping and fit when comparing two
      ## uninstantiated input types, such as when a multi-function hierarchy is built.

  SumType* {.pure, acyclic.} = ref object of Type
    parts*: ImSeq[Type]

  IntersectionType* {.pure, acyclic.} = ref object of Type
    parts*: ImSeq[Type]

  TupleType* {.pure, acyclic.} = ref object of Type
    elements*: ImSeq[Type]

  FunctionType* {.pure, acyclic.} = ref object of Type
    input*: TupleType
    output*: Type

  ListType* {.pure, acyclic.} = ref object of Type
    element*: Type

  MapType* {.pure, acyclic.} = ref object of Type
    key*, value*: Type

  MetaShape* = ref object
    ## A meta shape is the portion of a shape type fixed at compile time. That is, the shape type's property names and
    ## the corresponding property index.
    property_names*: ImSeq[string]
      ## Property names are ordered lexicographically, in the same order as the property index prescribes.
    property_index*: PropertyIndex

  ShapeType* {.pure, acyclic.} = ref object of Type
    meta*: MetaShape
    property_types*: ImSeq[Type]

  SymbolType* {.pure.} = ref object of Type
    name*: string

  Schema* {.inheritable, pure.} = ref object
    kind*: Kind
      ## Either `Trait` or `Struct`.
    name*: string
      ## The full name of the declared schema.
    type_parameters*: ImSeq[TypeParameter]
    supertraits*: ImSeq[TraitType]
      ## A list of directly extended traits. Type variables within these trait types are uninstantiated and reference
      ## this schema's type parameters.
    representative: DeclaredType
      ## The representative type of this schema, with uninstantiated type parameters.
    has_invariant_type_parameters: bool
      ## Whether the schema has invariant type parameters, which determines the path to take for declared types during
      ## type simplification.

  DeclaredType* {.pure.} = ref object of Type
    schema*: Schema
    type_arguments*: ImSeq[Type]
      ## The declared type's type arguments.
    supertraits*: ImSeq[TraitType]
      ## The schema's direct supertraits instantiated with the given type arguments.

  TraitSchema* {.pure.} = ref object of Schema
    inherited_shape_type: ShapeType
      ## As a trait may inherit directly and indirectly from shapes, each trait has an inherited shape type. This shape
      ## type can be used to decide trait/shape subtyping. Since most values on the left side of subtyping will
      ## actually be structs, the inherited shape type shouldn't be requested often, if at all.
      ##
      ## The inherited shape type may contain declared types that are placed later in the schema resolution order.
      ## Hence, inherited shape types have to be added to a schema in a later resolution step.

  TraitType* {.pure.} = ref object of DeclaredType
    inherited_shape_type_cache: ShapeType
      ## The schema's inherited shape type instantiated with the trait's type arguments. The cache is always used, even
      ## when the shape type contains no type variables.

  StructSchema* {.pure.} = ref object of Schema
    properties*: ImSeq[StructSchemaProperty]
      ## The properties of a struct must be ordered by their name. This allows the property index to map names directly
      ## to schema properties and property values. It also allows direct property access operations to fetch a property
      ## value via its index if the value is a struct at compile time.
    property_index*: PropertyIndex
    open_property_indices*: ImSeq[uint16]
      ## A list of indices where open properties occur. This can be used to quickly access all open property types when
      ## they are specifically requested, such as during struct/struct equality or subtyping.

  StructSchemaProperty* = object
    name*: string
    tpe*: Type
    is_open*: bool
    open_index*: uint16
      ## If the property is open, `open_index` contains the index at which the property's open type will be found in
      ## a struct type's `open_property_types`.

  StructType* {.pure.} = ref object of DeclaredType
    open_property_types*: ImSeq[Type]
      ## The run-time types of the struct's open properties. This sequence is only defined if the struct type has been
      ## created with open property types. Representatives of struct schemas never define open property types, even if
      ## one of their properties is open. However, if one open property type is specified in this list, all open
      ## property types need to be specified.
    property_type_cache*: FixedSeq[Type]
      ## Caches the actual run-time types of the struct's properties. The cache is always defined, even if the schema
      ## has no type parameters or open properties.
      ##
      ## The cache cannot be filled during type creation, because property types do not follow the schema resolution
      ## order. Hence, it would be impossible to directly compute a `property_types` sequence for the representative
      ## type and "out-of-order" types created during schema resolution.

type SubstitutionMode {.pure.} = enum
  None
    ## Executes the operation without substituting any types from assignments.
  T1
    ## Executes the operation while replacing type variables in `t1` with types from assignments.
  T2
    ## Executes the operation while replacing type variables in `t2` with types from assignments.

const max_type_parameters* = 32
  ## The maximum number of type parameters that a function may have is 32. This allows us to allocate certain arrays on
  ## the stack when checking for type fit.

proc instantiate_trait_schema*(schema: TraitSchema, type_arguments: ImSeq[Type]): TraitType
proc instantiate_struct_schema*(schema: StructSchema, type_arguments: ImSeq[Type], open_property_types: ImSeq[Type]): StructType

proc are_equal*(t1: Type, t2: Type): bool
proc are_all_equal(types: open_array[Type]): bool

proc is_subtype*(t1: Type, t2: Type): bool
proc is_subtype_substitute1*(t1: Type, t2: Type, assignments: open_array[Type]): bool
proc is_subtype_substitute2*(t1: Type, t2: Type, assignments: open_array[Type]): bool
proc is_subtype_substitute(substitution_mode: SubstitutionMode, t1: Type, t2: Type, assignments: open_array[Type]): bool

proc fits*(ts1: open_array[Type], ts2: open_array[Type], parameters: ImSeq[TypeParameter]): ImSeq[Type]
proc fits_poly1*(ts1: open_array[Type], ts2: open_array[Type], parameters: ImSeq[TypeParameter]): ImSeq[Type]

proc substitute*(tpe: Type, type_arguments: open_array[Type]): Type
proc substitute*(tpe: Type, type_arguments: ImSeq[Type]): Type

proc is_polymorphic(tpe: Type): bool

proc sum_simplified*(parts: open_array[Type]): Type
proc sum_simplified*(parts: ImSeq[Type]): Type

proc intersection_simplified*(parts: open_array[Type]): Type
proc intersection_simplified*(parts: ImSeq[Type]): Type

proc `$`*(tpe: Type): string

########################################################################################################################
# Constructors.                                                                                                        #
########################################################################################################################

let
  any_type* = Type(kind: Any)
  nothing_type* = Type(kind: Nothing)
  int_type* = Type(kind: Int)
  real_type* = Type(kind: Real)
  boolean_type* = Type(kind: Boolean)
  string_type* = Type(kind: String)
  unit_type* = TupleType(kind: Kind.Tuple, elements: empty_immutable_seq[Type]())

proc variable*(index: uint8): TypeVariable = TypeVariable(index: index, parameter: nil)
proc sum*(parts: ImSeq[Type]): SumType = SumType(kind: Kind.Sum, parts: parts)
proc sum*(parts: open_array[Type]): SumType = sum(new_immutable_seq(parts))
proc intersection*(parts: ImSeq[Type]): IntersectionType = IntersectionType(kind: Kind.Intersection, parts: parts)
proc intersection*(parts: open_array[Type]): IntersectionType = intersection(new_immutable_seq(parts))
proc tpl*(elements: ImSeq[Type]): TupleType = TupleType(kind: Kind.Tuple, elements: elements)
proc tpl*(elements: open_array[Type]): TupleType = tpl(new_immutable_seq(elements))
proc function*(input: TupleType, output: Type): FunctionType = FunctionType(kind: Kind.Function, input: input, output: output)
proc function_unsafe(input: Type, output: Type): FunctionType =
  assert(input.kind == Kind.Tuple)
  FunctionType(kind: Kind.Function, input: cast[TupleType](input), output: output)
proc list*(element: Type): ListType = ListType(kind: Kind.List, element: element)
proc map*(key: Type, value: Type): MapType = MapType(kind: Kind.Map, key: key, value: value)

# TODO (vm): Intern symbol types.
proc symbol*(name: string): SymbolType = SymbolType(kind: Kind.Symbol, name: name)

# These functions are workarounds when creating arrays of types.
proc sum_as_type*(parts: open_array[Type]): Type = sum(parts)
proc intersection_as_type*(parts: open_array[Type]): Type = intersection(parts)
proc tpl_as_type*(elements: open_array[Type]): Type = tpl(elements)
proc function_as_type*(input: TupleType, output: Type): Type = function(input, output)
proc list_as_type*(element: Type): Type = list(element)

########################################################################################################################
# Type parameters.                                                                                               #
########################################################################################################################

proc lower_bound_contains*(parameter: TypeParameter, tpe: Type, assignments: open_array[Type]): bool =
  ## Whether `parameter`'s lower bound (instantiated via the given assignments) contains `type`.
  if parameter.lower_bound.kind != Kind.Nothing:
    is_subtype_substitute1(parameter.lower_bound, tpe, assignments)
  else: true

proc upper_bound_contains*(parameter: TypeParameter, tpe: Type, assignments: open_array[Type]): bool =
  ## Whether `parameter`'s upper bound (instantiated via the given assignments) contains `type`.
  if parameter.upper_bound.kind != Kind.Any:
    is_subtype_substitute2(tpe, parameter.upper_bound, assignments)
  else: true

proc bounds_contain*(parameter: TypeParameter, tpe: Type, assignments: open_array[Type]): bool =
  ## Whether `parameter`'s lower and upper bound (instantiated via the given assignments) contain `type`.
  lower_bound_contains(parameter, tpe, assignments) and upper_bound_contains(parameter, tpe, assignments)

proc as_type_arguments*(type_parameters: ImSeq[TypeParameter]): ImSeq[TypeVariable] =
  ## Creates a list of type variables that reference the given type parameters exactly.
  var type_arguments = new_immutable_seq[TypeVariable](type_parameters.len)
  for i in 0 ..< type_parameters.len:
    let parameter = type_parameters[i]
    type_arguments[i] = TypeVariable(index: uint8(i), parameter: parameter)
  type_arguments

########################################################################################################################
# Shapes.                                                                                                              #
########################################################################################################################

# TODO (vm/parallel): This should be protected by a lock.
var interned_meta_shapes = new_table[ImSeq[string], MetaShape]()

proc get_meta_shape*(property_names: ImSeq[string]): MetaShape =
  ## Creates a new meta shape from the given sorted and unique list of property names, or gets the interned version.
  var meta_shape = interned_meta_shapes.get_or_default(property_names)
  if meta_shape == nil:
    meta_shape = MetaShape(
      property_names: property_names,
      property_index: get_interned_property_index(to_open_array(property_names)),
    )
    interned_meta_shapes[property_names] = meta_shape
  meta_shape

proc get_meta_shape*(property_names: open_array[string]): MetaShape = get_meta_shape(new_immutable_seq(property_names))

proc get_meta_shape_safe*(property_names: seq[string]): MetaShape =
  ## Creates a new meta shape from the given property names, or gets the interned version. The property names do not
  ## have to be sorted or unique.
  var names = property_names
  sort(names)
  names = deduplicate(names, is_sorted = true)
  get_meta_shape(names)

proc `===`(a: MetaShape, b: MetaShape): bool {.inline.} =
  ## Checks the equality of two interned meta shapes.
  cast[pointer](a) == cast[pointer](b)

proc property_count*(meta_shape: MetaShape): int {.inline.} = meta_shape.property_names.len
proc property_count*(tpe: ShapeType): int {.inline.} = tpe.meta.property_count

proc new_shape_type*(meta_shape: MetaShape, property_types: ImSeq[Type]): ShapeType =
  ## Creates a new shape type with the given property types. The property types must have the same length and order as
  ## the meta shape's property names.
  ShapeType(
    kind: Kind.Shape,
    meta: meta_shape,
    property_types: property_types,
  )

proc new_shape_type*(meta_shape: MetaShape, property_types: open_array[Type]): ShapeType =
  ## Creates a new shape type with the given property types. The property types must have the same length and order as
  ## the meta shape's property names.
  new_shape_type(meta_shape, new_immutable_seq(property_types))

proc copy_shape_type*(tpe: ShapeType): ShapeType =
  let property_types = new_immutable_seq(tpe.property_types)
  new_shape_type(tpe.meta, property_types)

proc shape_as_type*(meta_shape: MetaShape, property_types: open_array[Type]): Type = new_shape_type(meta_shape, property_types)

proc get_property_type*(tpe: ShapeType, name: string): Type =
  ## Gets the type of the property `name`. The name must be a valid property name for the given shape type.
  tpe.property_types[tpe.meta.property_index.find_offset(name)]

proc get_property_type_if_exists*(tpe: ShapeType, name: string): Type =
  ## Gets the type of the property `name`. If the property does not exist, this function returns `nil`.
  let offset = tpe.meta.property_index.find_offset_if_exists(name)
  if offset >= 0:
    tpe.property_types[offset]
  else:
    nil

########################################################################################################################
# Declared types.                                                                                                      #
########################################################################################################################

proc find_first_supertrait(tpe: DeclaredType, supertrait_schema: TraitSchema): TraitType
proc find_combined_supertrait(tpe: DeclaredType, supertrait_schema: TraitSchema): TraitType

proc `===`*(a: Schema, b: Schema): bool {.inline.} = cast[pointer](a) == cast[pointer](b)
proc `!==`*(a: Schema, b: Schema): bool {.inline.} = not (a === b)

proc is_constant*(schema: Schema): bool = schema.type_parameters.len == 0

proc get_representative*(schema: Schema): DeclaredType {.inline.} =
  ## Correctly types the representative of the schema based on the schema's Nim type.
  schema.representative
proc get_representative*(schema: TraitSchema): TraitType {.inline.} = cast[TraitType](schema.representative)
proc get_representative*(schema: StructSchema): StructType {.inline.} = cast[StructType](schema.representative)

proc get_schema*(tpe: DeclaredType): Schema {.inline.} =
  ## Correctly types the schema of the type based on the type's Nim type.
  tpe.schema
proc get_schema*(tpe: TraitType): TraitSchema {.inline.} = cast[TraitSchema](tpe.schema)
proc get_schema*(tpe: StructType): StructSchema {.inline.} = cast[StructSchema](tpe.schema)

proc initialize(
  schema: Schema,
  kind: Kind,
  name: string,
  type_parameters: ImSeq[TypeParameter],
  supertraits: ImSeq[TraitType],
  representative: DeclaredType,
) =
  ## Initializes common properties of the given schema.
  schema.kind = kind
  schema.name = name
  schema.type_parameters = type_parameters
  schema.supertraits = supertraits
  schema.representative = representative
  schema.has_invariant_type_parameters = type_parameters.any_it(it.variance == Variance.Invariant)

proc instantiate_schema*(schema: Schema, type_arguments: ImSeq[Type]): DeclaredType =
  ## Instantiates `schema` with the given type arguments. This is a convenience function that will delegate to the
  ## specific instantiation function. Struct open properties will be `nil`, so this function shouldn't be used with
  ## struct types that describe concrete struct values.
  ##
  ## If `schema` cannot be instantiated with the type arguments, `nil` is returned.
  if schema.kind == Kind.Trait:
    instantiate_trait_schema(cast[TraitSchema](schema), type_arguments)
  else:
    instantiate_struct_schema(cast[StructSchema](schema), type_arguments, nil)

template require_instantiated_schema[T](instantiate_type: T): T =
  let tpe = instantiate_type
  if tpe == nil:
    quit(fmt"Cannot instantiate schema {schema.name}: the type arguments {type_arguments} don't adhere to their bounds.")
  tpe

proc force_instantiate_schema*(schema: Schema, type_arguments: ImSeq[Type]): DeclaredType =
  require_instantiated_schema(instantiate_schema(schema, type_arguments))

proc bounds_contain(schema: Schema, type_arguments: ImSeq[Type]): bool =
  ## Whether the given type arguments fit into the schema's parameter bounds. Upper bounds for covariance and lower
  ## bounds for contravariance must be guaranteed by the compiler, but we need to check lower/upper bounds for
  ## covariant/contravariant type parameters.
  for i in 0 ..< schema.type_parameters.len:
    let parameter = schema.type_parameters[i]
    let argument = type_arguments[i]
    if parameter.variance == Variance.Covariant:
      if not lower_bound_contains(parameter, argument, to_open_array(type_arguments)):
        return false
    elif parameter.variance == Variance.Contravariant:
      if not upper_bound_contains(parameter, argument, to_open_array(type_arguments)):
        return false
  true

proc instantiate_supertraits(schema: Schema, type_arguments: ImSeq[Type]): ImSeq[TraitType] =
  if schema.is_constant or schema.supertraits.len == 0:
    return schema.supertraits

  var instantiated = new_immutable_seq[TraitType](schema.supertraits.len)
  for i in 0 ..< schema.supertraits.len:
    instantiated[i] = cast[TraitType](substitute(schema.supertraits[i], to_open_array(type_arguments)))
  instantiated

proc is_declared_type(tpe: Type): bool = tpe.kind == Kind.Trait or tpe.kind == Kind.Struct

template combine_type_arguments(
  schema: Schema,
  declared_types,
  handle_covariant: untyped,
  handle_contravariant: untyped,
  handle_invariant: untyped,
): ImSeq[Type] =
  ## Combines the type arguments of the given declared types using one of the `handle_*` blocks depending on variance.
  ## All handlers receive an injected variable `type_argument_candidates`, which is a REUSED `ImSeq[Type]`. Returns
  ## `nil` if the type arguments cannot be combined.
  if declared_types.len == 0: return nil
  if declared_types.len == 1: return declared_types[0]

  let type_parameters = schema.type_parameters
  var combined_type_arguments = new_immutable_seq[Type](type_parameters.len)

  # `argument_candidates` is reused for all iterations to save allocations.
  var type_argument_candidates {.inject.} = new_immutable_seq[Type](declared_types.len)
  for i in 0 ..< type_parameters.len:
    let type_parameter = type_parameters[i]
    for j in 0 ..< declared_types.len:
      type_argument_candidates[j] = declared_types[j].type_arguments[i]

    case type_parameter.variance
    of Variance.Covariant:
      combined_type_arguments[i] = handle_covariant
    of Variance.Contravariant:
      combined_type_arguments[i] = handle_contravariant
    of Variance.Invariant:
      combined_type_arguments[i] = handle_invariant

  combined_type_arguments

# TODO: The current implementation of `find_supertrait` essentially scans the whole supertrait hierarchy. Is there any
#       way in which we could improve performance?
#          - Idea 1: Keep a flat hash map of Schema -> Type in d1's schema, with uninstantiated type
#                    arguments. It contains a transitive closure of all supertypes. We can then quickly get
#                    the correct type with the given schema, and instantiate it as i1, then check if i1 is
#                    a subtype of t2. (The advantage is further that we probably don't need the more
#                    complicated algorithm for when `hasMultipleParameterizedInheritance` is true. We can
#                    combine these types at compile time when the Schema -> Type map is generated.)
#                      - The reverse (having a subtype map in the supertrait) would not work because there
#                        is no straight-forward way to handle type arguments directly.
#          - Idea 2: Idea 1, but build the hash map slowly as a cache. This would require us to implement
#                    all relevant algorithms (including the one for `hasMultipleParameterizedInheritance`),
#                    but might save memory since not all subtype/supertype combinations will likely be
#                    checked. For example, it is unlikely, albeit possible, that d1 is even a trait. So most
#                    of the caching will happen in structs. If we have a struct Fox <: (Mammal <: (Animal
#                    <: Hashable)) but we never check Fox <: Mammal and neither Fox <: Hashable, the cache
#                    of Fox will only have one entry `Animal<schema> -> Animal<representative>`.
#       The big downside here is memory. Suppose we have a type hierarchy where T1 has 10 map entries and T2
#       has 12 map entries. A type T3 that extends both T1 and T2 will have 10 + 12 + 2 = 24 map entries,
#       unless T1 and T2 share common supertraits.
#       Note that such a caching mechanism will require us to traverse the SCHEMA'S supertrait hierarchy
#       and instantiate type parameters as the last step. This might even render DeclaredType.supertraits
#       obsolete, which would save a lot of memory. (Well, the first layer would be to get `type.schema.supertraits`,
#       but from that point on, we need each trait TYPE's supertraits, so this would require substituting.
#       However, for each subtype/supertype combination, this substitution would only happen once and only
#       the result would be saved in the transitive supertype cache.)

proc find_supertrait(tpe: DeclaredType, supertrait_schema: TraitSchema): TraitType =
  ## Finds the supertrait with the given schema that `tpe` inherits from or `tpe` itself. If no such supertrait exists,
  ## the function returns `nil`. The algorithm combines all occurrences of the supertrait if `tpe` inherits from it
  ## multiple times.
  if tpe.schema === supertrait_schema:
    return cast[TraitType](tpe)

  # TODO (vm/schemas): Implement `has_multiple_parameterized_inheritance`.
  # TODO (vm): We can make `has_multiple_parameterized_inheritance` more fine-grained by listing the schemas which
  #            occur multiple times in the supertrait hierarchy. This can be a simple pointer hash map. This is
  #            important for the immediate performance impact of adding an inheritance relationship to a type that
  #            would flip a whole supertype hierarchy to "on". ANY supertraits with type parameters would be negatively
  #            affected by this simple change if `has_multiple_parameterized_inheritance` doesn't differentiate between
  #            schemas.
  #if supertrait_schema.is_constant or not tpe.schema.has_multiple_parameterized_inheritance:
  if supertrait_schema.is_constant:
    find_first_supertrait(tpe, supertrait_schema)
  else:
    find_combined_supertrait(tpe, supertrait_schema)

iterator find_supertrait_candidates(tpe: DeclaredType, supertrait_schema: TraitSchema): TraitType =
  ## Yields all supertraits in `tpe` which have the given schema.
  var queue: StackSeq[32, TraitType]
  for supertrait in tpe.supertraits:
    queue.add(supertrait)

  while queue.len > 0:
    let candidate = queue.pop()
    if candidate.schema === supertrait_schema:
      yield candidate
    for supertrait in candidate.supertraits:
      queue.add(supertrait)

proc find_first_supertrait(tpe: DeclaredType, supertrait_schema: TraitSchema): TraitType =
  ## Finds the first occurrence of `supertrait_schema` in `tpe`'s supertraits.
  for candidate in find_supertrait_candidates(tpe, supertrait_schema):
    return candidate
  nil

proc find_combined_supertrait(tpe: DeclaredType, supertrait_schema: TraitSchema): TraitType =
  ## Finds all occurrences of `supertrait_schema` in `tpe`'s supertraits and combines them. Returns `nil` if the
  ## supertraits cannot be combined.
  var candidates: StackSeq[8, TraitType]
  for candidate in find_supertrait_candidates(tpe, supertrait_schema):
    candidates.add(candidate)

  let type_arguments = combine_type_arguments(
    supertrait_schema,
    candidates,
    intersection_simplified(type_argument_candidates),
    sum_simplified(type_argument_candidates),
    block:
      # This is a tricky one. The compiler MUST guarantee that invariant type arguments are equal across a supertype
      # hierarchy. Hence, we can assume that all types in `type_argument_candidates` are equal. We normally wouldn't
      # even have to collect all `type_argument_candidates` for this, but I want to keep the assertion for now.
      if not are_all_equal(type_argument_candidates.to_open_array):
        quit(fmt"The declared type {tpe.schema.name} has supertraits {supertrait_schema.name} which have conflicting invariant type arguments.")
      type_argument_candidates[0]
  )
  supertrait_schema.instantiate_trait_schema(type_arguments)

########################################################################################################################
# Traits.                                                                                                              #
########################################################################################################################

proc new_trait_schema*(
  name: string,
  type_parameters: ImSeq[TypeParameter],
  supertraits: ImSeq[TraitType],
): TraitSchema =
  let schema = TraitSchema(
    # Inherited shape types are resolved in a second step.
    inherited_shape_type: nil,
  )
  let type_arguments = cast[ImSeq[Type]](type_parameters.as_type_arguments())
  let representative = TraitType(kind: Kind.Trait, schema: schema, type_arguments: type_arguments, supertraits: supertraits)
  schema.initialize(Kind.Trait, name, type_parameters, supertraits, representative)
  schema

proc attach_inherited_shape_type*(schema: TraitSchema, inherited_shape_type: ShapeType) =
  if schema.inherited_shape_type != nil:
    quit(fmt"An inherited shape type has already been attached to trait schema {schema.name}.")

  schema.inherited_shape_type = inherited_shape_type

proc instantiate_trait_schema*(schema: TraitSchema, type_arguments: ImSeq[Type]): TraitType =
  ## Instantiates `schema` with the given type arguments. If `schema` cannot be instantiated with the type arguments,
  ## `nil` is returned.
  # TODO (vm/intern): This function should intern the declared types.
  if schema.is_constant:
    return schema.get_representative

  if not bounds_contain(schema, type_arguments):
    return nil

  let supertraits = instantiate_supertraits(schema, type_arguments)
  TraitType(
    kind: Kind.Trait,
    schema: schema,
    type_arguments: type_arguments,
    supertraits: supertraits,
    inherited_shape_type_cache: nil
  )

proc force_instantiate_trait_schema*(schema: TraitSchema, type_arguments: ImSeq[Type]): TraitType =
  require_instantiated_schema(instantiate_trait_schema(schema, type_arguments))

proc get_inherited_shape_type*(tpe: TraitType): ShapeType =
  if tpe.inherited_shape_type_cache != nil:
    return tpe.inherited_shape_type_cache

  let schema: TraitSchema = tpe.get_schema
  let shape_type =
    if schema.is_constant:
      schema.inherited_shape_type
    else:
      # The substitution will only create a new type if the shape type is polymorphic. This wastes no memory if the
      # inherited shape type isn't polymorphic itself.
      cast[ShapeType](schema.inherited_shape_type.substitute(tpe.type_arguments))
  tpe.inherited_shape_type_cache = shape_type
  shape_type

########################################################################################################################
# Structs.                                                                                                             #
########################################################################################################################

proc new_struct_type(schema: StructSchema, type_arguments: ImSeq[Type], supertraits: ImSeq[TraitType], open_property_types: ImSeq[Type]): StructType

proc property_count*(schema: StructSchema): int {.inline.} = schema.properties.len
proc open_property_count*(schema: StructSchema): int {.inline.} = schema.open_property_indices.len
proc has_open_properties*(schema: StructSchema): bool {.inline.} = schema.open_property_count > 0

proc new_struct_schema*(
  name: string,
  type_parameters: ImSeq[TypeParameter],
  supertraits: ImSeq[TraitType],
  properties: ImSeq[StructSchemaProperty],
): StructSchema =
  ## Creates a new struct schema from the given arguments. The properties must be ordered lexicographically by their
  ## name. Property types must be `nil`, as they are resolved in a second step.
  var property_names: seq[string] = @[]
  var open_property_indices_accumulator: seq[uint16] = @[]
  for i in 0 ..< properties.len:
    let property = properties[i]
    property_names.add(property.name)
    if property.is_open:
      open_property_indices_accumulator.add(uint16(i))

  let property_index = get_interned_property_index(property_names)
  let open_property_indices = new_immutable_seq(open_property_indices_accumulator)
  let schema = StructSchema(
    properties: properties,
    property_index: property_index,
    open_property_indices: open_property_indices,
  )
  let type_arguments = cast[ImSeq[Type]](type_parameters.as_type_arguments())
  let representative = new_struct_type(schema, type_arguments, supertraits, nil)
  schema.initialize(Kind.Struct, name, type_parameters, supertraits, representative)
  schema

proc attach_property_type*(schema: StructSchema, property_name: string, property_type: Type) =
  let offset = schema.property_index.find_offset_if_exists(property_name)
  if offset < 0:
    quit(fmt"The struct schema {schema.name} has no property `{property_name}`.")

  var property = schema.properties[offset]
  property.tpe = property_type
  schema.properties[offset] = property

proc instantiate_struct_schema*(
  schema: StructSchema,
  type_arguments: ImSeq[Type],
  open_property_types: ImSeq[Type],
): StructType =
  ## Instantiates `schema` with the given type arguments and open property types. `open_property_types` must be `nil`
  ## if the struct type should have no concrete open property types. If `schema` cannot be instantiated with the type
  ## arguments, `nil` is returned.
  # TODO (vm/intern): This function should intern the declared types.
  if schema.is_constant and open_property_types == nil:
    return schema.get_representative

  if not bounds_contain(schema, type_arguments):
    return nil

  let supertraits = instantiate_supertraits(schema, type_arguments)
  new_struct_type(schema, type_arguments, supertraits, open_property_types)

proc force_instantiate_struct_schema*(schema: StructSchema, type_arguments: ImSeq[Type], open_property_types: ImSeq[Type]): StructType =
  require_instantiated_schema(instantiate_struct_schema(schema, type_arguments, open_property_types))

proc new_struct_type(
  schema: StructSchema,
  type_arguments: ImSeq[Type],
  supertraits: ImSeq[TraitType],
  open_property_types: ImSeq[Type],
): StructType =
  StructType(
    kind: Kind.Struct,
    schema: schema,
    type_arguments: type_arguments,
    supertraits: supertraits,
    open_property_types: open_property_types,
    property_type_cache: new_immutable_seq[Type](schema.property_count),
  )

proc get_property_type*(tpe: StructType, property_offset: uint16): Type =
  ## Returns the type of the property at the given offset. The property type is either an open property type or the
  ## schema's property type instantiated with the struct's type arguments. All results are cached in the struct type's
  ## `property_type_cache`.
  let cached_candidate = tpe.property_type_cache[property_offset]
  if cached_candidate != nil:
    return cached_candidate

  let schema = tpe.get_schema
  let property = schema.properties[property_offset]
  let candidate_type =
    if property.is_open and tpe.open_property_types != nil:
      tpe.open_property_types[property.open_index]
    elif schema.is_constant:
      property.tpe
    else:
      substitute(property.tpe, tpe.type_arguments)

  tpe.property_type_cache[property_offset] = candidate_type
  candidate_type

proc get_property_type*(tpe: StructType, name: string): Type {.inline.} =
  ## Returns the type of a property `name`, which must be a valid property name.
  tpe.get_property_type(tpe.get_schema.property_index.find_offset(name))

proc get_property_type_if_exists*(tpe: StructType, name: string): Type {.inline.} =
  ## Returns the type of a property `name`, or `nil` if it doesn't exist.
  let offset = tpe.get_schema.property_index.find_offset_if_exists(name)
  if offset >= 0:
    tpe.get_property_type(uint16(offset))
  else:
    nil

########################################################################################################################
# Type equality.                                                                                                       #
########################################################################################################################

proc `===`(a: Type, b: Type): bool {.inline.} =
  ## Checks the referential equality of the two types.
  cast[pointer](a) == cast[pointer](b)

proc has_equal_in(ts1: ImSeq[Type], ts2: ImSeq[Type]): bool
proc are_exactly_equal(ts1: ImSeq[Type], ts2: ImSeq[Type]): bool
proc are_declared_types_equal(t1: DeclaredType, t2: DeclaredType): bool
proc are_open_property_types_equal(t1: StructType, t2: StructType): bool

proc are_equal*(t1: Type, t2: Type): bool =
  # If the two types are referentially equal, they are obviously the same.
  if t1 === t2:
    return true

  if t1.kind != t2.kind:
    return false

  case t1.kind
  # Type variables can only be referentially equal.
  of Kind.TypeVariable: false
  of Kind.Any: true
  of Kind.Nothing: true
  of Kind.Real: true
  of Kind.Int: true
  of Kind.Boolean: true
  of Kind.String: true

  of Kind.Sum:
    let s1 = cast[SumType](t1)
    let s2 = cast[SumType](t2)
    has_equal_in(s1.parts, s2.parts) and has_equal_in(s2.parts, s1.parts)

  of Kind.Intersection:
    let i1 = cast[IntersectionType](t1)
    let i2 = cast[IntersectionType](t2)
    has_equal_in(i1.parts, i2.parts) and has_equal_in(i2.parts, i1.parts)

  of Kind.Tuple:
    let tp1 = cast[TupleType](t1)
    let tp2 = cast[TupleType](t2)
    are_exactly_equal(tp1.elements, tp2.elements)

  of Kind.Function:
    let f1 = cast[FunctionType](t1)
    let f2 = cast[FunctionType](t2)
    are_equal(f1.input, f2.input) and are_equal(f1.output, f2.output)

  of Kind.List:
    let l1 = cast[ListType](t1)
    let l2 = cast[ListType](t2)
    are_equal(l1.element, l2.element)

  of Kind.Map:
    let m1 = cast[MapType](t1)
    let m2 = cast[MapType](t2)
    are_equal(m1.key, m2.key) and are_equal(m1.value, m2.value)

  of Kind.Shape:
    let s1 = cast[ShapeType](t1)
    let s2 = cast[ShapeType](t2)
    if s1.meta === s2.meta:
      are_exactly_equal(s1.property_types, s2.property_types)
    else:
      false

  of Kind.Symbol:
    let s1 = cast[SymbolType](t1)
    let s2 = cast[SymbolType](t2)
    s1.name == s2.name

  of Kind.Trait:
    let t1 = cast[TraitType](t1)
    let t2 = cast[TraitType](t2)
    are_declared_types_equal(t1, t2)

  of Kind.Struct:
    # Structs are only equal if all of their open property types are equal. This is especially crucial for the dispatch
    # cache, where a single different open property type may change the target function.
    let t1 = cast[StructType](t1)
    let t2 = cast[StructType](t2)
    are_declared_types_equal(t1, t2) and are_open_property_types_equal(t1, t2)

macro are_equal_substitute_static(
  substitution_mode: static[SubstitutionMode],
  t1: Type,
  t2: Type,
  assignments: open_array[Type],
): bool =
  ## Checks whether `t1` and `t2` are equal types, if the `assignments` are substituted into either `t1`, `t2`, or
  ## neither based on the `substitution_mode`.
  # TODO (vm): We can optimize this similar to the implementation of `is_subtype` to avoid allocations and type tree
  #            iterations.
  case substitution_mode
  of SubstitutionMode.None:
    quote do: are_equal(`t1`, `t2`)
  of SubstitutionMode.T1:
    quote do: are_equal(substitute(`t1`, `assignments`), `t2`)
  of SubstitutionMode.T2:
    quote do: are_equal(`t1`, substitute(`t2`, `assignments`))

proc are_equal_substitute(
  substitution_mode: SubstitutionMode,
  t1: Type,
  t2: Type,
  assignments: open_array[Type],
): bool =
  ## Checks whether `t1` and `t2` are equal types, if the `assignments` are substituted into either `t1`, `t2`, or
  ## neither based on the `substitution_mode`.
  # TODO (vm): We can optimize this similar to the implementation of `is_subtype` to avoid allocations and type tree
  #            iterations.
  case substitution_mode
  of SubstitutionMode.None: are_equal(t1, t2)
  of SubstitutionMode.T1: are_equal(substitute(t1, assignments), t2)
  of SubstitutionMode.T2: are_equal(t1, substitute(t2, assignments))

proc are_all_equal(types: open_array[Type]): bool =
  ## Checks whether the types in the given open array are all equal, i.e. t1 == t2 == t3 == ..., and so on.
  for i in 0 ..< types.len - 1:
    if not are_equal(types[i], types[i + 1]):
      return false
  true

proc has_equal_in(ts1: ImSeq[Type], ts2: ImSeq[Type]): bool =
  for t1 in ts1:
    var found = false
    for t2 in ts2:
      if are_equal(t1, t2):
        found = true
    if not found: return false
  true

proc are_exactly_equal(ts1: ImSeq[Type], ts2: ImSeq[Type]): bool =
  if ts1.len != ts2.len: return false
  for i in 0 ..< ts1.len:
    if not are_equal(ts1[i], ts2[i]):
      return false
  true

proc are_declared_types_equal(t1: DeclaredType, t2: DeclaredType): bool =
  ## Checks whether the two given types are equal from the lens of declared types. This function does not take trait or
  ## struct specifics into account, which have to be checked in addition.
  if t1.schema !== t2.schema:
    return false

  if t1.schema.is_constant:
    # The common schema is constant and thus we don't have to check the type arguments.
    return true

  for i in 0 ..< t1.type_arguments.len:
    if not are_equal(t1.type_arguments[i], t2.type_arguments[i]):
      return false
  true

proc are_open_property_types_equal(t1: StructType, t2: StructType): bool =
  ## Checks whether `t1`'s open property types are equal to `t2`'s open property types. The struct types must have the
  ## same schema.
  if t1.open_property_types == nil and t2.open_property_types == nil:
    true
  elif t1.open_property_types != nil and t2.open_property_types != nil:
    for i in 0 ..< t1.open_property_types.len:
      if not are_equal(t1.open_property_types[i], t2.open_property_types[i]):
        return false
    true
  else:
    # One `open_property_types` is nil.
    for i in t1.get_schema.open_property_indices:
      let p1 = t1.get_property_type(i)
      let p2 = t2.get_property_type(i)
      if not are_equal(p1, p2):
        return false
    true

########################################################################################################################
# Subtyping.                                                                                                           #
########################################################################################################################

# TODO (vm/schemas): Rename to `is_subtype_substitute_static` to be in line with the naming convention.
macro is_subtype_rec(substitution_mode: static[SubstitutionMode], t1: Type, t2: Type): bool =
  case substitution_mode
  of SubstitutionMode.None:
    quote do: is_subtype(`t1`, `t2`)
  of SubstitutionMode.T1:
    quote do: is_subtype_substitute1(`t1`, `t2`, assignments)
  of SubstitutionMode.T2:
    quote do: is_subtype_substitute2(`t1`, `t2`, assignments)

macro variable_subtypes_type(substitution_mode: static[SubstitutionMode], tv1: TypeVariable, t2: Type): bool =
  case substitution_mode
  of SubstitutionMode.None, SubstitutionMode.T2:
    quote do:
      let tv1_evaluated = `tv1`
      assert(tv1_evaluated.parameter != nil)
      is_subtype(tv1_evaluated.parameter.upper_bound, `t2`)
  of SubstitutionMode.T1:
    quote do:
      let tv1_evaluated = `tv1`
      let t1 = assignments[tv1_evaluated.index]
      is_subtype_substitute1(t1, `t2`, assignments)

macro type_subtypes_variable(substitution_mode: static[SubstitutionMode], t1: Type, tv2: TypeVariable): bool =
  case substitution_mode
  of SubstitutionMode.None, SubstitutionMode.T1:
    quote do:
      let tv2_evaluated = `tv2`
      assert(tv2_evaluated.parameter != nil)
      is_subtype(`t1`, tv2_evaluated.parameter.lower_bound)
  of SubstitutionMode.T2:
    quote do:
      let tv2_evaluated = `tv2`
      let t2 = assignments[tv2_evaluated.index]
      is_subtype_substitute2(`t1`, t2, assignments)

template type_subtypes_sum(substitution_mode: SubstitutionMode, t1: Type, s2: SumType): bool =
  for p2 in s2.parts:
    if is_subtype_rec(substitution_mode, t1, p2):
      return true
  false

template sum_subtypes_sum(substitution_mode: SubstitutionMode, s1: SumType, s2: SumType): bool =
  for p1 in s1.parts:
    if not type_subtypes_sum(substitution_mode, p1, s2):
      return false
  true

template sum_subtypes_type(substitution_mode: SubstitutionMode, s1: SumType, t2: Type): bool =
  for p1 in s1.parts:
    if not is_subtype_rec(substitution_mode, p1, t2):
      return false
  true

template intersection_subtypes_type(substitution_mode: SubstitutionMode, i1: IntersectionType, t2: Type): bool =
  for p1 in i1.parts:
    if is_subtype_rec(substitution_mode, p1, t2):
      return true
  false

template intersection_subtypes_intersection(substitution_mode: SubstitutionMode, i1: IntersectionType, i2: IntersectionType): bool =
  for p2 in i2.parts:
    if not intersection_subtypes_type(substitution_mode, i1, p2):
      return false
  true

template type_subtypes_intersection(substitution_mode: SubstitutionMode, t1: Type, i2: IntersectionType): bool =
  for p2 in i2.parts:
    if not is_subtype_rec(substitution_mode, t1, p2):
      return false
  true

template tuple_subtypes_tuple(substitution_mode: SubstitutionMode, t1: TupleType, t2: TupleType): bool =
  let es1 = t1.elements
  let es2 = t2.elements
  if es1.len != es2.len:
    return false

  for i in 0 ..< es1.len:
    if not is_subtype_rec(substitution_mode, es1[i], es2[i]):
      return false
  true

proc shape_subtypes_shape(
  substitution_mode: SubstitutionMode,
  s1: ShapeType,
  s2: ShapeType,
  assignments: open_array[Type],
): bool =
  ## This is a `proc` to avoid blowing up the code size of `is_subtype`.
  if s1.meta === s2.meta:
    for i in 0 ..< s2.property_count:
      if not is_subtype_substitute(substitution_mode, s1.property_types[i], s2.property_types[i], assignments):
        return false
  else:
    for i in 0 ..< s2.property_count:
      let property_name = s2.meta.property_names[i]
      let p1_type = s1.get_property_type_if_exists(property_name)
      if p1_type == nil:
        return false
      let p2_type = s2.property_types[i]
      if not is_subtype_substitute(substitution_mode, p1_type, p2_type, assignments):
        return false
  true

proc type_arguments_subtype_type_arguments(
  substitution_mode: SubstitutionMode,
  t1: DeclaredType,
  t2: DeclaredType,
  assignments: open_array[Type],
): bool =
  ## Whether `t1`'s type arguments subtype `t2`'s type arguments. Both types must have the same schema.
  ##
  ## This is a `proc` to avoid blowing up the code size of `is_subtype`.
  if t1.schema.is_constant:
    return true

  let type_parameters = t1.schema.type_parameters
  for i in 0 ..< type_parameters.len:
    let type_parameter = type_parameters[i]
    let a1 = t1.type_arguments[i]
    let a2 = t2.type_arguments[i]

    case type_parameter.variance:
    of Variance.Covariant:
      if not is_subtype_substitute(substitution_mode, a1, a2, assignments):
        return false
    of Variance.Contravariant:
      if not is_subtype_substitute(substitution_mode, a2, a1, assignments):
        return false
    of Variance.Invariant:
      if not are_equal_substitute(substitution_mode, a1, a2, assignments):
        return false

  true

template declared_type_subtypes_trait(substitution_mode: SubstitutionMode, t1: DeclaredType, t2: TraitType): bool =
  if t1.schema === t2.schema:
    type_arguments_subtype_type_arguments(substitution_mode, t1, t2, assignments)
  else:
    let supertrait = t1.find_supertrait(t2.get_schema)
    supertrait != nil and is_subtype_rec(substitution_mode, supertrait, t2)

template struct_subtypes_shape(substitution_mode: SubstitutionMode, t1: StructType, t2: ShapeType): bool =
  for i in 0 ..< t2.property_count:
    # TODO (vm/schemas): Does the string from `property_names` get copied when the function is called? Check out
    #                    whether an allocation is hiding here.
    let property_name = t2.meta.property_names[i]
    let p1 = t1.get_property_type_if_exists(property_name)
    if p1 == nil or not is_subtype_rec(substitution_mode, p1, t2.property_types[i]):
      return false
  true

template struct_subtypes_struct(substitution_mode: SubstitutionMode, t1: StructType, t2: StructType): bool =
  ## Checks whether `t1` is a subtype of `t2` by comparing type arguments and open property types.
  if t1.schema === t2.schema and type_arguments_subtype_type_arguments(substitution_mode, t1, t2, assignments):
    # If the open property types of t2 are empty, and t1 and t2 agree in their type arguments, t2 will always be a
    # supertype of t1. Each property type of t2 is as general as possible.
    if t2.open_property_types == nil:
      return true

    for i in t1.get_schema.open_property_indices:
      let p1 = t1.get_property_type(i)
      let p2 = t2.get_property_type(i)
      if not is_subtype_rec(substitution_mode, p1, p2):
        return false
    true
  else:
    false

template is_subtype_impl(substitution_mode: SubstitutionMode, t1: Type, t2: Type): bool =
  ## `is_subtype` has two separate versions, both covered by this implementation template: the first being the regular
  ## one, while the second one implicitly substitutes type variables in `t2` with types from an `assignments` array,
  ## without allocating any new types. This allows subtyping in specific contexts (fit, type bounds checking) to be
  ## decided without new allocations. This template assumes an implicit `assignments` variable to be in scope when
  ## `has_assignments` is true.
  ##
  ## If a substitution mode is set, `assignments` must NOT contain any type variables. This is because the algorithm
  ## can't differentiate between type variables that should still be substituted and type variables that have come from
  ## `assignments`, presumably from a different context. For example, when building the dispatch hierarchy, the
  ## left-hand input type `t1` may contain type variables. In such a case, the assignments should be manually
  ## substituted into `t1´ and `is_subtype` should be used without a substitution mode.

  # Because basic types are interned, this case trivially covers all basic types without subtyping interactions.
  if t1 === t2:
    return true

  case t1.kind
  of Kind.TypeVariable:
    let tv1 = cast[TypeVariable](t1)
    return variable_subtypes_type(substitution_mode, tv1, t2)

  of Kind.Nothing:
    return true

  of Kind.Sum:
    let s1 = cast[SumType](t1)
    if t2.kind == Kind.Sum:
      let s2 = cast[SumType](t2)
      return sum_subtypes_sum(substitution_mode, s1, s2)
    else:
      if sum_subtypes_type(substitution_mode, s1, t2):
        return true

  of Kind.Intersection:
    let i1 = cast[IntersectionType](t1)
    if t2.kind == Kind.Intersection:
      let i2 = cast[IntersectionType](t2)
      return intersection_subtypes_intersection(substitution_mode, i1, i2)
    else:
      if intersection_subtypes_type(substitution_mode, i1, t2):
        return true

  of Kind.Tuple:
    if t2.kind == Kind.Tuple:
      return tuple_subtypes_tuple(substitution_mode, cast[TupleType](t1), cast[TupleType](t2))

  of Kind.Function:
    if t2.kind == Kind.Function:
      let f1 = cast[FunctionType](t1)
      let f2 = cast[FunctionType](t2)
      return is_subtype_rec(substitution_mode, f2.input, f1.input) and is_subtype_rec(substitution_mode, f1.output, f2.output)

  of Kind.List:
    if t2.kind == Kind.List:
      let l1 = cast[ListType](t1)
      let l2 = cast[ListType](t2)
      return is_subtype_rec(substitution_mode, l1.element, l2.element)

  of Kind.Map:
    if t2.kind == Kind.Map:
      let m1 = cast[MapType](t1)
      let m2 = cast[MapType](t2)
      # TODO (vm): Variance for maps?
      # TODO (vm): If `substitution_mode` is not None, we should either substitute here or implement the same for `are_equal`.
      return are_equal(m1.key, m2.key) and are_equal(m1.value, m2.value)

  of Kind.Shape:
    if t2.kind == Kind.Shape:
      return shape_subtypes_shape(substitution_mode, cast[ShapeType](t1), cast[ShapeType](t2), assignments)

  # TODO (vm): This case can be removed if symbol types are interned.
  of Kind.Symbol:
    if t2.kind == Kind.Symbol:
      let s1 = cast[SymbolType](t1)
      let s2 = cast[SymbolType](t2)
      return s1.name == s2.name

  of Kind.Trait, Kind.Struct:
    if t2.kind == Kind.Trait:
      return declared_type_subtypes_trait(substitution_mode, cast[DeclaredType](t1), cast[TraitType](t2))
    elif t2.kind == Kind.Shape:
      let s2 = cast[ShapeType](t2)
      if t1.kind == Kind.Struct:
        return struct_subtypes_shape(substitution_mode, cast[StructType](t1), s2)
      else: # t1.kind == Kind.Trait
        let tt1 = cast[TraitType](t1)
        return shape_subtypes_shape(substitution_mode, tt1.get_inherited_shape_type, s2, assignments)
    elif t2.kind == Kind.Struct and t1.kind == Kind.Struct:
      return struct_subtypes_struct(substitution_mode, cast[StructType](t1), cast[StructType](t2))

  else: discard

  case t2.kind
  of Kind.TypeVariable:
    let tv2 = cast[TypeVariable](t2)
    type_subtypes_variable(substitution_mode, t1, tv2)

  of Kind.Any:
    true

  of Kind.Sum:
    # t1 is definitely NOT a sum type, because the case SumType/SumType immediately returns in the first `case of`
    # statement above. Hence, we can safely call `type_subtypes_sum`.
    type_subtypes_sum(substitution_mode, t1, cast[SumType](t2))

  of Kind.Intersection:
    # t1 is definitely NOT an intersection type, because the case IntersectionType/IntersectionType immediately returns
    # in the first `cast of` statement above. Hence, we can safely call `type_subtypes_intersection`.
    type_subtypes_intersection(substitution_mode, t1, cast[IntersectionType](t2))

  else: false

proc is_subtype*(t1: Type, t2: Type): bool =
  ## Whether `t1` is a subtype of `t2`.
  # `assignments` is only defined to make `is_subtype_impl` compile. Assignments should never be accessed in this
  # substitution mode.
  let assignments: array[0, Type] = []
  is_subtype_impl(SubstitutionMode.None, t1, t2)

proc is_subtype_substitute1*(t1: Type, t2: Type, assignments: open_array[Type]): bool =
  ## Whether `t1` is a subtype of `t2` when type variables in `t1` are substituted with types from `assignments`. This
  ## function does *not* allocate new types for the substitution. As noted in `is_subtype_impl`, `assignments` may not
  ## contain any type variables.
  is_subtype_impl(SubstitutionMode.T1, t1, t2)

proc is_subtype_substitute2*(t1: Type, t2: Type, assignments: open_array[Type]): bool =
  ## Whether `t1` is a subtype of `t2` when type variables in `t2` are substituted with types from `assignments`. This
  ## function does *not* allocate new types for the substitution. As noted in `is_subtype_impl`, `assignments` may not
  ## contain any type variables.
  is_subtype_impl(SubstitutionMode.T2, t1, t2)

proc is_subtype*(ts1: open_array[Type], ts2: open_array[Type]): bool =
  ## Whether a tuple type `tpl(ts1)` is a subtype of `tpl(ts2)`. This function does *not* allocate a new tuple type.
  if ts1.len != ts2.len:
    return false

  for i in 0 ..< ts1.len:
    if not is_subtype(ts1[i], ts2[i]):
      return false
  true

proc is_subtype_substitute1*(ts1: open_array[Type], ts2: open_array[Type], assignments: open_array[Type]): bool =
  ## Whether a tuple type `tpl(ts1)` is a subtype of `tpl(ts2)` when type variables in `ts1` are substituted with types
  ## from `assignments`. This function does *not* allocate a new tuple type or any types for the substitution. As noted
  ## in `is_subtype_impl`, `assignments` may not contain any type variables.
  if ts1.len != ts2.len:
    return false

  for i in 0 ..< ts1.len:
    if not is_subtype_substitute1(ts1[i], ts2[i], assignments):
      return false
  true

proc is_subtype_substitute2*(ts1: open_array[Type], ts2: open_array[Type], assignments: open_array[Type]): bool =
  ## Whether a tuple type `tpl(ts1)` is a subtype of `tpl(ts2)` when type variables in `ts2` are substituted with types
  ## from `assignments`. This function does *not* allocate a new tuple type or any types for the substitution. As noted
  ## in `is_subtype_impl`, `assignments` may not contain any type variables.
  if ts1.len != ts2.len:
    return false

  for i in 0 ..< ts1.len:
    if not is_subtype_substitute2(ts1[i], ts2[i], assignments):
      return false
  true

proc is_subtype_substitute(substitution_mode: SubstitutionMode, t1: Type, t2: Type, assignments: open_array[Type]): bool =
  case substitution_mode
  of SubstitutionMode.None: is_subtype(t1, t2)
  of SubstitutionMode.T1: is_subtype_substitute1(t1, t2, assignments)
  of SubstitutionMode.T2: is_subtype_substitute2(t1, t2, assignments)

########################################################################################################################
# Fit.                                                                                                                 #
########################################################################################################################

type FitsAssignments = array[max_type_parameters, Type]
  ## Assignments are modeled as fixed-size arrays so that we can put type arguments on the stack before pushing them
  ## into an allocated ImSeq should the fit be successful.

proc fits_assign(t1: Type, t2: Type, assignments: var FitsAssignments): bool
proc fits_assign_shape(t1: Type, t2: ShapeType, assignments: var FitsAssignments): bool
proc fits_assign_declared_type(t1: DeclaredType, t2: DeclaredType, assignments: var FitsAssignments): bool

template fits_impl(
  is_poly1: bool,
  ts1: open_array[Type],
  ts2: open_array[Type],
  parameters: ImSeq[TypeParameter],
): ImSeq[Type] =
  ## `ts1` may contain type variables if and only if `is_poly1` is true. This allows `fits_impl` to choose the right
  ## subtyping strategy. Because subtyping `assignments` may not contain type variables, we have to substitute before
  ## calling `is_subtype` if `ts1` contains type variables.
  if ts1.len != ts2.len:
    return nil

  var assignments: FitsAssignments
  for i in 0 ..< ts1.len:
    let t1 = ts1[i]
    let t2 = ts2[i]
    if not fits_assign(t1, t2, assignments):
      # Consistency constraint: All variable assignments must be unique. (Baked into `assign`.)
      return nil

  # Consistency constraint: All variables must have an assignment.
  for i in 0 ..< parameters.len:
    if assignments[i] == nil:
      return nil

  # Consistency constraint: All bounds must be kept.
  for i in 0 ..< parameters.len:
    let parameter = parameters[i]
    let tpe = assignments[i]
    if not bounds_contain(parameter, tpe, assignments):
      return nil

  # Final check: `t1` must be a subtype of `t2` when substituting assignments into `t2`.
  for i in 0 ..< ts1.len:
    if is_poly1:
      if not is_subtype(ts1[i], substitute(ts2[i], assignments)):
        return nil
    else:
      if not is_subtype_substitute2(ts1[i], ts2[i], assignments):
        return nil

  # So far, we've used an array on the stack for the type assignments. We have to convert these to a heap-allocated
  # ImSeq now, so that they outlive the lifetime of this function call.
  new_immutable_seq(assignments, parameters.len)

proc fits*(t1: Type, t2: Type, parameters: ImSeq[TypeParameter]): ImSeq[Type] {.inline.} =
  ## Whether `t1` fits into `t2`. `fits` returns the list of assigned type arguments if true, or `nil` otherwise.
  ## `parameters` must contain all type parameters which variables in `t2` refer to, in the proper order. `t1` may NOT
  ## contain type variables.
  fits([t1], [t2], parameters)

proc fits_poly1(t1: Type, t2: Type, parameters: ImSeq[TypeParameter]): ImSeq[Type] {.inline.} =
  ## Equivalent to `fits(t1, t2, parameters)`, but `t1` may contain type variables.
  fits_poly1([t1], [t2], parameters)

proc fits*(ts1: open_array[Type], ts2: open_array[Type], parameters: ImSeq[TypeParameter]): ImSeq[Type] =
  ## Whether `ts1`, interpreted as the elements of a tuple type, fit into `ts2`. `fits` returns the list of assigned
  ## type arguments if true, or `nil` otherwise. `parameters` must contain all type parameters which variables in `t2`
  ## refer to, in the proper order. `ts1` may NOT contain type variables.
  fits_impl(false, ts1, ts2, parameters)

proc fits_poly1*(ts1: open_array[Type], ts2: open_array[Type], parameters: ImSeq[TypeParameter]): ImSeq[Type] =
  ## Equivalent to `fits(ts1, ts2, parameters)`, but `ts1` may contain type variables.
  fits_impl(true, ts1, ts2, parameters)

proc fits_assign(t1: Type, t2: Type, assignments: var FitsAssignments): bool =
  ## Assigns all matching types in `t1` to type variables in `t2`, saving them in `assignments`. If an assignment
  ## already exists and the existing type and new type aren't equal, `assign` returns false to signal that `t1` cannot
  ## fit into `t2`. This trivially covers one consistency check case: that assignments must be unique.
  # TODO (vm): We can technically reject more cases than just "assignment already exists". For example, when tuple
  #            lengths don't match up, we can be sure that the fit cannot be correct. Another example is when a shape
  #            type t2 has a property which isn't in a shape type t1. We know that t1 in this case can never be a
  #            subtype of t2.
  case t2.kind
  of Kind.TypeVariable:
    let index = cast[TypeVariable](t2).index
    let existing_assignment = assignments[index]
    if existing_assignment == nil:
      assignments[index] = t1
    else:
      if not are_equal(existing_assignment, t1):
        return false
    true

  of Kind.Sum:
    if is_polymorphic(t2):
      quit("Type variable assignment for sum types is not yet supported.")
    true

  of Kind.Intersection:
    if is_polymorphic(t2):
      quit("Type variable assignment for intersection types is not yet supported.")
    true

  of Kind.Tuple:
    if t1.kind == Kind.Tuple:
      let t1 = cast[TupleType](t1)
      let t2 = cast[TupleType](t2)
      if t1.elements.len == t2.elements.len:
        let length = t1.elements.len
        for i in 0 ..< length:
          if not fits_assign(t1.elements[i], t2.elements[i], assignments):
            return false
    true

  of Kind.Function:
    if t1.kind == Kind.Function:
      let t1 = cast[FunctionType](t1)
      let t2 = cast[FunctionType](t2)
      if not fits_assign(t1.input, t2.input, assignments):
        return false
      fits_assign(t1.output, t2.output, assignments)
    else: true

  of Kind.List:
    if t1.kind == Kind.List:
      let t1 = cast[ListType](t1)
      let t2 = cast[ListType](t2)
      fits_assign(t1.element, t2.element, assignments)
    else: true

  of Kind.Map:
    if t1.kind == Kind.Map:
      let t1 = cast[MapType](t1)
      let t2 = cast[MapType](t2)
      if not fits_assign(t1.key, t2.key, assignments):
        return false
      fits_assign(t1.value, t2.value, assignments)
    else: true

  of Kind.Shape:
    fits_assign_shape(t1, cast[ShapeType](t2), assignments)

  of Kind.Trait, Kind.Struct:
    if t1.is_declared_type:
      fits_assign_declared_type(cast[DeclaredType](t1), cast[DeclaredType](t2), assignments)
    else:
      true

  else: true

template fits_assign_shape_via_property_names(t1, t2, assignments): untyped =
  ## This template is a common piece of code that works for both shapes and structs.
  for i in 0 ..< t2.property_count:
    let property_name = t2.meta.property_names[i]
    let p1_type = t1.get_property_type_if_exists(property_name)
    if p1_type != nil and not fits_assign(p1_type, t2.property_types[i], assignments):
      return false

proc fits_assign_shape(t1: Type, t2: ShapeType, assignments: var FitsAssignments): bool =
  if t1.kind == Kind.Shape:
    let t1 = cast[ShapeType](t1)
    if t1.meta == t2.meta:
      for i in 0 ..< t2.property_count:
        if not fits_assign(t1.property_types[i], t2.property_types[i], assignments):
          return false
    else:
      fits_assign_shape_via_property_names(t1, t2, assignments)
  elif t1.kind == Kind.Struct:
    let t1 = cast[StructType](t1)
    fits_assign_shape_via_property_names(t1, t2, assignments)
  true

proc fits_assign_declared_type(t1: DeclaredType, t2: DeclaredType, assignments: var FitsAssignments): bool =
  if t2.schema.is_constant:
    return true

  let s1 =
    if t1.schema === t2.schema:
      t1
    elif t2.kind == Kind.Trait:
      t1.find_supertrait(cast[TraitSchema](t2.schema))
    else:
      return true

  for i in 0 ..< t2.type_arguments.len:
    if not fits_assign(s1.type_arguments[i], t2.type_arguments[i], assignments):
      return false
  true

########################################################################################################################
# Polymorphy.                                                                                                          #
########################################################################################################################

proc is_polymorphic(types: ImSeq[Type]): bool

proc is_polymorphic(tpe: Type): bool =
  ## Whether `tpe` is polymorphic. This can usually be decided based on the presence of type parameters, so this
  ## function should only be used if polymorphy must be decided for subterms in types.
  case tpe.kind
  of Kind.TypeVariable: true
  of Kind.Sum: is_polymorphic(cast[SumType](tpe).parts)
  of Kind.Intersection: is_polymorphic(cast[IntersectionType](tpe).parts)
  of Kind.Tuple: is_polymorphic(cast[TupleType](tpe).elements)
  of Kind.Function: is_polymorphic(cast[FunctionType](tpe).input) or is_polymorphic(cast[FunctionType](tpe).output)
  of Kind.List: is_polymorphic(cast[ListType](tpe).element)
  of Kind.Map: is_polymorphic(cast[MapType](tpe).key) or is_polymorphic(cast[MapType](tpe).value)
  of Kind.Shape: is_polymorphic(cast[ShapeType](tpe).property_types)
  else: false

proc is_polymorphic(types: ImSeq[Type]): bool =
  for tpe in types:
    if is_polymorphic(tpe):
      return true
  false

########################################################################################################################
# Simplification.                                                                                                      #
########################################################################################################################

template simplify_construct_covariant(kind: Kind, parts: open_array[Type]): untyped =
  if kind == Kind.Sum: sum_simplified(parts)
  elif kind == Kind.Intersection: intersection_simplified(parts)
  else: quit("Invalid kind for covariant construction.")

template simplify_construct_contravariant(kind: Kind, parts: open_array[Type]): untyped =
  if kind == Kind.Sum: intersection_simplified(parts)
  elif kind == Kind.Intersection: sum_simplified(parts)
  else: quit("Invalid kind for contravariant construction.")

proc add_unique_type[I](types: var StackSeq[I, Type], tpe: Type) = types.add_unique(tpe, are_equal(a, b))
proc add_unique_schema[I](schemas: var StackSeq[I, Schema], schema: Schema) = schemas.add_unique(schema, a === b)

template simplify_categorize_type(tpe: Type, kind: Kind) =
  case tpe.kind
  of Kind.Tuple:
    let tuple_type = cast[TupleType](tpe)
    case tuple_type.elements.len
    of 0: results.add_unique_type(tpe)
    of 1: tuples1.add(tuple_type)
    of 2: tuples2.add(tuple_type)
    of 3: tuples3.add(tuple_type)
    else: tuplesX.add(tuple_type)
  of Kind.Function: functions.add(cast[FunctionType](tpe))
  of Kind.List: lists.add(cast[ListType](tpe))
  of Kind.Shape:
    # Shape types are only simplified when they're part of an intersection type.
    if kind == Kind.Intersection:
      shapes.add(cast[ShapeType](tpe))
    else:
      results.add_unique_type(tpe)
  of Kind.Trait, Kind.Struct:
    let dt = cast[DeclaredType](tpe)
    if not dt.schema.has_invariant_type_parameters:
      schemas.add_unique_schema(dt.schema)
      declared_types.add(dt)
    else:
      results.add_unique_type(tpe)
  else: results.add_unique_type(tpe)

template simplify_flatten(tpe: Type, T: untyped, kind: Kind, expected_kind: Kind): untyped =
  if kind == expected_kind:
    let xary_type = cast[T](tpe)
    for child in xary_type.parts:
      simplify_categorize_type(child, kind)
  else:
    results.add_unique_type(tpe)

proc simplify_tuples(
  types: var StackSeq[8, TupleType],
  results: var StackSeq[32, Type],
  kind: Kind,
) =
  if types.len == 0:
    return
  elif types.len == 1:
    # This is guaranteed to be the only tuple of size 1, so we don't need `add_unique_type`.
    results.add(types[0])
    return

  # (A, B) | (C, D) :=: (A | C, B | D)
  # (A, B) & (C, D) :=: (A & C, B & D)
  let size = types[0].elements.len
  var elements = new_immutable_seq[Type](size)
  # We can reuse `element_parts` for each iteration as it's treated as immutable by `simplify`.
  var element_parts = new_immutable_seq[Type](types.len)
  for i in 0 ..< size:
    for j in 0 ..< types.len:
      element_parts[j] = types[j].elements[i]
    elements[i] = simplify_construct_covariant(kind, to_open_array(element_parts))

  results.add(tpl(elements))

proc simplify(kind: Kind, parts: open_array[Type]): Type {.inline.} =
  ## Simplifies `parts` as if they were contained in a sum or intersection type, determined by `kind`. This operation
  ## is very costly, so try to minimize its usage. `parts` won't be mutated by this function.
  if parts.len == 1:
    return parts[0]

  # Step 1: Flatten.
  # We want to allocate as many arrays on the stack as possible. However, a constructed sum/intersection type may have
  # an arbitrary length. Hence, we are using StackSeq to avoid allocations for small sum/intersection types. To avoid
  # two iterations over `parts`, we're immediately sorting types into boxes while flattening.
  #
  # We have to ensure that each type in `results` is unique. This does not apply to tuples, functions, lists, shapes,
  # and declared types of the same schema because these types are later combined in such a way that each result type
  # must be unique.
  #
  # Declared types must be handled per schema, but keeping a hash map to separate the types into boxes would be too
  # costly. We're assuming that usually no more than a handful of declared types will ever need to be simplified at
  # once. Hence, we are keeping declared types together in a StackSeq, while schemas are saved in an additional
  # StackSeq so that we know which schemas to iterate through. The declared types StackSeq will be iterated through
  # once per schema, but this should still be faster than allocating a map.
  var tuples1: StackSeq[8, TupleType]
  var tuples2: StackSeq[8, TupleType]
  var tuples3: StackSeq[8, TupleType]
  var tuplesX: StackSeq[8, TupleType]
  var functions: StackSeq[8, FunctionType]
  var lists: StackSeq[8, ListType]
  var shapes: StackSeq[8, ShapeType]
  var schemas: StackSeq[8, Schema]
  var declared_types: StackSeq[8, DeclaredType]
  var results: StackSeq[32, Type]

  for part in parts:
    case part.kind
    of Kind.Sum: simplify_flatten(part, SumType, kind, Kind.Sum)
    of Kind.Intersection: simplify_flatten(part, IntersectionType, kind, Kind.Intersection)
    else: simplify_categorize_type(part, kind)

  # Step 2: Simplify tuples, functions, and lists.
  if tuples1.len > 0: simplify_tuples(tuples1, results, kind)
  if tuples2.len > 0: simplify_tuples(tuples2, results, kind)
  if tuples3.len > 0: simplify_tuples(tuples3, results, kind)

  if tuplesX.len > 0:
    # To resolve tuples of length >= 4, we have to comb them out of `tuplesX` by size. For example, if we have four
    # tuples in `tuplesX`, two with length 6 and two with length 9, we call `simplify_tuples` once with all tuples of
    # length 6 and once with all tuples of length 9. We can reuse `tuples2` as a temporary storage.
    tuples2.clear()
    var last_size = 3
    var left_to_process = tuplesX.len
    while left_to_process > 0:
      # We have to find out which size to comb out next without iterating through all possible tuple sizes from 4 to
      # 31. That would just be wasteful. Hence, we want to find the smallest size larger than `last_size`. We can be
      # sure that there is such a size because `left_to_process` is not yet 0.
      var size = high(int)
      for tpe in tuplesX:
        let tpe_size = tpe.elements.len
        if tpe_size > last_size and tpe_size < size:
          size = tpe_size
      last_size = size

      for tpe in tuplesX:
        if tpe.elements.len == size:
          tuples2.add(tpe)

      simplify_tuples(tuples2, results, kind)
      left_to_process -= tuples2.len
      tuples2.clear()

  # (A => B) | (C => D) :=: (A & C) => (B | D)
  # (A => B) & (C => D) :=: (A | C) => (B & D)
  let functions_count = functions.len
  if functions_count > 0:
    var input_parts = new_immutable_seq[Type](functions_count)
    var output_parts = new_immutable_seq[Type](functions_count)
    for i in 0 ..< functions_count:
      let function_type = functions[i]
      input_parts[i] = function_type.input
      output_parts[i] = function_type.output

    # The combined input type must be a tuple type because all parts are tuples.
    let input = cast[TupleType](simplify_construct_contravariant(kind, to_open_array(input_parts)))
    assert(input.kind == Kind.Tuple)
    let output = simplify_construct_covariant(kind, to_open_array(output_parts))
    results.add(function(input, output))

  # [A] | [B] :=: [A | B]
  # [A] & [B] :=: [A & B]
  let lists_count = lists.len
  if lists_count > 0:
    var element_parts = new_immutable_seq[Type](lists_count)
    for i in 0 ..< lists_count:
      element_parts[i] = lists[i].element

    let element = simplify_construct_covariant(kind, to_open_array(element_parts))
    results.add(list(element))

  # { name: A } & { name: B } & { health: Int } :=: { name: A & B, health: Int }
  if shapes.len > 0:
    # This is basically `ShapeType.combine`, but we have to work with the StackSeq, so it's harder to put this into a
    # separate function.
    var property_names = new_seq[string]()
    for shape_type in shapes:
      # TODO (vm/schemas): Does the string from `property_names` get copied into `name`? If so, all of these string
      #                    loops would cause major performance issues due to allocations.
      for name in shape_type.meta.property_names:
        property_names.add(name)

    let meta_shape = get_meta_shape_safe(property_names)
    var property_types = new_immutable_seq[Type](meta_shape.property_count)
    var property_type_parts = new_seq_of_cap[Type](8)
    for i in 0 ..< property_types.len:
      let property_name = meta_shape.property_names[i]
      for shape_type in shapes:
        let property_type = shape_type.get_property_type_if_exists(property_name)
        if property_type != nil:
          property_type_parts.add(property_type)
      property_types[i] = simplify_construct_covariant(kind, property_type_parts)
      property_type_parts.set_len(0)

    results.add(new_shape_type(meta_shape, property_types))

  # Given a declared type `D[+X, -Y]`:
  # D[A, B] | D[C, D] :=: D[A | C, B & D]
  # D[A, B] & D[C, D] :=: D[A & C, B | D]
  if schemas.len > 0:
    # `candidates` caches the declared types for each schema.
    var candidates: StackSeq[8, DeclaredType]
    for schema in schemas:
      for dt in declared_types:
        if dt.schema === schema:
          candidates.add(dt)

      let type_arguments = combine_type_arguments(
        schema,
        candidates,
        simplify_construct_covariant(kind, to_open_array(type_argument_candidates)),
        simplify_construct_contravariant(kind, to_open_array(type_argument_candidates)),
        block:
          quit(fmt"At this stage, a type parameter may not be invariant. Type schema: {schema.name}.")
          nil
      )

      # Note that this instantiation ignores a struct type's open property types. This is okay, as simplified types
      # shouldn't contain open property types anyway.
      let declared_type = instantiate_schema(schema, type_arguments)
      if declared_type != nil:
        results.add(declared_type)
      else:
        # If the schema cannot be instantiated with the combined type arguments, default to the uncombined declared
        # types.
        for dt in candidates:
          results.add_unique_type(dt)

      candidates.clear()

  # Step 3: Filter relevant types, i.e. only take the most general/specific types.
  let results_count = results.len
  var relevants: StackSeq[32, Type]
  for i in 0 ..< results_count:
    let self = results[i]

    var j = 0
    while j < results_count:
      let other = results[j]
      if i != j:
        if kind == Kind.Sum and is_subtype(self, other): break
        elif kind == Kind.Intersection and is_subtype(other, self): break
      j += 1

    if j == results_count:
      relevants.add(self)

  # Step 4: Allocate an ImSeq for the relevant parts and build the simplified xary type around them.
  if relevants.len == 1:
    return relevants[0]

  var result_parts = new_immutable_seq[Type](relevants.len)
  for i in 0 ..< relevants.len:
    result_parts[i] = relevants[i]

  if kind == Kind.Sum: sum(result_parts)
  elif kind == Kind.Intersection: intersection(result_parts)
  else: quit(fmt"Invalid kind {kind} for simplification.")

proc sum_simplified*(parts: open_array[Type]): Type =
  ## Constructs a sum type from the given parts. But first, `parts` are flattened, combined according to their
  ## variance, and filtered for the most general/specific types. `parts` won't be mutated by this function.
  ##
  ## If a subterm is covariant, the type candidates are combined into a sum type. If a subterm is contravariant, the
  ## intersection is used instead. Invariant subterms cannot be combined, meaning types with invariant subterms are
  ## left as is.
  ##
  ## The following kinds of types will be simplified: Tuples of the same size, functions, lists, and declared types of
  ## the same schema.
  simplify(Kind.Sum, parts)

proc intersection_simplified*(parts: open_array[Type]): Type =
  ## Constructs an intersection type from the given parts. But first, `parts` are flattened, combined according to
  ## their variance, and filtered for the most general/specific types. `parts` won't be mutated by this function.
  ##
  ## If a subterm is covariant, the type candidates are combined into an intersection type. If a subterm is
  ## contravariant, the sum is used instead. Invariant subterms cannot be combined, meaning types with invariant
  ## subterms are left as is.
  ##
  ## The following kinds of types will be simplified: Tuples of the same size, functions, lists, shapes, and declared
  ## types of the same schema.
  simplify(Kind.Intersection, parts)

proc sum_simplified*(parts: ImSeq[Type]): Type = sum_simplified(to_open_array(parts))
proc intersection_simplified*(parts: ImSeq[Type]): Type = intersection_simplified(to_open_array(parts))

########################################################################################################################
# Substitution.                                                                                                        #
########################################################################################################################

proc substitute_optimized(tpe: Type, type_arguments: open_array[Type]): Type
proc substitute_multiple_optimized(types: ImSeq[Type], type_arguments: open_array[Type]): ImSeq[Type]

proc substitute*(tpe: Type, type_arguments: open_array[Type]): Type =
  ## Substitutes any type variables in `tpe` with the given type arguments, creating a new type. If `tpe` contains no
  ## variables to substitute, `tpe` itself is returned unchanged.
  let res = substitute_optimized(tpe, type_arguments)
  if res != nil: res
  else: tpe

proc substitute*(tpe: Type, type_arguments: ImSeq[Type]): Type =
  ## Substitutes any type variables in `tpe` with the given type arguments, creating a new type. If `tpe` contains no
  ## variables to substitute, `tpe` itself is returned unchanged.
  substitute(tpe, type_arguments.to_open_array)

template substitute_unary_and_construct(child0: Type, type_arguments: open_array[Type], constructor): Type =
  let result0 = substitute_optimized(child0, type_arguments)
  if result0 != nil: constructor(result0)
  else: nil

template substitute_binary_and_construct(child0: Type, child1: Type, type_arguments: open_array[Type], constructor): Type =
  var result0 = substitute_optimized(child0, type_arguments)
  var result1 = substitute_optimized(child1, type_arguments)
  call_if_any_exists(constructor, result0, child0, result1, child1, nil)

template substitute_xary_and_construct(children: ImSeq[Type], type_arguments: open_array[Type], constructor): Type =
  let results = substitute_multiple_optimized(children, type_arguments)
  if results != nil: constructor(results)
  else: nil

template substitute_declared_type_arguments(tpe: Type, type_arguments: open_array[Type]): ImSeq[Type] =
  ## Given `tpe`, substitutes `type_arguments` into `tpe`'s type arguments and returns them. Returns `nil` if no change
  ## to the type arguments is needed.
  let dt = cast[DeclaredType](tpe)
  if dt.schema.is_constant:
    return nil
  substitute_multiple_optimized(dt.type_arguments, type_arguments)

proc substitute_optimized(tpe: Type, type_arguments: open_array[Type]): Type =
  ## Substitutes any type variables in `tpe` with the given type arguments. If no substitutions occur, the function
  ## returns `nil`. This allows it to only allocate new types should a child type have changed.
  case tpe.kind
  of Kind.TypeVariable:
    let tv = cast[TypeVariable](tpe)
    if cast[int](tv.index) < type_arguments.len: type_arguments[tv.index]
    else: nil

  of Kind.Sum: substitute_xary_and_construct(cast[SumType](tpe).parts, type_arguments, sum_simplified)
  of Kind.Intersection: substitute_xary_and_construct(cast[IntersectionType](tpe).parts, type_arguments, intersection_simplified)
  of Kind.Tuple: substitute_xary_and_construct(cast[TupleType](tpe).elements, type_arguments, tpl)

  of Kind.Function:
    let tpe = cast[FunctionType](tpe)
    substitute_binary_and_construct(tpe.input, tpe.output, type_arguments, function_unsafe)

  of Kind.List: substitute_unary_and_construct(cast[ListType](tpe).element, type_arguments, list)

  of Kind.Map:
    let tpe = cast[MapType](tpe)
    substitute_binary_and_construct(tpe.key, tpe.value, type_arguments, map)

  of Kind.Shape:
    let tpe = cast[ShapeType](tpe)
    let property_types = substitute_multiple_optimized(tpe.property_types, type_arguments)
    if property_types != nil: new_shape_type(tpe.meta, property_types)
    else: nil

  of Kind.Trait:
    let type_arguments = substitute_declared_type_arguments(tpe, type_arguments)
    if type_arguments != nil: force_instantiate_trait_schema(cast[TraitType](tpe).get_schema, type_arguments)
    else: nil

  of Kind.Struct:
    let tpe = cast[StructType](tpe)
    let type_arguments = substitute_declared_type_arguments(tpe, type_arguments)
    if type_arguments != nil: force_instantiate_struct_schema(tpe.get_schema, type_arguments, tpe.open_property_types)
    else: nil

  else: nil

proc substitute_multiple_optimized(types: ImSeq[Type], type_arguments: open_array[Type]): ImSeq[Type] =
  var result_types: ImSeq[Type] = nil

  let length = types.len
  for i in 0 ..< length:
    let candidate = substitute_optimized(types[i], type_arguments)
    if candidate != nil:
      if result_types == nil:
        result_types = new_immutable_seq(types)
      result_types[i] = candidate

  result_types

########################################################################################################################
# Stringification.                                                                                                     #
########################################################################################################################

proc stringify_open_properties(tpe: DeclaredType): string

proc `$`*(tpe: Type): string =
  case tpe.kind
  of Kind.TypeVariable: "tv" & $cast[TypeVariable](tpe).index
  of Kind.Any: "Any"
  of Kind.Nothing: "Nothing"
  of Kind.Real: "Real"
  of Kind.Int: "Int"
  of Kind.Boolean: "Boolean"
  of Kind.String: "String"
  of Kind.Sum: "(" & cast[SumType](tpe).parts.join(" | ") & ")"
  of Kind.Intersection: "(" & cast[IntersectionType](tpe).parts.join(" & ") & ")"
  of Kind.Tuple: "(" & cast[TupleType](tpe).elements.join(", ") & ")"
  of Kind.Function:
    let tpe = cast[FunctionType](tpe)
    "(" & $tpe.input & " => " & $tpe.output & ")"
  of Kind.List: "[" & $cast[ListType](tpe).element & "]"
  of Kind.Map:
    let tpe = cast[MapType](tpe)
    "#[" & $tpe.key & " -> " & $tpe.value & "]"
  of Kind.Shape:
    let tpe = cast[ShapeType](tpe)
    var properties = new_immutable_seq[string](tpe.property_count)
    for i in 0 ..< tpe.property_count:
      properties[i] = tpe.meta.property_names[i] & ": " & $tpe.property_types[i]
    "%{ " & properties.join(", ") & " }"
  of Kind.Symbol: "#" & cast[SymbolType](tpe).name
  of Kind.Trait, Kind.Struct:
    let tpe = cast[DeclaredType](tpe)
    let type_arguments =
      if not tpe.schema.is_constant:
        "[" & tpe.type_arguments.join(", ") & "]"
      else:
        ""
    let open_properties = stringify_open_properties(tpe)
    tpe.schema.name & type_arguments & open_properties

proc stringify_open_properties(tpe: DeclaredType): string =
  if tpe.kind != Kind.Struct:
    return ""

  let tpe = cast[StructType](tpe)
  if tpe.open_property_types == nil:
    return ""

  let schema = tpe.get_schema
  let open_property_count = schema.open_property_indices.len
  var open_property_strings = new_immutable_seq[string](open_property_count)
  for i in 0 ..< open_property_count:
    let property_name = schema.properties[schema.open_property_indices[i]].name
    let property_type = $tpe.open_property_types[i]
    open_property_strings[i] = property_name & ": " & property_type
  "(" & open_property_strings.join(", ") & ")"

########################################################################################################################
# Type benchmarks.                                                                                                     #
########################################################################################################################

when is_main_module:
  from utils import benchmark

  let sum1 = sum([string_type, int_type, boolean_type])
  let sum2 = sum([string_type, int_type, boolean_type])
  let sum3 = sum([real_type, boolean_type])

  echo are_equal(sum1, sum1)
  benchmark("sum1 == sum1", 100_000_000):
    discard are_equal(sum1, sum1)

  echo are_equal(sum1, sum2)
  benchmark("sum1 == sum2", 50_000_000):
    discard are_equal(sum1, sum2)

  echo are_equal(sum1, sum3)
  benchmark("sum1 == sum3", 100_000_000):
    discard are_equal(sum1, sum3)

  let tuple1 = tpl([
    sum([string_type, int_type, boolean_type]),
    intersection([string_type, int_type, boolean_type]),
    list(map(string_type, int_type)),
  ])

  let tuple2 = tpl([
    sum([string_type, int_type, boolean_type]),
    intersection([string_type, int_type, boolean_type]),
    list(map(string_type, int_type)),
  ])

  echo are_equal(tuple1, tuple2)
  benchmark("tuple1 == tuple2", 25_000_000):
    discard are_equal(tuple1, tuple2)

  benchmark("tuple1 == tuple2 (+creation)", 10_000_000):
    let tuple1 = tpl([
      sum([string_type, int_type, boolean_type]),
      intersection([string_type, int_type, boolean_type]),
      list(map(string_type, int_type)),
    ])

    let tuple2 = tpl([
      sum([string_type, int_type, boolean_type]),
      intersection([string_type, int_type, boolean_type]),
      list(map(string_type, int_type)),
    ])

    discard are_equal(tuple1, tuple2)

  # These two simplfication examples are equal to the ones in `test/types/simplification.nim`.
  let primitives2 = new_immutable_seq([int_type, real_type, intersection([int_type, string_type]), sum([boolean_type])])

  benchmark("simplify sum of primitives", 10_000_000):
    discard sum_simplified(primitives2)

  let tuples2569 = new_immutable_seq([
    tpl_as_type([int_type, int_type, int_type, int_type, int_type, real_type]),
    tpl([int_type, real_type]),
    tpl([int_type, int_type, real_type, int_type, int_type]),
    tpl([int_type, int_type, int_type, int_type, real_type, int_type, int_type, int_type, int_type]),
    tpl([int_type, real_type, int_type, int_type, int_type, int_type]),
    tpl([real_type, real_type]),
    tpl([int_type, int_type, int_type, int_type, int_type, int_type, int_type, int_type, int_type]),
    tpl([int_type, int_type, int_type, real_type, int_type, int_type]),
  ])

  benchmark("simplify sum of tuples", 1_000_000):
    discard sum_simplified(tuples2569)
