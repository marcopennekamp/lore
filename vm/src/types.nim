from std/algorithm import sort
import std/macros
import std/sets
from std/sequtils import deduplicate
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
    property_name_set*: HashSet[string]
      ## Because a property index cannot be used to determine whether a given name is part of the shape, we need an
      ## additional name set for shape/shape subtyping and other operations.

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
    is_inherited_shape_type_polymorphic: bool
      ## `get_inherited_shape_type` only has to substitute type arguments when the inherited shape type is polymorphic.

  TraitType* {.pure.} = ref object of DeclaredType
    inherited_shape_type_cache: ShapeType
      ## The schema's inherited shape type instantiated with the trait's type arguments. The cache is only populated on
      ## demand and only when the schema's inherited shape type is polymorphic.

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
    name: string
    is_open: bool
    tpe: Type

  StructType* {.pure.} = ref object of DeclaredType
    property_types: ImSeq[Type]
      ## The actual run-time types of the struct's properties. This sequence is defined if and only if the schema is
      ## parameterized or if it has open properties.

const max_type_parameters* = 32
  ## The maximum number of type parameters that a function may have is 32. This allows us to allocate certain arrays on
  ## the stack when checking for type fit.

proc is_subtype_substitute1*(t1: Type, t2: Type, assignments: open_array[Type]): bool
proc is_subtype_substitute2*(t1: Type, t2: Type, assignments: open_array[Type]): bool

proc fits*(ts1: open_array[Type], ts2: open_array[Type], parameters: ImSeq[TypeParameter]): ImSeq[Type]

proc substitute*(tpe: Type, type_arguments: open_array[Type]): Type
proc substitute*(tpe: Type, type_arguments: ImSeq[Type]): Type

proc is_polymorphic(tpe: Type): bool

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
      property_name_set: to_hash_set(to_open_array(property_names)),
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

proc has_property*(meta_shape: MetaShape, name: string): bool {.inline.} = name in meta_shape.property_name_set
proc has_property*(tpe: ShapeType, name: string): bool {.inline.} = tpe.meta.has_property(name)

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
  ## Gets the type of the property named `name`. The name must be a valid property name for the given shape type.
  tpe.property_types[tpe.meta.property_index.find_offset(name)]

########################################################################################################################
# Declared types.                                                                                                      #
########################################################################################################################

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

proc check_type_parameter_bounds(schema: Schema, type_arguments: ImSeq[Type]) =
  if not bounds_contain(schema, type_arguments):
    quit(fmt"Cannot instantiate schema {schema.name}: the type arguments {type_arguments} don't adhere to their bounds.")

proc instantiate_supertraits(schema: Schema, type_arguments: ImSeq[Type]): ImSeq[TraitType] =
  if schema.is_constant or schema.supertraits.len == 0:
    return schema.supertraits

  var instantiated = new_immutable_seq[TraitType](schema.supertraits.len)
  for i in 0 ..< schema.supertraits.len:
    instantiated[i] = cast[TraitType](substitute(schema.supertraits[i], to_open_array(type_arguments)))
  instantiated

########################################################################################################################
# Traits.                                                                                                              #
########################################################################################################################

proc new_trait_schema*(
  name: string,
  type_parameters: ImSeq[TypeParameter],
  supertraits: ImSeq[TraitType],
): TraitSchema =
  let schema = TraitSchema(
    kind: Kind.Trait,
    name: name,
    type_parameters: type_parameters,
    supertraits: supertraits,
    # Inherited shape types are resolved in a second step.
    inherited_shape_type: nil,
    is_inherited_shape_type_polymorphic: false,
  )

  let type_arguments = cast[ImSeq[Type]](type_parameters.as_type_arguments())
  let representative = TraitType(schema: schema, type_arguments: type_arguments, supertraits: supertraits)
  schema.representative = representative
  schema

proc attach_inherited_shape_type*(schema: TraitSchema, inherited_shape_type: ShapeType) =
  if schema.inherited_shape_type != nil:
    quit(fmt"An inherited shape type has already been attached to trait schema {schema.name}.")

  schema.inherited_shape_type = inherited_shape_type
  schema.is_inherited_shape_type_polymorphic = is_polymorphic(inherited_shape_type)

proc instantiate_schema*(schema: TraitSchema, type_arguments: ImSeq[Type]): TraitType =
  ## Instantiates `schema` with the given type arguments.
  # TODO (vm/intern): This function should intern the declared types.
  if schema.is_constant:
    return schema.get_representative()

  check_type_parameter_bounds(schema, type_arguments)
  let supertraits = instantiate_supertraits(schema, type_arguments)
  TraitType(
    schema: schema,
    type_arguments: type_arguments,
    supertraits: supertraits,
    inherited_shape_type_cache: nil
  )

proc get_inherited_shape_type*(tpe: TraitType): ShapeType =
  let schema: TraitSchema = tpe.get_schema()

  if not schema.is_inherited_shape_type_polymorphic:
    return schema.inherited_shape_type

  if tpe.inherited_shape_type_cache != nil:
    return tpe.inherited_shape_type_cache

  let shape_type = cast[ShapeType](schema.inherited_shape_type.substitute(tpe.type_arguments))
  tpe.inherited_shape_type_cache = shape_type
  shape_type

########################################################################################################################
# Structs.                                                                                                             #
########################################################################################################################

proc new_struct_type(schema: StructSchema, type_arguments: ImSeq[Type], property_types: ImSeq[Type]): StructType

proc has_open_properties*(schema: StructSchema): bool = schema.open_property_indices.len > 0

proc instantiate_schema*(schema: StructSchema, type_arguments: ImSeq[Type]): StructType =
  ## Instantiates `schema` with the given type arguments without deviating open property types. This is used to
  ## instantiate constant struct types that aren't associated with a value, such as in input and output types.
  # TODO (vm/intern): This function should intern the declared types.
  if schema.is_constant:
    return schema.get_representative()

  var property_types = new_immutable_seq[Type](schema.properties.len)
  for i in 0 ..< schema.properties.len:
    property_types[i] = substitute(schema.properties[i].tpe, type_arguments)

  new_struct_type(schema, type_arguments, property_types)

proc instantiate_schema*(schema: StructSchema, type_arguments: ImSeq[Type], open_property_types: ImSeq[Type]): DeclaredType =
  ## Instantiates `schema` with the given type arguments and open property types. This is used to instantiate struct
  ## types associated with a value.
  # TODO (vm/intern): This function should intern the declared types.
  if open_property_types.len != schema.open_property_indices.len:
    quit(fmt"Cannot instantiate the struct schema {schema.name} with {open_property_types.len} open property types," &
     " as the schema expects {schema.open_property_indices.len}.")

  if schema.has_open_properties:
    var property_types = new_immutable_seq[Type](schema.properties.len)
    var open_counter = 0
    for i in 0 ..< schema.properties.len:
      if schema.properties[i].is_open:
        property_types[i] = open_property_types[open_counter]
        open_counter += 1
      else:
        property_types[i] =
          if schema.is_constant:
            schema.properties[i].tpe
          else:
            substitute(schema.properties[i].tpe, type_arguments)

    new_struct_type(schema, type_arguments, property_types)
  else:
    instantiate_schema(schema, type_arguments)

proc new_struct_type(schema: StructSchema, type_arguments: ImSeq[Type], property_types: ImSeq[Type]): StructType =
  check_type_parameter_bounds(schema, type_arguments)
  let supertraits = instantiate_supertraits(schema, type_arguments)
  StructType(
    schema: schema,
    type_arguments: type_arguments,
    supertraits: supertraits,
    property_types: property_types,
  )

########################################################################################################################
# Type equality.                                                                                                       #
########################################################################################################################

proc `===`(a: Type, b: Type): bool {.inline.} =
  ## Checks the referential equality of the two types.
  cast[pointer](a) == cast[pointer](b)

proc has_equal_in(ts1: ImSeq[Type], ts2: ImSeq[Type]): bool
proc are_exactly_equal(ts1: ImSeq[Type], ts2: ImSeq[Type]): bool

proc are_equal*(t1: Type, t2: Type): bool =
  # If the two types are referentially equal, they are obviously the same!
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

  else: false

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

########################################################################################################################
# Subtyping.                                                                                                           #
########################################################################################################################

type IsSubtypeSubstitutionMode {.pure.} = enum
  None
    ## Checks subtyping without substituting any types from assignments.
  T1
    ## Checks subtyping by replacing type variables in `t1` with types from assignments.
  T2
    ## Checks subtyping by replacing type variables in `t2` with types from assignments.

macro is_subtype_rec(substitution_mode: static[IsSubtypeSubstitutionMode], t1: Type, t2: Type): bool =
  case substitution_mode
  of IsSubtypeSubstitutionMode.None:
    quote do: is_subtype(`t1`, `t2`)
  of IsSubtypeSubstitutionMode.T1:
    quote do: is_subtype_substitute1(`t1`, `t2`, assignments)
  of IsSubtypeSubstitutionMode.T2:
    quote do: is_subtype_substitute2(`t1`, `t2`, assignments)

template type_subtypes_sum(substitution_mode: IsSubtypeSubstitutionMode, t1: Type, s2: SumType): bool =
  for p2 in s2.parts:
    if is_subtype_rec(substitution_mode, t1, p2):
      return true
  false

template sum_subtypes_sum(substitution_mode: IsSubtypeSubstitutionMode, s1: SumType, s2: SumType): bool =
  for p1 in s1.parts:
    if not type_subtypes_sum(substitution_mode, p1, s2):
      return false
  true

template sum_subtypes_type(substitution_mode: IsSubtypeSubstitutionMode, s1: SumType, t2: Type): bool =
  for p1 in s1.parts:
    if not is_subtype_rec(substitution_mode, p1, t2):
      return false
  true

template intersection_subtypes_type(substitution_mode: IsSubtypeSubstitutionMode, i1: IntersectionType, t2: Type): bool =
  for p1 in i1.parts:
    if is_subtype_rec(substitution_mode, p1, t2):
      return true
  false

template intersection_subtypes_intersection(substitution_mode: IsSubtypeSubstitutionMode, i1: IntersectionType, i2: IntersectionType): bool =
  for p2 in i2.parts:
    if not intersection_subtypes_type(substitution_mode, i1, p2):
      return false
  true

template type_subtypes_intersection(substitution_mode: IsSubtypeSubstitutionMode, t1: Type, i2: IntersectionType): bool =
  for p2 in i2.parts:
    if not is_subtype_rec(substitution_mode, t1, p2):
      return false
  true

template tuple_subtypes_tuple(substitution_mode: IsSubtypeSubstitutionMode, t1: TupleType, t2: TupleType): bool =
  let es1 = t1.elements
  let es2 = t2.elements
  if es1.len != es2.len:
    return false

  for i in 0 ..< es1.len:
    if not is_subtype_rec(substitution_mode, es1[i], es2[i]):
      return false
  true

template shape_subtypes_shape(substitution_mode: IsSubtypeSubstitutionMode, s1: ShapeType, s2: ShapeType): bool =
  for property_name in s2.meta.property_names:
    if not s1.has_property(property_name):
      return false
    let p1_type = s1.get_property_type(property_name)
    let p2_type = s2.get_property_type(property_name)
    if not is_subtype_rec(substitution_mode, p1_type, p2_type):
      return false
  true

macro variable_subtypes_type(substitution_mode: static[IsSubtypeSubstitutionMode], tv1: TypeVariable, t2: Type): bool =
  case substitution_mode
  of IsSubtypeSubstitutionMode.None, IsSubtypeSubstitutionMode.T2:
    quote do:
      let tv1_evaluated = `tv1`
      assert(tv1_evaluated.parameter != nil)
      is_subtype(tv1_evaluated.parameter.upper_bound, `t2`)
  of IsSubtypeSubstitutionMode.T1:
    quote do:
      let tv1_evaluated = `tv1`
      let t1 = assignments[tv1_evaluated.index]
      is_subtype_substitute1(t1, `t2`, assignments)

macro type_subtypes_variable(substitution_mode: static[IsSubtypeSubstitutionMode], t1: Type, tv2: TypeVariable): bool =
  case substitution_mode
  of IsSubtypeSubstitutionMode.None, IsSubtypeSubstitutionMode.T1:
    quote do:
      let tv2_evaluated = `tv2`
      assert(tv2_evaluated.parameter != nil)
      is_subtype(`t1`, tv2_evaluated.parameter.lower_bound)
  of IsSubtypeSubstitutionMode.T2:
    quote do:
      let tv2_evaluated = `tv2`
      let t2 = assignments[tv2_evaluated.index]
      is_subtype_substitute2(`t1`, t2, assignments)

template is_subtype_impl(substitution_mode: IsSubtypeSubstitutionMode, t1: Type, t2: Type): bool =
  ## `is_subtype` has two separate versions, both covered by this implementation template: the first being the regular
  ## one, while the second one implicitly substitutes type variables in `t2` with types from an `assignments` array,
  ## without allocating any new types. This allows subtyping in specific contexts (fit, type bounds checking) to be
  ## decided without new allocations. This template assumes an implicit `assignments` variable to be in scope when
  ## `has_assignments` is true.

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
      let s1 = cast[ShapeType](t1)
      let s2 = cast[ShapeType](t2)
      return shape_subtypes_shape(substitution_mode, s1, s2)

  # TODO (vm): This case can be removed if symbol types are interned.
  of Kind.Symbol:
    if t2.kind == Kind.Symbol:
      let s1 = cast[SymbolType](t1)
      let s2 = cast[SymbolType](t2)
      return s1.name == s2.name

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
    let s2 = cast[SumType](t2)
    type_subtypes_sum(substitution_mode, t1, s2)

  of Kind.Intersection:
    # t1 is definitely NOT an intersection type, because the case IntersectionType/IntersectionType immediately returns
    # in the first `cast of` statement above. Hence, we can safely call `type_subtypes_intersection`.
    let i2 = cast[IntersectionType](t2)
    type_subtypes_intersection(substitution_mode, t1, i2)

  else: false

proc is_subtype*(t1: Type, t2: Type): bool =
  ## Whether `t1` is a subtype of `t2`.
  is_subtype_impl(IsSubtypeSubstitutionMode.None, t1, t2)

proc is_subtype_substitute1*(t1: Type, t2: Type, assignments: open_array[Type]): bool =
  ## Whether `t1` is a subtype of `t2` when type variables in `t1` are substituted with types from `assignments`. This
  ## function does *not* allocate new types for the substitution.
  is_subtype_impl(IsSubtypeSubstitutionMode.T1, t1, t2)

proc is_subtype_substitute2*(t1: Type, t2: Type, assignments: open_array[Type]): bool =
  ## Whether `t1` is a subtype of `t2` when type variables in `t2` are substituted with types from `assignments`. This
  ## function does *not* allocate new types for the substitution.
  is_subtype_impl(IsSubtypeSubstitutionMode.T2, t1, t2)

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
  ## from `assignments`. This function does *not* allocate a new tuple type or any types for the substitution.
  if ts1.len != ts2.len:
    return false

  for i in 0 ..< ts1.len:
    if not is_subtype_substitute1(ts1[i], ts2[i], assignments):
      return false
  true

proc is_subtype_substitute2*(ts1: open_array[Type], ts2: open_array[Type], assignments: open_array[Type]): bool =
  ## Whether a tuple type `tpl(ts1)` is a subtype of `tpl(ts2)` when type variables in `ts2` are substituted with types
  ## from `assignments`. This function does *not* allocate a new tuple type or any types for the substitution.
  if ts1.len != ts2.len:
    return false

  for i in 0 ..< ts1.len:
    if not is_subtype_substitute2(ts1[i], ts2[i], assignments):
      return false
  true

########################################################################################################################
# Fit.                                                                                                                 #
########################################################################################################################

type FitsAssignments = array[max_type_parameters, Type]
  ## Assignments are modeled as fixed-size arrays so that we can put type arguments on the stack before pushing them
  ## into an allocated ImSeq should the fit be successful.

proc fits_assign(t1: Type, t2: Type, assignments: var FitsAssignments): bool

proc fits*(t1: Type, t2: Type, parameters: ImSeq[TypeParameter]): ImSeq[Type] =
  ## Whether `t1` fits into `t2`. `fits` returns the list of assigned type arguments if true, or `nil` otherwise.
  ## `parameters` must contain all type parameters which variables in `t2` refer to, in the proper order.
  fits([t1], [t2], parameters)

proc fits*(ts1: open_array[Type], ts2: open_array[Type], parameters: ImSeq[TypeParameter]): ImSeq[Type] =
  ## Whether `ts1`, interpreted as the elements of a tuple type, fit into `ts2`. `fits` returns the list of assigned
  ## type arguments if true, or `nil` otherwise. `parameters` must contain all type parameters which variables in `t2`
  ## refer to, in the proper order.
  if ts1.len != ts2.len:
    return nil

  let length = ts1.len
  assert(parameters.len == length)

  var assignments: FitsAssignments
  for i in 0 ..< length:
    let t1 = ts1[i]
    let t2 = ts2[i]
    if not fits_assign(t1, t2, assignments):
      # Consistency constraint: All variable assignments must be unique. (Baked into `assign`.)
      return nil

  # Consistency constraint: All variable must have an assignment.
  for i in 0 ..< length:
    if assignments[i] == nil:
      return nil

  # Consistency constraint: All bounds must be kept.
  for i in 0 ..< length:
    let parameter = parameters[i]
    let tpe = assignments[i]
    if not bounds_contain(parameter, tpe, assignments):
      return nil

  # Final check: `t1` must be a subtype of `t2` when substituting assignments into `t2`.
  for i in 0 ..< length:
    if not is_subtype_substitute2(ts1[i], ts2[i], assignments):
      return nil

  # So far, we've used an array on the stack for the type assignments. We have to convert these to a heap-allocated
  # ImSeq now, so that they outlive the lifetime of this function call.
  new_immutable_seq(assignments, length)

proc fits_assign(t1: Type, t2: Type, assignments: var FitsAssignments): bool =
  ## Assigns all matching types in `t1` to type variables in `t2`, saving them in `assignments`. If an assignment
  ## already exists and the existing type and new type aren't equal, `assign` returns false to signal that `t1` cannot
  ## fit into `t2`. This trivially covers one consistency check case: that assignments must be unique.
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
    if t1.kind == Kind.Shape:
      let s1 = cast[ShapeType](t1)
      let s2 = cast[ShapeType](t2)
      for property_name in s2.meta.property_names:
        if not s1.has_property(property_name):
          return false
        if not fits_assign(s1.get_property_type(property_name), s2.get_property_type(property_name), assignments):
          return false
    true

  else: true

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

proc sum_simplified*(parts: open_array[Type]): Type
proc sum_simplified*(parts: ImSeq[Type]): Type = sum_simplified(to_open_array(parts))

proc intersection_simplified*(parts: open_array[Type]): Type
proc intersection_simplified*(parts: ImSeq[Type]): Type = intersection_simplified(to_open_array(parts))

template simplify_construct_covariant(kind: Kind, parts: open_array[Type]): untyped =
  if kind == Kind.Sum: sum_simplified(parts)
  elif kind == Kind.Intersection: intersection_simplified(parts)
  else: quit("Invalid kind for covariant construction.")

template simplify_construct_contravariant(kind: Kind, parts: open_array[Type]): untyped =
  if kind == Kind.Sum: intersection_simplified(parts)
  elif kind == Kind.Intersection: sum_simplified(parts)
  else: quit("Invalid kind for contravariant construction.")

proc add_unique(results: var StackSeq[32, Type], tpe: Type) =
  var is_unique = true
  for result_type in results:
    if are_equal(result_type, tpe):
      is_unique = false
      break

  if is_unique:
    results.add(tpe)

template simplify_categorize_type(tpe: Type, kind: Kind) =
  case tpe.kind
  of Kind.Tuple:
    let tuple_type = cast[TupleType](tpe)
    case tuple_type.elements.len
    of 0: results.add_unique(tpe)
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
      results.add_unique(tpe)
  else: results.add_unique(tpe)

template simplify_flatten(tpe: Type, T: untyped, kind: Kind, expected_kind: Kind): untyped =
  if kind == expected_kind:
    let xary_type = cast[T](tpe)
    for child in xary_type.parts:
      simplify_categorize_type(child, kind)
  else:
    results.add_unique(tpe)

proc simplify_tuples(
  types: var StackSeq[8, TupleType],
  results: var StackSeq[32, Type],
  kind: Kind,
) =
  if types.len == 0:
    return
  elif types.len == 1:
    # This is guaranteed to be the only tuple of size 1, so we don't need `add_unique`.
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
  # We have to ensure that each type in `results` is unique. This does not apply to tuples, functions, lists, and
  # shapes because these types are later combined in such a way that each result type must be unique.
  var tuples1: StackSeq[8, TupleType]
  var tuples2: StackSeq[8, TupleType]
  var tuples3: StackSeq[8, TupleType]
  var tuplesX: StackSeq[8, TupleType]
  var functions: StackSeq[8, FunctionType]
  var lists: StackSeq[8, ListType]
  var shapes: StackSeq[8, ShapeType]
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
      for name in shape_type.meta.property_names:
        property_names.add(name)

    let meta_shape = get_meta_shape_safe(property_names)
    var property_types = new_immutable_seq[Type](meta_shape.property_count)
    var property_type_parts = new_seq_of_cap[Type](8)
    for i in 0 ..< property_types.len:
      let property_name = meta_shape.property_names[i]
      for shape_type in shapes:
        if shape_type.has_property(property_name):
          property_type_parts.add(shape_type.get_property_type(property_name))
      property_types[i] = simplify_construct_covariant(kind, property_type_parts)
      property_type_parts.set_len(0)

    results.add(new_shape_type(meta_shape, property_types))

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
  ## variance, and filtered for the most general/specific types.
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
  ## their variance, and filtered for the most general/specific types.
  ##
  ## If a subterm is covariant, the type candidates are combined into an intersection type. If a subterm is
  ## contravariant, the sum is used instead. Invariant subterms cannot be combined, meaning types with invariant
  ## subterms are left as is.
  ##
  ## The following kinds of types will be simplified: Tuples of the same size, functions, lists, shapes, and declared
  ## types of the same schema.
  simplify(Kind.Intersection, parts)

########################################################################################################################
# Substitution.                                                                                                        #
########################################################################################################################

proc substitute_optimized(tpe: Type, type_arguments: open_array[Type]): Type
proc substitute_multiple_optimized(types: ImSeq[Type], type_arguments: open_array[Type]): ImSeq[Type]

proc substitute*(tpe: Type, type_arguments: open_array[Type]): Type =
  ## Substitutes any type variables in `tpe` with the given type arguments, creating a new type.
  let res = substitute_optimized(tpe, type_arguments)
  if res != nil: res
  else: tpe

proc substitute*(tpe: Type, type_arguments: ImSeq[Type]): Type =
  ## Substitutes any type variables in `tpe` with the given type arguments, creating a new type.
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

  else:
    quit(fmt"Type substitution has not been implemented for kind {tpe.kind}.")

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
  else: "unknown"

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
