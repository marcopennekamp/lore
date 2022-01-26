import std/strformat, std/strutils

import imseqs
import property_index
from types import Kind, MetaShape, Type, FunctionType, ShapeType, Schema, StructSchema, StructType, property_count,
                  open_property_count, has_open_properties, get_schema
from utils import call_if_any_exists

# TODO (vm/schemas): We should be able to just `import types`. I'm currently avoiding this because functions like `list`
#                    and `map` are named too generally. We should rename these to `new_list_type` and `new_map_type`.

type
  TaggedValue* = distinct uint64
    ## A TaggedValue is a compact representation of a Lore value. Every Lore value needs to carry type information at
    ## run time, including the primitives Int, Real, and Boolean. This is because at times, the compiler might
    ## encounter a sum type such as `Int | Real` or `Real | #error`. Such a type might stand in a list, or as a
    ## parameter, or anywhere else. Especially in storage situations (such as lists), we want to tag values as
    ## efficiently as possible without the need to create separate List implementations for each primitive.
    ##
    ## We can keep the size of TaggedValue to 8 bytes with a technique called pointer tagging. Essentially, on 64-bit
    ## systems the lower three bits of a pointer are always 0 due to alignment, so we can use these bits to tag Ints,
    ## Booleans, and references. This reduces the range of an Int, for example, from 64 bits to 61 bits, but we don't
    ## anticipate any issues with that in the foreseeable future. There are no plans to port Lore to 32-bit systems.
    ##
    ## There is a slight performance cost associated with TaggedValues, as every time we want to use an Int, we have to
    ## untag it using a shift. Every time we want to create an Int, we have to shift the value and OR the tag bits.
    ## Reference access is free, as the reference tag is 0. Booleans don't need shifts, because there are only two
    ## possible values: `0b0011` for false and `0b1011` for true, which includes the tag bits.
    ##
    ## Strings must be represented as reference Values, because a string's reference must be discoverable by the
    ## garbage collector. If we tag a string, its reference is going to be obfuscated and the string might be collected
    ## prematurely.
    ##
    ## Reals are currently represented as Values, because the only technique for putting a 64-bit float into a 64-bit
    ## value AND still be able to tag it is nan-boxing, which is not future-proof.

  Value* {.inheritable, pure.} = ref object
    ## The pragmas `inheritable` and `pure` omit the `m_type` pointer from each value instance, as `tpe` already
    ## sufficiently distinguishes between value kinds.
    tpe*: Type

  RealValue* {.pure.} = ref object of Value
    ## Real values are currently boxed, because a 64-bit float doesn't fit into a 64-bit tagged pointer. The technique
    ## to use would be nanboxing, but that is not quite future-proof because it relies on pointers only using the lower
    ## 48 bits.
    ##
    ## There are other Real optimizations which we may try first. For example, we can introduce specific operations
    ## which work with unboxed Reals. If a function parameter is definitely a Real, we also don't need to pass the
    ## boxed value to it, nor require the function itself to work with a boxed value. These and other optimizations may
    ## relieve us of the burden of nanboxing.
    real*: float64

  # TODO (vm): Rethink string handling: Nim strings are mutable, so we can't just throw Nim strings around. We might
  #            have to implement our own string type. For now, this can work, though.
  #            This would have the added benefit of saving one allocation. Right now, we're allocating the StringValue,
  #            which allocates the string, which allocates the character array. With our own string type, we could bake
  #            the fields saved in `string` into StringValue.
  StringValue* {.pure, shallow.} = ref object of Value
    string*: string

  # TODO (vm): We can optimize this by introducing special types for small tuples, such as Tuple2 and Tuple3, which
  #            would be 24 and 32 bytes large due to alignment. This will remove one layer of pointer indirection and
  #            save an allocation, but obviously complicate the code.
  TupleValue* {.pure, shallow.} = ref object of Value
    elements*: ImSeq[TaggedValue]

  FunctionValue* {.pure, shallow.} = ref object of Value
    ## A FunctionValue is either a fixed function value (pointing to a fixed function or a lambda) or a multi-function
    ## value. This is determined by the flag `is_fixed`. The implementation of the `target` is either a
    ## `ptr FunctionInstance` or a `MultiFunction` reference. The actual type is hidden inside the module `definitions`
    ## to avoid cyclic dependencies between Nim modules.
    is_fixed*: bool
    target*: pointer

  ListValue* {.pure, shallow.} = ref object of Value
    elements*: ImSeq[TaggedValue]

  ShapeValue* {.pure, shallow.} = ref object of Value
    meta*: MetaShape
    property_values*: UncheckedArray[TaggedValue]

  SymbolValue* {.pure, shallow.} = ref object of Value
    name*: string

  StructValue* {.pure, shallow.} = ref object of Value
    property_values*: UncheckedArray[TaggedValue]

const
  TagMask: uint64 = 0b111
    ## This mask can filter out the lowest three tag bits of a pointer.
  TagReference*: uint64 = 0b000
  TagInt*: uint64 = 0b001
  TagBoolean*: uint64 = 0b010
  False*: TaggedValue = TaggedValue(0 or TagBoolean)
  True*: TaggedValue = TaggedValue((1 shl 3) or TagBoolean)

proc `==`(v1: TaggedValue, v2: TaggedValue): bool =
  uint64(v1) == uint64(v2)

# This is called `get_type` so it doesn't clash with Nim's system function `typeof`.
proc get_type*(value: TaggedValue): Type

########################################################################################################################
# Primitives.                                                                                                          #
########################################################################################################################

proc get_tag*(value: TaggedValue): uint64 = value.uint and TagMask

proc is_reference*(value: TaggedValue): bool = get_tag(value) == TagReference
proc is_int*(value: TaggedValue): bool = get_tag(value) == TagInt
proc is_boolean*(value: TaggedValue): bool = get_tag(value) == TagBoolean

proc tag_reference*(value: Value): TaggedValue = cast[TaggedValue](value)
proc untag_reference*(value: TaggedValue): Value = cast[Value](value)
template untag_reference*(value: TaggedValue, tpe: untyped): untyped = cast[tpe](untag_reference(value))

proc tag_int*(value: int64): TaggedValue = cast[TaggedValue]((value shl 3) or cast[int64](TagInt))
proc untag_int*(value: TaggedValue): int64 = cast[int64](value) shr 3

proc tag_boolean*(value: bool): TaggedValue = (if value: True else: False)
proc untag_boolean*(value: TaggedValue): bool = value == True

proc new_real*(value: float64): Value = RealValue(tpe: types.real_type, real: value)
proc new_real_tagged*(value: float64): TaggedValue = tag_reference(new_real(value))

########################################################################################################################
# Strings.                                                                                                             #
########################################################################################################################

proc new_string*(value: string): Value =
  ## Note that the resulting StringValue's `string` will be a shallow copy of `value`. If `value` is subsequently
  ## modified, `string` will change as well. This is a valid optimization because `new_string` will usually be called
  ## with a fresh string value.
  let string_value = StringValue(tpe: types.string_type)
  shallow_copy(string_value.string, value)
  string_value

proc new_string_tagged*(value: string): TaggedValue = tag_reference(new_string(value))

########################################################################################################################
# Tuples.                                                                                                              #
########################################################################################################################

proc new_tuple*(elements: ImSeq[TaggedValue], tpe: Type): Value =
  ## Creates a new tuple, forcing its type to be `tpe` instead of taking the type from the elements.
  TupleValue(tpe: tpe, elements: elements)

proc new_tuple_tagged*(elements: ImSeq[TaggedValue], tpe: Type): TaggedValue = tag_reference(new_tuple(elements, tpe))

proc new_tuple*(elements: ImSeq[TaggedValue]): Value =
  let length = elements.len
  var element_types = new_immutable_seq[Type](length)
  for i in 0 ..< length:
    element_types[i] = get_type(elements[i])
  new_tuple(elements, types.tpl(element_types))

proc new_tuple_tagged*(elements: ImSeq[TaggedValue]): TaggedValue = tag_reference(new_tuple(elements))

let unit*: TaggedValue = new_tuple_tagged(empty_immutable_seq[TaggedValue]())

########################################################################################################################
# Functions.                                                                                                           #
########################################################################################################################

proc new_function*(is_fixed: bool, target: pointer, tpe: Type): Value = FunctionValue(tpe: tpe, is_fixed: is_fixed, target: target)

proc new_function_tagged*(is_fixed: bool, target: pointer, tpe: Type): TaggedValue = tag_reference(new_function(is_fixed, target, tpe))

proc arity*(function: FunctionValue): int = cast[FunctionType](function.tpe).input.elements.len

########################################################################################################################
# Lists.                                                                                                               #
########################################################################################################################

proc new_list*(elements: ImSeq[TaggedValue], tpe: Type): Value = ListValue(tpe: tpe, elements: elements)

proc new_list_tagged*(elements: ImSeq[TaggedValue], tpe: Type): TaggedValue = tag_reference(new_list(elements, tpe))

########################################################################################################################
# Shapes.                                                                                                              #
########################################################################################################################

proc property_count*(shape: ShapeValue): int = shape.meta.property_names.len

proc alloc_shape_value(meta_shape: MetaShape): ShapeValue =
  ## Allocates a new shape value with the correct number of property values, which must be initialized after. The
  ## value's type also must be set!
  let shape_value = cast[ShapeValue](alloc0(sizeof(ShapeValue) + meta_shape.property_names.len * sizeof(TaggedValue)))
  shape_value.meta = meta_shape
  shape_value

proc copy_shape_value(shape: ShapeValue): ShapeValue =
  ## Copies a shape value AND its type. The type is copied because this operation is used to copy and then mutate a
  ## a shape value and type. The type will have to change if property values are changed subsequently.
  let res = alloc_shape_value(shape.meta)
  res.tpe = types.copy_shape_type(cast[ShapeType](shape.tpe))
  for i in 0 ..< shape.property_count:
    res.property_values[i] = shape.property_values[i]
  res

proc new_shape_value*(meta_shape: MetaShape, property_values: open_array[TaggedValue]): ShapeValue =
  ## Creates a new shape value. The property values must be in the correct order as defined by the meta shape.
  # TODO (vm/hash): Don't forget to hash the shape type after its property types are set!
  let shape_value = alloc_shape_value(meta_shape)
  var property_types = new_immutable_seq[Type](meta_shape.property_count)
  for i in 0 ..< meta_shape.property_count:
    let property_value = property_values[i]
    shape_value.property_values[i] = property_value
    property_types[i] = get_type(property_value)

  shape_value.tpe = types.new_shape_type(meta_shape, property_types)
  shape_value

proc new_shape_value_tagged*(meta_shape: MetaShape, property_values: open_array[TaggedValue]): TaggedValue =
  tag_reference(new_shape_value(meta_shape, property_values))

proc get_property_value*(shape: ShapeValue, name: string): TaggedValue =
  ## Gets the value associated with the property named `name`. The name must be a valid property.
  shape.property_values[shape.meta.property_index.find_offset(name)]

########################################################################################################################
# Symbols.                                                                                                             #
########################################################################################################################

proc new_symbol*(name: string): Value = SymbolValue(tpe: types.symbol(name), name: name)

proc new_symbol_tagged*(name: string): TaggedValue = tag_reference(new_symbol(name))

########################################################################################################################
# Structs.                                                                                                             #
########################################################################################################################

proc alloc_struct_value(schema: StructSchema): StructValue =
  ## Allocates a new struct value with the correct number of property values, which must be initialized after. The
  ## value's type also must be set!
  let shape_value = cast[StructValue](alloc0(sizeof(StructValue) + schema.property_count * sizeof(TaggedValue)))
  shape_value

proc new_struct_value*(schema: StructSchema, type_arguments: ImSeq[Type], property_values: open_array[TaggedValue]): StructValue =
  ## Creates a new struct value. The property values must be in the correct order as defined by the struct's schema.
  let value = alloc_struct_value(schema)
  let open_property_types =
    if schema.has_open_properties:
      var types = new_immutable_seq[Type](schema.open_property_count)
      for i in 0 ..< schema.open_property_count:
        types[i] = get_type(property_values[schema.open_property_indices[i]])
      types
    else: nil

  copy_mem(addr value.property_values, unsafe_addr property_values, schema.property_count * sizeof(TaggedValue))
  value.tpe = types.instantiate_struct_schema(schema, type_arguments, open_property_types)
  value

proc new_struct_value_tagged*(schema: Schema, type_arguments: ImSeq[Type], property_values: open_array[TaggedValue]): TaggedValue =
  if schema.kind != Kind.Struct:
    quit(fmt"Cannot construct a struct value from a trait schema {schema.name}.")
  tag_reference(new_struct_value(cast[StructSchema](schema), type_arguments, property_values))

proc get_property_value*(struct: StructValue, name: string): TaggedValue =
  ## Gets the value associated with the property `name`. The name must be a valid property.
  let tpe = cast[StructType](struct.tpe)
  struct.property_values[tpe.get_schema.property_index.find_offset(name)]

########################################################################################################################
# Value types.                                                                                                         #
########################################################################################################################

proc get_type*(value: TaggedValue): Type =
  let tag = get_tag(value)
  if tag == TagReference:
    let ref_value = untag_reference(value)
    if ref_value != nil: ref_value.tpe else: types.any_type
  elif tag == TagInt:
    types.int_type
  elif tag == TagBoolean:
    types.boolean_type
  else:
    quit(fmt"Unknown tag {tag}.")

########################################################################################################################
# Substitution.                                                                                                        #
########################################################################################################################

proc substitute_optimized(value: TaggedValue, type_arguments: ImSeq[Type]): TaggedValue
proc substitute_multiple_optimized(values: ImSeq[TaggedValue], type_arguments: ImSeq[Type]): ImSeq[TaggedValue]

proc substitute_types*(value: TaggedValue, type_arguments: ImSeq[Type]): TaggedValue =
  ## Substitutes any type variables in `value`'s type with the given type arguments, returning a new value and leaving
  ## `value` as is. This applies recursively to any sub-values in `value`.
  let res = substitute_optimized(value, type_arguments)

  # `substitute_optimized` will always return a reference. Hence, we can untag the reference here and check for `nil`.
  # If `value` was a primitive, `nil` will also have been returned in this form.
  if untag_reference(res) != nil: res
  else: value

proc substitute_optimized(value: TaggedValue, type_arguments: ImSeq[Type]): TaggedValue =
  ## Substitutes any type variables in `value`'s type with the given type arguments. If no substitutions occur, the
  ## function returns `nil`. This allows it to only allocate new values should a child value have changed.
  if not is_reference(value):
    return tag_reference(nil)

  let reference = untag_reference(value)
  let tpe = reference.tpe

  case tpe.kind
  of Kind.Real: tag_reference(nil)

  of Kind.Tuple:
    let value = cast[TupleValue](reference)
    let new_elements = substitute_multiple_optimized(value.elements, type_arguments)
    if new_elements != nil: new_tuple_tagged(new_elements)
    else: tag_reference(nil)

  of Kind.Function:
    let value = cast[FunctionValue](reference)
    let new_type = types.substitute(value.tpe, type_arguments)
    if new_type != nil: new_function_tagged(value.is_fixed, value.target, new_type)
    else: tag_reference(nil)

  of Kind.List:
    let value = cast[ListValue](reference)
    var new_elements = substitute_multiple_optimized(value.elements, type_arguments)
    var new_type = types.substitute(value.tpe, type_arguments)
    call_if_any_exists(new_list_tagged, new_elements, value.elements, new_type, value.tpe, tag_reference(nil))

  of Kind.Shape:
    let value = cast[ShapeValue](reference)

    # TODO (vm/hash): Don't forget to hash the shape value's type after all its types have been assigned.
    var result_value: ShapeValue = nil

    for i in 0 ..< value.property_count:
      let candidate = substitute_optimized(value.property_values[i], type_arguments)
      if untag_reference(candidate) != nil:
        if result_value == nil:
          result_value = copy_shape_value(value)
        result_value.property_values[i] = candidate
        # `copy_shape_value` produces a copy of the shape's type, so this mutation is fine.
        cast[ShapeType](result_value.tpe).property_types[i] = get_type(candidate)

    tag_reference(result_value)

  of Kind.Symbol: tag_reference(nil)

  else: quit(fmt"Values of type kind {reference.tpe.kind} cannot exist.")

proc substitute_multiple_optimized(values: ImSeq[TaggedValue], type_arguments: ImSeq[Type]): ImSeq[TaggedValue] =
  var results: ImSeq[TaggedValue] = nil

  let length = values.len
  for i in 0 ..< length:
    let candidate = substitute_optimized(values[i], type_arguments)
    if untag_reference(candidate) != nil:
      if results == nil:
        results = new_immutable_seq(values)
      results[i] = candidate

  results

########################################################################################################################
# Stringification.                                                                                                     #
########################################################################################################################

func `$`*(value: Value): string

func `$`*(tagged_value: TaggedValue): string =
  let tag = get_tag(tagged_value)
  if tag == TagReference:
    let value = untag_reference(tagged_value)
    if value != nil:
      $value
    else:
      "nil"
  elif tag == TagInt:
    $untag_int(tagged_value)
  elif tag == TagBoolean:
    if tagged_value == True: "true"
    else: "false"
  else:
    "unknown"

func `$`*(tagged_values: seq[TaggedValue]): string = tagged_values.join(", ")

func `$`*(value: Value): string =
  case value.tpe.kind
  of Kind.Real: $cast[RealValue](value).real
  of Kind.String: cast[StringValue](value).string
  of Kind.Tuple:
    let tpl = cast[TupleValue](value)
    "(" & $tpl.elements.join(", ") & ")"
  of Kind.Function:
    # TODO (vm): We should print the name instead of the target address, but this is non-trivial when avoiding cyclic dependencies.
    let function = cast[FunctionValue](value)
    if function.is_fixed:
      "<fixed function: " & $cast[uint](function.target) & ">"
    else:
      "<multi-function: " & $cast[uint](function.target) & ">"
  of Kind.List:
    let list = cast[ListValue](value)
    "[" & $list.elements.join(", ") & "]"
  of Kind.Shape:
    let shape = cast[ShapeValue](value)
    var properties = new_seq[string]()
    for i in 0 ..< shape.property_count:
      properties.add(shape.meta.property_names[i] & ": " & $shape.property_values[i])
    "%{ " & properties.join(", ") & " }"
  of Kind.Symbol:
    let symbol = cast[SymbolValue](value)
    "#" & symbol.name
  else: "unknown"
