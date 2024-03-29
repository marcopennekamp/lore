import std/strformat
import std/strutils
import std/sugar

import imseqs
import property_index
import types

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

  # TODO (vm): The `name` property is redundant, as it's already known via the type.
  SymbolValue* {.pure, shallow.} = ref object of Value
    name*: string

  # TODO (vm): We can optimize this by introducing special types for small tuples, such as Tuple2 and Tuple3, which
  #            would be 24 and 32 bytes large due to alignment. This will remove one layer of pointer indirection and
  #            save an allocation, but obviously complicate the code.
  TupleValue* {.pure, shallow.} = ref object of Value
    elements*: ImSeq[TaggedValue]

  FunctionValue* {.pure, shallow.} = ref object of Value
    variant*: FunctionValueVariant
    target*: pointer
      ## A `MultiFunction` reference if `variant` is `Multi` and a `ptr FunctionInstance` otherwise. The actual type is
      ## hidden inside the module `definitions` to avoid cyclic dependencies between Nim modules.

  FunctionValueVariant* {.pure.} = enum
    Multi
    Single
      ## A single function value can represent a direct single function, a fixed function, or a lambda function. A
      ## lambda function instance may additionally have an associated context if it captures any variables.

  ListValue* {.pure, shallow.} = ref object of Value
    elements*: ImSeq[TaggedValue]

  ShapeValue* = ref ShapeValueObj
  ShapeValueObj {.pure, shallow.} = object of Value
    meta*: MetaShape
    property_values*: UncheckedArray[TaggedValue]

  StructValue* = ref StructValueObj
  StructValueObj {.pure, shallow.} = object of Value
    property_values*: UncheckedArray[TaggedValue]

  IntrospectionTypeValue* {.pure.} = ref object of Value
    ## The backing value of the `lore.core.Type` trait. The value's `tpe` must be the initialized backing struct type.
    boxed_type*: Type
      ## The type that the introspection type value encapsulates.

const
  TagMask: uint64 = 0b111
    ## This mask can filter out the lowest three tag bits of a pointer.
  TagReference: uint64 = 0b000
  TagInt: uint64 = 0b001
  TagBoolean: uint64 = 0b010
  False: TaggedValue = TaggedValue(0 or TagBoolean)
  True: TaggedValue = TaggedValue((1 shl 3) or TagBoolean)

proc `===`*(v1: TaggedValue, v2: TaggedValue): bool {.inline.} = uint64(v1) == uint64(v2)
proc `!==`*(v1: TaggedValue, v2: TaggedValue): bool {.inline.} = not (v1 === v2)

proc `===`*(v1: Value, v2: Value): bool {.inline.} = cast[pointer](v1) == cast[pointer](v2)
proc `!==`*(v1: Value, v2: Value): bool {.inline.} = not (v1 === v2)

proc get_type*(value: TaggedValue): Type
proc get_kind*(value: TaggedValue): Kind

proc are_equal*(v1: TaggedValue, v2: TaggedValue, rec: (TaggedValue, TaggedValue) -> bool): bool
proc is_less_than*(v1: TaggedValue, v2: TaggedValue, rec: (TaggedValue, TaggedValue) -> bool): bool

proc stringify*(tagged_value: TaggedValue, rec: TaggedValue -> string): string
proc stringify*(value: Value, rec: TaggedValue -> string): string

proc `$`*(tagged_value: TaggedValue): string
proc `$`*(value: Value): string

########################################################################################################################
# Tags.                                                                                                                #
########################################################################################################################

proc get_tag*(value: TaggedValue): uint64 = cast[uint64](value) and TagMask

proc is_reference*(value: TaggedValue): bool = get_tag(value) == TagReference
proc is_int*(value: TaggedValue): bool = get_tag(value) == TagInt
proc is_boolean*(value: TaggedValue): bool = get_tag(value) == TagBoolean

proc tag_reference*(value: Value): TaggedValue = cast[TaggedValue](value)
proc untag_reference*(value: TaggedValue): Value = cast[Value](value)
template untag_reference*(value: TaggedValue, tpe: untyped): untyped = cast[tpe](untag_reference(value))

proc tag_int*(value: int64): TaggedValue = cast[TaggedValue]((value shl 3) or cast[int64](TagInt))
proc untag_int*(value: TaggedValue): int64 = cast[int64](value) shr 3

proc tag_boolean*(value: bool): TaggedValue = (if value: True else: False)
proc untag_boolean*(value: TaggedValue): bool = value === True

proc is_nil_reference*(value: TaggedValue): bool = is_reference(value) and untag_reference(value) === nil

########################################################################################################################
# Reals.                                                                                                               #
########################################################################################################################

proc new_real_value*(value: float64): Value = RealValue(tpe: real_type, real: value)
proc new_real_value_tagged*(value: float64): TaggedValue = tag_reference(new_real_value(value))

########################################################################################################################
# Strings.                                                                                                             #
########################################################################################################################

proc new_string_value*(value: string): Value =
  ## Note that the resulting StringValue's `string` will be a shallow copy of `value`. If `value` is subsequently
  ## modified, `string` will change as well. This is a valid optimization because `new_string` will usually be called
  ## with a fresh string value.
  let string_value = StringValue(tpe: string_type)
  shallow_copy(string_value.string, value)
  string_value

proc new_string_value_tagged*(value: string): TaggedValue = tag_reference(new_string_value(value))

########################################################################################################################
# Symbols.                                                                                                             #
########################################################################################################################

# TODO (vm): Intern symbol values.
proc new_symbol_value*(name: string): Value = SymbolValue(tpe: new_symbol_type(name), name: name)

proc new_symbol_value_tagged*(name: string): TaggedValue = tag_reference(new_symbol_value(name))

########################################################################################################################
# Tuples.                                                                                                              #
########################################################################################################################

let unit_value*: Value = TupleValue(tpe: unit_type, elements: empty_immutable_seq[TaggedValue]())
let unit_value_tagged*: TaggedValue = tag_reference(unit_value)

proc new_tuple_value*(elements: ImSeq[TaggedValue], tpe: Type): Value =
  ## Creates a new tuple, forcing its type to be `tpe` instead of taking the type from the elements.
  if elements.len == 0:
    return unit_value
  TupleValue(tpe: tpe, elements: elements)

proc new_tuple_value_tagged*(elements: ImSeq[TaggedValue], tpe: Type): TaggedValue = tag_reference(new_tuple_value(elements, tpe))

proc new_tuple_value*(elements: ImSeq[TaggedValue]): Value =
  let length = elements.len
  var element_types = new_immutable_seq[Type](length)
  for i in 0 ..< length:
    element_types[i] = elements[i].get_type
  new_tuple_value(elements, new_tuple_type(element_types))

proc new_tuple_value_tagged*(elements: ImSeq[TaggedValue]): TaggedValue = tag_reference(new_tuple_value(elements))

########################################################################################################################
# Functions.                                                                                                           #
########################################################################################################################

proc new_function_value*(variant: FunctionValueVariant, target: pointer, tpe: Type): Value =
  FunctionValue(tpe: tpe, variant: variant, target: target)

proc new_multi_function_value*(target: pointer, tpe: Type): Value =
  new_function_value(FunctionValueVariant.Multi, target, tpe)

proc new_single_function_value*(target: pointer, tpe: Type): Value =
  new_function_value(FunctionValueVariant.Single, target, tpe)

proc arity*(function: FunctionValue): int = cast[FunctionType](function.tpe).input.elements.len

########################################################################################################################
# Lists.                                                                                                               #
########################################################################################################################

proc new_list_value*(elements: ImSeq[TaggedValue], tpe: Type): Value = ListValue(tpe: tpe, elements: elements)

proc new_list_value_tagged*(elements: ImSeq[TaggedValue], tpe: Type): TaggedValue = tag_reference(new_list_value(elements, tpe))

proc element_type*(list: ListValue): Type = cast[ListType](list.tpe).element

########################################################################################################################
# Shapes.                                                                                                              #
########################################################################################################################

proc property_count*(shape: ShapeValue): int = shape.meta.property_names.len

proc alloc_shape_value(meta_shape: MetaShape): ShapeValue =
  ## Allocates a new shape value with the correct number of property values, which must be initialized after. The
  ## value's type also must be set!
  let shape_value = cast[ShapeValue](alloc0(sizeof(ShapeValueObj) + meta_shape.property_names.len * sizeof(TaggedValue)))
  shape_value.meta = meta_shape
  shape_value

proc new_shape_value*(meta_shape: MetaShape, property_values: open_array[TaggedValue]): ShapeValue =
  ## Creates a new shape value. The property values must be in the correct order as defined by the meta shape.
  # TODO (vm/hash): Don't forget to hash the shape type after its property types are set!
  let shape_value = alloc_shape_value(meta_shape)
  var property_types = new_immutable_seq[Type](meta_shape.property_count)
  for i in 0 ..< meta_shape.property_count:
    let property_value = property_values[i]
    shape_value.property_values[i] = property_value
    property_types[i] = property_value.get_type

  shape_value.tpe = new_shape_type(meta_shape, property_types)
  shape_value

proc new_shape_value_tagged*(meta_shape: MetaShape, property_values: open_array[TaggedValue]): TaggedValue =
  tag_reference(new_shape_value(meta_shape, property_values))

proc get_property_value*(shape: ShapeValue, name: string): TaggedValue =
  ## Gets the value associated with the property named `name`. The name must be a valid property.
  shape.property_values[shape.meta.property_index.find_offset(name)]

proc get_property_value_if_exists*(shape: ShapeValue, name: string): TaggedValue =
  ## Gets the type of the property `name`. If the property does not exist, this function returns a tagged `nil`.
  let offset = shape.meta.property_index.find_offset_if_exists(name)
  if offset >= 0:
    shape.property_values[offset]
  else:
    tag_reference(nil)

let empty_meta_shape*: MetaShape = get_meta_shape([])

let empty_shape*: TaggedValue = new_shape_value_tagged(empty_meta_shape, [])

########################################################################################################################
# Structs.                                                                                                             #
########################################################################################################################

proc get_open_property_types*(schema: StructSchema, property_values: open_array[TaggedValue]): ImSeq[Type]

proc alloc_struct_value(schema: StructSchema): StructValue =
  ## Allocates a new struct value with the correct number of property values, which must be initialized after. The
  ## value's type also must be set!
  cast[StructValue](alloc0(sizeof(StructValueObj) + schema.property_count * sizeof(TaggedValue)))

proc copy_struct_properties(value: StructValue, schema: StructSchema, property_values: open_array[TaggedValue]) =
  ## Copies the given property values into the struct value.
  copy_mem(
    addr value.property_values,
    unsafe_addr property_values,
    schema.property_count * sizeof(TaggedValue),
  )

proc new_struct_value*(schema: StructSchema, type_arguments: ImSeq[Type], property_values: open_array[TaggedValue]): StructValue =
  ## Creates a new struct value. The property values must be in the correct order as defined by the struct's schema.
  let open_property_types = get_open_property_types(schema, property_values)
  let value = alloc_struct_value(schema)
  value.tpe = instantiate_struct_schema(schema, type_arguments, open_property_types)
  copy_struct_properties(value, schema, property_values)
  value

proc new_struct_value_tagged*(schema: Schema, type_arguments: ImSeq[Type], property_values: open_array[TaggedValue]): TaggedValue =
  if schema.kind != Kind.Struct:
    quit(fmt"Cannot construct a struct value from a trait schema {schema.name}.")
  tag_reference(new_struct_value(cast[StructSchema](schema), type_arguments, property_values))

proc new_struct_value*(tpe: StructType, property_values: open_array[TaggedValue]): StructValue =
  ## Creates a new struct value from the given struct type and property values. If the struct has open properties, a
  ## new struct type will be instantiated. Otherwise, `tpe` will directly be used as the struct value's type.
  let schema = tpe.get_schema
  let open_property_types = get_open_property_types(schema, property_values)
  let new_type =
    if open_property_types !== nil: instantiate_struct_schema(tpe, open_property_types)
    else: tpe

  let value = alloc_struct_value(schema)
  value.tpe = new_type
  copy_struct_properties(value, schema, property_values)
  value

proc get_open_property_types*(schema: StructSchema, property_values: open_array[TaggedValue]): ImSeq[Type] =
  ## Returns the open property types for the given schema and property values, or `nil` if the schema has no open
  ## properties.
  if schema.has_open_properties:
    var types = new_immutable_seq[Type](schema.open_property_count)
    for i in 0 ..< schema.open_property_count:
      types[i] = property_values[schema.open_property_indices[i]].get_type
    types
  else: nil

proc struct_type*(struct: StructValue): StructType {.inline.} = cast[StructType](struct.tpe)

proc get_schema*(struct: StructValue): StructSchema {.inline.} = struct.struct_type.get_schema

proc property_count*(struct: StructValue): int {.inline.} = struct.get_schema.properties.len

proc get_property_value*(struct: StructValue, name: string): TaggedValue =
  ## Gets the value associated with the property `name`. The name must be a valid property.
  struct.property_values[struct.get_schema.property_index.find_offset(name)]

proc set_property_value*(struct: StructValue, name: string, value: TaggedValue) =
  ## Sets the value of the property `name` to `value`. The name must be a valid property. Open properties should NOT be
  ## mutated, but this is not enforced by `set_property_value`.
  struct.property_values[struct.get_schema.property_index.find_offset(name)] = value

########################################################################################################################
# Combined property functions.                                                                                         #
########################################################################################################################

proc get_property_value*(instance: TaggedValue, name: string): TaggedValue =
  ## Gets the value associated with the property `name`. The name must be a valid property. If `instance` is not a
  ## shape or struct, the function will panic.
  let tpe = instance.get_type
  case tpe.kind
  of Kind.Shape: untag_reference(instance, ShapeValue).get_property_value(name)
  of Kind.Struct: untag_reference(instance, StructValue).get_property_value(name)
  else: quit(fmt"Cannot get a property {name} from a value with kind {tpe.kind}.")

########################################################################################################################
# Value types and kinds.                                                                                                         #
########################################################################################################################

proc get_type*(value: TaggedValue): Type =
  ## Returns the type of `value`. This function is called `get_type` so it doesn't clash with Nim's system function
  ## `typeof`.
  let tag = get_tag(value)
  if tag == TagReference: untag_reference(value).tpe
  elif tag == TagInt: int_type
  else: boolean_type

proc get_kind*(value: TaggedValue): Kind =
  ## Returns the kind of `value`. This function avoids pointer indirection for Int and Boolean values compared to
  ## calling `get_type(value).kind`.
  let tag = get_tag(value)
  if tag == TagReference: untag_reference(value).tpe.kind
  elif tag == TagInt: Kind.Int
  else: Kind.Boolean

########################################################################################################################
# Value equality and comparison.                                                                                       #
########################################################################################################################

proc are_equal*(v1: TaggedValue, v2: TaggedValue, rec: (TaggedValue, TaggedValue) -> bool): bool =
  ## Whether `v1` and `v2` are equal under the rules of default equality, using `rec` to compare sub-values. This
  ## function is the default implementation for `lore.core.equal?`.
  # The first case covers the equality of Int and Boolean values, as well as strict equality for references.
  if v1 === v2: true
  elif is_reference(v1) or is_reference(v2):
    let kind1 = v1.get_kind
    let kind2 = v2.get_kind
    if kind1 == kind2:
      case kind1
      of Kind.Real:
        untag_reference(v1, RealValue).real == untag_reference(v2, RealValue).real
      of Kind.String:
        untag_reference(v1, StringValue).string == untag_reference(v2, StringValue).string
      of Kind.Symbol:
        # TODO (vm): Once symbol values are interned, this case is already covered by the `v1 === v2` check above.
        untag_reference(v1, SymbolValue).name == untag_reference(v2, SymbolValue).name
      of Kind.Tuple:
        let v1 = untag_reference(v1, TupleValue)
        let v2 = untag_reference(v2, TupleValue)
        if v1.elements.len != v2.elements.len:
          return false
        for i in 0 ..< v1.elements.len:
          if not rec(v1.elements[i], v2.elements[i]):
            return false
        true
      of Kind.List:
        let v1 = untag_reference(v1, ListValue)
        let v2 = untag_reference(v2, ListValue)
        if v1.elements.len != v2.elements.len:
          return false
        for i in 0 ..< v1.elements.len:
          if not rec(v1.elements[i], v2.elements[i]):
            return false
        true
      of Kind.Shape:
        let v1 = untag_reference(v1, ShapeValue)
        let v2 = untag_reference(v2, ShapeValue)
        if v1.meta !== v2.meta:
          return false

        # As the meta shapes are equal, the property values must be in the same order.
        for i in 0 ..< v1.property_count:
          if not rec(v1.property_values[i], v2.property_values[i]):
            return false
        true
      of Kind.Struct:
        let v1 = untag_reference(v1, StructValue)
        let v2 = untag_reference(v2, StructValue)
        if v1.struct_type.schema !== v2.struct_type.schema:
          return false

        # As the struct schemas are equal, the property values must be in the same order.
        for i in 0 ..< v1.property_count:
          if not rec(v1.property_values[i], v2.property_values[i]):
            return false
        true
      else:
        # Functions can only be referentially equal, so they are covered by this case.
        false
    elif kind1 == Kind.Int and kind2 == Kind.Real:
      float64(untag_int(v1)) == untag_reference(v2, RealValue).real
    elif kind1 == Kind.Real and kind2 == Kind.Int:
      untag_reference(v1, RealValue).real == float64(untag_int(v2))
    else: false
  else: false

proc is_less_than*(v1: TaggedValue, v2: TaggedValue, rec: (TaggedValue, TaggedValue) -> bool): bool =
  ## Whether `v1` is less than `v2` under the rules of default comparisons, using `rec` to compare sub-values. This
  ## function is the default implementation for `lore.core.less_than?`.
  let kind1 = v1.get_kind
  let kind2 = v2.get_kind
  if kind1 != kind2:
    if kind1 == Kind.Int and kind2 == Kind.Real:
      float64(untag_int(v1)) < untag_reference(v2, RealValue).real
    elif kind1 == Kind.Real and kind2 == Kind.Int:
      untag_reference(v1, RealValue).real < float64(untag_int(v2))
    else:
      ord(kind1) < ord(kind2)
  elif kind1 == Kind.Int: untag_int(v1) < untag_int(v2)
  elif kind1 == Kind.Boolean: untag_boolean(v1) < untag_boolean(v2)
  else:
    case kind1
    of Kind.Real:
      untag_reference(v1, RealValue).real < untag_reference(v2, RealValue).real
    of Kind.String:
      # This is a byte-wise comparison, but yields the same results as a code point-wise comparison in UTF-8. This
      # should use the same approach as the instructions `StringLt` and `StringLte`.
      untag_reference(v1, StringValue).string < untag_reference(v2, StringValue).string
    of Kind.Symbol:
      untag_reference(v1, SymbolValue).name < untag_reference(v2, SymbolValue).name
    of Kind.Tuple:
      let v1 = untag_reference(v1, TupleValue)
      let v2 = untag_reference(v2, TupleValue)
      if v1.elements.len != v2.elements.len:
        return v1.elements.len < v2.elements.len
      for i in 0 ..< v1.elements.len:
        if rec(v1.elements[i], v2.elements[i]):
          return true
        if rec(v2.elements[i], v1.elements[i]):
          return false
      false
    of Kind.List:
      let v1 = untag_reference(v1, ListValue)
      let v2 = untag_reference(v2, ListValue)
      let length = min(v1.elements.len, v2.elements.len)
      for i in 0 ..< length:
        if rec(v1.elements[i], v2.elements[i]):
          return true
        if rec(v2.elements[i], v1.elements[i]):
          return false
      # At this point, all elements are equal. If `v1` is the shorter list, it'll be less than `v2`.
      v1.elements.len < v2.elements.len
    of Kind.Shape:
      let v1 = untag_reference(v1, ShapeValue)
      let v2 = untag_reference(v2, ShapeValue)
      if v1.property_count == v2.property_count:
        if v1.meta === v2.meta:
          # `v1` and `v2` have the same property names, so we can compare the values.
          for index in 0 ..< v1.property_count:
            let p1 = v1.property_values[index]
            let p2 = v2.property_values[index]
            if rec(p1, p2):
              return true
            if rec(p2, p1):
              return false
          false
        else:
          # `v1` and `v2` have different property names, so we need to compare the name lists.
          v1.meta.property_names < v2.meta.property_names
      else:
        v1.property_count < v2.property_count
    of Kind.Struct:
      let v1 = untag_reference(v1, StructValue)
      let v2 = untag_reference(v2, StructValue)
      let schema1 = v1.get_schema
      let schema2 = v2.get_schema
      if schema1 === schema2:
        for property_name in schema1.property_declaration_order:
          let offset = schema1.property_index.find_offset(property_name)
          let p1 = v1.property_values[offset]
          let p2 = v2.property_values[offset]
          if rec(p1, p2):
            return true
          if rec(p2, p1):
            return false
        false
      else:
        # The structs can't be compared directly. To still induce a default order, structs of differing schemas are
        # ordered by their type name.
        schema1.name < schema2.name
    else:
      # Functions have no default order.
      false

########################################################################################################################
# Stringification.                                                                                                     #
########################################################################################################################

proc stringify*(tagged_value: TaggedValue, rec: TaggedValue -> string): string =
  ## Stringifies the given value, using `rec` to stringify sub-values.
  let tag = get_tag(tagged_value)
  if tag == TagReference:
    let value = untag_reference(tagged_value)
    if value !== nil: stringify(value, rec)
    else: "nil"
  elif tag == TagInt:
    $untag_int(tagged_value)
  elif tag == TagBoolean:
    if tagged_value === True: "true"
    else: "false"
  else:
    "unknown"

proc stringify*(value: Value, rec: TaggedValue -> string): string =
  ## Stringifies the given value, using `rec` to stringify sub-values.
  case value.tpe.kind
  of Kind.Real: $cast[RealValue](value).real
  of Kind.String: cast[StringValue](value).string
  of Kind.Symbol:
    let symbol = cast[SymbolValue](value)
    "#" & symbol.name
  of Kind.Tuple:
    let tpl = cast[TupleValue](value)
    "(" & $tpl.elements.map_it(string, rec(it)).join(", ") & ")"
  of Kind.Function:
    # TODO (vm): We should print the name instead of the target address, but this is non-trivial when avoiding cyclic dependencies.
    let function = cast[FunctionValue](value)
    case function.variant
    of FunctionValueVariant.Multi: "<multi-function: " & $cast[uint](function.target) & ">"
    of FunctionValueVariant.Single: "<single function: " & $cast[uint](function.target) & ">"
  of Kind.List:
    let list = cast[ListValue](value)
    "[" & $list.elements.map_it(string, rec(it)).join(", ") & "]"
  of Kind.Shape:
    let shape = cast[ShapeValue](value)
    var properties = new_seq[string]()
    if shape.property_count == 0:
      "%{ }"
    else:
      for i in 0 ..< shape.property_count:
        properties.add(shape.meta.property_names[i] & ": " & rec(shape.property_values[i]))
      "%{ " & properties.join(", ") & " }"
  of Kind.Struct:
    let struct = cast[StructValue](value)
    let schema = struct.get_schema
    var properties = new_seq[string]()
    for property_name in schema.property_declaration_order:
      properties.add(rec(struct.get_property_value(property_name)))
    schema.name & "(" & properties.join(", ") & ")"
  else: "unknown"

proc `$`*(tagged_value: TaggedValue): string = stringify(tagged_value, v => $v)

proc `$`*(value: Value): string = stringify(value, v => $v)
