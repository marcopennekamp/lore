import std/strformat, std/strutils

from types import Kind, Type

type
  ## A TaggedValue is a compact representation of a Lore value. Every Lore value needs to carry type information at run
  ## time, including the primitives Int, Real, and Boolean. This is because at times, the compiler might encounter a
  ## sum type such as `Int | Real` or `Real | #error`. Such a type might stand in a list, or as a parameter, or
  ## anywhere else. Especially in storage situations (such as lists), we want to tag values as efficiently as possible
  ## without the need to create separate List implementations for each primitive.
  ##
  ## We can keep the size of TaggedValue to 8 bytes with a technique called pointer tagging. Essentially, on 64-bit
  ## systems the lower three bits of a pointer are always 0 due to alignment, so we can use these bits to tag Ints,
  ## Reals, Booleans, and references. This reduces the range of an Int, for example, from 64 bits to 61 bits, but we
  ## don't anticipate any issues with that in the foreseeable future. There are no plans to port Lore to 32-bit
  ## systems.
  ##
  ## There is a slight performance cost associated with TaggedValues, as every time we want to use an Int or a Real, we
  ## have to untag it using a shift. Every time we want to create an Int or Real, we have to shift the value and OR the
  ## tag bits. Reference access is free, as the reference tag is 0. Booleans we don't need shifts, because there are
  ## only two possible values: `0b0011` for false and `0b1011` for true, which includes the tag bits.
  ##
  ## Strings must be represented as reference Values, because a string's reference must be discoverable by the garbage
  ## collector. If we tag a string, its reference is going to be obfuscated and the string might be collected
  ## prematurely.
  TaggedValue* {.union.} = object
    uint*: uint64
    reference: Value
    int: int64

  ## The pragmas `inheritable` and `pure` omit the `m_type` pointer from each value instance, as `tpe` already
  ## sufficiently distinguishes between value kinds.
  Value* {.inheritable, pure.} = ref object
    tpe*: Type

  ## Real values are currently boxed, because a 64-bit float doesn't fit into a 64-bit tagged pointer. The technique to
  ## use would be nanboxing, but that is not quite future-proof because it relies on pointers only using the lower 48
  ## bits.
  ##
  ## There are other Real optimizations which we may try first. For example, we can introduce specific operations which
  ## work with unboxed Reals. If a function parameter is definitely a Real, we also don't need to pass the boxed value
  ## to it, nor require the function itself to work with a boxed value. These and other optimizations may relieve us of
  ## the burden of nanboxing.
  RealValue* {.pure.} = ref object of Value
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
    elements*: seq[TaggedValue]

  ## A FunctionValue is either a fixed function value (pointing to a fixed function or a lambda) or a multi-function
  ## value. This is determined by the flag `is_fixed`. The implementation of the `target` is either a `Function` or a
  ## `MultiFunction` reference. The actual type is hidden inside the module `definitions` to avoid cyclic dependencies
  ## between Nim modules.
  FunctionValue* {.pure, shallow.} = ref object of Value
    is_fixed*: bool
    target*: pointer

  ListValue* {.pure, shallow.} = ref object of Value
    elements*: seq[TaggedValue]

  SymbolValue* {.pure, shallow.} = ref object of Value
    name*: string

const
  ## This mask can filter out the lowest three tag bits of a pointer.
  TagMask: uint64 = 0b111
  TagReference*: uint64 = 0b000
  TagInt*: uint64 = 0b001
  TagBoolean*: uint64 = 0b010
  False*: uint64 = 0 or TagBoolean
  True*: uint64 = (1 shl 3) or TagBoolean

proc type_of*(value: TaggedValue): Type

proc get_tag*(value: TaggedValue): uint64 = value.uint and TagMask

proc is_reference*(value: TaggedValue): bool = get_tag(value) == TagReference
proc is_int*(value: TaggedValue): bool = get_tag(value) == TagInt
proc is_boolean*(value: TaggedValue): bool = get_tag(value) == TagBoolean

proc tag_reference*(value: Value): TaggedValue = TaggedValue(reference: value)
proc untag_reference*(value: TaggedValue): Value = value.reference
template untag_reference*(value: TaggedValue, tpe: untyped): untyped = cast[tpe](untag_reference(value))

proc tag_int*(value: int64): TaggedValue = TaggedValue(int: (value shl 3) or cast[int64](TagInt))
proc untag_int*(value: TaggedValue): int64 = value.int shr 3

proc tag_boolean*(value: bool): TaggedValue = TaggedValue(uint: if value: True else: False)
proc untag_boolean*(value: TaggedValue): bool = value.uint == True

proc new_real*(value: float64): Value = RealValue(tpe: types.real, real: value)
proc new_real_tagged*(value: float64): TaggedValue = tag_reference(new_real(value))

proc new_string*(value: string): Value = StringValue(tpe: types.string, string: value)
proc new_string_tagged*(value: string): TaggedValue = tag_reference(new_string(value))

## Creates a new tuple, forcing its type to be `tpe` instead of taking the type from the elements.
proc new_tuple*(elements: seq[TaggedValue], tpe: Type): Value = TupleValue(tpe: tpe, elements: elements)
proc new_tuple_tagged*(elements: seq[TaggedValue], tpe: Type): TaggedValue = tag_reference(new_tuple(elements, tpe))

proc new_tuple*(elements: seq[TaggedValue]): Value =
  var element_types = new_seq_of_cap[Type](elements.len)
  for element in elements:
    element_types.add(type_of(element))
  new_tuple(elements, types.tpl(element_types))

proc new_tuple_tagged*(elements: seq[TaggedValue]): TaggedValue = tag_reference(new_tuple(elements))

let unit*: TaggedValue = new_tuple_tagged(@[])

proc new_function*(is_fixed: bool, target: pointer, tpe: Type): Value = FunctionValue(tpe: tpe, is_fixed: is_fixed, target: target)
proc new_function_tagged*(is_fixed: bool, target: pointer, tpe: Type): TaggedValue = tag_reference(new_function(is_fixed, target, tpe))

proc new_list*(elements: seq[TaggedValue], tpe: Type): Value = ListValue(tpe: tpe, elements: elements)
proc new_list_tagged*(elements: seq[TaggedValue], tpe: Type): TaggedValue = tag_reference(new_list(elements, tpe))

proc new_symbol*(name: string): Value = SymbolValue(tpe: types.symbol(name), name: name)
proc new_symbol_tagged*(name: string): TaggedValue = tag_reference(new_symbol(name))

proc type_of*(value: TaggedValue): Type =
  let tag = get_tag(value)
  if tag == TagReference:
    untag_reference(value).tpe
  elif tag == TagInt:
    types.int
  elif tag == TagBoolean:
    types.boolean
  else:
    quit(fmt"Unknown tag {tag}.")

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
    if tagged_value.uint == True: "true"
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
    "(" & $tpl.elements & ")"
  of Kind.Function:
    # TODO (vm): We should print the name instead of the target address, but this is non-trivial when avoiding cyclic dependencies.
    let function = cast[FunctionValue](value)
    if function.is_fixed:
      "<fixed function: " & $cast[uint](function.target) & ">"
    else:
      "<multi-function: " & $cast[uint](function.target) & ">"
  of Kind.List:
    let list = cast[ListValue](value)
    "[" & $list.elements & "]"
  of Kind.Symbol:
    let symbol = cast[SymbolValue](value)
    "#" & symbol.name
  else: "unknown"
