from std/math import nil
import std/strformat
import std/strutils
from std/unicode import runeLen, runeAt, runeAtPos, toUTF8

from definitions import FramePtr, Intrinsic, IntrinsicFunction
from evaluator import nil
import imseqs
import types
import values

type Arguments = open_array[TaggedValue]

template arg(index: int): TaggedValue = arguments[index]
template arg_int(index: int): int64 = untag_int(arg(index))
template arg_real(index: int): float64 = untag_reference(arg(index), RealValue).real
template arg_string(index: int): string = untag_reference(arg(index), StringValue).string
template arg_function(index: int): FunctionValue = untag_reference(arg(index), FunctionValue)
template arg_list(index: int): ListValue = untag_reference(arg(index), ListValue)
template arg_symbol(index: int): SymbolValue = untag_reference(arg(index), SymbolValue)

var error_symbol: TaggedValue = TaggedValue(cast[uint64](nil))

proc get_error_symbol(): TaggedValue =
  ## Returns a symbol value `#error`, which is used by various intrinsics to communicate an error to Pyramid. We cannoi
  ## use Option because it's not defined within the VM.
  if untag_reference(error_symbol) == nil:
    error_symbol = new_symbol_value_tagged("error")
  error_symbol

proc core_equal(frame: FramePtr, arguments: Arguments): TaggedValue =
  # TODO (assembly): Implement.
  quit("`core_equal` is not yet implemented.")

proc core_less_than(frame: FramePtr, arguments: Arguments): TaggedValue =
  # TODO (assembly): Implement.
  quit("`core_less_than` is not yet implemented.")

proc core_to_string(frame: FramePtr, arguments: Arguments): TaggedValue =
  values.new_string_value_tagged($arg(0))

proc core_panic(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## panic(message: String): Nothing
  quit(fmt"Panic: {arg(0)}")

proc int_to_real(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## to_real(value: Int): Real
  let value = arg_int(0)
  new_real_value_tagged(float64(value))

proc real_to_int(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## to_int(value: Real): Int
  tag_int(int64(arg_real(0)))

proc real_parse(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## parse(value: String): Real | #error
  try:
    new_real_value_tagged(parse_float(arg_string(0)))
  except ValueError:
    get_error_symbol()

proc real_is_nan(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## nan?(value: Real): Boolean
  tag_boolean(arg_real(0) == NaN)

proc string_length(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## length(string: String): Int
  tag_int(arg_string(0).runeLen)

proc string_at(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## at(string: String, position: Int): String
  let rune = arg_string(0).runeAtPos(int(arg_int(1)))
  new_string_value_tagged(rune.toUTF8)

proc string_at_index(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## at_index(string: String, index: Int): String
  let rune = arg_string(0).runeAt(arg_int(1))
  new_string_value_tagged(rune.toUTF8)

proc string_byte_size(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## byte_size(string: String): Int
  tag_int(arg_string(0).len)

proc string_byte_at(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## byte_at(string: String, index: Int): Int
  tag_int(arg_string(0)[arg_int(1)].ord)

proc list_concat(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## concat(list1: [A], list2: [B]): [A | B]
  let list1 = arg_list(0)
  let list2 = arg_list(1)
  let elements = concat(list1.elements, list2.elements)
  let tpe =
    # TODO (vm): This kind of optimization should be baked into `sum_simplified`.
    if are_equal(list1.tpe, list2.tpe): list1.tpe
    else: new_list_type(sum_simplified([list1.element_type, list2.element_type]))
  new_list_value_tagged(elements, tpe)

proc list_slice(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## slice(list: [A], start: Int, length: Int): [A]
  let list = arg_list(0)
  let start = arg_int(1)
  let length = arg_int(2)
  let elements = list.elements.slice(int(start), int(length))
  new_list_value_tagged(elements, list.tpe)

proc list_flatten(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## flatten(lists: [[A]]): [A]
  let lists = arg_list(0)
  let elements = lists.elements.flatten_it(TaggedValue, cast[ListValue](it).elements)
  new_list_value_tagged(elements, lists.element_type)

proc list_map(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## map(list: [A], f: B => C): [C] where A, B >: A, C
  let list = arg_list(0)
  let function = arg_function(1)
  let elements = list.elements.map_it(TaggedValue, evaluator.evaluate_function_value(function, frame, it))
  let tpe = new_list_type(cast[FunctionType](function.tpe).output)
  new_list_value_tagged(elements, tpe)

proc list_flat_map(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## flat_map(list: [A], f: B => [C]): [C] where A, B >: A, C
  let list = arg_list(0)
  let function = arg_function(1)
  let elements = list.elements.flat_map_it(
    TaggedValue,
    cast[ListValue](evaluator.evaluate_function_value(function, frame, it)).elements,
  )
  let tpe = cast[FunctionType](function.tpe).output
  new_list_value_tagged(elements, tpe)

proc list_each(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## each(list: [A], f: B => Unit): Unit
  let list = arg_list(0)
  let function = arg_function(1)
  for element in list.elements:
    discard evaluator.evaluate_function_value(function, frame, element)
  values.unit_value_tagged

proc list_filter(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## filter(list: [A], predicate: B => Boolean): [A] where A, B >: A
  let list = arg_list(0)
  let predicate = arg_function(1)
  let elements = list.elements.filter_it(untag_boolean(evaluator.evaluate_function_value(predicate, frame, it)))
  new_list_value_tagged(elements, list.tpe)

proc symbol_name(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## name(value: Any): String
  new_string_value_tagged(arg_symbol(0).name)

proc io_println(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## println(value: Any): Unit
  echo arg(0)
  values.unit_value_tagged

proc math_floor(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## floor(x: Real): Int
  tag_int(math.floor(arg_real(0)).int64)

proc math_ceil(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## ceil(x: Real): Int
  tag_int(math.ceil(arg_real(0)).int64)

proc math_round(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## round(x: Real): Int
  tag_int(math.round(arg_real(0)).int64)

proc math_remainder_int(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## remainder(a: Int, b: Int): Int
  tag_int(arg_int(0) mod arg_int(1))

proc math_pow(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## pow(base: Real, exponent: Real): Real
  let base = arg_real(0)
  let exponent = arg_real(1)
  new_real_value_tagged(math.pow(base, exponent))

proc intr(name: string, function: IntrinsicFunction, arity: int): Intrinsic {.inline.} =
  Intrinsic(name: name, function: function, arity: arity)

let intrinsics*: seq[Intrinsic] = @[
  intr("lore.core.equal?", core_equal, 2),
  intr("lore.core.less_than?", core_less_than, 2),
  intr("lore.core.to_string", core_to_string, 1),
  intr("lore.core.panic", core_panic, 1),

  intr("lore.int.to_real", int_to_real, 1),

  intr("lore.real.to_int", real_to_int, 1),
  intr("lore.real.parse", real_parse, 1),
  intr("lore.real.nan?", real_is_nan, 1),

  intr("lore.string.length", string_length, 1),
  intr("lore.string.at!", string_at, 2),
  intr("lore.string.at_index!", string_at_index, 2),
  intr("lore.string.byte_size", string_byte_size, 1),
  intr("lore.string.byte_at!", string_byte_at, 2),

  intr("lore.list.concat", list_concat, 2),
  intr("lore.list.slice", list_slice, 3),
  intr("lore.list.flatten", list_flatten, 1),
  intr("lore.list.map", list_map, 2),
  intr("lore.list.flat_map", list_flat_map, 2),
  intr("lore.list.each", list_each, 2),
  intr("lore.list.filter", list_filter, 2),

  intr("lore.symbol.name", symbol_name, 1),

  intr("lore.io.println", io_println, 1),

  intr("lore.math.floor", math_floor, 1),
  intr("lore.math.ceil", math_ceil, 1),
  intr("lore.math.round", math_round, 1),
  intr("lore.math.remainder[int]", math_remainder_int, 2),
  intr("lore.math.pow", math_pow, 2),
]
