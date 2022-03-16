import std/strformat
import std/strutils

from definitions import FramePtr, Intrinsic, IntrinsicFunction
from evaluator import nil
import imseqs
import values

var error_symbol: TaggedValue = TaggedValue(cast[uint64](nil))

proc get_error_symbol(): TaggedValue =
  ## Returns a symbol value `#error`, which is used by various intrinsics to communicate an error to Pyramid. We cannoi
  ## use Option because it's not defined within the VM.
  if untag_reference(error_symbol) == nil:
    error_symbol = new_symbol_value_tagged("error")
  error_symbol

proc core_equal(a: TaggedValue, b: TaggedValue): TaggedValue =
  # TODO (assembly): Implement.
  quit("`core_equal` is not yet implemented.")

proc core_less_than(a: TaggedValue, b: TaggedValue): TaggedValue =
  # TODO (assembly): Implement.
  quit("`core_less_than` is not yet implemented.")

proc core_to_string(value: TaggedValue): TaggedValue =
  values.new_string_value_tagged($value)

proc core_panic(message: TaggedValue): TaggedValue =
  quit(fmt"Panic: {message}")

proc int_to_real(value: TaggedValue): TaggedValue =
  let value = untag_int(value)
  new_real_value_tagged(float64(value))

proc real_to_int(value: TaggedValue): TaggedValue =
  ## real_to_int(value: Real): Int
  let value = untag_reference(value, RealValue)
  tag_int(int64(value.real))

proc real_parse(value: TaggedValue): TaggedValue =
  ## real_parse(value: String): Real | #error
  let string = untag_reference(value, StringValue).string
  try:
    new_real_value_tagged(parse_float(string))
  except ValueError:
    get_error_symbol()

proc real_is_nan(value: TaggedValue): TaggedValue =
  let value = untag_reference(value, RealValue)
  tag_boolean(value.real == NaN)

proc string_length(string: TaggedValue): TaggedValue =
  let string = untag_reference(string, StringValue)
  tag_int(string.string.len)

proc list_each(frame: FramePtr, list: TaggedValue, function: TaggedValue): TaggedValue =
  let list = untag_reference(list, ListValue)
  let function = untag_reference(function, FunctionValue)
  for element in list.elements:
    discard evaluator.evaluate_function_value(function, frame, element)
  values.unit_value_tagged

proc symbol_name(value: TaggedValue): TaggedValue =
  let symbol = untag_reference(value, SymbolValue)
  new_string_value_tagged(symbol.name)

proc io_println(value: TaggedValue): TaggedValue =
  echo value
  values.unit_value_tagged

template nullary(arg_name, arg_function): untyped = Intrinsic(name: arg_name, is_frame_aware: false, function: IntrinsicFunction(nullary: arg_function))
template unary(arg_name, arg_function): untyped = Intrinsic(name: arg_name, is_frame_aware: false, function: IntrinsicFunction(unary: arg_function))
template unary_fa(arg_name, arg_function): untyped = Intrinsic(name: arg_name, is_frame_aware: true, function: IntrinsicFunction(unary_fa: arg_function))
template binary(arg_name, arg_function): untyped = Intrinsic(name: arg_name, is_frame_aware: false, function: IntrinsicFunction(binary: arg_function))
template binary_fa(arg_name, arg_function): untyped = Intrinsic(name: arg_name, is_frame_aware: true, function: IntrinsicFunction(binary_fa: arg_function))

let intrinsics*: seq[Intrinsic] = @[
  binary("lore.core.equal?", core_equal),
  binary("lore.core.less_than?", core_less_than),
  unary("lore.core.to_string", core_to_string),
  unary("lore.core.panic", core_panic),

  unary("lore.int.to_real", int_to_real),

  unary("lore.real.to_int", real_to_int),
  unary("lore.real.parse", real_parse),
  unary("lore.real.nan?", real_is_nan),

  #binary("lore.string.at", string_at),
  unary("lore.string.length", string_length),

  binary_fa("lore.list.each", list_each),

  unary("lore.symbol.name", symbol_name),

  unary("lore.io.println", io_println),
]
